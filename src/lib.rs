use core::fmt;
use encoding::all::WINDOWS_1252;
use encoding::{DecoderTrap, Encoding};
use std::cmp::Ordering;
use std::ffi::{c_void, CStr};
use std::os::raw::c_char;
use std::rc::Rc;
use std::slice;
use std::time::Duration;
use windows::Win32::Foundation::{
    CloseHandle, GetLastError, HANDLE, HWND, LPARAM, WIN32_ERROR, WPARAM,
};
use windows::Win32::System::{Memory, Threading};
use windows::Win32::UI::WindowsAndMessaging::{RegisterWindowMessageA, SendNotifyMessageA};

pub mod flags;

const IRSDK_MAX_BUFS: usize = 4;
const IRSDK_MAX_STRING: usize = 32;
// descriptions can be longer than max_string!
const IRSDK_MAX_DESC: usize = 64;

// HWND_BROADCAST doesn't appear to be in windows crate?
const HWND_BROADCAST: HWND = HWND(0xFFFF);

// define markers for unlimited session lap and time
pub const IRSDK_UNLIMITED_LAPS: i32 = 32767;
pub const IRSDK_UNLIMITED_TIME: f64 = 604800.0;

pub struct Client {
    conn: Option<Rc<Connection>>,
    session_id: i32, // Incremented each time we issue a new session. Allows for session to determine its expired even if
                     // iRacing started a new session.
}
impl Client {
    pub fn new() -> Client {
        Client {
            conn: None,
            session_id: 0,
        }
    }
    // Attempts to connect to iracing if we're not already.
    // Returns true if we're now connected (or was already connected), false otherwise
    unsafe fn connect(&mut self) -> bool {
        match &self.conn {
            Some(c) => c.connected(),
            None => match Connection::new() {
                Ok(c) => {
                    let result = c.connected();
                    self.conn = Some(Rc::new(c));
                    result
                }
                Err(_) => false,
            },
        }
    }
    pub unsafe fn session(&mut self) -> Option<Session> {
        if !self.connect() {
            None
        } else {
            let sid = self.session_id;
            self.session_id += 1;
            let mut s = Session {
                session_id: sid,
                conn: self.conn.as_ref().unwrap().clone(),
                last_tick_count: 0,
                data: bytes::BytesMut::new(),
                expired: false,
            };
            if DataUpdateResult::Updated == s.get_new_data() {
                Some(s)
            } else {
                None
            }
        }
    }
    // TODO wait_for_session()
}

#[derive(Debug, PartialEq)]
pub enum DataUpdateResult {
    Updated,
    NoUpdate,
    FailedToCopyRow,
    SessionExpired,
}

#[derive(Debug)]
pub struct Session {
    session_id: i32,
    conn: Rc<Connection>,
    last_tick_count: i32,
    data: bytes::BytesMut,
    expired: bool,
}
impl Session {
    pub unsafe fn connected(&self) -> bool {
        !self.expired()
    }
    pub unsafe fn expired(&self) -> bool {
        self.expired || (!self.conn.connected())
    }
    pub unsafe fn wait_for_data(&mut self, wait: Duration) -> DataUpdateResult {
        let r = self.get_new_data();
        if r == DataUpdateResult::NoUpdate {
            self.conn.wait_for_new_data(wait);
            self.get_new_data()
        } else {
            r
        }
    }
    pub unsafe fn get_new_data(&mut self) -> DataUpdateResult {
        if self.expired() {
            self.expired = true;
            return DataUpdateResult::SessionExpired;
        }
        let (buf_hdr, row) = self.conn.lastest();
        match buf_hdr.tick_count.cmp(&self.last_tick_count) {
            Ordering::Greater => {
                for _tries in 0..2 {
                    let curr_tick_count = buf_hdr.tick_count;
                    self.data.clear();
                    self.data.extend_from_slice(row);
                    if curr_tick_count == buf_hdr.tick_count {
                        self.last_tick_count = curr_tick_count;
                        return DataUpdateResult::Updated;
                    }
                }
                DataUpdateResult::FailedToCopyRow
            }
            Ordering::Less => {
                // If ours is newer than the latest then iRacing has started a new
                // session and this one is done.
                self.expired = true;
                DataUpdateResult::SessionExpired
            }
            Ordering::Equal => DataUpdateResult::NoUpdate,
        }
    }
    pub unsafe fn dump_vars(&self) {
        for var_header in self.conn.variables() {
            let var = Var {
                hdr: *var_header,
                session_id: self.session_id,
            };
            let value = self.var_value(&var);
            println!(
                "{:40} {:32}: {:?}: {}: {}: {:?}",
                var.desc(),
                var.name(),
                var.var_type(),
                var.count(),
                var.hdr.count_as_time,
                value,
            );
        }
    }
    pub unsafe fn find_var(&self, name: &str) -> Option<Var> {
        for var_header in self.conn.variables() {
            if var_header.has_name(name) {
                return Some(Var {
                    hdr: *var_header,
                    session_id: self.session_id,
                });
            }
        }
        None
    }
    pub unsafe fn var_value(&self, var: &Var) -> Value {
        assert_eq!(
            var.session_id, self.session_id,
            "programmer error, Var was issued by a different Session"
        );
        let x = self.data.as_ptr().add(var.hdr.offset as usize);
        if var.hdr.count == 1 {
            match var.hdr.var_type {
                VarType::Char => Value::Char(*x),
                VarType::Bool => Value::Bool(*(x as *const bool)),
                VarType::Int => Value::Int(*(x as *const i32)),
                VarType::Bitfield => Value::Bitfield(*(x as *const i32)),
                VarType::Float => Value::Float(*(x as *const f32)),
                VarType::Double => Value::Double(*(x as *const f64)),
                _ => todo!(), // ETCount
            }
        } else {
            let l = var.count();
            match var.hdr.var_type {
                VarType::Char => Value::Chars(slice::from_raw_parts(x, l)),
                VarType::Bool => Value::Bools(slice::from_raw_parts(x as *const bool, l)),
                VarType::Int => Value::Ints(slice::from_raw_parts(x as *const i32, l)),
                VarType::Bitfield => Value::Bitfields(slice::from_raw_parts(x as *const i32, l)),
                VarType::Float => Value::Floats(slice::from_raw_parts(x as *const f32, l)),
                VarType::Double => Value::Doubles(slice::from_raw_parts(x as *const f64, l)),
                _ => todo!(), // ETCount
            }
        }
    }
    pub unsafe fn value<T: FromValue>(&self, var: &Var) -> Result<T, Error> {
        let v = self.var_value(var);
        T::var_result(&v)
    }
    pub unsafe fn session_info_update(&self) -> i32 {
        (*self.conn.header).session_info_update
    }
    pub unsafe fn session_info(&self) -> String {
        let bytes = self.conn.session_info();
        // as we're using replace, this should not ever return an error
        WINDOWS_1252.decode(bytes, DecoderTrap::Replace).unwrap()
    }

    pub unsafe fn broadcast_msg(&self, msg: flags::BroadcastMsg) -> Result<(), WIN32_ERROR> {
        let (cmd_msg_id, (var1, var2)) = msg.params();
        let x = makelong(cmd_msg_id, var1);
        let r = SendNotifyMessageA(
            HWND_BROADCAST,
            self.conn.broadcast_msg_id,
            WPARAM(x),
            LPARAM(var2),
        );
        if r.as_bool() {
            Ok(())
        } else {
            Err(GetLastError())
        }
    }
}
fn makelong(var1: i16, var2: i16) -> usize {
    // aka MAKELONG
    let x = ((var1 as u32 & 0xFFFFu32) as u32) | (((var2 as u32) & 0xFFFFu32) << 16);
    x as usize
}

pub struct Var {
    hdr: IrsdkVarHeader,
    session_id: i32,
}
impl Var {
    pub fn var_type(&self) -> VarType {
        self.hdr.var_type
    }
    pub fn name(&self) -> &str {
        self.hdr.name().unwrap()
    }
    pub fn desc(&self) -> &str {
        self.hdr.desc().unwrap()
    }
    pub fn unit(&self) -> &str {
        self.hdr.unit().unwrap()
    }
    pub fn count(&self) -> usize {
        self.hdr.count as usize
    }
}
impl fmt::Debug for Var {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{} {:?} ({})", self.name(), self.var_type(), self.desc())
    }
}

#[derive(Debug)]
struct Connection {
    file_mapping: HANDLE,
    shared_mem: *mut c_void,
    header: *mut IrsdkHeader,
    new_data: HANDLE,
    broadcast_msg_id: u32,
}
impl Connection {
    // This will return an error if iRacing is not running.
    // However once you have a Connection it will remain usable even if iracing is exited and started again.
    unsafe fn new() -> Result<Self, WIN32_ERROR> {
        let file_mapping =
            Memory::OpenFileMappingA(Memory::FILE_MAP_READ.0, false, "Local\\IRSDKMemMapFileName");
        if file_mapping.is_invalid() {
            return Err(GetLastError());
        }
        let shared_mem = Memory::MapViewOfFile(file_mapping, Memory::FILE_MAP_READ, 0, 0, 0);
        if shared_mem.is_null() {
            let e = Err(GetLastError());
            CloseHandle(file_mapping);
            return e;
        }
        let new_data = Threading::OpenEventA(
            windows::Win32::Storage::FileSystem::SYNCHRONIZE.0,
            false,
            "Local\\IRSDKDataValidEvent",
        );
        if new_data.is_invalid() {
            let e = Err(GetLastError());
            Memory::UnmapViewOfFile(shared_mem);
            CloseHandle(file_mapping);
            return e;
        }
        let bc_id = RegisterWindowMessageA("IRSDK_BROADCASTMSG");
        if bc_id == 0 {
            let e = Err(GetLastError());
            CloseHandle(new_data);
            Memory::UnmapViewOfFile(shared_mem);
            CloseHandle(file_mapping);
            return e;
        }
        Ok(Connection {
            file_mapping,
            shared_mem,
            header: shared_mem as *mut IrsdkHeader,
            new_data,
            broadcast_msg_id: bc_id,
        })
    }
    unsafe fn connected(&self) -> bool {
        (*self.header)
            .status
            .intersects(flags::StatusField::CONNECTED)
    }
    unsafe fn variables(&self) -> &[IrsdkVarHeader] {
        let vhbase = self
            .shared_mem
            .add((*self.header).var_header_offset as usize)
            as *const IrsdkVarHeader;
        slice::from_raw_parts(vhbase, (*self.header).num_vars as usize)
    }
    unsafe fn buffers(&self) -> &[IrsdkBuf] {
        let l = (*self.header).num_buf as usize;
        assert!(l <= IRSDK_MAX_BUFS);
        &(*self.header).var_buf[..l]
    }
    // returns the telemetry buffer with the highest tick count, along with the actual data
    // this is the buffer in the shared mem, so you should copy it.
    unsafe fn lastest(&self) -> (&IrsdkBuf, &[u8]) {
        let b = self.buffers();
        let mut latest = &b[0];
        for buff in b {
            if buff.tick_count > latest.tick_count {
                latest = buff;
            }
        }
        let buf_len = (*self.header).buf_len as usize;
        let src = self.shared_mem.add(latest.buf_offset as usize);
        return (latest, slice::from_raw_parts(src as *const u8, buf_len));
    }
    unsafe fn wait_for_new_data(&self, wait: Duration) {
        Threading::WaitForSingleObject(self.new_data, wait.as_millis().try_into().unwrap());
    }
    unsafe fn session_info(&self) -> &[u8] {
        let p = self
            .shared_mem
            .add((*self.header).session_info_offset as usize) as *const u8;
        let mut bytes = std::slice::from_raw_parts(p, (*self.header).session_info_len as usize);
        // session_info_len is the size of the buffer, not necessarily the size of the string
        // so we have to look for the null terminatior.
        for i in 0..bytes.len() {
            if bytes[i] == 0 {
                bytes = &bytes[0..i];
                break;
            }
        }
        bytes
    }
}
impl Drop for Connection {
    fn drop(&mut self) {
        unsafe {
            CloseHandle(self.new_data);
            Memory::UnmapViewOfFile(self.shared_mem);
            windows::Win32::Foundation::CloseHandle(self.file_mapping);
        }
    }
}
#[repr(C)]
struct IrsdkBuf {
    tick_count: i32, // used to detect changes in data
    buf_offset: i32, // offset from header
    pad: [i32; 2],   // (16 byte align)
}

#[repr(C)]
struct IrsdkHeader {
    ver: i32,                   // this api header version, see IRSDK_VER
    status: flags::StatusField, // bitfield using irsdk_StatusField
    tick_rate: i32,             // ticks per second (60 or 360 etc)

    // session information, updated periodicaly
    session_info_update: i32, // Incremented when session info changes
    session_info_len: i32,    // Length in bytes of session info string
    session_info_offset: i32, // Session info, encoded in YAML format

    // State data, output at tickRate
    num_vars: i32,          // length of array pointed to by varHeaderOffset
    var_header_offset: i32, // offset to irsdk_varHeader[numVars] array, Describes the variables received in varBuf

    num_buf: i32,                        // <= IRSDK_MAX_BUFS (3 for now)
    buf_len: i32,                        // length in bytes for one line
    pad1: [i32; 2],                      // (16 byte align)
    var_buf: [IrsdkBuf; IRSDK_MAX_BUFS], // buffers of data being written to
}

#[repr(C)]
#[derive(Clone, Copy, Debug)]
struct IrsdkVarHeader {
    var_type: VarType, // irsdk_VarType
    offset: i32,       // offset fron start of buffer row
    count: i32, // number of entrys (array) so length in bytes would be irsdk_VarTypeBytes[type] * count

    count_as_time: u8,
    pad: [i8; 3], // (16 byte align)

    name: [u8; IRSDK_MAX_STRING],
    desc: [u8; IRSDK_MAX_DESC],
    unit: [u8; IRSDK_MAX_STRING], // something like "kg/m^2"
}
impl IrsdkVarHeader {
    fn name(&self) -> Result<&str, std::str::Utf8Error> {
        unsafe { CStr::from_ptr(self.name.as_ptr() as *const c_char).to_str() }
    }
    fn desc(&self) -> Result<&str, std::str::Utf8Error> {
        unsafe { CStr::from_ptr(self.desc.as_ptr() as *const c_char).to_str() }
    }
    fn unit(&self) -> Result<&str, std::str::Utf8Error> {
        unsafe { CStr::from_ptr(self.unit.as_ptr() as *const c_char).to_str() }
    }
    fn has_name(&self, n: &str) -> bool {
        if n.len() > IRSDK_MAX_STRING {
            return false;
        }
        let b = n.as_bytes();
        for (i, item) in b.iter().enumerate() {
            if *item != self.name[i] {
                return false;
            }
        }
        for i in b.len()..IRSDK_MAX_STRING {
            if self.name[i] != 0 {
                return false;
            }
        }
        true
    }
}

#[derive(Debug)]
pub enum Error {
    InvalidType,
    InvalidEnumValue(i32),
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum VarType {
    // 1 byte
    Char = 0,
    Bool = 1,

    // 4 bytes
    Int = 2,
    Bitfield = 3,
    Float = 4,

    // 8 bytes
    Double = 5,

    //index, don't use
    #[deprecated]
    Etcount = 6,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Value<'a> {
    Char(u8),
    Chars(&'a [u8]),
    Bool(bool),
    Bools(&'a [bool]),
    Int(i32),
    Ints(&'a [i32]),
    Bitfield(i32),
    Bitfields(&'a [i32]),
    Float(f32),
    Floats(&'a [f32]),
    Double(f64),
    Doubles(&'a [f64]),
}

pub trait FromValue: Sized {
    /// Converts an iracing Value into Rust value.
    fn var_result(value: &Value) -> Result<Self, Error>;
}

impl<'a> Value<'a> {
    pub fn as_f64(&self) -> Result<f64, Error> {
        match *self {
            Value::Double(f) => Ok(f),
            _ => Err(Error::InvalidType),
        }
    }
    pub fn as_f32(&self) -> Result<f32, Error> {
        match *self {
            Value::Float(f) => Ok(f),
            _ => Err(Error::InvalidType),
        }
    }
    pub fn as_i32(&self) -> Result<i32, Error> {
        match *self {
            Value::Int(f) => Ok(f),
            Value::Bitfield(f) => Ok(f),
            _ => Err(Error::InvalidType),
        }
    }
    pub fn as_bool(&self) -> Result<bool, Error> {
        match *self {
            Value::Bool(f) => Ok(f),
            _ => Err(Error::InvalidType),
        }
    }
    pub fn as_u8(&self) -> Result<u8, Error> {
        match *self {
            Value::Char(f) => Ok(f),
            _ => Err(Error::InvalidType),
        }
    }
    pub fn as_f64s(&self) -> Result<&[f64], Error> {
        match *self {
            Value::Doubles(f) => Ok(f),
            _ => Err(Error::InvalidType),
        }
    }
    pub fn as_f32s(&self) -> Result<&[f32], Error> {
        match *self {
            Value::Floats(f) => Ok(f),
            _ => Err(Error::InvalidType),
        }
    }
    pub fn as_i32s(&self) -> Result<&[i32], Error> {
        match *self {
            Value::Ints(f) => Ok(f),
            _ => Err(Error::InvalidType),
        }
    }
    pub fn as_bools(&self) -> Result<&[bool], Error> {
        match *self {
            Value::Bools(f) => Ok(f),
            _ => Err(Error::InvalidType),
        }
    }
    pub fn as_u8s(&self) -> Result<&[u8], Error> {
        match *self {
            Value::Chars(f) => Ok(f),
            _ => Err(Error::InvalidType),
        }
    }
}

impl FromValue for bool {
    fn var_result(value: &Value) -> Result<Self, Error> {
        value.as_bool()
    }
}
impl FromValue for u8 {
    fn var_result(value: &Value) -> Result<Self, Error> {
        value.as_u8()
    }
}
impl FromValue for i32 {
    fn var_result(value: &Value) -> Result<Self, Error> {
        value.as_i32()
    }
}
impl FromValue for f32 {
    fn var_result(value: &Value) -> Result<Self, Error> {
        value.as_f32()
    }
}
impl FromValue for f64 {
    fn var_result(value: &Value) -> Result<Self, Error> {
        value.as_f64()
    }
}
impl FromValue for flags::EngineWarnings {
    fn var_result(value: &Value) -> Result<Self, Error> {
        let v = value.as_i32()?;
        Ok(Self::from_bits_truncate(v))
    }
}
impl FromValue for flags::Flags {
    fn var_result(value: &Value) -> Result<Self, Error> {
        let v = value.as_i32()?;
        Ok(Self::from_bits_truncate(v as u32))
    }
}
impl FromValue for flags::SessionState {
    fn var_result(value: &Value) -> Result<Self, Error> {
        let v = value.as_i32()?;
        match num::FromPrimitive::from_i32(v) {
            Some(t) => Ok(t),
            None => Err(Error::InvalidEnumValue(v)),
        }
    }
}
impl FromValue for flags::TrackLocation {
    fn var_result(value: &Value) -> Result<Self, Error> {
        let v = value.as_i32()?;
        match num::FromPrimitive::from_i32(v) {
            Some(t) => Ok(t),
            None => Err(Error::InvalidEnumValue(v)),
        }
    }
}
impl FromValue for flags::TrackSurface {
    fn var_result(value: &Value) -> Result<Self, Error> {
        let v = value.as_i32()?;
        match num::FromPrimitive::from_i32(v) {
            Some(t) => Ok(t),
            None => Err(Error::InvalidEnumValue(v)),
        }
    }
}
impl FromValue for flags::CarLeftRight {
    fn var_result(value: &Value) -> Result<Self, Error> {
        let v = value.as_i32()?;
        match num::FromPrimitive::from_i32(v) {
            Some(t) => Ok(t),
            None => Err(Error::InvalidEnumValue(v)),
        }
    }
}
impl FromValue for flags::CameraState {
    fn var_result(value: &Value) -> Result<Self, Error> {
        let v = value.as_i32()?;
        Ok(Self::from_bits_truncate(v))
    }
}
impl FromValue for flags::PitSvcFlags {
    fn var_result(value: &Value) -> Result<Self, Error> {
        let v = value.as_i32()?;
        Ok(Self::from_bits_truncate(v))
    }
}
impl FromValue for flags::PitSvcStatus {
    fn var_result(value: &Value) -> Result<Self, Error> {
        let v = value.as_i32()?;
        match num::FromPrimitive::from_i32(v) {
            Some(t) => Ok(t),
            None => Err(Error::InvalidEnumValue(v)),
        }
    }
}
impl FromValue for flags::PaceMode {
    fn var_result(value: &Value) -> Result<Self, Error> {
        let v = value.as_i32()?;
        match num::FromPrimitive::from_i32(v) {
            Some(t) => Ok(t),
            None => Err(Error::InvalidEnumValue(v)),
        }
    }
}
impl FromValue for flags::PaceFlags {
    fn var_result(value: &Value) -> Result<Self, Error> {
        let v = value.as_i32()?;
        Ok(Self::from_bits_truncate(v))
    }
}
