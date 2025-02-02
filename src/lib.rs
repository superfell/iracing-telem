//! iracing-telem is a rust port of the iRacing provided c++ SDK.
//!
//! It allows for access to telemetetry data from a running instance of the iRacing simulator
//! As well as the ability to send certain control messages to the simulator (e.g to change
//! Pitstop settings)
//!
//! The iRacing data is exposed through a memory mapped file. Because of this, and the potential
//! issue for the data to not be in the expected locations almost all methods are marked as unsafe.
//!
//! Details of the c++ SDK are available on the iRacing forums. Note you will need an active
//! iRacing subsription to access these.
//!
//! <https://forums.iracing.com/discussion/62/iracing-sdk>
use core::fmt;
use encoding::all::WINDOWS_1252;
use encoding::{DecoderTrap, Encoding};
use std::cmp::Ordering;
use std::ffi::{c_void, CStr};
use std::os::raw::c_char;
use std::rc::Rc;
use std::time::{Duration, Instant};
use std::{slice, thread};
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

/// define markers for unlimited session laps
pub const IRSDK_UNLIMITED_LAPS: i32 = 32767;

/// define markers for unlimited session time (in seconds)
pub const IRSDK_UNLIMITED_TIME: f64 = 604800.0;

/// Client is main entry point into the library. Create a client and then you can
/// access Sessions that have the telemetry data in then.
#[derive(Debug)]
pub struct Client {
    conn: Option<Rc<Connection>>,
    // Incremented each time we issue a new session. Allows for session to determine its expired even if
    // iRacing started a new session.
    session_id: i32,
}
impl Client {
    /// Creates a new Client
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
    /// When iRacing is running, then a Session will be available that contains the
    /// telemetry data. When iRacing is not running, then this returns None.
    /// See also wait_for_session.
    ///
    /// # Safety
    /// Creating a session requires dealing with memory mapped files and c strucutres
    /// that are in them. A mismatch between our definition of the struct and iRacings
    /// definition could cause chaos.
    pub unsafe fn session(&mut self) -> Option<Session> {
        if !self.connect() {
            None
        } else {
            let sid = self.session_id;
            self.session_id += 1;
            let mut s = Session {
                session_id: sid,
                conn: self.conn.as_ref().unwrap().clone(),
                last_tick_count: -2,
                data: bytes::BytesMut::new(),
                expired: false,
            };
            let d = s.wait_for_data(Duration::from_millis(16));
            if DataUpdateResult::Updated == d {
                Some(s)
            } else {
                None
            }
        }
    }
    /// Will wait upto the the supplied wait duration for a iRacing session to be available.
    /// If the wait timeout's returns None, otherwise returns the new Session.
    /// Wait must fit into a 32bit number of milliseconds, about 49 days. otherwise it'll panic.
    ///
    /// # Safety
    /// Creating a session requires dealing with memory mapped files and c strucutres
    /// that are in them. A mismatch between our definition of the struct and iRacings
    /// definition could cause chaos.
    pub unsafe fn wait_for_session(&mut self, wait: Duration) -> Option<Session> {
        // If we had a prior connection then we can use the new data event to wait for the
        // session. Otherwise we need to poll session fn above.
        match &self.conn {
            Some(c) => {
                c.wait_for_new_data(wait);
                self.session()
            }
            None => {
                let start = Instant::now();
                let loop_wait = Duration::from_millis(1000);
                loop {
                    let r = self.session();
                    if r.is_some() || start.elapsed() > wait {
                        return r;
                    }
                    // It takes iRacing a few seconds to get from when you can connect
                    // to the telemetry to when you can actually do anything in the simulator
                    // so a once a second check seems like plenty.
                    thread::sleep(loop_wait);
                }
            }
        }
    }
}
impl Default for Client {
    fn default() -> Self {
        Self::new()
    }
}

/// The outcome of trying to read a row of telemetery data.
#[derive(Debug, PartialEq)]
pub enum DataUpdateResult {
    /// The data was updated, and the new values are available via value & var_value
    Updated,
    /// There's no new data available, the previous data is still available.
    NoUpdate,
    /// We were unable to copy the row of data out of the iRacing buffer and into
    /// our own. This is usually a timing issue, or an extremely slow CPU.
    FailedToCopyRow,
    /// When iRacing is closed, attempts to get new data will return SessionExpired.
    SessionExpired,
}

/// A Session is used to access data from iRacing.
///
/// The data is split between metadata about the available data, and the data itself
/// The metadata (returned in a Var) is valid for the life of the Session. The
/// Var can be used to get the current value for that variable out of the last read
/// row of data.
///
/// # Safety
/// All the method in Session are marked as unsafe. They all ultimately interact with
/// memory mapped data from iRacing in addition some use Win32 APIs as well.
#[derive(Debug)]
pub struct Session {
    session_id: i32,
    conn: Rc<Connection>,
    last_tick_count: i32,
    data: bytes::BytesMut,
    expired: bool,
}
impl Session {
    /// Is this session still connected to the iRacing data.
    ///
    /// # Safety
    /// see details on Session
    pub unsafe fn connected(&self) -> bool {
        !self.expired()
    }
    /// Has this session expired? i.e. no longer connected to iRacing.
    ///
    /// # Safety
    /// see details on Session
    pub unsafe fn expired(&self) -> bool {
        self.expired || (!self.conn.connected())
    }
    /// Waits for upto 'wait' amount of time for a new row of data to be available.
    /// The wait value should not exceed an u32's worth of milliseconds, approx ~49 days
    ///
    /// # Safety
    /// see details on Session
    pub unsafe fn wait_for_data(&mut self, wait: Duration) -> DataUpdateResult {
        let r = self.get_new_data();
        if r == DataUpdateResult::NoUpdate {
            self.conn.wait_for_new_data(wait);
            self.get_new_data()
        } else {
            r
        }
    }
    /// Attempt to get newer data from iRacing.
    ///
    /// # Safety
    /// see details on Session
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
    /// dump_vars is for diagnostics and investigation, it writes out all the
    /// variables definitions and their current value.
    ///
    /// # Safety
    /// see details on Session
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
    /// find_var will look for an iracing data point/variable with
    /// the supplied name (case sensitive). None is returned if its
    /// unable to find a matching item. The return Var is only
    /// valid for use with the Session that created it.
    ///
    /// # Safety
    /// see details on Session
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
    /// return the value of the supplied variable as of the most recently fetched row of data.
    /// this will panic if you pass a Var instance generated by a different Session instance.
    ///
    /// # Safety
    /// see details on Session
    pub unsafe fn var_value(&self, var: &Var) -> Value {
        assert_eq!(
            var.session_id, self.session_id,
            "programmer error, Var was issued by a different Session"
        );
        // verify that value is inside the buffer
        let var_offset = var.hdr.offset as usize;
        assert!(
            var.size() + var_offset <= self.data.len(),
            "The value appears to be outside the buffer"
        );

        let x = self.data.as_ptr().add(var_offset);
        if var.hdr.count == 1 {
            match var.hdr.var_type {
                VarType::Char => Value::Char(std::ptr::read_unaligned(x as *const u8)),
                VarType::Bool => Value::Bool(std::ptr::read_unaligned(x as *const bool)),
                VarType::Int => Value::Int(std::ptr::read_unaligned(x as *const i32)),
                VarType::Bitfield => Value::Bitfield(std::ptr::read_unaligned(x as *const i32)),
                VarType::Float => Value::Float(std::ptr::read_unaligned(x as *const f32)),
                VarType::Double => Value::Double(std::ptr::read_unaligned(x as *const f64)),
                _ => todo!(), // ETCount
            }
        } else {
            let l = var.count();
            match var.hdr.var_type {
                VarType::Char => Value::Chars(&*std::ptr::slice_from_raw_parts(x, l)),
                VarType::Bool => Value::Bools(&*std::ptr::slice_from_raw_parts(x as *const bool, l)),
                VarType::Int => Value::Ints(&*std::ptr::slice_from_raw_parts(x as *const i32, l)),
                VarType::Bitfield => Value::Bitfields(&*std::ptr::slice_from_raw_parts(x as *const i32, l)),
                VarType::Float => Value::Floats(&*std::ptr::slice_from_raw_parts(x as *const f32, l)),
                VarType::Double => Value::Doubles(&*std::ptr::slice_from_raw_parts(x as *const f64, l)),
                _ => todo!(), // ETCount
            }
        }
    }
    /// Read the value of the supplied variable, and convert it to the relevent rust type. The
    /// rust type can be a primitive such as i32,f32,f64 or one of the bitfield/enums defined
    /// in the flags package.
    ///
    /// # Safety
    /// see details on Session
    pub unsafe fn value<'a, T>(&'a self, var: &Var) -> Result<T, T::Error>
    where
        T: TryFrom<Value<'a>, Error = Error>,
    {
        let v = self.var_value(var);
        v.try_into()
    }
    /// iRacing has a second set of data called session_info that changes at a much slower rate.
    /// session_info_update is incremented each time the session_info data is updated.
    ///
    /// # Safety
    /// see details on Session
    pub unsafe fn session_info_update(&self) -> i32 {
        (*self.conn.header).session_info_update
    }
    /// Returns the current Session info string. This is a Yaml formatted string that you'll
    /// need to parse.
    ///
    /// # Safety
    /// see details on Session
    pub unsafe fn session_info(&self) -> String {
        let bytes = self.conn.session_info();
        // as we're using replace, this should not ever return an error
        WINDOWS_1252.decode(bytes, DecoderTrap::Replace).unwrap()
    }

    /// A number of things can be controlled in iRacing remotely via broadcast messages.
    /// This will send the supplied message. There is no way to determine that iRacing has
    /// actually acted on the message. Many of the messages only work when the simulator
    /// is in a specific state.
    ///
    /// # Safety
    /// Unlike the rest of Session, this doesn't interact with the memory mapped data,but
    /// it does use Win32 calls to broadcast the message to iRacing.
    pub unsafe fn broadcast_msg(&self, msg: flags::BroadcastMsg) -> Result<(), WIN32_ERROR> {
        let (cmd_msg_id, (var1, var2)) = msg.params();
        let x = makelong(cmd_msg_id, var1);
        let r = SendNotifyMessageA(
            HWND_BROADCAST,
            self.conn.broadcast_msg_id,
            WPARAM(x as usize),
            LPARAM(var2),
        );
        if r.as_bool() {
            Ok(())
        } else {
            Err(GetLastError())
        }
    }
}
fn makelong(var1: i16, var2: i16) -> isize {
    // aka MAKELONG
    let x = ((var1 as u32) & 0xFFFF) | (((var2 as u32) & 0xFFFF) << 16);
    x as isize
}

/// Var is a handle to a variable or telemetry data point.
///
/// Var's are obtained via the find_var() method on Session, and are then
/// valid for the lifetime of that Session. They can only be used with the
/// Session that generated them.
/// In addition the metadata available from Var, Var is also used as a key
/// to read the current value for the item.
pub struct Var {
    hdr: IrsdkVarHeader,
    session_id: i32,
}
impl Var {
    /// returns the data type of this item, e.g. Float, Int
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
    /// returns the count. This indicates how many instances of the datapoint value
    /// are associated with this variable. Typically its one, but there is a small
    /// subset of points that have more, typically 64 (i.e. one per driver).
    /// Values for Var's with a count > 1 are returned as slices of the relevent type
    pub fn count(&self) -> usize {
        self.hdr.count as usize
    }
    /// returns the size of a value for this Var in bytes.
    fn size(&self) -> usize {
        self.var_type().size() * self.count()
    }
}
impl fmt::Debug for Var {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{} {:?} ({})", self.name(), self.var_type(), self.desc())
    }
}

/// Connection is a wrapper around the mechanics of opening the iRacing generated memory mapped file
/// that all the data comes from. Its mostly dealing with win32 shenanigans.
///
/// The data returned out of Connection is typically directly mapped into the underlying memory mapped data.
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
#[derive(Debug)]
struct IrsdkBuf {
    tick_count: i32, // used to detect changes in data
    buf_offset: i32, // offset from header
    pad: [i32; 2],   // (16 byte align)
}

#[repr(C)]
#[derive(Debug)]
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

/// These errors can be returned when accessing variable values and there is a mismatch
/// between the type of the variable, and the type of value asked for.
#[derive(Debug)]
pub enum Error {
    InvalidType,
    InvalidEnumValue(i32),
}

/// The different types of variables or datapoints available.
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
impl VarType {
    fn size(&self) -> usize {
        match *self {
            VarType::Char => 1,
            VarType::Bool => 1,
            VarType::Int => 4,
            VarType::Bitfield => 4,
            VarType::Float => 4,
            VarType::Double => 8,
            _ => todo!(), //Etcount
        }
    }
}
/// An instance of a value for a variable.
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
    pub fn as_f64s(&self) -> Result<&'a [f64], Error> {
        match *self {
            Value::Doubles(f) => Ok(f),
            _ => Err(Error::InvalidType),
        }
    }
    pub fn as_f32s(self) -> Result<&'a [f32], Error> {
        match self {
            Value::Floats(f) => Ok(f),
            _ => Err(Error::InvalidType),
        }
    }
    pub fn as_i32s(&self) -> Result<&'a [i32], Error> {
        match *self {
            Value::Ints(f) => Ok(f),
            Value::Bitfields(f) => Ok(f),
            _ => Err(Error::InvalidType),
        }
    }
    pub fn as_bools(&self) -> Result<&'a [bool], Error> {
        match *self {
            Value::Bools(f) => Ok(f),
            _ => Err(Error::InvalidType),
        }
    }
    pub fn as_u8s(&self) -> Result<&'a [u8], Error> {
        match *self {
            Value::Chars(f) => Ok(f),
            _ => Err(Error::InvalidType),
        }
    }
}

impl TryFrom<Value<'_>> for bool {
    type Error = Error;
    fn try_from(v: Value) -> Result<Self, Self::Error> {
        v.as_bool()
    }
}
impl TryFrom<Value<'_>> for u8 {
    type Error = Error;
    fn try_from(v: Value) -> Result<Self, Self::Error> {
        v.as_u8()
    }
}
impl TryFrom<Value<'_>> for i32 {
    type Error = Error;
    fn try_from(v: Value) -> Result<Self, Self::Error> {
        v.as_i32()
    }
}
impl TryFrom<Value<'_>> for f32 {
    type Error = Error;
    fn try_from(v: Value) -> Result<Self, Self::Error> {
        v.as_f32()
    }
}
impl TryFrom<Value<'_>> for f64 {
    type Error = Error;
    fn try_from(value: Value) -> Result<Self, Self::Error> {
        value.as_f64()
    }
}
impl<'a> TryFrom<Value<'a>> for &'a [bool] {
    type Error = Error;
    fn try_from(v: Value<'a>) -> Result<Self, Self::Error> {
        v.as_bools()
    }
}
impl<'a> TryFrom<Value<'a>> for &'a [u8] {
    type Error = Error;
    fn try_from(v: Value<'a>) -> Result<Self, Self::Error> {
        v.as_u8s()
    }
}
impl<'a> TryFrom<Value<'a>> for &'a [i32] {
    type Error = Error;
    fn try_from(v: Value<'a>) -> Result<Self, Self::Error> {
        v.as_i32s()
    }
}
impl<'a> TryFrom<Value<'a>> for &'a [f32] {
    type Error = Error;
    fn try_from(v: Value<'a>) -> Result<Self, Self::Error> {
        v.as_f32s()
    }
}
impl<'a> TryFrom<Value<'a>> for &'a [f64] {
    type Error = Error;
    fn try_from(v: Value<'a>) -> Result<Self, Self::Error> {
        v.as_f64s()
    }
}
impl TryFrom<Value<'_>> for flags::EngineWarnings {
    type Error = Error;
    fn try_from(v: Value) -> Result<Self, Self::Error> {
        Ok(Self::from_bits_truncate(v.as_i32()?))
    }
}
impl TryFrom<Value<'_>> for flags::Flags {
    type Error = Error;
    fn try_from(v: Value) -> Result<Self, Self::Error> {
        Ok(Self::from_bits_truncate(v.as_i32()? as u32))
    }
}
impl TryFrom<Value<'_>> for flags::SessionState {
    type Error = Error;
    fn try_from(value: Value) -> Result<Self, Self::Error> {
        let v = value.as_i32()?;
        match num::FromPrimitive::from_i32(v) {
            Some(t) => Ok(t),
            None => Err(Error::InvalidEnumValue(v)),
        }
    }
}
impl TryFrom<Value<'_>> for flags::TrackLocation {
    type Error = Error;
    fn try_from(value: Value) -> Result<Self, Self::Error> {
        let v = value.as_i32()?;
        match num::FromPrimitive::from_i32(v) {
            Some(t) => Ok(t),
            None => Err(Error::InvalidEnumValue(v)),
        }
    }
}
impl TryFrom<Value<'_>> for flags::TrackSurface {
    type Error = Error;
    fn try_from(value: Value) -> Result<Self, Self::Error> {
        let v = value.as_i32()?;
        match num::FromPrimitive::from_i32(v) {
            Some(t) => Ok(t),
            None => Err(Error::InvalidEnumValue(v)),
        }
    }
}
impl TryFrom<Value<'_>> for flags::CarLeftRight {
    type Error = Error;
    fn try_from(value: Value) -> Result<Self, Self::Error> {
        let v = value.as_i32()?;
        match num::FromPrimitive::from_i32(v) {
            Some(t) => Ok(t),
            None => Err(Error::InvalidEnumValue(v)),
        }
    }
}
impl TryFrom<Value<'_>> for flags::CameraState {
    type Error = Error;
    fn try_from(v: Value) -> Result<Self, Self::Error> {
        Ok(Self::from_bits_truncate(v.as_i32()?))
    }
}
impl TryFrom<Value<'_>> for flags::PitSvcFlags {
    type Error = Error;
    fn try_from(v: Value) -> Result<Self, Self::Error> {
        Ok(Self::from_bits_truncate(v.as_i32()?))
    }
}
impl TryFrom<Value<'_>> for flags::PitSvcStatus {
    type Error = Error;
    fn try_from(value: Value) -> Result<Self, Self::Error> {
        let v = value.as_i32()?;
        match num::FromPrimitive::from_i32(v) {
            Some(t) => Ok(t),
            None => Err(Error::InvalidEnumValue(v)),
        }
    }
}
impl TryFrom<Value<'_>> for flags::PaceMode {
    type Error = Error;
    fn try_from(value: Value) -> Result<Self, Self::Error> {
        let v = value.as_i32()?;
        match num::FromPrimitive::from_i32(v) {
            Some(t) => Ok(t),
            None => Err(Error::InvalidEnumValue(v)),
        }
    }
}
impl TryFrom<Value<'_>> for flags::PaceFlags {
    type Error = Error;
    fn try_from(v: Value) -> Result<Self, Self::Error> {
        Ok(Self::from_bits_truncate(v.as_i32()?))
    }
}

#[cfg(test)]
mod tests {

    use std::ptr;

    use crate::flags::StatusField;

    use super::*;

    #[test]
    fn test_makelong() {
        assert_eq!(makelong(0, 0), 0);
        assert_eq!(makelong(0, 0x12), 0x00120000);
        assert_eq!(makelong(0x12, 0), 0x12);
        assert_eq!(makelong(0x12, 0x24), 0x00240012);
        assert_eq!(makelong(-1, -1), 0xFFFFFFFF);
        assert_eq!(makelong(0, -1), 0xFFFF0000);
        assert_eq!(makelong(-1, 0), 0x0000FFFF);
        assert_eq!(makelong(0x1234, 0x5678), 0x56781234);
        // sanity check the as usize cast for WPARAM;
        let m = |a, b| makelong(a, b) as usize;
        assert_eq!(m(0, 0), 0);
        assert_eq!(m(0, 0x12), 0x00120000);
        assert_eq!(m(0x12, 0), 0x12);
        assert_eq!(m(0x12, 0x24), 0x00240012);
        assert_eq!(m(-1, -1), 0xFFFFFFFF);
        assert_eq!(m(0, -1), 0xFFFF0000);
        assert_eq!(m(-1, 0), 0x0000FFFF);
        assert_eq!(m(0x1234, 0x5678), 0x56781234);
    }

    #[test]
    fn test_var_size() {
        let f = |t, c| {
            Var {
                session_id: 0,
                hdr: IrsdkVarHeader {
                    var_type: t,
                    offset: 0,
                    count: c,
                    count_as_time: 0,
                    pad: [0, 0, 0], // (16 byte align)
                    name: [0; IRSDK_MAX_STRING],
                    desc: [0; IRSDK_MAX_DESC],
                    unit: [0; IRSDK_MAX_STRING],
                },
            }
        };
        assert_eq!(512, f(VarType::Double, 64).size());
        assert_eq!(24, f(VarType::Double, 3).size());
        assert_eq!(8, f(VarType::Double, 1).size());
        assert_eq!(12, f(VarType::Float, 3).size());
        assert_eq!(4, f(VarType::Float, 1).size());
        assert_eq!(12, f(VarType::Int, 3).size());
        assert_eq!(4, f(VarType::Int, 1).size());
        assert_eq!(12, f(VarType::Bitfield, 3).size());
        assert_eq!(4, f(VarType::Bitfield, 1).size());
        assert_eq!(3, f(VarType::Char, 3).size());
        assert_eq!(1, f(VarType::Char, 1).size());
        assert_eq!(3, f(VarType::Bool, 3).size());
        assert_eq!(1, f(VarType::Bool, 1).size());
    }

    #[test]
    fn test_irsdk_var_header() {
        let mut h = IrsdkVarHeader {
            var_type: VarType::Float,
            offset: 32,
            count: 1,
            count_as_time: 0,
            pad: [0; 3],
            name: [0; IRSDK_MAX_STRING],
            desc: [0; IRSDK_MAX_DESC],
            unit: [0; IRSDK_MAX_STRING],
        };
        // there must be an easier way than this
        h.name[0] = 'b' as u8;
        h.name[1] = 'o' as u8;
        h.name[2] = 'b' as u8;
        assert_eq!(Ok("bob"), h.name());
        assert!(h.has_name("bob"));
        assert!(!h.has_name("alice"));
        assert!(!h.has_name("bobby"));
    }

    #[test]
    fn test_var_value() {
        let b = || IrsdkBuf {
            tick_count: 1,
            buf_offset: 0,
            pad: [0, 2],
        };
        let mut h = IrsdkHeader {
            ver: 2,
            status: StatusField::CONNECTED,
            tick_rate: 60,
            session_info_update: 1,
            session_info_len: 0,
            session_info_offset: 100,
            num_vars: 0,
            var_header_offset: 0,
            num_buf: 3,
            buf_len: 12,
            pad1: [0; 2],
            var_buf: [b(), b(), b(), b()],
        };
        let mut s = Session {
            session_id: 1,
            conn: Rc::new(Connection {
                file_mapping: HANDLE::default(),
                shared_mem: ptr::null_mut(),
                header: ptr::addr_of_mut!(h),
                new_data: HANDLE::default(),
                broadcast_msg_id: 1,
            }),
            last_tick_count: 1,
            data: bytes::BytesMut::new(),
            expired: false,
        };
        // char/bool
        s.data.extend_from_slice(&[55, 56, 57, 58, 1, 0, 1, 0]);
        // int
        s.data.extend_from_slice(&[1, 2, 3, 4, 5, 6, 7, 8]);
        // float
        s.data
            .extend_from_slice(&[0x00, 0x00, 0x80, 0x3f, 0, 0, 0, 0xc0]);
        // double
        s.data
            .extend_from_slice(&[0, 0, 0, 0, 0, 0, 0, 0xc0, 2, 0, 0, 0, 0, 0, 0xF0, 0x3F]);
        let v = |t, o, c| Var {
            hdr: IrsdkVarHeader {
                var_type: t,
                offset: o,
                count: c,
                count_as_time: 0,
                pad: [0; 3],
                name: [0; IRSDK_MAX_STRING],
                desc: [0; IRSDK_MAX_DESC],
                unit: [0; IRSDK_MAX_STRING],
            },
            session_id: 1,
        };
        unsafe {
            assert_eq!(s.var_value(&v(VarType::Char, 0, 1)), Value::Char(55));
            assert_eq!(s.var_value(&v(VarType::Char, 1, 1)), Value::Char(56));
            assert_eq!(s.var_value(&v(VarType::Bool, 4, 1)), Value::Bool(true));
            assert_eq!(s.var_value(&v(VarType::Bool, 5, 1)), Value::Bool(false));
            assert_eq!(s.var_value(&v(VarType::Int, 8, 1)), Value::Int(0x04030201));
            assert_eq!(
                s.var_value(&v(VarType::Bitfield, 8, 1)),
                Value::Bitfield(0x04030201)
            );
            assert_eq!(s.var_value(&v(VarType::Float, 16, 1)), Value::Float(1.0));
            assert_eq!(s.var_value(&v(VarType::Float, 20, 1)), Value::Float(-2.0));
            assert_eq!(s.var_value(&v(VarType::Double, 24, 1)), Value::Double(-2.0));
            assert_eq!(
                s.var_value(&v(VarType::Double, 32, 1)),
                Value::Double(1.0000000000000004)
            );
            assert_eq!(
                s.var_value(&v(VarType::Char, 0, 3)),
                Value::Chars(&[55, 56, 57])
            );
            assert_eq!(
                s.var_value(&v(VarType::Bool, 4, 4)),
                Value::Bools(&[true, false, true, false])
            );
            assert_eq!(
                s.var_value(&v(VarType::Int, 8, 2)),
                Value::Ints(&[0x04030201, 0x08070605])
            );
            assert_eq!(
                s.var_value(&v(VarType::Bitfield, 8, 2)),
                Value::Bitfields(&[0x04030201, 0x08070605])
            );
            assert_eq!(
                s.var_value(&v(VarType::Float, 16, 2)),
                Value::Floats(&[1.0, -2.0])
            );
            assert_eq!(
                s.var_value(&v(VarType::Double, 24, 2)),
                Value::Doubles(&[-2.0, 1.0000000000000004])
            );
        }
    }
    #[test]
    #[should_panic]
    fn test_cant_read_past_buffer() {
        // ugh, need something better for setting these types of tests up.
        let b = || IrsdkBuf {
            tick_count: 1,
            buf_offset: 0,
            pad: [0, 2],
        };
        let mut h = IrsdkHeader {
            ver: 2,
            status: StatusField::CONNECTED,
            tick_rate: 60,
            session_info_update: 1,
            session_info_len: 0,
            session_info_offset: 100,
            num_vars: 0,
            var_header_offset: 0,
            num_buf: 3,
            buf_len: 12,
            pad1: [0; 2],
            var_buf: [b(), b(), b(), b()],
        };
        let mut s = Session {
            session_id: 1,
            conn: Rc::new(Connection {
                file_mapping: HANDLE::default(),
                shared_mem: ptr::null_mut(),
                header: ptr::addr_of_mut!(h),
                new_data: HANDLE::default(),
                broadcast_msg_id: 1,
            }),
            last_tick_count: 1,
            data: bytes::BytesMut::new(),
            expired: false,
        };
        s.data.extend_from_slice(&[1, 2, 3, 4]);
        let v = Var {
            hdr: IrsdkVarHeader {
                var_type: VarType::Int,
                offset: 2,
                count: 1,
                count_as_time: 0,
                pad: [0; 3],
                name: [0; IRSDK_MAX_STRING],
                desc: [0; IRSDK_MAX_DESC],
                unit: [0; IRSDK_MAX_STRING],
            },
            session_id: 1,
        };
        unsafe {
            s.var_value(&v);
        }
    }
}
