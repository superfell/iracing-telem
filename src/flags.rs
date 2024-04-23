//! flags contains definitions for the Bitfield and Enum values that are returned
//! as variable values.
//!
//! e.g. the variable "SessionState" has an VarType::Int which can be mapped into
//! the SessionState enum. variables with VarType::Bitfield can be mapped into
//! the relevant bitfield struct.
//!

use super::makelong;
use bitflags::bitflags;
use num::ToPrimitive;
use num_derive::{FromPrimitive, ToPrimitive};

bitflags! {
    pub struct StatusField:i32 {
        const CONNECTED = 1;
    }
}

// BITFIELD & ENUMs that can appear as telemetry values

bitflags! {
    pub struct EngineWarnings:i32 {
        const WATER_TEMP_WARNING    = 0x01;
        const FUEL_PRESSURE_WARNING = 0x02;
        const OIL_PRESSURE_WARNING  = 0x04;
        const ENGINE_STALLED        = 0x08;
        const PIT_SPEED_LIMITER     = 0x10;
        const REV_LIMITER_ACTIVE    = 0x20;
        const OIL_TEMP_WARNING      = 0x40;
    }
}

bitflags! {
    /// The state of the flags being waved on the track, as well as the
    /// state of the start lights.
    pub struct Flags:u32 {
        // global flags
        const CHECKERED = 0x00000001;
        const WHITE     = 0x00000002;
        const GREEN     = 0x00000004;
        const YELLOW    = 0x00000008;
        const RED       = 0x00000010;
        const BLUE      = 0x00000020;
        const DEBRIS    = 0x00000040;
        const CROSSED   = 0x00000080;
        const YELLOW_WAVING = 0x00000100;
        const ONE_TO_GREEN  = 0x00000200;
        const GREEN_HELD    = 0x00000400;
        const LAPS_10_TO_GO = 0x00000800;
        const LAPS_5_TO_GO  = 0x00001000;
        const RANDOM_WAVING = 0x00002000;
        const CAUTION       = 0x00004000;
        const CAUTION_WAVING= 0x00008000;

        // driver black flags
        const BLACK         = 0x00010000;
        const DISQUALIFY    = 0x00020000;
        const SERVICABLE    = 0x00040000;   // aka can pit
        const FURLED        = 0x00080000;
        const REPAIR        = 0x00100000;

        // start lights
        const START_HIDDEN  = 0x10000000;
        const START_READY   = 0x20000000;
        const START_SET     = 0x40000000;
        const START_GO      = 0x80000000;
    }
}

#[derive(Clone, Copy, Debug, PartialEq, FromPrimitive)]
pub enum TrackLocation {
    NotInWorld = -1,
    OffTrack,
    InPitStall,
    ApproachingPits,
    OnTrack,
}

#[derive(Clone, Copy, Debug, PartialEq, FromPrimitive)]
pub enum TrackSurface {
    SurfaceNotInWorld = -1,
    UndefinedMaterial = 0,

    Asphalt1Material,
    Asphalt2Material,
    Asphalt3Material,
    Asphalt4Material,
    Concrete1Material,
    Concrete2Material,
    RacingDirt1Material,
    RacingDirt2Material,
    Paint1Material,
    Paint2Material,
    Rumble1Material,
    Rumble2Material,
    Rumble3Material,
    Rumble4Material,

    Grass1Material,
    Grass2Material,
    Grass3Material,
    Grass4Material,
    Dirt1Material,
    Dirt2Material,
    Dirt3Material,
    Dirt4Material,
    SandMaterial,
    Gravel1Material,
    Gravel2Material,
    GrasscreteMaterial,
    AstroturfMaterial,
}

#[derive(Clone, Copy, Debug, PartialEq, FromPrimitive)]
pub enum SessionState {
    Invalid,
    GetInCar,
    Warmup,
    ParadeLaps,
    Racing,
    Checkered,
    CoolDown,
}

#[allow(clippy::enum_variant_names)]
#[derive(Clone, Copy, Debug, PartialEq, FromPrimitive)]
pub enum CarLeftRight {
    Off,
    /// no cars around us.
    Clear,
    /// there is a car to our left.
    CarLeft,
    /// there is a car to our right.    
    CarRight,
    /// there are cars on each side.
    CarLeftRight,
    /// there are two cars to our left.
    TwoCarsLeft,
    /// there are two cars to our right.
    TwoCarsRight,
}

bitflags! {
    pub struct CameraState:i32 {
        /// The camera tool can only be activated if viewing the session screen (out of car)
        const IS_SESSION_SCREEN     = 0x0001;
        /// the scenic camera is active (no focus car)
        const IS_SCENIC_ACTIVE      = 0x0002;

        //these can be changed with a broadcast message
        const CAM_TOOL_ACTIVE           = 0x0004;
        const UI_HIDDEN                 = 0x0008;
        const USE_AUTO_SHOT_SELECTION   = 0x0010;
        const USE_TEMPORARY_EDITS       = 0x0020;
        const USE_KEY_ACCELERATION      = 0x0040;
        const USE_KEY_10X_ACCELERATION  = 0x0080;
        const USE_MOUSE_AIM_MODE        = 0x0100;
    }
}
impl CameraState {
    pub fn params(&self) -> i32 {
        self.bits()
    }
}
bitflags! {
    pub struct PitSvcFlags:i32 {
        const LF_TIRE_CHANGE	= 0x0001;
        const RF_TIRE_CHANGE	= 0x0002;
        const LR_TIRE_CHANGE    = 0x0004;
        const RR_TIRE_CHANGE	= 0x0008;

        const FUEL_FILL			= 0x0010;
        const WINDSHIELD_TEAROFF= 0x0020;
        const FAST_REPAIR		= 0x0040;
    }
}

#[derive(Clone, Copy, Debug, PartialEq, FromPrimitive)]
pub enum PitSvcStatus {
    // status
    None = 0,
    InProgress,
    Complete,

    // errors
    TooFarLeft = 100,
    TooFarRight,
    TooFarForward,
    TooFarBack,
    BadAngle,
    CantFixThat,
}

#[derive(Clone, Copy, Debug, PartialEq, FromPrimitive)]
pub enum PaceMode {
    SingleFileStart = 0,
    DoubleFileStart,
    SingleFileRestart,
    DoubleFileRestart,
    NotPacing,
}

bitflags! {
    pub struct PaceFlags:i32 {
        const END_OF_LINE   = 0x01;
        const FREE_PASS     = 0x02;
        const WAVED_AROUND  = 0x04;
    }
}

/// Enums for broadcast msg
//----

/// Remote control the sim by sending these messages.
/// camera and replay commands only work when you are out of your car,
/// pit commands only work when in your car.
#[derive(Debug)]
pub enum BroadcastMsg {
    /// car position, group, camera (this is per the SDK docs, but seems like in practice group & camera are revesed)
    CamSwitchPos(CameraFocus, i16, i16),
    /// driver #, group, camera (this is per the SDK docs, but seems like in practice group & camera are revesed)
    CamSwitchNum(CameraFocus, i16, i16),
    // irsdk_CameraState, unused, unused
    CamSetState(CameraState),
    /// speed, slowMotion, unused
    ReplaySetPlaySpeed(i16, bool),
    /// irsdk_RpyPosMode, Frame Number (60 frames a second)
    ReplaySetPlayPosition(ReplayPos, i32),
    // irsdk_RpySrchMode, unused, unused
    ReplaySearch(ReplaySearch),
    // irsdk_RpyStateMode, unused, unused
    ReplaySetState(ReplayState),
    /// irsdk_ReloadTexturesMode, carIdx, unused
    ReloadTextures(ReloadTextures),
    /// irsdk_ChatCommandMode, subCommand, unused
    ChatComand(ChatCommand),
    /// irsdk_PitCommandMode, parameter   
    PitCommand(PitCommand),
    /// irsdk_TelemCommandMode, unused, unused        
    TelemCommand(TelemCommand),
    /// irsdk_FFBCommandMode, value (float, high, low)       
    FFBCommand(FFBCommand),
    /// sessionNum, sessionTimeMS (high, low)    
    ReplaySearchSessionTime(i16, std::time::Duration),
    /// irsdk_VideoCaptureMode, unused, unused
    VideoCapture(VideoCapture),
}
impl BroadcastMsg {
    pub fn params(&self) -> (i16, (i16, isize)) {
        match self {
            BroadcastMsg::CamSwitchPos(car, group, cam) => {
                (0, (car.params(), makelong(*group, *cam)))
            }
            BroadcastMsg::CamSwitchNum(car, group, cam) => {
                (1, (car.params(), makelong(*group, *cam)))
            }
            BroadcastMsg::CamSetState(cs) => (2, (cs.params() as i16, 0)), // CameraState would appear to overflow an i16
            BroadcastMsg::ReplaySetPlaySpeed(speed, slow) => {
                (3, (*speed, if *slow { 1 } else { 0 }))
            }
            BroadcastMsg::ReplaySetPlayPosition(pos, frame) => (4, (pos.params(), *frame as isize)),
            BroadcastMsg::ReplaySearch(s) => (5, (s.parms(), 0)),
            BroadcastMsg::ReplaySetState(s) => (6, (s.params(), 0)),
            BroadcastMsg::ReloadTextures(r) => (7, (r.params())),
            BroadcastMsg::ChatComand(c) => (8, (c.params())),
            BroadcastMsg::PitCommand(c) => (9, c.params()),
            BroadcastMsg::TelemCommand(t) => (10, (t.params(), 0)),
            BroadcastMsg::FFBCommand(f) => (11, f.params()),
            BroadcastMsg::ReplaySearchSessionTime(ses, tm) => (12, (*ses, tm.as_millis() as isize)),
            BroadcastMsg::VideoCapture(v) => (13, (v.params(), 0)),
        }
    }
}

/// irsdk_BroadcastCamSwitchPos or irsdk_BroadcastCamSwitchNum camera focus defines
/// pass these in for the first parameter to select the 'focus at' types in the camera system.
#[derive(Debug)]
pub enum CameraFocus {
    Incident,
    Leader,
    Exciting,
    // ctFocusAtDriver + car number...
    Driver(i16),
}
impl CameraFocus {
    fn params(&self) -> i16 {
        match self {
            CameraFocus::Incident => -3,
            CameraFocus::Leader => -2,
            CameraFocus::Exciting => -1,
            CameraFocus::Driver(d) => *d,
        }
    }
}

#[derive(Debug, ToPrimitive)]
pub enum ReplayPos {
    Begin = 0,
    Current,
    End,
}
impl ReplayPos {
    fn params(&self) -> i16 {
        self.to_i16().unwrap()
    }
}

#[derive(Debug, ToPrimitive)]
pub enum ReplaySearch {
    ToStart = 0,
    ToEnd,
    PrevSession,
    NextSession,
    PrevLap,
    NextLap,
    PrevFrame,
    NextFrame,
    PrevIncident,
    NextIncident,
}
impl ReplaySearch {
    fn parms(&self) -> i16 {
        self.to_i16().unwrap()
    }
}

#[derive(Debug, ToPrimitive)]
pub enum ReplayState {
    /// clear any data in the replay tape
    EraseTape = 0,
}
impl ReplayState {
    fn params(&self) -> i16 {
        self.to_i16().unwrap()
    }
}

#[derive(Debug)]
pub enum ReloadTextures {
    /// reload all textures
    All,
    /// reload only textures for the specific carIdx
    CarIdx(i16),
}
impl ReloadTextures {
    fn params(&self) -> (i16, isize) {
        match self {
            ReloadTextures::All => (0, 0),
            ReloadTextures::CarIdx(i) => (1, *i as isize),
        }
    }
}

#[derive(Debug)]
pub enum ChatCommand {
    /// pass in a number from 1-15 representing the chat macro to launch (actual values 0-14)
    Macro(u8),
    /// Open up a new chat window
    BeginChat,
    /// Reply to last private chat
    Reply,
    /// Close chat window
    Cancel,
}
impl ChatCommand {
    fn params(&self) -> (i16, isize) {
        match self {
            ChatCommand::Macro(m) => (0, *m as isize),
            ChatCommand::BeginChat => (1, 0),
            ChatCommand::Reply => (2, 0),
            ChatCommand::Cancel => (3, 0),
        }
    }
}

// this only works when the driver is in the car
#[derive(Debug)]
pub enum PitCommand {
    /// Clear all pit checkboxes
    Clear,
    /// WS: Clean the winshield, using one tear off
    TearOff,
    /// Add fuel, optionally specify the amount to add in liters
    Fuel(Option<i16>),
    /// Change the left front tire, optionally specifying the pressure in KPa
    LF(Option<i16>),
    /// Change the right front tire, optionally specifying the pressure in KPa
    RF(Option<i16>),
    /// Change the left rear tire, optionally specifying the pressure in KPa
    LR(Option<i16>),
    /// Change the right rear tire, optionally specifying the pressure in KPa
    RR(Option<i16>),
    /// Clear tire pit checkboxes
    ClearTires,
    /// FR: Request a fast repair   
    FastRepair,
    /// Uncheck Clean the winshield checkbox
    ClearWS,
    /// Uncheck request a fast repair  
    ClearFR,
    /// Uncheck add fuel
    ClearFuel,
}
impl PitCommand {
    fn params(&self) -> (i16, isize) {
        match self {
            PitCommand::Clear => (0, 0),
            PitCommand::TearOff => (1, 0),
            PitCommand::Fuel(l) => (2, pit_amt(l)),
            PitCommand::LF(p) => (3, pit_amt(p)),
            PitCommand::RF(p) => (4, pit_amt(p)),
            PitCommand::LR(p) => (5, pit_amt(p)),
            PitCommand::RR(p) => (6, pit_amt(p)),
            PitCommand::ClearTires => (7, 0),
            PitCommand::FastRepair => (8, 0),
            PitCommand::ClearWS => (9, 0),
            PitCommand::ClearFR => (10, 0),
            PitCommand::ClearFuel => (11, 0),
        }
    }
}
fn pit_amt(a: &Option<i16>) -> isize {
    match a {
        Some(l) => *l as isize,
        None => 0,
    }
}

/// You can call this any time, but telemtry only records when driver is in there car
#[derive(Debug, ToPrimitive)]
pub enum TelemCommand {
    /// Turn telemetry recording off
    Stop = 0,
    /// Turn telemetry recording on
    Start,
    /// Write current file to disk and start a new one
    Restart,
}
impl TelemCommand {
    fn params(&self) -> i16 {
        self.to_i16().unwrap()
    }
}

/// You can call this any time
#[derive(Debug)]
pub enum FFBCommand {
    /// Set the maximum force when mapping steering torque force to direct input units (float in Nm)
    MaxForce(f32),
}
impl FFBCommand {
    fn params(&self) -> (i16, isize) {
        match self {
            FFBCommand::MaxForce(f) => {
                // in c assigning an int to an LPARAM (long) doesn't fill any of the new MSB.
                // int x = 0xFFFFFFFF; LPARAM x2 = x; // x2 is 00000000FFFFFFFF;
                // in rust casting the sign gets extended so 0xFFFFFFFF becomes 0xFFFFFFFFFFFFFFFF
                // which is why we have the extra &
                (0, (((*f * 65536.0) as isize) & 0x00000000FFFFFFFF))
            }
        }
    }
}

#[derive(Debug, ToPrimitive)]
pub enum VideoCapture {
    /// save a screenshot to disk
    TriggerScreenShot = 0,
    /// start capturing video
    Start,
    /// stop capturing video
    End,
    /// toggle video capture on/off
    Toggle,
    /// show video timer in upper left corner of display
    ShowVideoTimer,
    /// hide video timer
    HideVideoTimer,
}
impl VideoCapture {
    fn params(&self) -> i16 {
        self.to_i16().unwrap()
    }
}

#[cfg(test)]
mod tests {

    use super::*;

    #[test]
    fn test_pitcommand() {
        assert_eq!(PitCommand::Fuel(Some(12)).params(), (2, 12));
        assert_eq!(PitCommand::Fuel(None).params(), (2, 0));
    }

    #[test]
    fn test_ffb() {
        assert_eq!(FFBCommand::MaxForce(50.0).params(), (0, 0x320000));
        assert_eq!(FFBCommand::MaxForce(0.0).params(), (0, 0x00000000));
        assert_eq!(FFBCommand::MaxForce(42.42).params(), (0, 0x2a6b85));
        assert_eq!(FFBCommand::MaxForce(-1.0).params(), (0, 0xffff0000));
    }
    #[test]
    fn test_bcast_msg() {
        assert_eq!(
            BroadcastMsg::PitCommand(PitCommand::LF(Some(140))).params(),
            (9, (3, 140))
        )
    }
}
