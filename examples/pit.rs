use std::time::Duration;

use iracing_telem::flags::{BroadcastMsg, PitCommand};
use windows::Win32::Foundation::WIN32_ERROR;

fn main() -> Result<(), WIN32_ERROR> {
    let mut c = iracing_telem::Client::new();
    println!("Start iRacing");
    unsafe {
        match c.wait_for_session(Duration::new(600, 0)) {
            None => {
                println!("Remember to start iRacing");
            }
            Some(s) => {
                s.broadcast_msg(BroadcastMsg::PitCommand(PitCommand::Fuel(Some(5))))?;
                s.broadcast_msg(BroadcastMsg::PitCommand(PitCommand::LR(Some(150))))?;
            }
        }
    }
    Ok(())
}
