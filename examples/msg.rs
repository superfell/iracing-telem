use std::{thread, time::Duration};

use iracing_telem::flags::{BroadcastMsg, FFBCommand};
use windows::Win32::Foundation::WIN32_ERROR;

fn main() -> Result<(), WIN32_ERROR> {
    let mut c = iracing_telem::Client::new();
    println!("Start iRacing");
    unsafe {
        match c.wait_for_session(Duration::new(600, 0)) {
            None => {
                println!("remember to start iRacing");
            }
            Some(s) => {
                s.broadcast_msg(BroadcastMsg::FFBCommand(FFBCommand::MaxForce(22.0)))?;
                thread::sleep(Duration::new(5, 0));
                s.broadcast_msg(BroadcastMsg::FFBCommand(FFBCommand::MaxForce(-1.0)))?;
            }
        }
    }
    Ok(())
}
