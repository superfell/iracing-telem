use std::{thread, time::Duration};

use iracing_telem::flags::{BroadcastMsg, FFBCommand};

fn main() {
    let mut c = iracing_telem::Client::new();
    unsafe {
        let s = c.session().unwrap();
        s.broadcast_msg(BroadcastMsg::FFBCommand(FFBCommand::MaxForce(22.0)))
            .unwrap();
        thread::sleep(Duration::new(5, 0));
        s.broadcast_msg(BroadcastMsg::FFBCommand(FFBCommand::MaxForce(-1.0)))
            .unwrap();
    }
}
