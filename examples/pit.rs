use iracing_telem::flags::{BroadcastMsg, PitCommand};

fn main() {
    let mut c = iracing_telem::Client::new();
    unsafe {
        let s = c.session().unwrap();
        s.broadcast_msg(BroadcastMsg::PitCommand(PitCommand::Fuel(Some(5))))
            .unwrap();
        s.broadcast_msg(BroadcastMsg::PitCommand(PitCommand::LR(Some(150))))
            .unwrap();
    }
}
