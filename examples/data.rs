use std::time::Duration;

use iracing_telem::{flags, Client, DataUpdateResult};

fn main() {
    let mut c = Client::new();
    unsafe {
        let mut s = c.session().unwrap();
        let vss = s.find_var("SessionState").unwrap();
        let vst = s.find_var("SessionTime").unwrap();
        let vrpm = s.find_var("RPM").unwrap();
        println!("variables\n{:?}\n{:?}\n{:?}", vss, vst, vrpm);
        println!("State    SessionTime   RPM");
        loop {
            match s.wait_for_data(Duration::from_millis(20)) {
                DataUpdateResult::Updated => {
                    // You can call value and it'll try and map the result to the relevent type
                    let st: flags::SessionState = s.value(&vss).unwrap();
                    let tm: f64 = s.value(&vst).unwrap();
                    // or you can call var_value, and get the value out of the Value yourself.
                    let rpm: f32 = s.var_value(&vrpm).as_f32().unwrap();
                    println!("{:?} {:<14.3}{:.1}", st, tm, rpm);
                }
                DataUpdateResult::NoUpdate => {}
                DataUpdateResult::FailedToCopyRow => {
                    println!("too slow")
                }
                DataUpdateResult::SessionExpired => break,
            }
        }
    }
}
