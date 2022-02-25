use std::{thread, time::Duration};

use iracing_telem::{Client, DataUpdateResult};

fn main() {
    let mut c = Client::new();
    unsafe {
        let mut s = c.session().unwrap();
        let st = s.find_var("SessionTime").unwrap();
        let rpm = s.find_var("RPM").unwrap();
        println!("variables\n{:?}\n{:?}", st, rpm);
        println!("SessionTime   RPM");
        loop {
            // if you don't need the full 60 updates a second you
            // can call get_new_data() at a slower rate.
            match s.get_new_data() {
                DataUpdateResult::Updated => {
                    let tm: f64 = s.value(&st).unwrap();
                    let rpm: f32 = s.value(&rpm).unwrap();
                    println!("{:<14.3}{:.1}", tm, rpm);
                }
                DataUpdateResult::NoUpdate => {}
                DataUpdateResult::FailedToCopyRow => {
                    println!("too slow")
                }
                DataUpdateResult::SessionExpired => break,
            }
            thread::sleep(Duration::from_millis(100));
        }
    }
}
