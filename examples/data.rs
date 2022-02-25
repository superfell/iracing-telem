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
            match s.wait_for_data(Duration::from_millis(20)) {
                DataUpdateResult::Updated => {
                    let tm: f64 = s.value(&st).unwrap();
                    let rpm: f32 = s.value(&rpm).unwrap();
                    println!("{:<14.3}{:.1}", tm, rpm);
                }
                DataUpdateResult::NoUpdate => unreachable!(),
                DataUpdateResult::FailedToCopyRow => {
                    println!("too slow")
                }
                DataUpdateResult::SessionExpired => break,
            }
        }
    }
}
