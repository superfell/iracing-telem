use std::time::Duration;

use iracing_telem::{flags, Client, DataUpdateResult};

fn main() -> Result<(), iracing_telem::Error> {
    let mut c = Client::new();
    loop {
        println!("start iRacing");
        unsafe {
            match c.wait_for_session(Duration::new(600, 0)) {
                None => {
                    println!("remember to start iRacing!");
                    return Ok(());
                }
                Some(mut s) => {
                    let vss = s.find_var("SessionState").unwrap();
                    let vst = s.find_var("SessionTime").unwrap();
                    let vrpm = s.find_var("RPM").unwrap();
                    println!("variables\n\t{:?}\n\t{:?}\n\t{:?}", vss, vst, vrpm);
                    println!("State  SessionTime   RPM");
                    loop {
                        match s.wait_for_data(Duration::from_millis(20)) {
                            DataUpdateResult::Updated => {
                                // You can call value and it'll try and map the result to the relevent type
                                let st: flags::SessionState = s.value(&vss)?;
                                let tm: f64 = s.value(&vst)?;
                                // or you can call var_value, and get the value out of the Value yourself.
                                let rpm: f32 = s.var_value(&vrpm).as_f32()?;
                                println!("{:?} {:<14.3}{:.1}", st, tm, rpm);
                            }
                            DataUpdateResult::NoUpdate => {
                                println!("no update")
                            }
                            DataUpdateResult::FailedToCopyRow => {
                                println!("too slow")
                            }
                            DataUpdateResult::SessionExpired => break,
                        }
                    }
                }
            }
        }
    }
}
