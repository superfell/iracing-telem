# iracing-telem

This is a Rust port of the iRacing SDK for accessing telementry from a running instance of iRacing.

It allows for access to telemetetry data from a running instance of the iRacing simulator
as well as the ability to send certain control messages to the simulator (e.g to change Pitstop settings)

docs: <https://docs.rs/iracing-telem/latest/iracing_telem/>
crate: <https://crates.io/crates/iracing-telem/>

The iRacing data is exposed through a memory mapped file. Because of this, and the potential issue
for the data to not be in the expected locations almost all methods are marked as unsafe.

Details of the c++ SDK are available on the iRacing forums.
Note you will need an active iRacing subsription to access these.

<https://forums.iracing.com/discussion/62/iracing-sdk>