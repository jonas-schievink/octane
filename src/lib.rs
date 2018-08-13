// TODO: Write crate docs

#![doc(html_root_url = "https://docs.rs/xe/0.1.0")]
#![warn(missing_debug_implementations)]
//#![warn(missing_docs)]

#[macro_use] extern crate bitflags;
#[macro_use] extern crate bitpat;
#[macro_use] extern crate log;
#[macro_use] extern crate num_derive;
extern crate num_traits;
extern crate xbe;
extern crate core;
extern crate memmap;
extern crate termcolor;

pub mod cpu;
pub mod loader;
pub mod memory;
mod utils;
