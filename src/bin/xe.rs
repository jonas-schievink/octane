extern crate xe;
extern crate xbe;
extern crate env_logger;
extern crate termcolor;
extern crate log;
#[macro_use] extern crate structopt;

use xe::cpu::disasm::TermPrinter;
use xe::cpu::interpret::Interpreter;
use xe::memory::MmapMemory;
use xe::kernel::Kernel;
use xbe::Xbe;

use structopt::StructOpt;
use termcolor::{ColorChoice, StandardStream};
use std::{fs, process};
use std::error::Error;
use std::path::PathBuf;

#[derive(Debug, StructOpt)]
#[structopt(name = "xe", about = "Xbox emulator.")]
struct Opt {
    /// Path to the XBE file to run.
    #[structopt(parse(from_os_str))]
    path: PathBuf,
}

fn run() -> Result<(), Box<Error>> {
    let opt = Opt::from_args();

    let contents = fs::read(&opt.path)?;
    let xbe = Xbe::parse(&contents)?;
    eprintln!("opened '{}'", xbe.title_name());

    let mut mem = MmapMemory::new();
    let kernel = Kernel::load(&xbe, &mut mem)?;

    let initial_state = kernel.current_thread_state();
    let mut interpreter = Interpreter::new(mem, initial_state, kernel);

    loop {
        {
            let pc = interpreter.state_mut().eip();
            let mut printer = TermPrinter::new(StandardStream::stdout(ColorChoice::Auto), &xbe, interpreter.mem_mut(), pc);
            match printer.disassemble() {
                Ok(_) => println!(),
                Err(e) => eprintln!("(disassembler failed: {})", e),
            }
        }
        interpreter.step()?;
    }
}

fn main() {
    env_logger::init();

    match run() {
        Ok(()) => {},
        Err(e) => {
            eprintln!("exiting due to error: {}", e);
            process::exit(1);
        },
    }
}
