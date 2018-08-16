extern crate xe;
extern crate xbe;
extern crate env_logger;
extern crate termcolor;
extern crate log;
#[macro_use] extern crate structopt;

use xe::cpu::disasm::TermPrinter;
use xe::cpu::interpret::Interpreter;
use xe::loader;
use xe::memory::MmapMemory;
use xe::kernel::Kernel;
use xbe::Xbe;

use structopt::StructOpt;
use termcolor::{ColorChoice, StandardStream};
use std::fs;
use std::error::Error;
use std::path::PathBuf;

#[derive(Debug, StructOpt)]
#[structopt(name = "xe", about = "Xbox emulator.")]
struct Opt {
    /// Path to the XBE file to run.
    #[structopt(parse(from_os_str))]
    path: PathBuf,
}

fn main() -> Result<(), Box<Error>> {
    env_logger::init();

    let opt = Opt::from_args();

    let contents = fs::read(&opt.path)?;
    let xbe = Xbe::parse(&contents)?;
    eprintln!("opened '{}'", xbe.title_name());

    let mut mem = MmapMemory::new();
    let load_info = loader::load(&xbe, &mut mem)?;
    let kernel = Kernel::new();

    let entry = xbe.entry_point();
    let mut interpreter = Interpreter::new(mem, entry, load_info.esp, kernel);

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
