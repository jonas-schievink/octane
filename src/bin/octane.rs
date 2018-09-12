extern crate octane;
extern crate xbe;
extern crate env_logger;
extern crate termcolor;
extern crate log;
#[macro_use] extern crate structopt;
extern crate gdbstub;

use octane::cpu::disasm::TermPrinter;
use octane::cpu::interpret::Interpreter;
use octane::cpu::ExecutionEngine;
use octane::memory::MmapMemory;
use octane::kernel::Kernel;
use xbe::Xbe;

use structopt::StructOpt;
use termcolor::{ColorChoice, StandardStream};
use std::{fs, process};
use std::error::Error;
use std::path::PathBuf;
use std::net::TcpListener;
use octane::cpu::debugger::Debugger;
use gdbstub::GdbStub;

#[derive(Debug, StructOpt)]
#[structopt(name = "octane", about = "Xbox emulator.")]
struct Opt {
    /// Path to the XBE file to run.
    #[structopt(parse(from_os_str))]
    path: PathBuf,
    /// When specified, Octane will open a gdbserver and wait for a debugger to
    /// connect, allowing to debug the running game.
    #[structopt(long = "debugger")]
    debugger: bool,
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

    interpreter.set_tracer(|interp: &mut Interpreter<_, _>, eip, _instr: &_| {
        // (partial) type annotation in the closure args are needed to avoid a type mismatch
        let mut printer = TermPrinter::new(StandardStream::stdout(ColorChoice::Auto), &xbe, interp.mem_mut(), eip);
        match printer.disassemble() {
            Ok(_) => println!(),
            Err(e) => eprintln!("(disassembler failed: {})", e),
        }
    });

    if opt.debugger {
        let dbg = Debugger::new(interpreter);

        let sockaddr = "127.0.0.1:9001";
        let sock = TcpListener::bind(sockaddr)?;
        println!("GDB server listening on {}", sockaddr);
        println!("Waiting for debugger to connect...");

        let (stream, addr) = sock.accept()?;
        println!("Debugger connected from {}", addr);

        let stub = GdbStub::new(stream, dbg);
        stub.poll()?;
        println!("Debugger closed connection. Exiting.");
    } else {
        interpreter.run()?;
    }
    Ok(())
}

fn main() {
    // By default, log all `info!` messages and higher
    env_logger::Builder::from_default_env()
        .filter(None, log::LevelFilter::Info)
        .init();

    match run() {
        Ok(()) => {},
        Err(e) => {
            eprintln!("exiting due to error: {}", e);
            process::exit(1);
        },
    }
}
