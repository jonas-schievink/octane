extern crate octane;
extern crate xbe;
extern crate env_logger;
extern crate termcolor;
extern crate log;
#[macro_use] extern crate structopt;

use octane::cpu::instr::{Instr, Operand};
use octane::cpu::disasm::{AsmPrinter, TermPrinter, FunctionExtentTracker};
use octane::loader;
use octane::memory::{MmapMemory, VirtualMemory};
use xbe::Xbe;

use structopt::StructOpt;
use termcolor::{ColorChoice, StandardStream};
use std::{fs, u32};
use std::error::Error;
use std::path::PathBuf;
use std::process::{Command, Stdio};
use std::io::Write;
use std::str::FromStr;
use std::num::ParseIntError;
use std::collections::HashSet;

/// Parse a number that might be hexadecimal.
fn parse_hex(src: &str) -> Result<u32, ParseIntError> {
    if src.starts_with("0x") {
        u32::from_str_radix(&src[2..], 16)
    } else {
        u32::from_str(src)
    }
}

#[derive(Debug, StructOpt)]
#[structopt(name = "octane-disasm", about = "Disassemble code in an XBE image")]
struct Opt {
    /// Select the tool to use for disassembly (nasm or builtin).
    ///
    /// Note that external tool might display garbage at the end when stopping
    /// disassembly inside an instruction.
    #[structopt(long = "tool", parse(try_from_str))]
    tool: Option<Disassembler>,

    /// Number of bytes to disassemble after the entry point (can also be a
    /// hexadecimal value starting with `0x`). Defaults to the entire function,
    /// or 150 bytes for external disassemblers.
    #[structopt(long = "bytes", parse(try_from_str = "parse_hex"))]
    bytes: Option<u32>,

    /// Virtual address to start disassembling at (can also be a hexadecimal
    /// value starting with `0x`). Defaults to the program's entry point.
    #[structopt(long = "start", parse(try_from_str = "parse_hex"))]
    start: Option<u32>,

    /// Whether to keep disassembling after the function (for external
    /// disassemblers, this is always active).
    #[structopt(long = "continue")]
    cont: bool,

    /// Whether to follow `call` instructions and disassemble all callees
    /// recursively. Only works with the builtin disassembler.
    #[structopt(long = "follow-calls")]
    follow_calls: bool,

    /// Path to the XBE file to disassemble.
    #[structopt(parse(from_os_str))]
    path: PathBuf,
}

#[derive(Debug, Copy, Clone)]
enum Disassembler {
    /// Netwide (dis)assembler (`ndisasm`).
    Nasm,
    /// Octane's builtin instruction decoder and printer.
    Builtin,
}

impl FromStr for Disassembler {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, String> {
        Ok(match s {
            "nasm" => Disassembler::Nasm,
            "builtin" => Disassembler::Builtin,
            _ => return Err(format!("invalid disassembler: {}", s)),
        })
    }
}

// returns the list of callees of this function
fn builtin<M: VirtualMemory>(xbe: &Xbe, _opt: &Opt, mem: &M, start: u32, byte_count: u32) -> HashSet<u32> {
    let mut printer = TermPrinter::new(StandardStream::stdout(ColorChoice::Auto), xbe, mem, start);

    printer.print_addr_or_offset(&format!("{:08X}  ", start));
    let section = xbe.find_section_containing(start)
        .map(|sec| sec.name())
        .unwrap_or("<unmapped>");
    printer.print_symbols(&format!("disassembly start (in section '{}')", section));
    println!();

    let mut tracker = FunctionExtentTracker::new(start);
    let mut callees = HashSet::new();
    while let Ok(instr) = printer.disassemble() {
        if let Instr::Call { target: Operand::Imm(imm) } = &instr {
            // calls to fixed callees can be followed
            callees.insert(imm.zero_extended());
        }

        println!();

        if tracker.process(&instr, printer.eip()) {
            // end of function
            break;
        }

        let disassembled_bytes = printer.eip() - start;
        if disassembled_bytes >= byte_count {
            break;
        }
    }

    callees
}

fn ndisasm<M: VirtualMemory>(_opt: &Opt, mem: &M, start: u32, byte_count: u32) -> Result<(), Box<Error>> {
    let mut cmd = Command::new("ndisasm")
        .arg("-b32")
        .arg(format!("-o{:x}h", start))
        .arg("-")   // disassemble stdin
        .stdin(Stdio::piped())
        .spawn()?;

    let mut stdin = cmd.stdin.take().unwrap();
    for addr in start..start+byte_count {
        stdin.write(&[mem.load(addr)?])?;
    }

    drop(stdin);
    cmd.wait()?;
    Ok(())
}

fn main() -> Result<(), Box<Error>> {
    env_logger::init();

    let opt = Opt::from_args();

    let contents = fs::read(&opt.path)?;
    let xbe = Xbe::parse(&contents)?;
    eprintln!("opened '{}'", xbe.title_name());

    let mut mem = MmapMemory::new();
    loader::load(&xbe, &mut mem)?;

    let tool = opt.tool.unwrap_or(Disassembler::Builtin);
    let start = opt.start.unwrap_or(xbe.entry_point());
    let bytes = opt.bytes.unwrap_or_else(|| {
        match tool {
            Disassembler::Builtin => u32::MAX,
            _ => 150,
        }
    });

    match tool {
        Disassembler::Nasm => {
            ndisasm(&opt, &mem, start, bytes)?;
        }
        Disassembler::Builtin => {
            // FIXME might be nice to print the call stack
            let mut all_callees = HashSet::<u32>::new();
            let mut callees = builtin(&xbe, &opt, &mem, start, bytes);
            while opt.follow_calls && !callees.is_empty() {
                let new_callees = callees.iter().flat_map(|callee| {
                    if all_callees.contains(callee) {
                        HashSet::new()  // don't add any callees
                    } else {
                        builtin(&xbe, &opt, &mem, *callee, u32::MAX)
                    }
                }).collect();
                all_callees.extend(callees.iter().cloned());
                callees = new_callees;
            }
        }
    }

    Ok(())
}
