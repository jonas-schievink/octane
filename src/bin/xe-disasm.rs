extern crate xe;
extern crate xbe;
extern crate env_logger;
extern crate termcolor;
#[macro_use] extern crate log;
#[macro_use] extern crate structopt;

use xe::cpu::decode::*;
use xe::cpu::instr::{Instr, Operand, Addressing};
use xe::cpu::disasm::{Printer, print_instr};
use xe::loader;
use xe::memory::{TableMemory, VirtualMemory};
use xbe::Xbe;

use structopt::StructOpt;
use termcolor::{ColorChoice, Color, ColorSpec, StandardStream, WriteColor};
use std::{cmp, fs, u32};
use std::error::Error;
use std::path::PathBuf;
use std::process::{Command, Stdio};
use std::io::Write;
use std::fmt::Write as _Write;
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
#[structopt(name = "xe-disasm", about = "Disassemble code in an XBE image")]
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
    /// Xe's builtin instruction decoder and printer.
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

const COLOR_MNEMONIC: Color = Color::Blue;
const COLOR_REGISTER: Color = Color::Red;
const COLOR_IMMEDIATE: Color = Color::Green;
const COLOR_ADDR: Color = Color::Cyan;
const COLOR_TARGET: Color = Color::Yellow;

struct TermPrinter<W: WriteColor> {
    w: W,
    pc: u32,
}

impl<W: WriteColor> TermPrinter<W> {
    fn print(&mut self, color: Color, text: &str) {
        self.w.set_color(ColorSpec::new().set_fg(Some(color))).unwrap();
        write!(self.w, "{}", text).unwrap();
        self.w.set_color(ColorSpec::new().set_fg(None)).unwrap();
    }
}

impl<W: WriteColor> Printer for TermPrinter<W> {
    fn print_mnemonic(&mut self, mnemonic: &str) {
        self.print(COLOR_MNEMONIC, mnemonic);
    }

    fn print_register(&mut self, name: &str) {
        self.print(COLOR_REGISTER, name);
    }

    fn print_immediate(&mut self, imm: &str) {
        self.print(COLOR_IMMEDIATE, imm);
    }

    fn print_addr_or_offset(&mut self, addr: &str) {
        self.print(COLOR_ADDR, addr);
    }

    fn print_jump_target(&mut self, target: &str) {
        self.print(COLOR_TARGET, target);
    }

    fn print_symbols(&mut self, sym: &str) {
        write!(self.w, "{}", sym).unwrap();
    }

    fn done(&mut self) {}

    fn pc_ref(&self) -> Option<u32> {
        Some(self.pc)
    }
}

/// Extracts helpful information for virtual address `addr`.
fn addr_info(xbe: &Xbe, addr: u32) -> String {
    // kernel function?
    let thunk_tbl = xbe.kernel_thunk_table();
    if addr >= thunk_tbl.virt_addr() && addr < thunk_tbl.virt_addr() + thunk_tbl.len() {
        // find out which index
        let offset = addr - thunk_tbl.virt_addr();
        let index = offset / 4;
        debug!("addr {:#010X} at thunk tbl offset {:#010X} index {}", addr, offset, index);

        // this should never be out of bounds if thunk table info and decoding is correct
        return thunk_tbl.import_ids()[index as usize].name().into();
    }

    let info = xbe.header().find_address_info(addr);
    let info = match info.section() {
        None => "(not statically mapped)".to_string(),
        Some(section) => format!("(inside {})", section.name()),
    };

    info
}

// returns the list of callees of this function
fn builtin<M: VirtualMemory>(xbe: &Xbe, opt: &Opt, mem: &M, start: u32, byte_count: u32) -> Vec<u32> {
    let mut printer = TermPrinter {
        w: StandardStream::stdout(ColorChoice::Auto),
        pc: start,
    };

    printer.print(COLOR_ADDR, &format!("{:08X}  ", start));
    let section = xbe.header()
        .find_section_containing(start)
        .map(|sec| sec.name())
        .unwrap_or("<unmapped>");
    printer.print_symbols(&format!("disassembly start (in section '{}')", section));
    println!();

    let mut dec = Decoder::new(mem, start);
    let mut max_jump_target = start;
    let mut callees = Vec::new();
    loop {
        let pc_before = dec.current_address();
        let result = dec.decode_next();
        match result {
            Ok(instr) => {
                printer.print(COLOR_ADDR, &format!("{:08X}  ", pc_before));

                let pc = dec.current_address();
                printer.pc = pc;

                let mut raw = String::new();
                for addr in pc_before..pc {
                    write!(raw, "{:02X} ", mem.load(addr).expect("could decode instr but not read mem?")).unwrap();
                }
                raw = format!("{:21} ", raw);

                printer.print(COLOR_IMMEDIATE, &raw);

                print_instr(&instr, &mut printer);

                if let Instr::Call { target: Operand::Imm(imm) } = &instr {
                    // calls to fixed callees can be followed
                    callees.push(imm.zero_extended());
                }

                // track function extent and stop at the last `ret`
                match instr {
                    Instr::Ret { .. } if max_jump_target < pc => {
                        // function ends here
                        if !opt.cont {
                            println!();
                            break;
                        }
                    }
                    Instr::Jump { target: Operand::Imm(imm) }
                    if imm.zero_extended() < pc && max_jump_target < pc => {
                        // unconditional backwards jump as last instr =
                        // function ends here
                        if !opt.cont {
                            println!();
                            break;
                        }
                    }
                    Instr::Jump { target, .. } | Instr::JumpIf { target, ..} => {
                        match target {
                            Operand::Imm(target) => {
                                let addr = target.zero_extended();
                                max_jump_target = cmp::max(max_jump_target, addr);
                            }
                            _ => {} // can't track indirect jumps, assume they're outside of fn
                        }
                    }
                    // display the target for indirect calls where the target
                    // addr. is at an abs. addr. in memory
                    Instr::Call { target: Operand::Mem(mem) }
                    | Instr::Mov { dest: Operand::Mem(mem), src: _ }
                    | Instr::Mov { dest: _, src: Operand::Mem(mem) }
                    | Instr::Push { operand: Operand::Mem(mem) } => {
                        // we can only be helpful if the access uses a flat
                        // address space
                        if !mem.base_segment.may_be_used() {
                            // ...and if the access has a fixed, abs. address
                            if let Addressing::Disp { base: None, disp } = mem.addressing {
                                let info = addr_info(xbe, disp as u32);

                                print!("\t{}", info);
                            }
                        }
                    }
                    _ => {}
                }
                println!();

                let disassembled_bytes = dec.current_address() - start;
                if disassembled_bytes >= byte_count {
                    break;
                }
            },
            Err(e) => {
                println!("decoding error at {:#010X}: {:?}", pc_before, e);
                break;
            },
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

    let mut mem = TableMemory::new();
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
                        vec![]
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
