//! Instruction disassembler and pretty printer.

use cpu::instr::*;
use cpu::visit::{self, Visitor};
use memory::VirtualMemory;

use xbe::Xbe;
use std::cmp;

/// Trait for assembly printing contexts.
///
/// This can be implemented to color specific parts of an instruction and to
/// supply a base address to resolve relative `call` targets.
pub trait AsmPrinter {
    /// Print an instruction mnemonic/name.
    fn print_mnemonic(&mut self, mnemonic: &str);

    /// Prints a register operand (or part of an operand).
    fn print_register(&mut self, name: &str);

    /// Prints an immediate operand.
    fn print_immediate(&mut self, imm: &str);

    /// Prints an address or an address offset of an operand.
    fn print_addr_or_offset(&mut self, addr: &str);

    /// Prints a jump or call target.
    fn print_jump_target(&mut self, target: &str);

    /// Print a string of symbol characters like `&,[]+ `.
    fn print_symbols(&mut self, sym: &str);

    /// Called when the instruction is fully printed.
    fn done(&mut self);

    /// Get the program counter reference.
    ///
    /// This is the value of `EIP` after the currently printed instruction
    /// completes execution. x86 determines relative jump and call targets
    /// relative to that address.
    ///
    /// If this is provided, some instruction may be displayed using a more
    /// user-friendly format.
    fn pc_ref(&self) -> Option<u32> { None }
}

/// Prints the instruction to a string, without formatting.
impl AsmPrinter for String {
    fn print_mnemonic(&mut self, mnemonic: &str) {
        self.push_str(mnemonic);
    }

    fn print_register(&mut self, name: &str) {
        self.push_str(name);
    }

    fn print_immediate(&mut self, imm: &str) {
        self.push_str(imm);
    }

    fn print_addr_or_offset(&mut self, addr: &str) {
        self.push_str(addr);
    }

    fn print_jump_target(&mut self, target: &str) {
        self.push_str(target);
    }

    fn print_symbols(&mut self, sym: &str) {
        self.push_str(sym);
    }

    fn done(&mut self) {}
}

/// Extension trait for internal use by the disassembly printer.
trait PrinterExt {
    fn resolve_target(&self, offset: i32) -> Option<u32>;
    fn space(&mut self);
    fn with_indirect<F>(&mut self, f: F)
        where F: FnOnce(&mut Self);
    fn print_segment(&mut self, segment: Segment);
    fn print_operand(&mut self, op: &Operand, hint: ImmReprHint, ambig_size: bool);
    fn print_jump_target_operand(&mut self, target: &Operand, is_branch: bool);
}

/// Printer hint for operands - this will cause immediates to be printed as
/// decimal or hexadecimal.
#[derive(Copy, Clone)]
enum ImmReprHint {
    Dec,
    Hex,
}

impl<P: AsmPrinter> PrinterExt for P {
    fn resolve_target(&self, offset: i32) -> Option<u32> {
        self.pc_ref().map(|pc| ((pc as i32 + offset) as u32))
    }

    fn space(&mut self) {
        self.print_symbols(" ");
    }

    fn with_indirect<F>(&mut self, f: F)
        where F: FnOnce(&mut Self) {
        self.print_symbols("[");
        f(self);
        self.print_symbols("]");
    }

    fn print_segment(&mut self, segment: Segment) {
        let name = match segment {
            Segment::Cs => "cs",
            Segment::Ds => "ds",
            Segment::Es => "es",
            Segment::Fs => "fs",
            Segment::Gs => "gs",
            Segment::Ss => "ss",
        };

        self.print_register(name);
        self.print_symbols(":");
    }

    /// # Parameters
    ///
    /// * `op`: The operand to print.
    /// * `hint`: If the operand is an immediate, print it as hex or decimal
    /// * `ambig_size`: Whether the instruction can have an ambiguous operand
    ///   that needs clarification.
    ///
    /// # Ambiguous Operand Sizes
    ///
    /// Some operations like `push` can use different operands and work on
    /// different operand sizes (eg. `push` can push a 16-bit or 32-bit word).
    /// For an operation like `push [eax]`, it is unclear how many bytes are
    /// actually getting pushed. In those cases, `ambig_size` should be set to
    /// `true`, which instead prints it as `push byte [eax]` (for example).
    ///
    /// Note that many binary operations like `cmp ax,[eax]` are *not*
    /// ambiguous, since the first operand has a known size of 16 bits and both
    /// operands always have the same size. Rule of thumb: If at least one
    /// operand is a register, the size of all operands that are forced to have
    /// the same size is not ambiguous.
    fn print_operand(&mut self, op: &Operand, hint: ImmReprHint, ambig_size: bool) {
        let print_size = |p: &mut Self| {
            let word = match op.size() {
                OpSize::Bits8 => "byte",
                OpSize::Bits16 => "word",
                OpSize::Bits32 => "dword",
            };

            p.print_immediate(word);
            p.space();
        };

        match op {
            Operand::Reg(reg) => self.print_register(reg.name()),
            Operand::Imm(imm) => {
                if ambig_size {
                    print_size(self);
                }

                let s = match hint {
                    ImmReprHint::Dec => format!("{}", imm),
                    ImmReprHint::Hex => format!("{:#x}", imm),
                };
                self.print_immediate(&s);
            }
            Operand::Mem(mem) => {
                match mem.addressing {
                    Addressing::Disp { base, disp } => {
                        if ambig_size {
                            print_size(self);
                        }

                        self.with_indirect(|p| {
                            if mem.base_segment != Segment::Ds {    // FIXME not always default!
                                p.print_segment(mem.base_segment);
                            }

                            if let Some(base) = base {
                                p.print_register(base.name());
                                if disp != 0 {
                                    p.print_symbols(if disp > 0 { "+" } else { "-" });
                                    p.print_addr_or_offset(&format!("{:#x}", disp.abs()));
                                }
                            } else {
                                p.print_addr_or_offset(&format!("{:#x}", disp));
                            }
                        });
                    }
                    Addressing::Sib { scale, index, base, disp } => {
                        if ambig_size {
                            print_size(self);
                        }

                        self.with_indirect(|p| {
                            if mem.base_segment != Segment::Ds {    // FIXME not always default!
                                p.print_segment(mem.base_segment);
                            }

                            match (base, index) {
                                (Some(base), None) => {
                                    p.print_register(base.name());
                                }
                                (Some(base), Some(index)) => {
                                    p.print_register(base.name());
                                    p.print_symbols("+");
                                    p.print_register(index.name());
                                    if scale > 1 {
                                        p.print_symbols("*");
                                        p.print_addr_or_offset(&scale.to_string());
                                    }
                                }
                                (None, Some(index)) => {
                                    p.print_register(index.name());
                                    if scale > 1 {
                                        p.print_symbols("*");
                                        p.print_addr_or_offset(&scale.to_string());
                                    }
                                }
                                (None, None) => unreachable!("SIB addressing without base and index"),
                            }

                            if disp != 0 {
                                p.print_symbols(if disp > 0 { "+" } else { "-" });
                                p.print_addr_or_offset(&format!("{:#x}", disp.abs()));
                            }
                        });
                    }
                }
            }
        }
    }

    /// Prints the target of a jump or call.
    ///
    /// # Parameters
    ///
    /// * `target`: Operand evaluating to the target address.
    /// * `is_branch`: Whether this is likely an intra-procedural branch.
    ///   Changes printing to include the EIP-relative offset, which might be
    ///   more readable in some cases.
    fn print_jump_target_operand(&mut self, target: &Operand, is_branch: bool) {
        if let Operand::Imm(imm) = target {
            // Absolute target address
            let target = imm.zero_extended();
            if is_branch && self.pc_ref().is_some() {
                let rel = target.wrapping_sub(self.pc_ref().unwrap()) as i32;
                self.print_jump_target(&format!("{:+}", rel));
                self.print_symbols(" (-> ");
                self.print_jump_target(&format!("{:#010X}", target));
                self.print_symbols(")");
            } else {
                self.print_jump_target(&format!("{:#010X}", target));
            }
        } else {
            self.print_operand(target, ImmReprHint::Hex, true);
        }
    }
}

struct Disassembler<'a, A: AsmPrinter + 'a> {
    printer: &'a mut A,
    operand_is_jump_target: bool,
    dont_disambiguate: bool,
    is_rel_jump: bool,
    imm_fmt: ImmReprHint,
}

impl<'a, A: AsmPrinter> Visitor for Disassembler<'a, A> {
    fn visit_instr(&mut self, instr: &Instr) {
        use cpu::instr::Instr::*;

        self.imm_fmt = match instr {
            Alu { op, .. } => match op {
                AluOp::Add => ImmReprHint::Dec,
                AluOp::Or => ImmReprHint::Hex,
                AluOp::Adc => ImmReprHint::Dec,
                AluOp::Sbb => ImmReprHint::Dec,
                AluOp::And => ImmReprHint::Hex,
                AluOp::Sub => ImmReprHint::Dec,
                AluOp::Xor => ImmReprHint::Hex,
                AluOp::Cmp => ImmReprHint::Hex,
            },
            Ret { .. } => ImmReprHint::Dec,
            Imul { .. }
            | ImulTrunc { .. }
            | Idiv { .. } => ImmReprHint::Dec,
            Shift { .. } => ImmReprHint::Dec,
            _ => ImmReprHint::Hex,
        };

        self.dont_disambiguate = match instr {
            Ret { .. }
            | Jump { .. }
            | JumpIf { .. }
            | Call { .. }
            | Int { .. }
            | Leave { .. }
            | IntO => true,
            _ => false,
        };

        let (is_rel_jump, operand_is_jump_target) = match instr {
            JumpIf { .. }
            | Jump { .. } => (true, true),
            | Call { .. } => (false, true),
            _ => (false, false),
        };
        self.is_rel_jump = is_rel_jump;
        self.operand_is_jump_target = operand_is_jump_target;

        visit::walk_instr(self, instr);
    }

    fn visit_prefixes(&mut self, prefixes: &[&str]) {
        for prefix in prefixes {
            self.printer.print_mnemonic(prefix);
            self.printer.print_symbols(" ");
        }
    }

    fn visit_mnemonic(&mut self, mnemonic: &str) {
        self.printer.print_mnemonic(mnemonic);
    }

    fn visit_operands(&mut self, operands: &[&Operand]) {
        let mut size = None;
        let mut same_sizes = true;
        for op in operands {
            match size {
                None => size = Some(op.size()),
                Some(size) if op.size() != size => same_sizes = false,
                _ => {}
            }
        }

        if !operands.is_empty() {
            self.printer.space();
        }

        // All operands are by themselves ambiguous
        let all_ambig = operands.iter().all(|op| match op {
            Operand::Imm(_)
            | Operand::Mem(_) => true,
            Operand::Reg(_) => false,
        });
        // Whether the size of ambiguous operands (memory locations and
        // immediates) should be disambiguated by using a
        // `word`/`byte`/`dword`-like prefix.
        let mut disambiguate_operand_sizes = if self.dont_disambiguate {
            false
        } else {
            all_ambig || !same_sizes
        };

        for (i, op) in operands.iter().enumerate() {
            if i != 0 {
                self.printer.print_symbols(",");
            }

            if let Operand::Reg(_) = op {   // regs always have a known size
            } else {
                if disambiguate_operand_sizes {
                    let word = match op.size() {
                        OpSize::Bits8 => "byte",
                        OpSize::Bits16 => "word",
                        OpSize::Bits32 => "dword",
                    };

                    self.printer.print_immediate(word);
                    self.printer.space();

                    if same_sizes {
                        // Disambiguating the first operand is enough if all
                        // have the same size
                        disambiguate_operand_sizes = false;
                    }
                }
            }

            visit::walk_operand(self, op);
        }
    }

    fn visit_register(&mut self, reg: Register) {
        self.printer.print_register(reg.name());
    }

    fn visit_immediate(&mut self, imm: &Immediate) {
        if self.operand_is_jump_target {
            // The operand is the absolute jump/call target
            let target = imm.zero_extended();
            if self.is_rel_jump && self.printer.pc_ref().is_some() {
                let rel = target.wrapping_sub(self.printer.pc_ref().unwrap()) as i32;
                self.printer.print_jump_target(&format!("{:+}", rel));
                self.printer.print_symbols(" (-> ");
                self.printer.print_jump_target(&format!("{:#010X}", target));
                self.printer.print_symbols(")");
            } else {
                self.printer.print_jump_target(&format!("{:#010X}", target));
            }
        } else {
            let s = match self.imm_fmt {
                ImmReprHint::Dec => format!("{}", imm),
                ImmReprHint::Hex => format!("{:#x}", imm),
            };
            self.printer.print_immediate(&s);
        }
    }

    fn visit_memory_location(&mut self, mem: &MemoryLocation) {
        self.printer.with_indirect(|p| {
            match mem.addressing {
                Addressing::Disp { base, disp } => {
                    if mem.base_segment != Segment::Ds {    // FIXME not always default!
                        p.print_segment(mem.base_segment);
                    }

                    if let Some(base) = base {
                        p.print_register(base.name());
                        if disp != 0 {
                            p.print_symbols(if disp > 0 { "+" } else { "-" });
                            p.print_addr_or_offset(&format!("{:#x}", disp.abs()));
                        }
                    } else {
                        p.print_addr_or_offset(&format!("{:#x}", disp));
                    }
                }
                Addressing::Sib { scale, index, base, disp } => {
                    if mem.base_segment != Segment::Ds {    // FIXME not always default!
                        p.print_segment(mem.base_segment);
                    }

                    match (base, index) {
                        (Some(base), None) => {
                            p.print_register(base.name());
                        }
                        (Some(base), Some(index)) => {
                            p.print_register(base.name());
                            p.print_symbols("+");
                            p.print_register(index.name());
                            if scale > 1 {
                                p.print_symbols("*");
                                p.print_addr_or_offset(&scale.to_string());
                            }
                        }
                        (None, Some(index)) => {
                            p.print_register(index.name());
                            if scale > 1 {
                                p.print_symbols("*");
                                p.print_addr_or_offset(&scale.to_string());
                            }
                        }
                        (None, None) => unreachable!("SIB addressing without base and index"),
                    }

                    if disp != 0 {
                        p.print_symbols(if disp > 0 { "+" } else { "-" });
                        p.print_addr_or_offset(&format!("{:#x}", disp.abs()));
                    }
                }
            }
        });
    }
}

/// Prints the disassembly of an x86 instruction.
pub fn print_instr<P: AsmPrinter>(instr: &Instr, p: &mut P) {
    Disassembler {
        printer: p,
        operand_is_jump_target: false,
        dont_disambiguate: false,
        is_rel_jump: false,
        imm_fmt: ImmReprHint::Hex,
    }.visit_instr(instr);
}

/// An instruction visitor that obtains helpful information for memory addresses
/// from initialized `VirtualMemory`.
#[derive(Debug)]
pub struct MemHelper<'a, M: VirtualMemory + 'a> {
    xbe: &'a Xbe<'a>,
    mem: &'a M,
    info: Option<String>,
}

impl<'a, M: VirtualMemory> MemHelper<'a, M> {
    /// Create a new `MemHelper`.
    ///
    /// # Parameters
    ///
    /// * `xbe`: The parsed XBE file to consult regarding kernel functions.
    /// * `mem`: The virtual memory of the process to consult for section names.
    pub fn new(xbe: &'a Xbe<'a>, mem: &'a M) -> Self {
        // FIXME: This can probably work without the XBE
        Self {
            xbe,
            mem,
            info: None,
        }
    }

    /// Obtain section/kernel function help for the memory accessed by `instr`.
    ///
    /// Returns `None` if `instr` doesn't access memory.
    pub fn obtain_info(&mut self, instr: &Instr) -> Option<String> {
        self.visit_instr(instr);
        self.info.take()
    }

    fn addr_info(&self, addr: u32) -> String {
        // kernel function?
        let thunk_tbl = self.xbe.kernel_thunk_table();
        if addr >= thunk_tbl.virt_addr() && addr < thunk_tbl.virt_addr() + thunk_tbl.len() {
            // find out which index
            let offset = addr - thunk_tbl.virt_addr();
            let index = offset / 4;
            debug!("addr {:#010X} at thunk tbl offset {:#010X} index {}", addr, offset, index);

            // this should never be out of bounds if thunk table info and decoding is correct
            return thunk_tbl.import_ids()[index as usize].name().into();
        }

        match self.mem.mapping_containing_addr(addr) {
            Some(mapping) => {
                let start = *mapping.virt_range().start();
                let offset = addr - start;
                format!("({:#X} bytes into '{}')", offset, mapping.name())
            }
            None => "(not mapped)".to_string(),
        }
    }
}

impl<'a, M: VirtualMemory> Visitor for MemHelper<'a, M> {
    fn visit_memory_location(&mut self, loc: &MemoryLocation) {
        // we can only be helpful if the access uses a flat address space
        // (but maybe we can also print the TIB field of the access!)
        if !loc.base_segment.may_be_used() {
            // ...and if the access has a fixed, absolute address
            // (aka "displacement-only")
            if let Addressing::Disp { base: None, disp } = loc.addressing {
                self.info = Some(self.addr_info(disp as u32));
            }
            // maybe it's a good idea to also do this with the *base* address
        }
    }
}

/// An instruction visitor that will calculate the maximum extent of a function.
///
/// This is helpful when a disassembler wants to stop after the last instruction
/// in a function.
///
/// Note that this is still a heuristic - for example, calls to diverging
/// functions will not be counted as the end of the containing function.
#[derive(Debug)]
pub struct FunctionExtentTracker {
    /// Largest virtual address reached by any (un)conditional jump instruction.
    max_jump_target: u32,
}

impl FunctionExtentTracker {
    /// Creates a new function extent tracker starting at the function's start
    /// address.
    pub fn new(start: u32) -> Self {
        Self {
            max_jump_target: start,
        }
    }

    /// Processes `instr` and determines if it ends the function containing it.
    ///
    /// # Parameters
    ///
    /// * `instr`: The instruction to process.
    /// * `eip`: Value of `eip` / program counter *after* `instr`.
    pub fn process(&mut self, instr: &Instr, eip: u32) -> bool {
        self.visit_instr(instr);

        // determine if `instr` can end the function
        match instr {
            Instr::Jump { target: Operand::Imm(imm) } if imm.zero_extended() < eip => {
                // unconditional backwards jump can end the function
                true
            },
            Instr::Ret { .. } => {
                // `ret` can obviously end the functions
                true
            },
            _ => false,
        }
    }
}

impl Visitor for FunctionExtentTracker {
    fn visit_instr(&mut self, instr: &Instr) {
        use cpu::instr::Instr::*;

        match instr {
            JumpIf { .. }
            | Jump { .. } => visit::walk_instr(self, instr),
            _ => {}     // not interesting
        }
    }

    fn visit_immediate(&mut self, imm: &Immediate) {
        // Jump target (always an absolute address)
        self.max_jump_target = cmp::max(self.max_jump_target, imm.zero_extended());
    }
}
