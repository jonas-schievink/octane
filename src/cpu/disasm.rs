//! Instruction disassembler and pretty printer.

use cpu::instr::*;

/// Trait for assembly printing contexts.
///
/// This can be implemented to color specific parts of an instruction and to
/// supply a base address to resolve relative `call` targets.
pub trait Printer {
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
    /// user-friendly format. For example, `call` targets are displayed as
    /// absolute addresses instead of offsets.
    fn pc_ref(&self) -> Option<u32> { None }
}

/// Prints the instruction to a string, without formatting.
impl Printer for String {
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
    fn print_operand(&mut self, op: &Operand, hint: ImmReprHint, ambig_size: bool);
    fn print_jump_target_operand(&mut self, target: &Operand, is_branch: bool);
    fn print_instr(&mut self, instr: &Instr);
}

/// Printer hint for operands - this will cause immediates to be printed as
/// decimal or hexadecimal.
#[derive(Copy, Clone)]
enum ImmReprHint {
    Dec,
    Hex,
}

impl<P: Printer> PrinterExt for P {
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
                    // FIXME: Print immediates with 0-padding to avoid having to disambiguate
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
                            if let Some(base) = base {
                                p.print_register(base.name());
                                p.print_symbols("+");
                            }

                            p.print_register(index.name());
                            if scale > 1 {
                                p.print_symbols("*");
                                p.print_addr_or_offset(&scale.to_string());
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

    fn print_instr(&mut self, instr: &Instr) {
        use cpu::instr::Instr::*;

        self.print_mnemonic(&mnemonic(&instr));

        match instr {
            Alu { op, dest, src } => {
                // FIXME maybe immediates should be printed depending on op?
                // (eg. signed for add etc. but unsigned for and)
                let hint = match op {
                    AluOp::Add => ImmReprHint::Dec,
                    AluOp::Or => ImmReprHint::Hex,
                    AluOp::Adc => ImmReprHint::Dec,
                    AluOp::Sbb => ImmReprHint::Dec,
                    AluOp::And => ImmReprHint::Hex,
                    AluOp::Sub => ImmReprHint::Dec,
                    AluOp::Xor => ImmReprHint::Hex,
                    AluOp::Cmp => ImmReprHint::Hex,
                };
                self.space();
                self.print_operand(dest, hint, false);
                self.print_symbols(",");
                self.print_operand(src, hint, false);
            }
            Shift { dest, src, .. } => {
                self.space();
                self.print_operand(dest, ImmReprHint::Dec, true);
                self.print_symbols(",");
                self.print_operand(src, ImmReprHint::Dec, false);
            }
            Mov { dest, src } => {
                self.space();
                self.print_operand(dest, ImmReprHint::Hex, false);  // always has 1 reg operand
                self.print_symbols(",");
                self.print_operand(src, ImmReprHint::Hex, false);
            }
            JumpIf { cc: _, target } => {
                self.space();
                self.print_jump_target_operand(target, true);
            }
            Jump { target } => {
                self.space();
                self.print_jump_target_operand(target, true);
            }
            Call { target } => {
                self.space();
                self.print_jump_target_operand(target, false);
            }
            Ret { pop } => {
                if *pop > 0 {
                    self.space();
                    self.print_immediate(&format!("{:}", pop));
                }
            }
            Push { operand } => {
                self.space();
                self.print_operand(operand, ImmReprHint::Hex, true);
            }
            Pop { reg } => {
                self.space();
                self.print_register(reg.name());
            }
            Lea { dest, src } => {
                self.space();
                self.print_register(dest.name());
                self.print_symbols(",");
                self.print_operand(src, ImmReprHint::Hex, false);
            }
            Test { lhs, rhs } => {
                self.space();
                self.print_operand(lhs, ImmReprHint::Hex, false);   // always has 1 reg
                self.print_symbols(",");
                self.print_operand(rhs, ImmReprHint::Hex, false);
            }
            Not { operand } => {
                self.space();
                self.print_operand(operand, ImmReprHint::Hex, true);
            }
            Neg { operand } => {
                self.space();
                self.print_operand(operand, ImmReprHint::Dec, true);
            }
            Mul { operand } |
            Imul { operand } |
            Div { operand } |
            Idiv { operand } => {
                self.space();
                self.print_operand(operand, ImmReprHint::Dec, true);
            }
            ImulTrunc { dest, src1, src2 } => {
                self.space();
                self.print_register(dest.name());
                if src1 != &Operand::Reg(*dest) {
                    self.print_symbols(",");
                    self.print_operand(src1, ImmReprHint::Dec, false);  // same size as op0 (register)
                }
                self.print_symbols(",");
                self.print_operand(src2, ImmReprHint::Dec, false);  // size not important
            }
            Inc { operand } |
            Dec { operand } => {
                self.space();
                self.print_operand(operand, ImmReprHint::Dec, true);
            }
            Cwd | Cdq => {},    // no operands
        }

        self.done();
    }
}

fn mnemonic(instr: &Instr) -> String {
    use cpu::instr::Instr::*;
    use cpu::instr::AluOp;

    let mut f = String::new();
    f.push_str(match instr {
        Alu { op, .. } => match op {
            AluOp::Add => "add",
            AluOp::Or => "or",
            AluOp::Adc => "adc",
            AluOp::Sbb => "sbb",
            AluOp::And => "and",
            AluOp::Sub => "sub",
            AluOp::Xor => "xor",
            AluOp::Cmp => "cmp",
        },
        Shift { op, .. } => match op {
            ShiftOp::Rol => "rol",
            ShiftOp::Ror => "ror",
            ShiftOp::Rcl => "rcl",
            ShiftOp::Rcr => "rcr",
            ShiftOp::Shl => "shl",
            ShiftOp::Shr => "shr",
            ShiftOp::Sal => "sal",
            ShiftOp::Sar => "sar",
        }
        Mov { .. } => "mov",
        JumpIf { .. } => "j",
        Jump { .. } => "jmp",
        Call { .. } => "call",
        Ret { .. } => "ret",
        Push { .. } => "push",
        Pop { .. } => "pop",
        Lea { .. } => "lea",
        Test { .. } => "test",
        Not { .. } => "not",
        Neg { .. } => "neg",
        Mul { .. } => "mul",
        Imul { .. } | ImulTrunc { .. } => "imul",
        Div { .. } => "div",
        Idiv { .. } => "idiv",
        Inc { .. } => "inc",
        Dec { .. } => "dec",
        Cwd => "cwd",
        Cdq => "cdq",
    });

    if let JumpIf { cc, .. } = instr {
        f.push_str(condition_code(*cc));
    }

    f
}

fn condition_code(cc: ConditionCode) -> &'static str {
    use self::ConditionCode::*;

    match cc {
        Above => "a",
        NotCarry => "nc",
        Carry => "c",
        BelowOrEqual => "be",
        Equal => "e",
        Greater => "g",
        GreaterOrEqual => "ge",
        Less => "l",
        LessOrEqual => "le",
        NotEqual => "ne",
        NotOverflow => "no",
        NotParity => "np",
        NotSign => "ns",
        Overflow => "o",
        Parity => "p",
        Sign => "s",
    }
}

pub fn print_instr<P: Printer>(instr: &Instr, p: &mut P) {
    p.print_instr(instr);
}
