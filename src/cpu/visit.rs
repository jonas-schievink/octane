//! Contains x86 instruction visitor machinery.

use cpu::instr::*;

/// Instruction visitor.
///
/// When overriding a method, call the corresponding `walk_*` method to keep the
/// default behaviour of descending into the object. Omit it to stop descending.
pub trait Visitor {
    /// Visit the whole instruction.
    ///
    /// This is called when a new instruction is about to be processed. By
    /// default, it calls into `walk_instr`, which will decompose the
    /// instruction into its prefixes, mnemonic and operands and call the
    /// corresponding visitor methods on those.
    fn visit_instr(&mut self, instr: &Instr) { walk_instr(self, instr) }
    fn visit_prefixes(&mut self, _prefixes: &[&str]) {}
    fn visit_mnemonic(&mut self, _mnemonic: &str) {}

    /// Visit the assembly-level operands of an instruction.
    fn visit_operands(&mut self, operands: &[&Operand]) {
        for op in operands {
            walk_operand(self, op);
        }
    }

    /// Visit a register operand.
    fn visit_register(&mut self, _reg: Register) {}

    /// Visit an immediate operand.
    ///
    /// Note that, depending on the instruction, this might be an interrupt or
    /// port number instead of a numeric operand.
    fn visit_immediate(&mut self, _imm: &Immediate) {}

    /// Visit a memory location operand.
    fn visit_memory_location(&mut self, _mem: &MemoryLocation) {}
}

/// Decomposes an `Instr` into its components and calls the corresponding
/// visitor methods.
pub fn walk_instr<V: Visitor + ?Sized>(v: &mut V, instr: &Instr) {
    use cpu::instr::Instr::*;

    v.visit_prefixes(&prefixes(instr));
    v.visit_mnemonic(&mnemonic(&instr));

    match instr {
        // operand, operand
        Alu { dest, src, op: _ }
        | Shift { dest, src, op: _ }
        | Mov { dest, src }
        | Test { lhs: dest, rhs: src } => {
            v.visit_operands(&[dest, src]);
        }
        // register, operand
        MovZx { dest, src }
        | MovSx { dest, src }
        | BitScan { dest, src, .. } => {
            let dest = &Operand::Reg(*dest);
            v.visit_operands(&[dest, src]);
        }
        JumpIf { cc: _, target }
        | Jump { target }
        | Call { target } => {
            v.visit_operands(&[target]);
        }
        Ret { pop } => {
            if *pop > 0 {
                let pop = &Operand::Imm(Immediate::from(*pop));
                v.visit_operands(&[pop]);
            }
        }
        Push { operand }
        | Pop { operand }
        | Not { operand }
        | Neg { operand }
        | Mul { operand }
        | Imul { operand }
        | Div { operand }
        | Idiv { operand }
        | SetIf { cc: _, operand }
        | Inc { operand }
        | Dec { operand } => {
            v.visit_operands(&[operand]);
        }
        Lea { dest, src } => {
            let dest = &Operand::Reg(*dest);
            let src = &Operand::Mem(src.clone());
            v.visit_operands(&[dest, src]);
        }
        ImulTrunc { dest, src1, src2 } => {
            // 2/3 operand imul
            let dest = &Operand::Reg(*dest);
            if src1 == dest {
                // 2 operands
                v.visit_operands(&[dest, src2]);
            } else {
                // 3 operands
                v.visit_operands(&[dest, src1, src2]);
            }
        }
        Int { vector } => {
            let vector = &Operand::Imm(Immediate::from(*vector));
            v.visit_operands(&[vector]);
        }
        StrMem { op: StrMemOp::Outs(port), .. }
        | StrMem { op: StrMemOp::Ins(port), .. } => {
            let port = &Operand::Imm(Immediate::from(*port));
            v.visit_operands(&[port]);
        }
        Leave { .. }
        | IntO
        | StrMem { .. }
        | Cwd
        | Cdq => {},    // no operands
    }
}

/// Decomposes an operand into its components and calls the corresponding
/// visitor methods.
pub fn walk_operand<V: Visitor + ?Sized>(v: &mut V, op: &Operand) {
    match op {
        Operand::Reg(reg) => v.visit_register(*reg),
        Operand::Imm(imm) => v.visit_immediate(imm),
        Operand::Mem(mem) => v.visit_memory_location(mem),
    }
}

fn prefixes(instr: &Instr) -> Vec<&'static str> {
    use cpu::instr::Instr::*;

    match instr {
        Alu { .. }
        | Shift { .. }
        | Mov { .. }
        | MovZx { .. }
        | MovSx { .. }
        | JumpIf { .. }
        | Jump { .. }
        | SetIf { .. }
        | Call { .. }
        | Ret { .. }
        | Push { .. }
        | Pop { .. }
        | Lea { .. }
        | Test { .. }
        | Not { .. }
        | Neg { .. }
        | Mul { .. }
        | Imul { .. }
        | ImulTrunc { .. }
        | Div { .. }
        | Idiv { .. }
        | Inc { .. }
        | Dec { .. }
        | Int { .. }
        | BitScan { .. }
        | IntO
        | Cwd
        | Cdq => vec![],  // prefixes don't need display or are unsupported
        StrMem { rep: true, .. } => vec!["rep"],
        StrMem { rep: false, .. } => vec![],
        Leave { size: OpSize::Bits16 } => vec!["data16"],
        Leave { size: _ } => vec![],
    }
}

fn mnemonic(instr: &Instr) -> String {
    use cpu::instr::Instr::*;
    use cpu::instr::AluOp;

    let simple = match instr {
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
        MovZx { .. } => "movzx",
        MovSx { .. } => "movsx",
        JumpIf { cc, .. } => return format!("j{}", condition_code(*cc)),
        Jump { .. } => "jmp",
        SetIf { cc, .. } => return format!("set{}", condition_code(*cc)),
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
        StrMem { op, size, rep: _ } => {
            let mut s = String::new();
            s.push_str(match op {
                StrMemOp::Ins(_) => "ins",
                StrMemOp::Outs(_) => "outs",
                StrMemOp::Movs => "movs",
                StrMemOp::Lods => "lods",
                StrMemOp::Stos => "stos",
            });
            s.push_str(match size {
                OpSize::Bits8 => "b",
                OpSize::Bits16 => "w",
                OpSize::Bits32 => "d",
            });
            return s;
        },
        BitScan { reverse: false, .. } => "bsf",
        BitScan { reverse: true, .. } => "bsr",
        Leave { .. } => "leave",
        Int { .. } => "int",
        IntO => "into",
        Cwd => "cwd",
        Cdq => "cdq",
    };

    simple.to_string()
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
