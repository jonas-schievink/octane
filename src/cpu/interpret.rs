//! x86 interpreter.

// FIXME this is almost completely untested

use cpu::decode::{Decoder, DecoderError};
use cpu::instr::*;
use cpu::{State, Flags};
use memory::{MemoryError, VirtualMemory};

use std::fmt;
use std::error::Error;
use num_traits::PrimInt;
use num_traits::Signed;
use utils::NoDebug;
use cpu::ExecutionEngine;

/// Provides an interface to trace execution of individual instructions.
pub trait Trace<H: Hooks, M: VirtualMemory> {
    /// Called after an instruction has been decoded from the instruction
    /// stream, but before it is executed.
    ///
    /// # Parameters
    ///
    /// * `interp`: The interpreter, holding the CPU state and memory.
    /// * `eip`: Address of the instruction to be executed.
    /// * `instr`: The instruction decoded from memory address `eip`.
    fn trace(&mut self, interp: &mut Interpreter<H, M>, eip: u32, instr: &Instr);
}

impl<T, H: Hooks, M: VirtualMemory> Trace<H, M> for T
where T: for<'a> FnMut(&'a mut Interpreter<H, M>, u32, &'a Instr) {
    fn trace(&mut self, interp: &mut Interpreter<H, M>, eip: u32, instr: &Instr) {
        self(interp, eip, instr);
    }
}

/// Provides instruction hooks that can alter the interpreter's behaviour.
pub trait Hooks: Sized {
    /// `call` instruction.
    ///
    /// This can be hooked to perform HLE calls. The hook is invoked after the
    /// call target has been loaded from memory, but before the return address
    /// is pushed onto the stack.
    ///
    /// If this returns `HookAction::Nop`, the return address will not be pushed
    /// onto the stack at all.
    ///
    /// # Parameters
    ///
    /// * `interp`: Interpreter and system state.
    /// * `eip`: Return address pointing just after the `call` instruction.
    /// * `target`: The call target as a virtual address.
    fn call<M: VirtualMemory>(
        &mut self,
        interp: &mut Interpreter<Self, M>,
        eip: u32,
        target: u32,
    ) -> Result<HookAction, HookError> {
        let _ = (interp, eip, target);
        Ok(HookAction::Continue)
    }

    /// `ret` instruction.
    ///
    /// This is called after the stack pointer has been adjusted according to
    /// the `ret` instruction's parameters.
    ///
    /// If this returns `HookAction::Nop`, `eip` will not be changed.
    ///
    /// # Parameters
    ///
    /// * `interp`: The interpreter.
    /// * `target`: The return address to jump to.
    fn ret<M: VirtualMemory>(
        &mut self,
        interp: &mut Interpreter<Self, M>,
        target: u32,
    ) -> Result<HookAction, HookError> {
        let _ = (interp, target);
        Ok(HookAction::Continue)
    }
}

/// Determines how the interpreter should proceed after calling a hook.
#[derive(Debug, PartialEq, Eq)]
pub enum HookAction {
    /// Continue executing the instruction as if the hook wasn't there.
    Continue,
    /// Skip the instruction as if it's a nop. The hook already performed all
    /// required effects.
    ///
    /// Note that operands will still be loaded from memory before the hook
    /// itself is called, so any side effects from that will still happen.
    Nop,
    /// Exit the interpreter without executing further instructions.
    Exit,
}

/// An error returned by an interpreter hook.
#[derive(Debug)]
pub struct HookError(String);

impl HookError {
    pub fn new(msg: String) -> Self {
        HookError(msg)
    }
}

impl fmt::Display for HookError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.0.fmt(f)
    }
}

/// A struct implementing no-op `Hooks`. All instructions will behave like on a
/// "real" x86.
#[derive(Debug)]
pub struct NoHooks;

impl Hooks for NoHooks {}

/// The x86 instruction interpreter.
#[derive(Debug)]
pub struct Interpreter<'t, H: Hooks, M: VirtualMemory> {
    /// CPU state (registers).
    pub state: State,
    /// Virtual memory.
    pub mem: M,
    stopped: bool,
    hooks: Option<Box<H>>,
    tracer: Option<NoDebug<Box<Trace<H, M> + 't>>>,
}

impl<'t, H: Hooks, M: VirtualMemory> Interpreter<'t, H, M> {
    /// Creates a new interpreter.
    ///
    /// # Parameters
    ///
    /// * `mem`: The virtual memory space to operate on.
    /// * `state`: Initial CPU state.
    /// * `hooks`: A `Hooks` implementor.
    pub fn new(mem: M, state: State, hooks: H) -> Self {
        Self {
            state,
            mem,
            stopped: false,
            hooks: Some(Box::new(hooks)),
            tracer: None,
        }
    }

    /// Register a tracer object that will be called before each instruction is
    /// executed.
    pub fn set_tracer<T: Trace<H, M> + 't>(&mut self, tracer: T) {
        self.tracer = Some(NoDebug(Box::new(tracer)));
    }

    pub fn state(&self) -> &State {
        &self.state
    }

    pub fn state_mut(&mut self) -> &mut State {
        &mut self.state
    }

    pub fn mem(&self) -> &M {
        &self.mem
    }

    pub fn mem_mut(&mut self) -> &mut M {
        &mut self.mem
    }
    // TODO remove accessors

    pub fn is_stopped(&self) -> bool {
        self.stopped
    }

    /// Fetch, decode and execute the next instruction.
    pub fn step(&mut self) -> Result<(), InterpreterError> {
        if self.stopped {
            return Err(InterpreterError::Stopped);
        }

        // fetch and decode next instruction
        let (instr, new_eip) = {
            let mut decoder = Decoder::new(&self.mem, self.state.eip());
            let instr = decoder.decode_next()?;
            // FIXME this is not necessarily correct when faults happen
            (instr, decoder.current_address())
        };

        if let Some(mut tracer) = self.tracer.take() {
            let eip = self.state.eip();
            tracer.trace(self, eip, &instr);
            self.tracer = Some(tracer);
        }

        // FIXME: This is wrong when an exception occurs
        self.state.set_eip(new_eip);

        self.execute(&instr)
    }

    /// Execute a decoded instruction and perform its side effects.
    fn execute(&mut self, instr: &Instr) -> Result<(), InterpreterError> {
        use cpu::instr::Instr::*;

        match instr {
            Alu { op, dest, src } => {
                let (lhs, rhs) = (self.eval_operand(dest)?, self.eval_operand(src)?);
                let (lhs, rhs) = (lhs.sign_extended(), rhs.sign_extended());
                let res: i32 = match op {
                    AluOp::Sub | AluOp::Cmp => {
                        // subtract without borrow
                        let res = lhs.wrapping_sub(rhs);
                        self.state.update_flags(Flags::CF, lhs < rhs);
                        self.update_of_after_subtraction(lhs, rhs, res);
                        self.update_sfzfpf(res);
                        // TODO adjust flag
                        if *op == AluOp::Cmp {
                            // don't write back
                            return Ok(());
                        }
                        res
                    }
                    AluOp::Add => {
                        let res = lhs.wrapping_add(rhs);
                        let carry = (lhs as u32).checked_add(rhs as u32).is_none();
                        self.state.update_flags(Flags::CF, carry);
                        self.update_of_after_addition(lhs, rhs, res);
                        self.update_sfzfpf(res);
                        // TODO adjust flag
                        res
                    }
                    AluOp::And => {
                        let res = lhs & rhs;
                        self.state.update_flags(Flags::OF | Flags::CF, false);
                        self.update_sfzfpf(res);
                        res
                    }
                    AluOp::Or => {
                        let res = lhs | rhs;
                        self.state.update_flags(Flags::OF | Flags::CF, false);
                        self.update_sfzfpf(res);
                        res
                    }
                    AluOp::Xor => {
                        let res = lhs ^ rhs;
                        self.state.update_flags(Flags::OF | Flags::CF, false);
                        self.update_sfzfpf(res);
                        res
                    }
                    _ => unimplemented!("alu op {:?}", op),
                };
                // result is always an i32, so we need to truncate it down to
                // the expected size
                let res = Immediate::from(res).sign_ext_to(dest.size());
                self.store_to_operand(dest, res)?;
            }
            Shift { op, dest, src } => {
                assert_eq!(src.size(), OpSize::Bits8);
                let value = self.eval_operand(dest)?;
                let amount = self.eval_operand(src)?.as_u8().expect("non-8-bit shift amount?");

                if amount != 0 {    // count = 0 doesn't affect anything
                    // cast to the right type
                    let result = match value {
                        Immediate::Imm8(v) => perform_shift(v, *op, amount).into_imm(),
                        Immediate::Imm16(v) => perform_shift(v, *op, amount).into_imm(),
                        Immediate::Imm32(v) => perform_shift(v, *op, amount).into_imm(),
                    };
                    self.state.update_flags(Flags::CF, result.cf);
                    self.state.update_flags(Flags::OF, result.of);
                    self.update_sfzfpf(result.value);
                }
            }
            Mov { dest, src } => {
                let value = self.eval_operand(src)?;
                self.store_to_operand(dest, value)?;
            }
            MovZx { dest, src } => {
                let value = self.eval_operand(src)?;
                let value = value.zero_ext_to(dest.size());
                self.store_to_operand(&Operand::from(*dest), value)?;
            }
            MovSx { dest, src } => {
                let value = self.eval_operand(src)?;
                let value = value.sign_ext_to(dest.size());
                self.store_to_operand(&Operand::from(*dest), value)?;
            }
            JumpIf { cc, target } => {
                if self.eval_cc(*cc) {
                    // FIXME do the side effects happen when `cc` is false?
                    let addr = self.eval_operand(target)?.zero_extended();
                    self.state.set_eip(addr);
                }
            }
            Jump { target } => {
                let addr = self.eval_operand(target)?.zero_extended();
                self.state.set_eip(addr);
            }
            Call { target } => {
                let target = self.eval_operand(target)?.as_u32()
                    .expect("call target not 32-bit?");
                let eip = self.state.eip();

                match self.with_hooks(|interp, hooks| hooks.call(interp, eip, target))? {
                    HookAction::Continue => {
                        trace!("call: esp={:#010X}, push eip={:#010X}, target={:#010X}", self.state.esp(), eip, target);
                        self.push(eip.into())?;
                        self.state.set_eip(target);
                    }
                    HookAction::Nop | HookAction::Exit => {}
                }
            }
            Ret { pop } => {
                let esp = self.state.esp();
                let return_addr = self.mem.load_i32(esp)? as u32;
                let new_esp = esp + 4 + u32::from(*pop);
                self.state.set_esp(new_esp);

                match self.with_hooks(|interp, hooks| hooks.ret(interp, return_addr))? {
                    HookAction::Continue => {
                        self.state.set_eip(return_addr);
                        trace!("ret: esp={:#010X}, return={:#010X}, new esp={:#010X}", esp, return_addr, new_esp);
                    }
                    HookAction::Nop | HookAction::Exit => {
                        trace!("hook prevented return");
                    }
                }
            }
            Push { operand } => {   // FIXME <- bug in here
                let value = self.eval_operand(operand)?;
                trace!("push {}: {:#X}", value.size(), value);
                self.push(value)?;
            }
            Pop { operand } => {
                let value: Immediate = match operand.size() {
                    OpSize::Bits8 => panic!("attempted to pop single byte"),
                    OpSize::Bits16 => {
                        self.mem.load_i16(self.state.esp())?.into()
                    }
                    OpSize::Bits32 => {
                        self.mem.load_i32(self.state.esp())?.into()
                    }
                };
                self.store_to_operand(operand, value)?;
                // FIXME is esp only incremented if the memory access works?
                let esp = self.state.esp().wrapping_add(value.size().bytes());
                self.state.set_esp(esp);
            }
            Lea { dest, src } => {
                let addr = self.eval_address(src);
                self.store_to_operand(&Operand::Reg(*dest), addr)?;
            }
            Test { lhs, rhs } => {
                // like `And`
                let (lhs, rhs) = (self.eval_operand(lhs)?, self.eval_operand(rhs)?);
                let (lhs, rhs) = (lhs.zero_extended(), rhs.zero_extended());
                let res = lhs & rhs;
                self.state.update_flags(Flags::OF | Flags::CF, false);
                self.update_sfzfpf(res);
            }
            Idiv { operand } => {
                // FIXME double check this - it's probably wrong, the pseudo code
                // and docs are super confusing for this
                let divisor = self.eval_operand(operand)?.sign_extended() as i64;
                if divisor == 0 {
                    return Err(InterpreterError::DivisionException);
                }

                let dividend = match operand.size() {
                    OpSize::Bits8  => self.state.ax() as i16 as i64,
                    OpSize::Bits16 => {
                        ((self.state.dx() as u32) << 16 | self.state.ax() as u32) as i32 as i64
                    },
                    OpSize::Bits32 => {
                        ((self.state.edx() as u64) << 16 | self.state.eax() as u64) as i64
                    }
                };

                let quot: i64 = dividend / divisor;
                let rem: i64 = dividend % divisor;
                trace!("idiv: quot = {}, rem = {}", quot, rem);

                // store quotient or raise exception
                match operand.size() {
                    OpSize::Bits8 => {
                        // for each destination size, check if `quot` can
                        // "go there" and back without changing
                        if quot as i8 as i64 != quot {
                            return Err(InterpreterError::DivisionException);
                        }

                        self.state.set_al(quot as u8);
                        self.state.set_ah(rem as u8);
                    }
                    OpSize::Bits16 => {
                        if quot as i16 as i64 != quot {
                            return Err(InterpreterError::DivisionException);
                        }

                        self.state.set_ax(quot as u16);
                        self.state.set_dx(rem as u16);
                    }
                    OpSize::Bits32 => {
                        if quot as i32 as i64 != quot {
                            return Err(InterpreterError::DivisionException);
                        }

                        self.state.set_eax(quot as u32);
                        self.state.set_edx(rem as u32);
                    }
                }
            }
            Inc { operand } |
            Dec { operand } => {
                let inc = if let Inc { .. } = instr { true } else { false };
                let value = self.eval_operand(operand)?;
                let offset = if inc { 1 } else { -1 };
                let new_value = match value {
                    Immediate::Imm8(i) => Immediate::Imm8(i.wrapping_add(offset)),
                    Immediate::Imm16(i) => Immediate::Imm16(i.wrapping_add(offset.into())),
                    Immediate::Imm32(i) => Immediate::Imm32(i.wrapping_add(offset.into())),
                };
                self.store_to_operand(operand, new_value)?;

                let (lhs, rhs, res) = (value.sign_extended(), offset.into(), new_value.sign_extended());
                if inc {
                    self.update_of_after_addition(lhs, rhs, res);
                } else {
                    self.update_of_after_subtraction(lhs, rhs, res);
                }
                self.update_sfzfpf(new_value);
                // TODO adjust flag
            }
            Int { vector } => {
                if *vector == 3 {
                    // debugger breakpoint
                    return Err(InterpreterError::Breakpoint);
                } else {
                    panic!("unhandled interrupt vector {}", vector);
                }
            }
            Cwd => {
                let bit = self.state.ax() & 0x8000 != 0;
                self.state.set_dx(if bit { 0xffff } else { 0 });
            }
            Cdq => {
                let bit = self.state.eax() & 0x8000_0000 != 0;
                self.state.set_edx(if bit { 0xffffffff } else { 0 });
            }
            _ => unimplemented!("{}", instr.to_string()),
        }

        Ok(())
    }

    /// Evaluates an operand by returning the immediate, reading a register or
    /// reading from memory.
    ///
    /// Returns an error if a memory access fails.
    fn eval_operand(&self, operand: &Operand) -> Result<Immediate, InterpreterError> {
        Ok(match operand {
            Operand::Reg(reg) => self.state.get_register(*reg),
            Operand::Imm(imm) => *imm,
            Operand::Mem(loc) => {
                let addr = self.eval_address(loc);
                let value = match loc.size {
                    OpSize::Bits8 => self.mem.load(addr)?.into(),
                    OpSize::Bits16 => self.mem.load_i16(addr)?.into(),
                    OpSize::Bits32 => self.mem.load_i32(addr)?.into(),
                };
                trace!("eval_operand: loc {:?}={:#010X}: {} value={:#X}", loc, addr, loc.size, value);
                value
            }
        })
    }

    /// Evaluates the address of a `MemoryLocation`, which might depend on
    /// register values.
    ///
    /// Returns the virtual address referenced by `loc` at this point in time.
    /// Changing any state (registers) can change the referenced address.
    fn eval_address(&self, loc: &MemoryLocation) -> u32 {
        if loc.base_segment.may_be_used() {
            unimplemented!("TIB access / non-linear segmentation");
        }

        let base = match loc.addressing {
            Addressing::Disp { base, disp } |
            Addressing::Sib { base, disp, .. } => {
                let base = base.map(|reg| self.state.get_register(reg).zero_extended()).unwrap_or(0);
                base.wrapping_add(disp as u32)
            }
        };

        if let Addressing::Sib { scale, index, .. } = loc.addressing {
            let index = index.map(|reg| self.state.get_register(reg).zero_extended()).unwrap_or(0);
            base + index * u32::from(scale)
        } else {
            base
        }
    }

    /// Stores a value (in the form of an immediate) to a writeable operand.
    ///
    /// The `dest` operand must be a register or memory location.
    fn store_to_operand<I: Into<Immediate>>(
        &mut self,
        dest: &Operand,
        value: I
    ) -> Result<(), InterpreterError> {
        use cpu::instr::Register::*;

        let value = value.into();

        // Sizes must match. Use `Immediate::zero/sign_ext_to` to adjust.
        assert_eq!(dest.size(), value.size(), "cannot store {} value to {} operand", value.size(), dest.size());

        match dest {
            Operand::Reg(reg) => match reg {
                Al => self.state.set_al(value.as_u8().unwrap()),
                Ah => self.state.set_ah(value.as_u8().unwrap()),
                Bl => self.state.set_bl(value.as_u8().unwrap()),
                Bh => self.state.set_bh(value.as_u8().unwrap()),
                Cl => self.state.set_cl(value.as_u8().unwrap()),
                Ch => self.state.set_ch(value.as_u8().unwrap()),
                Dl => self.state.set_dl(value.as_u8().unwrap()),
                Dh => self.state.set_dh(value.as_u8().unwrap()),
                Ax => self.state.set_ax(value.as_u16().unwrap()),
                Bx => self.state.set_bx(value.as_u16().unwrap()),
                Cx => self.state.set_cx(value.as_u16().unwrap()),
                Dx => self.state.set_dx(value.as_u16().unwrap()),
                Eax => self.state.set_eax(value.as_u32().unwrap()),
                Ebx => self.state.set_ebx(value.as_u32().unwrap()),
                Ecx => self.state.set_ecx(value.as_u32().unwrap()),
                Edx => self.state.set_edx(value.as_u32().unwrap()),
                Si => self.state.set_si(value.as_u16().unwrap()),
                Esi => self.state.set_esi(value.as_u32().unwrap()),
                Di => self.state.set_di(value.as_u16().unwrap()),
                Edi => self.state.set_edi(value.as_u32().unwrap()),
                Bp => self.state.set_bp(value.as_u16().unwrap()),
                Ebp => self.state.set_ebp(value.as_u32().unwrap()),
                Sp => self.state.set_sp(value.as_u16().unwrap()),
                Esp => self.state.set_esp(value.as_u32().unwrap()),
            }
            Operand::Imm(_) => panic!("attempted to store to immediate"),
            Operand::Mem(loc) => {
                let addr = self.eval_address(loc);
                match loc.size {
                    OpSize::Bits8 => self.mem.store(addr, value.as_u8().unwrap())?,
                    OpSize::Bits16 => self.mem.store_u16(addr, value.as_u16().unwrap())?,
                    OpSize::Bits32 => self.mem.store_u32(addr, value.as_u32().unwrap())?,
                }
            }
        }

        Ok(())
    }

    /// Pushes a 16- or 32-bit value onto the stack and decrements the stack
    /// pointer accordingly.
    ///
    /// `value` may not be an 8-bit immediate.
    fn push(&mut self, value: Immediate) -> Result<(), InterpreterError> {
        match value {
            Immediate::Imm8(_) => panic!("cannot push 8-bit value"),
            Immediate::Imm16(i) => {
                let esp = self.state.esp().wrapping_sub(2);
                self.mem.store_u16(esp, i as u16)?;
                // FIXME esp is only decremented if the store succeeds, right?
                self.state.set_esp(esp);
            }
            Immediate::Imm32(i) => {
                let esp = self.state.esp().wrapping_sub(4);
                self.mem.store_u32(esp, i as u32)?;
                // FIXME esp is only decremented if the store succeeds, right?
                self.state.set_esp(esp);
            }
        }

        Ok(())
    }

    /// Calculates new values for the SF, ZF and PF flags given the result of
    /// the performed operation.
    fn update_sfzfpf<T: Into<Immediate>>(&mut self, result: T) {
        // Extract SF, ZF, and the lowest result byte for PF
        let (sf, zf, pf_bits) = match result.into() {
            Immediate::Imm8(v) => (v < 0, v == 0, v as u8),
            Immediate::Imm16(v) => (v < 0, v == 0, v as i8 as u8),
            Immediate::Imm32(v) => (v < 0, v == 0, v as i8 as u8),
        };

        self.state.update_flags(Flags::SF, sf);
        self.state.update_flags(Flags::ZF, zf);
        self.state.update_flags(Flags::PF, pf_bits.count_ones() & 1 == 0);
    }

    /// Update the overflow flag (OF) after performing the addition
    /// `op1 + op2 = res`.
    fn update_of_after_addition(&mut self, op1: i32, op2: i32, res: i32) {
        // calculate sign bits
        let (op1, op2, res) = (op1 < 0, op2 < 0, res < 0);
        let over = op1 == op2 && op1 != res;    // inputs have same sign, but output changed
        self.state.update_flags(Flags::OF, over);
    }

    /// Update the overflow flag (OF) after performing the subtraction
    /// `op1 - op2 = res`.
    fn update_of_after_subtraction(&mut self, op1: i32, op2: i32, res: i32) {
        // calculate sign bits
        let (op1, op2, res) = (op1 < 0, op2 < 0, res < 0);
        let over = res == op2 && op1 != res;
        self.state.update_flags(Flags::OF, over);
    }
    // Overflow logic according to:
    // http://teaching.idallen.com/dat2343/10f/notes/040_overflow.txt

    /// Checks a condition code against the currently set status flags.
    ///
    /// Returns `true` if the condition is fulfilled, `false` if not.
    fn eval_cc(&self, cc: ConditionCode) -> bool {
        use cpu::instr::ConditionCode::*;

        let flags = self.state.flags();
        let sf = flags.contains(Flags::SF);
        let of = flags.contains(Flags::OF);
        match cc {
            Above          => !flags.contains(Flags::CF | Flags::ZF),
            NotCarry       => !flags.contains(Flags::CF),
            Carry          => flags.contains(Flags::CF),
            BelowOrEqual   => flags.contains(Flags::CF) || flags.contains(Flags::ZF),
            Equal          => flags.contains(Flags::ZF),
            Greater        => !flags.contains(Flags::ZF) && (sf == of),
            GreaterOrEqual => sf == of,
            Less           => sf != of,
            LessOrEqual    => flags.contains(Flags::ZF) || (sf != of),
            NotEqual       => !flags.contains(Flags::ZF),
            NotOverflow    => !flags.contains(Flags::OF),
            NotParity      => !flags.contains(Flags::PF),
            NotSign        => !flags.contains(Flags::SF),
            Overflow       => flags.contains(Flags::OF),
            Parity         => flags.contains(Flags::PF),
            Sign           => flags.contains(Flags::SF),
        }
    }

    fn with_hooks<F>(&mut self, f: F) -> Result<HookAction, HookError>
    where F: FnOnce(&mut Self, &mut H) -> Result<HookAction, HookError> {
        let mut hooks = self.hooks.take().expect("hooks appear to have gone for a walk");
        let result = f(self, &mut hooks);
        self.hooks = Some(hooks);

        if let Ok(HookAction::Exit) = result {
            // Set the stop flag, but rely on the interpreter impl to not do
            // anything.
            self.stopped = true;
        }

        result
    }
}

impl<'t, H: Hooks, M: VirtualMemory> ExecutionEngine for Interpreter<'t, H, M> {
    type Memory = M;
    type Error = InterpreterError;

    fn state(&mut self) -> &mut State { &mut self.state }

    fn memory(&mut self) -> &mut Self::Memory { &mut self.mem }

    fn step(&mut self) -> Result<(), Self::Error> {
        self.step()
    }

    fn run(&mut self) -> Result<(), Self::Error> {
        loop {
            self.step()?;
        }
    }
}

struct ShiftResult<N> {
    value: N,
    cf: bool,
    of: bool,
}

impl<N> ShiftResult<N> where N: Into<Immediate> {
    fn into_imm(self) -> ShiftResult<Immediate> {
        ShiftResult {
            value: self.value.into(),
            cf: self.cf,
            of: self.of,
        }
    }
}

fn perform_shift<N: PrimInt + Signed>(mut value: N, op: ShiftOp, amount: u8) -> ShiftResult<N> {
    let orig = value;
    let amount = amount & 0b11111;  // almost all x86 processors mask this to 5 bits
    let mut cf = false;
    for _ in 0..amount {
        match op {
            ShiftOp::Sal | ShiftOp::Shl => {
                cf = value < N::zero();  // cl <- MSB(value)
                value = value << N::one().to_usize().unwrap();
            }
            ShiftOp::Shr => {
                // logic right shift
                cf = value & N::one() != N::zero();  // cl <- LSB(value)
                value = value >> N::one().to_usize().unwrap();
            }
            _ => unimplemented!("shit op {:?}", op),
        }
    }

    let mut of = false;
    if amount == 1 {
        of = match op {
            ShiftOp::Sal | ShiftOp::Shl => {
                let msb = value < N::zero();  // cl <- MSB(value)
                msb ^ cf
            }
            ShiftOp::Sar => false,
            ShiftOp::Shr => orig < N::zero(),  // cl <- MSB(orig)
            _ => unimplemented!(),
        }
    }

    ShiftResult {
        value,
        cf,
        of,
    }
}

#[derive(Debug)]
pub enum InterpreterError {
    /// Error while decoding an instruction.
    Decode(DecoderError),
    /// Memory access error during execution of an instruction.
    Memory(MemoryError),
    /// A hook returned an error.
    Hook(HookError),
    /// `#DE` was raised by a division instruction.
    DivisionException,
    /// An `int3` was hit.
    Breakpoint,
    /// Attempted to use a stopped interpreter.
    Stopped,
}

impl From<DecoderError> for InterpreterError {
    fn from(e: DecoderError) -> Self {
        InterpreterError::Decode(e)
    }
}

impl From<MemoryError> for InterpreterError {
    fn from(e: MemoryError) -> Self {
        InterpreterError::Memory(e)
    }
}

impl From<HookError> for InterpreterError {
    fn from(e: HookError) -> Self {
        InterpreterError::Hook(e)
    }
}

impl fmt::Display for InterpreterError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            InterpreterError::Decode(err) => err.fmt(f),
            InterpreterError::Memory(err) => err.fmt(f),
            InterpreterError::Hook(err) => err.fmt(f),
            InterpreterError::DivisionException => write!(f, "division exception"),
            InterpreterError::Breakpoint => write!(f, "hit breakpoint"),
            InterpreterError::Stopped => write!(f, "interpreter has exited"),
        }
    }
}

impl Error for InterpreterError {}
