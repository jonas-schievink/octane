//! x86 interpreter.

use cpu::decode::{Decoder, DecoderError};
use cpu::instr::*;
use cpu::state::State;
use cpu::flags::FlagSet;
use memory::{MemoryError, VirtualMemory};

use std::fmt;
use std::error::Error;

#[derive(Debug)]
pub struct Interpreter<M: VirtualMemory> {
    state: State,
    mem: M,
}

impl<M: VirtualMemory> Interpreter<M> {
    /// Creates a new interpreter.
    ///
    /// # Parameters
    ///
    /// * `mem`: The virtual memory space to operate on.
    /// * `eip`: Initial value of the `eip` register. Points to the first
    ///   instruction that will be executed.
    /// * `esp`: Initial stack pointer value. Usually points at the first
    ///   address behind the allocated stack.
    pub fn new(mem: M, eip: u32, esp: u32) -> Self {
        Self {
            state: State::new(eip, esp),
            mem,
        }
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

    /// Execute the next instruction.
    pub fn step(&mut self) -> Result<(), InterpreterError> {
        use cpu::instr::Instr::*;

        let instr = {
            let mut decoder = Decoder::new(&self.mem, self.state.eip());
            let instr = decoder.decode_next()?;
            self.state.set_eip(decoder.current_address());
            instr
        };

        match &instr {
            Alu { op, dest, src } => {
                let (lhs, rhs) = (self.eval_operand(dest)?, self.eval_operand(src)?);
                let (lhs, rhs) = (lhs.sign_extended(), rhs.sign_extended());
                match op {
                    AluOp::Sub => {
                        // subtract without borrow
                        let res = lhs - rhs;
                        self.state.update_flags(FlagSet::CF, lhs < rhs);
                        self.update_of_after_subtraction(lhs, rhs, res);
                        self.update_sfzfpf(res);
                        // TODO adjust flag
                        self.store_to_operand(dest, res)?;
                    }
                    _ => unimplemented!("alu op {:?}", op),
                }
            }
            Mov { dest, src } => {
                let value = self.eval_operand(src)?;
                self.store_to_operand(dest, value)?;
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
                match loc.size {
                    OpSize::Bits8 => self.mem.load(addr)?.into(),
                    OpSize::Bits16 => self.mem.load_i16(addr)?.into(),
                    OpSize::Bits32 => self.mem.load_i32(addr)?.into(),
                }
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
            index * u32::from(scale)
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

    /// Calculates new values for the SF, ZF and PF flags given the result of
    /// the performed operation.
    fn update_sfzfpf<T: Into<Immediate>>(&mut self, result: T) {
        // Extract SF, ZF, and the lowest result byte for PF
        let (sf, zf, pf_bits) = match result.into() {
            Immediate::Imm8(v) => (v < 0, v == 0, v as u8),
            Immediate::Imm16(v) => (v < 0, v == 0, v as i8 as u8),
            Immediate::Imm32(v) => (v < 0, v == 0, v as i8 as u8),
        };

        self.state.update_flags(FlagSet::SF, sf);
        self.state.update_flags(FlagSet::ZF, zf);
        self.state.update_flags(FlagSet::PF, pf_bits.count_ones() & 1 == 0);
    }

    fn update_of_after_addition(&mut self, op1: i32, op2: i32, res: i32) {
        // calculate sign bits
        let (op1, op2, res) = (op1 < 0, op2 < 0, res < 0);
        let over = op1 == op2 && op1 != res;    // inputs have same sign, but output changed
        self.state.update_flags(FlagSet::OF, over);
    }

    fn update_of_after_subtraction(&mut self, op1: i32, op2: i32, res: i32) {
        // calculate sign bits
        let (op1, op2, res) = (op1 < 0, op2 < 0, res < 0);
        let over = res == op2 && op1 != res;
        self.state.update_flags(FlagSet::OF, over);
    }
    // Overflow logic according to:
    // http://teaching.idallen.com/dat2343/10f/notes/040_overflow.txt
}

#[derive(Debug)]
pub enum InterpreterError {
    /// Error while decoding an instruction.
    Decode(DecoderError),
    /// Memory access error during execution of an instruction.
    Memory(MemoryError),
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

impl fmt::Display for InterpreterError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            InterpreterError::Decode(err) => err.fmt(f),
            InterpreterError::Memory(err) => err.fmt(f),
        }
    }
}

impl Error for InterpreterError {}
