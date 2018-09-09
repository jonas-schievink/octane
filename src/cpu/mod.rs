//! Emulation of the Xbox x86 CPU.

mod prefix;
mod flags;
mod defs;
mod state;
pub mod debugger;
pub mod decode;
pub mod disasm;
pub mod instr;
pub mod interpret;
pub mod visit;
pub mod imm;

pub use self::flags::Flags;
pub use self::state::State;

use memory::VirtualMemory;

// general goal that might be useful: replace all `as` casts with safer alternatives

/// Trait for CPU implementations / executors.
pub trait ExecutionEngine {
    type Memory: VirtualMemory;
    type Error;

    /// Get a reference to the CPU's state.
    fn state(&mut self) -> &mut State;

    /// Get a reference to the memory.
    fn memory(&mut self) -> &mut Self::Memory;

    /// Executes the next instruction, then returns control back to the caller.
    fn step(&mut self) -> Result<(), Self::Error>;

    /// Executes instructions in a loop.
    ///
    /// This is equivalent to calling `step` in a loop, but may be faster. Any
    /// error returned by `step` is returned to the caller.
    fn run(&mut self) -> Result<(), Self::Error>;
}
