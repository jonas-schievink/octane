//! Implements the XBE loader.

use xbe::Xbe;
use memory::{VirtualMemory, MapError};

use std::error::Error;
use std::fmt;

/// Load an XBE into memory and prepare it for execution.
pub fn load<M: VirtualMemory>(xbe: &Xbe, mem: &mut M) -> Result<LoaderInfo, LoaderError> {
    // Map the XBE headers at the specified base address
    let xbe_range = xbe.base_address()..=xbe.base_address()+xbe.header_size();
    mem.add_mapping(xbe_range, &xbe.raw_data()[..xbe.header_size() as usize], "<xbe headers>")
        .map_err(LoaderError)?;

    // XBE's usually specify the right values so that the mapped headers end
    // exactly where the first section starts (after rounding up to whole pages)

    // FIXME we probably want the xbe crate to check that no sections overlap
    for section in xbe.sections() {
        let range = section.virt_range();
        mem.add_mapping(range, section.data(), section.name())
            .map_err(LoaderError)?;
    }

    // TODO: Resolve kernel imports

    // TODO: Allocate stack properly
    let stack_start = 0x7000_0000;
    let stack_size = 1024 * 64; // 64 KB
    let stack_end = stack_start + stack_size;

    mem.add_mapping(stack_start..=stack_end, &[], "<stack>")
        .map_err(LoaderError)?;

    info!("loaded '{}'. memory map:", xbe.title_name());
    for mapping in mem.mappings() {
        info!("{}", mapping);
    }

    Ok(LoaderInfo {
        esp: stack_end,
    })
}

/// Result of the loader.
///
/// The loader mainly modifies the virtual memory map, but also returns a
/// handful of additional values in this struct.
#[derive(Debug)]
pub struct LoaderInfo {
    /// First address after the allocated stack.
    ///
    /// This is suitable as the initial value of the `esp` register (`push`
    /// decrements the stack pointer first, then writes the value).
    pub esp: u32,
}

#[derive(Debug)]
pub struct LoaderError(MapError);

impl Error for LoaderError {}

impl fmt::Display for LoaderError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.0.fmt(f)
    }
}
