use xbe::Xbe;
use memory::{VirtualMemory, MapError};

use std::error::Error;
use std::fmt;

pub fn load<M: VirtualMemory>(xbe: &Xbe, mem: &mut M) -> Result<(), LoaderError> {
    let xbe_range = xbe.base_address()..=xbe.base_address()+xbe.raw_data().len() as u32;
    info!("mapping XBE '{}' to {:#010X}-{:#010X}", xbe.title_name(), xbe_range.start(), xbe_range.end());
    mem.add_mapping(xbe_range, xbe.raw_data())
        .map_err(LoaderError)?;

    // FIXME we probably want the xbe crate to check that no sections overlap
    // FIXME while we're at it, we need a way to display the virtual addr. space like glibc does
    for section in xbe.sections() {
        let range = section.virt_range();
        info!("mapping section '{}' to {:#010X}-{:#010X}", section.name(), range.start(), range.end());
        mem.add_mapping(range, section.data())
            .map_err(LoaderError)?;
    }

    // TODO: Resolve kernel imports

    Ok(())
}

#[derive(Debug)]
pub struct LoaderError(MapError);

impl Error for LoaderError {}

impl fmt::Display for LoaderError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.0.fmt(f)
    }
}
