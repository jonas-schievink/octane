use xbe::Xbe;
use memory::{VirtualMemory, MapError};

use std::error::Error;
use std::fmt;

pub fn load<M: VirtualMemory>(xbe: &Xbe, mem: &mut M) -> Result<(), LoaderError> {
    // FIXME we probably want the xbe crate to check that no sections overlap
    // FIXME while we're at it, we need a way to display the virtual addr. space like glibc does
    for section in xbe.sections() {
        info!("mapping section '{}'", section.header().name());
        mem.add_mapping(section.header().virt_range(), section.data())
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
