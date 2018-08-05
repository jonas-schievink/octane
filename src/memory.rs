//! x86 virtual memory implementation.
//!
//! Theoretically, it is relatively straightforward to imagine implementing the
//! Xbox CPUs virtual memory as a 4 GB area in our *own* virtual memory that we
//! can map different memory areas into. Any segmentation faults caused by the
//! game then show up as segmentation faults of the emulator and can be caught
//! by installing a `SIGSEGV` handler. Practically, this is very difficult to
//! implement, platform-specific, and pretty bug-prone.
//!
//! This difficulty means that it's pretty attractive to implement a slower but
//! simpler way to deal with the guest's virtual memory first. This is similar
//! to how an interpreter is a far simpler but slower implementation of a CPU
//! compared to a JIT.

// TODO: Revisit this and document the memory layout used by Windows NT on the Xbox
// no segmentation, flat virtual memory space, except the custom segment registers for the TIB

use memmap::MmapMut;

use std::collections::BTreeMap;
use std::ops::RangeInclusive;
use std::error::Error;
use std::{fmt, ptr, u32};

pub trait VirtualMemory {
    fn load(&self, virt_addr: u32) -> Result<u8, MemoryError>;

    /// Maps a block of data into the virtual address space.
    ///
    /// It is an error to call this when `virt_range` overlaps an already mapped
    /// piece of memory.
    ///
    /// If `data` is too small to fill the entire virtual range, it is padded
    /// with 0 bytes.
    fn add_mapping(&mut self, virt_range: RangeInclusive<u32>, data: &[u8]) -> Result<(), MapError>;

    fn load_i32(&self, virt_addr: u32) -> Result<i32, MemoryError> {
        let (b0, b1, b2, b3) = (
            self.load(virt_addr)? as u32,
            self.load(virt_addr + 1)? as u32,
            self.load(virt_addr + 2)? as u32,
            self.load(virt_addr + 3)? as u32,
        );

        Ok((
            b3 << 24 |
            b2 << 16 |
            b1 << 8 |
            b0
        ) as i32)
    }

    fn load_i16(&self, virt_addr: u32) -> Result<i16, MemoryError> {
        let (b0, b1) = (
            self.load(virt_addr)? as u16,
            self.load(virt_addr + 1)? as u16,
        );

        Ok((
            b1 << 8 |
            b0
        ) as i16)
    }
}

/// Very slow memory implementation that stores mapped data in tables.
#[derive(Debug)]
pub struct TableMemory {
    /// Maps start addresses of virtual memory blocks to statically mapped
    /// sections loaded from the XBE.
    static_map: BTreeMap<u32, Box<[u8]>>,
}

impl TableMemory {
    /// Creates a new, empty table-based virtual memory.
    pub fn new() -> Self {
        Self {
            static_map: BTreeMap::new(),
        }
    }

    fn entries_overlapping(&self, virt_range: RangeInclusive<u32>) -> impl Iterator<Item=(u32, &[u8])> {
        self.static_map.range(virt_range.clone())   // overlaps definitely
            .chain(self.static_map.range(.. *virt_range.start())
                .next_back()
                .filter(|(start, data)| *start + (data.len() as u32) >= *virt_range.start())
            )
            .map(|(start, data)| (*start, &**data))
    }
}

impl VirtualMemory for TableMemory {
    fn load(&self, virt_addr: u32) -> Result<u8, MemoryError> {
        let (start, bytes) = self.entries_overlapping(virt_addr..=virt_addr)
            .next()
            .ok_or_else(|| MemoryError::Fault)?;    // not mapped?

        let entry_range = start ..= start + (bytes.len() as u32);
        debug!("load {:#010X}: found entry spanning {:#010X}..={:#010X}", virt_addr, entry_range.start(), entry_range.end());

        if *entry_range.end() < virt_addr {
            return Err(MemoryError::Fault);
        }

        Ok(bytes[(virt_addr - start) as usize])
    }

    fn add_mapping(&mut self, virt_range: RangeInclusive<u32>, data: &[u8]) -> Result<(), MapError> {
        if *virt_range.end() >= 0x8000_0000 {
            return Err(MapError::KernelSpace);
        }

        if self.entries_overlapping(virt_range.clone()).next().is_some() {
            return Err(MapError::Overlap);
        }

        let mut vec = data.to_vec();
        let virt_len = virt_range.end() - virt_range.start() + 1;
        vec.resize(virt_len as usize, 0);
        self.static_map.insert(*virt_range.start(), data.to_vec().into_boxed_slice());

        Ok(())
    }
}

/// A virtual memory implementation that stores everything in a `Vec`.
///
/// This is mostly useful for tests and benchmarks.
#[derive(Debug)]
pub struct ArrayMemory {
    mem: Vec<u8>,
}

impl ArrayMemory {
    pub fn new(data: Vec<u8>) -> Self {
        Self {
            mem: data
        }
    }

    pub fn as_vec_mut(&mut self) -> &mut Vec<u8> {
        &mut self.mem
    }
}

impl VirtualMemory for ArrayMemory {
    fn load(&self, virt_addr: u32) -> Result<u8, MemoryError> {
        self.mem.get(virt_addr as usize).cloned().ok_or(MemoryError::Fault)
    }

    fn add_mapping(&mut self, _: RangeInclusive<u32>, _: &[u8]) -> Result<(), MapError> {
        unimplemented!("ArrayMemory is static - `add_mapping` will not work")
    }
}

#[derive(Debug)]
pub struct MmapMemory {
    mapping: MmapMut,
}

impl MmapMemory {
    pub fn new() -> Self {
        Self {
            mapping: MmapMut::map_anon(u32::MAX as usize)
                .expect("could not map memory"),
        }
    }

    fn ptr(&self, virt_addr: u32) -> *const u8 {
        unsafe {
            self.mapping.as_ptr().offset(virt_addr as usize as isize)
        }
    }
}

impl VirtualMemory for MmapMemory {
    fn load(&self, virt_addr: u32) -> Result<u8, MemoryError> {
        // unchecked indexing possible since we map the whole 4G
        let val = unsafe { self.mapping.get_unchecked(virt_addr as usize) };
        Ok(*val)
    }

    fn add_mapping(&mut self, virt_range: RangeInclusive<u32>, data: &[u8]) -> Result<(), MapError> {
        if *virt_range.end() >= 0x8000_0000 {
            return Err(MapError::KernelSpace);
        }

        let mut vec = data.to_vec();
        let virt_len = virt_range.end() - virt_range.start() + 1;
        vec.resize(virt_len as usize, 0);

        // copy
        let range = *virt_range.start() as usize ..= *virt_range.end() as usize;
        self.mapping[range].copy_from_slice(&vec);

        Ok(())
    }

    fn load_i32(&self, virt_addr: u32) -> Result<i32, MemoryError> {
        unsafe {
            Ok(ptr::read_unaligned(self.ptr(virt_addr) as *const i32))
        }
    }

    fn load_i16(&self, virt_addr: u32) -> Result<i16, MemoryError> {
        unsafe {
            Ok(ptr::read_unaligned(self.ptr(virt_addr) as *const i16))
        }
    }
}

/// An error that can occur when reading or writing memory.
///
/// Both correspond to a page fault.
#[derive(Debug)]
pub enum MemoryError {
    /// Accessed address is not mapped at all.
    Fault,
    /// Accessed address is mapped as read-only and was attempted to be written
    /// to.
    NotWriteable,
}

impl fmt::Display for MemoryError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "memory error")
    }
}

impl Error for MemoryError {}

/// Error returned by `add_mapping`.
#[derive(Debug)]
pub enum MapError {
    /// The mapping would overlap with an existing one.
    Overlap,

    /// Attempted to map something into kernel-reserved address space (>2G).
    KernelSpace,
}

impl fmt::Display for MapError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            MapError::Overlap => write!(f, "existing mapping overlaps"),
            MapError::KernelSpace => write!(f, "attempt to map memory into kernel space"),
        }
    }
}

impl Error for MapError {}
