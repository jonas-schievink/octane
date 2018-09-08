//! `Mm*` functions (**M**emory **M**anagement subsystem).

use memory::VirtualMemory;
use kernel::types::*;
use std::ops::RangeInclusive;
use std::cmp::Ordering;
use std::{fmt, u32};

/// The memory management subsystem.
#[derive(Debug)]
pub struct Subsystem {
    /// Available virtual memory ranges.
    ///
    /// This free list must always be as compact as possible, meaning that no
    /// unnecessary splits may be present (eg. `0..=1` and `1..=2` would be
    /// illegal and should be replaced by `0..=2`).
    free_ranges: Vec<FreeRange>,
}

impl Subsystem {
    /// Initializes the memory allocators.
    pub fn init() -> Self {
        Self {
            // Everything is available at the beginning. The loading process
            // will mark the right areas as used.
            free_ranges: vec![FreeRange::all()],
        }
    }

    /// Force-allocates the given virtual address range.
    ///
    /// The range is rounded to page boundaries and returned back. This is the
    /// effective usable range of addresses for the allocation.
    ///
    /// # Errors
    ///
    /// Returns an `AllocError` if `range` overlaps with an existing allocation.
    pub fn allocate_exact_range(&mut self, range: RangeInclusive<u32>) -> Result<RangeInclusive<u32>, AllocError> {
        trace!("allocate_exact_range({:#010X}..={:#010X})", range.start(), range.end());

        let range = round_to_page_boundary(range);

        // Find the free range containing `range` (or die tryin'). The free list
        // does actually form a strict ordering which we can exploit for a
        // binary search.
        let index = self.free_ranges.binary_search_by(|free| {
            if free.contains_range(range.clone()) {
                return Ordering::Equal;
            }
            if free.end() < *range.end() {
                return Ordering::Less;
            }
            assert!(free.start() > *range.start());
            Ordering::Greater
        }).map_err(|_| AllocError(Request::Range(range.clone())))?;

        self.alloc_exact_replace(range.clone(), index);
        Ok(range)
    }

    /// Allocate pages in virtual memory.
    ///
    /// This does not make the bytes accessible by the program. It only sets up
    /// the kernel's internal structures to mark the area as allocated.
    ///
    /// At least `bytes` Bytes will be allocated, but only whole 4K pages can be
    /// allocated.
    pub fn allocate(&mut self, bytes: u32) -> Result<RangeInclusive<u32>, AllocError> {
        // naive first-fit allocator
        let bytes = round_up_to_page(bytes);

        let free_range = self.free_ranges.iter().cloned().enumerate().find(|(_, range)| range.len() >= bytes);
        if let Some(free_range) = free_range {
            let alloc_range = free_range.1.start()..=free_range.1.start()+bytes-1;
            self.alloc_exact_replace(alloc_range.clone(), free_range.0);
            trace!("allocate({}) -> {:#X}..={:#X}", bytes, alloc_range.start(), alloc_range.end());
            Ok(alloc_range)
        } else {
            trace!("allocate({}) -> OOM", bytes);
            Err(AllocError(Request::Bytes(bytes)))
        }
    }

    /// Allocates `range` inside the free range with index `index`.
    ///
    /// `index` must refer to an existing range in the free list that contains
    /// the given `range` to allocate.
    fn alloc_exact_replace(&mut self, range: RangeInclusive<u32>, index: usize) {
        assert!(self.free_ranges[index].contains_range(range.clone()), "invalid index");

        let target = self.free_ranges[index].clone();
        // The target range will be replaced with 0, 1 or 2 new free ranges,
        // depending on whether the ranges align at their start and/or end.
        match (target.start() == *range.start(), target.end() == *range.end()) {
            (true, true) => {   // equal -> remove target range from free list
                self.free_ranges.remove(index);
            }
            (true, false) => {  // shorten `target` at beginning
                let new = target.split(range.end() + 1).1;
                self.free_ranges[index] = new;
            }
            (false, true) => {  // shorten `target` at end
                let new = target.split(*range.start()).0;
                self.free_ranges[index] = new;
            }
            (false, false) => { // split `target` into 2 free ranges
                let before = target.clone().split(*range.start()).0;
                let after = target.split(range.end() + 1).1;
                self.free_ranges.insert(index, before);
                self.free_ranges[index+1] = after;
            }
        }

        self.sanity_check();
    }

    /// Checks that the internal invariants of the free list are upheld.
    fn sanity_check(&self) {
        if self.free_ranges.is_empty() {
            return; // nothing to check - out of virtual memory
        }

        let mut prev = self.free_ranges.first().unwrap();
        for free in self.free_ranges.iter().skip(1) {
            assert!(prev.end() < free.start() + 1);
            prev = free;
        }
    }
}

/// An non-empty range of unallocated virtual memory pages.
#[derive(Debug, Clone)]
struct FreeRange(RangeInclusive<u32>);

impl FreeRange {
    /// A range spanning the entire virtual memory.
    fn all() -> Self {
        FreeRange(0x00000000 ..= 0xffffffff)
    }

    fn start(&self) -> u32 {
        *self.0.start()
    }

    fn end(&self) -> u32 {
        *self.0.end()
    }

    /// Returns the number of bytes inside the range.
    fn len(&self) -> u32 {
        self.0.end() - self.0.start()
    }

    /// Splits this range at `addr` so that the first returned range contains
    /// all addresses `<addr` and the second range all `>=addr`.
    ///
    /// `addr` must be page-aligned and inside `self`. If it isn't, this method
    /// will panic.
    fn split(self, addr: u32) -> (Self, Self) {
        let (start, end) = (self.start(), *self.0.end());
        assert!(page_aligned(addr), "{:#010X} is not page-aligned", addr);
        assert!(
            addr >= start && addr <= end,
            "address {:#010X} outside of range {:#010X}..={:#010X}",
            addr, start, end
        );

        let (first, second) = (
            FreeRange(start..=addr-1),
            FreeRange(addr..=end),
        );
        assert!(page_aligned(first.start()));
        assert!(page_aligned(first.end() + 1));
        assert!(page_aligned(second.start()));
        if second.end() < u32::MAX {
            assert!(page_aligned(second.end() + 1));
        }
        assert!(first.start() < first.end(), "first range is empty {:?}", first);
        assert!(second.start() < second.end(), "second range is empty {:?}", second);

        (first, second)
    }

    /// Returns `true` if `self` completely contains `other`.
    fn contains_range(&self, other: RangeInclusive<u32>) -> bool {
        self.start() <= *other.start() && self.end() >= *other.end()
    }
}

// FIXME: This might want to use the host's page size
const PAGE_SIZE: u32 = 4096;
const PAGE_SIZE64: u64 = PAGE_SIZE as u64;

fn round_to_page_boundary(range: RangeInclusive<u32>) -> RangeInclusive<u32> {
    let start = u64::from(*range.start()) & !(PAGE_SIZE64 - 1);
    let end = ((u64::from(*range.end()) + PAGE_SIZE64) & !(PAGE_SIZE64 - 1)) - 1;
    assert!(start <= u64::from(u32::MAX));
    assert!(end <= u64::from(u32::MAX));

    start as u32 ..= end as u32
}

/// Rounds the argument up to a multiple of the page size.
fn round_up_to_page(v: u32) -> u32 {
    let rounded = (u64::from(v) + PAGE_SIZE64 - 1) & !(PAGE_SIZE64 - 1);
    assert!(rounded <= u64::from(u32::MAX));
    rounded as u32
}

fn page_aligned(addr: u32) -> bool {
    addr & !(PAGE_SIZE - 1) == addr
}

#[derive(Debug)]
enum Request {
    Range(RangeInclusive<u32>),
    Bytes(u32),
}

/// Error returned by allocation functions when not enough memory is available.
#[derive(Debug)]
pub struct AllocError(Request);

impl fmt::Display for AllocError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match &self.0 {
            Request::Range(range) => write!(f, "range overlaps existing allocation: {:#010X}..={:#010X}", range.start(), range.end()),
            Request::Bytes(b) => write!(f, "out of memory when allocating {} Bytes", b),
        }
    }
}

#[allow(non_snake_case)]
impl<'a, M: VirtualMemory> super::Syscall<'a, M> {
    /// Allocates a number of memory pages.
    pub fn allocate_pages(&mut self, bytes: u32) -> u32 {
        match self.kernel.mm.allocate(bytes) {
            Ok(range) => {
                // map 0 bytes to the allocated pages
                self.mem.add_mapping(range.clone(), &[], "MmAllocateContiguousMemory").unwrap();
                *range.start()
            }
            Err(e) => {
                error!("{}", e);
                0
            }
        }
    }

    /// Allocates physically contiguous memory pages.
    pub fn MmAllocateContiguousMemory(&mut self, (number_of_bytes,): (Ulong,)) -> u32 {
        // FIXME: This doesn't actually allocate *physically contiguous* memory
        // because we currently do not model physical memory.
        self.allocate_pages(*number_of_bytes)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn round_page() {
        assert_eq!(round_to_page_boundary(0..=4095), 0..=4095);
        assert_eq!(round_to_page_boundary(1..=4095), 0..=4095);
        assert_eq!(round_to_page_boundary(1..=4094), 0..=4095);
        assert_eq!(round_to_page_boundary(0..=0), 0..=4095);
        assert_eq!(round_to_page_boundary(0..=1), 0..=4095);
        assert_eq!(round_to_page_boundary(4095..=4095), 0..=4095);
        assert_eq!(round_to_page_boundary(4095..=4096), 0..=4096 * 2 - 1);
        assert_eq!(round_to_page_boundary(4096..=4096), 4096..=4096 * 2 - 1);
    }

    #[test]
    fn round_page_up() {
        assert_eq!(round_up_to_page(0), 0);
        assert_eq!(round_up_to_page(1), PAGE_SIZE);
        assert_eq!(round_up_to_page(PAGE_SIZE), PAGE_SIZE);
        assert_eq!(round_up_to_page(PAGE_SIZE+1), PAGE_SIZE*2);
    }

    #[test]
    fn free_range() {
        let all = FreeRange::all();
        assert!(all.contains_range(0 ..= !0));
        let (a, b) = all.split(4096);
        assert!(a.contains_range(0 ..= 4096 - 1));
        assert!(!a.contains_range(0 ..= 4096));
        assert!(b.contains_range(4096 ..= 4096));
        assert!(!b.contains_range(0..=0));
        assert!(!b.contains_range(4096 - 1 ..= 10000));
    }

    #[test]
    fn allocate_exact() {
        let mut mm = Subsystem::init();
        mm.allocate_exact_range(0x00000000 ..= 1).unwrap();
        mm.allocate_exact_range(PAGE_SIZE ..= 0x10000000-1).unwrap();
        mm.allocate_exact_range(0x00000000 ..= 0x10000000-1).unwrap_err();
        mm.allocate_exact_range(0x00000000 ..= 0x00000000).unwrap_err();
        mm.allocate_exact_range(0x10000000-1 ..= 0x10000000-1).unwrap_err();
        assert_eq!(
            mm.allocate_exact_range(0x10000000 ..= 0x10000000).unwrap(),
            0x10000000 ..= 0x10001000-1
        );
        mm.allocate_exact_range(0x20000000 ..= 0x30000000-1).unwrap();
        mm.allocate_exact_range(0xE0000000 ..= 0xffffffff).unwrap();

        mm.allocate_exact_range(0x20001000 ..= 0x20006000).unwrap_err();
        mm.allocate_exact_range(0x1ff00000 ..= 0x20006000).unwrap_err();
        mm.allocate_exact_range(0x2ff01000 ..= 0x30006000).unwrap_err();
    }

    #[test]
    fn allocate() {
        let mut mm = Subsystem::init();
        assert_eq!(mm.allocate(1).unwrap(), 0x00000000..=PAGE_SIZE-1);
        assert_eq!(mm.allocate(1).unwrap(), PAGE_SIZE..=PAGE_SIZE*2-1);
        assert_eq!(mm.allocate(PAGE_SIZE).unwrap(), PAGE_SIZE*2..=PAGE_SIZE*3-1);
        assert_eq!(mm.allocate(PAGE_SIZE+1).unwrap(), PAGE_SIZE*3..=PAGE_SIZE*5-1); // 2 pages
    }
}
