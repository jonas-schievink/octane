//! Defines types, traits and conversions for kernel variables and function
//! arguments.

use std::{fmt, mem};
use std::marker::PhantomData;
use std::ops::Deref;
use memory;

/// Trait implemented by types used for exported kernel variables.
///
/// This tells the loader how to allocate memory for the type. The `Default`
/// requirement is needed so the loader fills the allocated memory with
/// appropriate bytes.
pub trait KernelVariable: Default + Sized {
    /// Size of the variable in bytes.
    const SIZE: u32 = mem::size_of::<Self>() as u32;
    /// Alignment required by the host system.
    const ALIGN: u32 = mem::align_of::<Self>() as u32;
}

/// `ULONG` (unsigned long) variable type. This is 32 bits on 32-bit x86.
#[derive(Debug, Default, Copy, Clone)]
#[repr(transparent)]
pub struct Ulong(u32);

impl KernelVariable for Ulong {}
impl From<u32> for Ulong {
    fn from(raw: u32) -> Self {
        Ulong(raw)
    }
}

impl Deref for Ulong {
    type Target = u32;

    fn deref(&self) -> &u32 {
        &self.0
    }
}

/// A boolean (zero = false, non-zero = true).
#[derive(Default, Copy, Clone)]
#[repr(transparent)]
pub struct Boolean(bool);

impl From<u32> for Boolean {
    fn from(raw: u32) -> Self {
        // In true C fashion, we interpret anything that's not 0 as "true"
        Boolean(raw != 0)
    }
}

impl Deref for Boolean {
    type Target = bool;

    fn deref(&self) -> &bool {
        &self.0
    }
}

impl fmt::Debug for Boolean {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.0.fmt(f)
    }
}

/// Xbox address space pointer to a value of type `T`.
///
/// Might be improperly aligned or point into garbage, since the emulated
/// program controls the memory and the passed pointer.
#[derive(Copy, Clone)]
pub struct XPtr<T> {
    addr: u32,
    _phantom: PhantomData<*const T>,
}

impl<T> XPtr<T> {
    pub fn raw_addr(&self) -> u32 {
        self.addr
    }
}

impl<T> From<u32> for XPtr<T> {
    fn from(raw: u32) -> Self {
        Self {
            addr: raw,
            _phantom: PhantomData,
        }
    }
}

impl<T> fmt::Debug for XPtr<T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:#010X}", self.addr)
    }
}

/// Magic virtual address assigned to a host value of type `T`.
///
/// The plan is that these are stored in host-side hash maps and look like
/// normal kernel space pointers to the program being run. When the program then
/// passes one of these to the kernel, it is looked up in the right hash map and
/// we fetch the host-controlled `T` and can't run into trouble where the
/// program overwrites it or hands us a garbage address (since it wouldn't be in
/// the map).
///
/// This is usable only for opaque pointers to kernel structures.
///
/// Note that `PHANDLE` is a program-controlled pointer (`XPtr`) to such a
/// handle.
pub struct Handle<T> {
    addr: u32,
    _phantom: PhantomData<*const T>,
}

impl<T> Handle<T> {
    /// The raw address as seen by the emulated program.
    pub fn raw_addr(&self) -> u32 {
        self.addr
    }
}

impl<T> Clone for Handle<T> {
    fn clone(&self) -> Self {
        Self {
            addr: self.addr,
            _phantom: PhantomData,
        }
    }
}

impl<T> From<u32> for Handle<T> {
    fn from(raw: u32) -> Self {
        Self {
            addr: raw,
            _phantom: PhantomData,
        }
    }
}

impl<T> fmt::Debug for Handle<T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:#010X}", self.addr)
    }
}

/// `NTSTATUS` enum indicating success or error conditions, returned by many
/// APIs.
// FIXME: These are taken from https://msdn.microsoft.com/en-us/library/cc704588.aspx
// check if those are the same on Xbox.
#[allow(bad_style)]
#[derive(Debug)]
pub enum NtStatus {
    STATUS_SUCCESS = 0x00000000,
    STATUS_ACCESS_VIOLATION = 0xC0000005,
    STATUS_NO_MEMORY = 0xC0000017,
}

impl Into<u32> for NtStatus {
    fn into(self) -> u32 {
        self as u32
    }
}

/// `Ok(())` converts to `STATUS_SUCCESS`, `Err` converts to its appropriate
/// status code.
impl<E> From<Result<(), E>> for NtStatus
where E: Into<NtStatus> {
    fn from(res: Result<(), E>) -> Self {
        match res {
            Ok(()) => NtStatus::STATUS_SUCCESS,
            Err(e) => e.into(),
        }
    }
}

impl From<memory::MemoryError> for NtStatus {
    fn from(_: memory::MemoryError) -> Self {
        NtStatus::STATUS_ACCESS_VIOLATION
    }
}

/// Trait for return types of API functions.
///
/// All return values eventually end up as a `u32` stored in the `eax` register,
/// so this can be seen as a custom version of `Into<u32>` that works with some
/// `Result`s.
pub trait ApiReturnValue {
    /// Convert `self` to a `u32`.
    ///
    /// This conversion must not fail.
    fn into_u32(self) -> u32;
}

// What we really want is a blanket impl for everything that impls `Into<u32>`,
// but the overlap rules prevent that.
impl ApiReturnValue for u32 {
    fn into_u32(self) -> u32 {
        self
    }
}

impl<S> ApiReturnValue for S
where S: Into<NtStatus> {
    fn into_u32(self) -> u32 {
        self.into().into()
    }
}

/// Trait for converting a slice of raw `u32` args to the right argument types
/// for kernel functions.
///
/// This is implemented for all tuples (or would be, if Rust had variadic
/// generics - in reality it's implemented for all tuples up to a certain
/// length) whose elements implement `From<u32>`.
///
/// When performing an HLE call, `from_args` is called to convert the arguments
/// pushed onto the stack by the program to the right types needed by the kernel
/// function.
///
/// None of these conversions can fail. The HLE code always passes the right
/// number of arguments, and `From<u32>` also can't fail. All kernel functions
/// need to check their arguments and return an appropriate error to the
/// program.
pub trait FromRawArgs {
    /// Convert a raw slice of `u32`s to the argument tuple.
    ///
    /// The code may assume that `raw` has the expected length for the
    /// conversion and panic if this is not the case.
    fn from_args(raw: &[u32]) -> Self;
}

macro_rules! tuple_impl {
    ($($tyvar:ident),*) => {
        impl<$($tyvar),*> FromRawArgs for ( $($tyvar,)* )
        where $($tyvar: From<u32>),*
        {
            fn from_args(raw: &[u32]) -> Self {
                // FIXME assert that `raw` isn't too long
                let mut _i = raw.iter().cloned();
                (
                    $(
                        $tyvar::from(_i.next().expect("bug: missing argument for HLE call - check arities"))
                    ,)*
                )
            }
        }
    };
}

tuple_impl!();
tuple_impl!(T);
tuple_impl!(T, U);
tuple_impl!(T, U, V);
tuple_impl!(T, U, V, W);
tuple_impl!(T, U, V, W, X);
tuple_impl!(T, U, V, W, X, Y);
tuple_impl!(T, U, V, W, X, Y, Z);
tuple_impl!(T, U, V, W, X, Y, Z, A);
tuple_impl!(T, U, V, W, X, Y, Z, A, B);
tuple_impl!(T, U, V, W, X, Y, Z, A, B, C);
tuple_impl!(T, U, V, W, X, Y, Z, A, B, C, D);
