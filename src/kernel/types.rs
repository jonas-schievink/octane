//! Defines types, traits and conversions for kernel variables and function
//! arguments.

use std::{fmt, mem};
use std::marker::PhantomData;

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
#[derive(Debug, Default)]
#[repr(transparent)]
pub struct Ulong(u32);

impl KernelVariable for Ulong {}
impl From<u32> for Ulong {
    fn from(raw: u32) -> Self {
        Ulong(raw)
    }
}

#[derive(Default)]
#[repr(transparent)]
pub struct Boolean(bool);

impl From<u32> for Boolean {
    fn from(raw: u32) -> Self {
        // In true C fashion, we interpret anything that's not 0 as "true"
        Boolean(raw != 0)
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
pub struct XPtr<T> {
    addr: u32,
    _phantom: PhantomData<*const T>,
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
#[allow(bad_style)]
#[derive(Debug)]
pub enum NtStatus {
    STATUS_SUCCESS = 0x00000000,
}

impl Into<u32> for NtStatus {
    fn into(self) -> u32 {
        self as u32
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
    fn from_args(raw: &[u32]) -> Self;
}

macro_rules! tuple_impl {
    ($($tyvar:ident),*) => {
        impl<$($tyvar),*> FromRawArgs for ( $($tyvar,)* )
        where $($tyvar: From<u32>),*
        {
            fn from_args(raw: &[u32]) -> Self {
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
