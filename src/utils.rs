use std::fmt;
use std::ops::{Deref, DerefMut};

/// Wraps any value and suppresses its debug output when printed with `{:?}`.
pub struct NoDebug<T>(pub T);

impl<T> fmt::Debug for NoDebug<T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.write_str("(debug output omitted)")
    }
}

impl<T> Deref for NoDebug<T> {
    type Target = T;

    fn deref(&self) -> &T {
        &self.0
    }
}

impl<T> DerefMut for NoDebug<T> {
    fn deref_mut(&mut self) -> &mut T {
        &mut self.0
    }
}

impl<T> From<T> for NoDebug<T> {
    fn from(t: T) -> Self {
        NoDebug(t)
    }
}
