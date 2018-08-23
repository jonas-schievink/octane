//! Object management, up/downcasting, handle association.

use kernel::types::*;
use kernel::ps::Thread;

use generational_arena::{Arena, Index};
use std::collections::BTreeMap;
use std::collections::btree_map::Entry;

#[derive(Debug)]
struct ObjectSetEntry {
    /// The stored object.
    object: Object,
    /// The number of handles pointing at this object.
    refcount: u32,
}

/// Stores a collections of `Object`s along with `Handle`s associated with each
/// object.
///
/// See `types::Handle<T>`.
#[derive(Debug)]
pub struct ObjectSet {
    /// Maps handle values to indices into the `objects` arena.
    map: BTreeMap<u32, Index>,
    /// Object arena.
    objects: Arena<ObjectSetEntry>,
}

impl ObjectSet {
    /// Creates a new, empty object set.
    pub fn new() -> Self {
        Self {
            map: BTreeMap::new(),
            objects: Arena::new(),
        }
    }

    /// Insert a new object into the set.
    ///
    /// The handle must not refer to an existing object. If it does, this method
    /// will panic.
    pub fn insert<T>(&mut self, handle: &Handle<T>, value: T)
    where
        T: Into<Object>,
        Object: Downcast<T>,
    {
        match self.map.entry(handle.raw_addr()) {
            Entry::Vacant(e) => {
                let index = self.objects.insert(ObjectSetEntry {
                    object: value.into(),
                    refcount: 1,
                });
                e.insert(index);
            }
            Entry::Occupied(_) => {
                panic!("attempted to overwrite value associated to handle {:?}", handle);
            }
        }
    }

    /// Duplicate the handle `src` into a new handle `dest`, making both refer
    /// to the same object.
    ///
    /// `dest` must not refer to a value, while `src` must refer to a valid
    /// value in `self`. If this is not the case, an error is returned.
    ///
    /// Neither `src` nor `dest` will be modified. The handle values will just
    /// be remapped to different objects.
    pub fn dup<T>(&mut self, src: &Handle<T>, dest: &mut Handle<T>) -> Result<(), ()> {
        let index = *self.map.get(&src.raw_addr()).ok_or(())?;
        match self.map.entry(dest.raw_addr()) {
            Entry::Vacant(e) => {
                self.objects[index].refcount += 1;
                e.insert(index);
            }
            Entry::Occupied(_) => {
                panic!("dup: dest handle {:?} already associated", dest);
            }
        }

        Ok(())
    }

    /// Decrements the reference count of the object referred to by `handle`.
    ///
    /// If the reference count reaches 0, the object will be destroyed.
    ///
    /// Returns an error if the handle is not associated with an object.
    pub fn remove<T>(&mut self, handle: Handle<T>) -> Result<Option<Object>, ()> {
        let index = self.map.remove(&handle.raw_addr()).ok_or(())?;
        {
            // slightly contorted control flow to avoid double lookup
            let obj = &mut self.objects[index];
            if obj.refcount > 1 {
                obj.refcount -= 1;
                return Ok(None);
            }
        }

        // last reference, destroy the object
        let entry = self.objects.remove(index).unwrap();
        Ok(Some(entry.object))
    }

    /// Obtain a reference to the object referred to by `handle`.
    pub fn get<T>(&self, handle: &Handle<T>) -> Option<&T>
    where Object: Downcast<T> {
        self.map.get(&handle.raw_addr())
            .map(|index| &self.objects[*index])
            .and_then(|obj| obj.object.try_downcast())
    }

    /*pub fn get_mut(&mut self, handle: &mut Handle<T>) -> Option<&mut T> {
        self.map.get_mut(&handle.raw_addr()).map(|index| &mut self.objects[*index])
    }*/
}

/// A Kernel object.
#[derive(Debug)]
pub enum Object {
    Thread(Thread),
    Dummy,
}

impl Object {
    /// The last handle to the object was closed, destroy it.
    pub fn destroy(self, _kernel: &mut super::Kernel) {
        match self {
            Object::Thread(_) => {
                // we don't have to do anything for threads since they can only
                // be destroyed via `PsTerminateSystemThread` which does
                // everything it needs to.
            }
            Object::Dummy => {}
        }
    }
}

/// Trait for types that can be cast to a more specific type `T`.
pub trait Downcast<T> {
    /// If `self` is an instance of `T`, returns a reference to the `T`. If not,
    /// returns `None`.
    fn try_downcast(&self) -> Option<&T>;

    /// If `self` is an instance of `T`, returns a mutable reference to the `T`.
    /// If not, returns `None`.
    fn try_downcast_mut(&mut self) -> Option<&mut T>;
}

/// A type `S` is a superclass of `T` if any `T` can be converted to `S` via
/// `Into`/`From` and if `S` can be downcasted to `T`.
pub trait SuperclassOf<T>: From<T> + Downcast<T> {}
impl<T, S> SuperclassOf<T> for S where S: From<T> + Downcast<T> {}

/// Implements upcasting the given type to an `Object` via `From<T>` and
/// downcasting an `Object` to `T` via `Downcast<T>`.
///
/// `Object` must have an appropriate variant to fit the type.
macro_rules! object_cast {
    ($variant:ident) => {
        impl From<$variant> for Object {
            fn from(t: $variant) -> Self {
                Object::$variant(t)
            }
        }

        impl Downcast<$variant> for Object {
            fn try_downcast(&self) -> Option<&$variant> {
                if let Object::$variant(v) = self {
                    Some(v)
                } else {
                    None
                }
            }

            fn try_downcast_mut(&mut self) -> Option<&mut $variant> {
                if let Object::$variant(v) = self {
                    Some(v)
                } else {
                    None
                }
            }
        }
    };
}

object_cast!(Thread);
