use core::cell;
use core::fmt;
use core::ops::{Deref, DerefMut};

#[derive(Default)]
pub struct Cell<T>(cell::Cell<T>);

impl<T> Cell<T> {
    pub fn new(value: T) -> Self {
        Self(cell::Cell::new(value))
    }
}

impl<T> Deref for Cell<T> {
    type Target = cell::Cell<T>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<T> DerefMut for Cell<T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

impl<A, T> From<A> for Cell<T>
where
    A: Into<cell::Cell<T>>,
{
    fn from(a: A) -> Self {
        Self(a.into())
    }
}

impl<T: Copy> Cell<T> {
    pub fn with<F, R>(&self, f: F) -> R
    where
        F: FnOnce(&T) -> R,
    {
        f(&self.get())
    }

    pub fn with_mut<F, R>(&self, f: F) -> R
    where
        F: FnOnce(&mut T) -> R,
    {
        let mut value = self.get();
        let result = f(&mut value);
        self.set(value);
        result
    }
}

pub trait CellDefaultExt<T> {
    fn get(&self) -> T
    where
        T: Clone;

    fn with<F, R>(&self, f: F) -> R
    where
        F: FnOnce(&T) -> R;

    fn with_mut<F, R>(&self, f: F) -> R
    where
        F: FnOnce(&mut T) -> R;
}

impl<T: Default> CellDefaultExt<T> for Cell<T> {
    fn get(&self) -> T
    where
        T: Clone,
    {
        let value = self.take();
        let clone = value.clone();
        self.set(value);
        clone
    }

    fn with<F, R>(&self, f: F) -> R
    where
        F: FnOnce(&T) -> R,
    {
        let value = self.take();
        let result = f(&value);
        self.set(value);
        result
    }

    fn with_mut<F, R>(&self, f: F) -> R
    where
        F: FnOnce(&mut T) -> R,
    {
        let mut value = self.take();
        let result = f(&mut value);
        self.set(value);
        result
    }
}

impl<T> fmt::Debug for Cell<T>
where
    T: Copy + fmt::Debug,
{
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.0.fmt(fmt)
    }
}
