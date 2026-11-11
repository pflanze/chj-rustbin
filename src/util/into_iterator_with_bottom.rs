//! An `IntoIterator` for sequences with generic Item type that allows
//! `()` as the empty sequence with fixed Item type
//!
//! If the item type is generic, then an empty sequence that doesn't
//! otherwise have a defined item type (e.g. literal `[]`) requires to
//! declare the item type explicitly--even though no item ever exists.
//!
//! The `IntoIteratorWithBottom` trait allows `()` as the empty
//! sequence with a type specified at `impl IntoIteratorWithBottom`
//! time, rather than at value passing time.

use std::marker::PhantomData;

pub trait IntoIteratorWithBottom<Item> {
    // (BTW, why does IntoIterator call this type IntoIter and not
    // Iter? I guess to indicate that it is a resulting type? But then
    // Item is down the line, too?)
    type Iter: Iterator<Item = Item>;
    fn into_iter_with_bottom(self) -> Self::Iter;
}

pub struct EmptyIterator<Item>(PhantomData<Item>);

impl<Item> Iterator for EmptyIterator<Item> {
    type Item = Item;

    fn next(&mut self) -> Option<Self::Item> {
        None
    }
}

// -----------------------------------------------------------------------------
// Make it workable for all situations, even though unergonomically:

/// Wrapper around any kind of Iterator to allow it to be used as
/// `IntoIteratorWithBottom`.
pub struct IteratorWrapper<Item, I: Iterator<Item = Item>>(I);

impl<Item, I: Iterator<Item = Item>> IntoIteratorWithBottom<Item>
    for IteratorWrapper<Item, I>
{
    type Iter = I;

    fn into_iter_with_bottom(self) -> Self::Iter {
        self.0
    }
}

// -----------------------------------------------------------------------------
// The whole point of this trait:

impl<Item> IntoIteratorWithBottom<Item> for () {
    type Iter = EmptyIterator<Item>;

    fn into_iter_with_bottom(self) -> Self::Iter {
        EmptyIterator(PhantomData)
    }
}

impl<'t, Item> IntoIteratorWithBottom<Item> for &'t () {
    type Iter = EmptyIterator<Item>;

    fn into_iter_with_bottom(self) -> Self::Iter {
        EmptyIterator(PhantomData)
    }
}

// -----------------------------------------------------------------------------
// Implement it for various sequence types:

impl<const N: usize, Item> IntoIteratorWithBottom<Item> for [Item; N] {
    type Iter = std::array::IntoIter<Item, N>;

    fn into_iter_with_bottom(self) -> Self::Iter {
        IntoIterator::into_iter(self)
    }
}

impl<'t, const N: usize, Item, IterItem: ?Sized>
    IntoIteratorWithBottom<&'t IterItem> for &'t [Item; N]
where
    &'t Item: AsRef<IterItem>,
    Item: AsRef<IterItem>,
{
    type Iter = std::iter::Map<
        std::slice::Iter<'t, Item>,
        fn(&'t Item) -> &'t IterItem,
    >;

    fn into_iter_with_bottom(self) -> Self::Iter {
        IntoIterator::into_iter(self).map(AsRef::<IterItem>::as_ref)
    }
}

impl<'t, const N: usize, Item, IterItem: ?Sized>
    IntoIteratorWithBottom<&'t IterItem> for &'t &'t [Item; N]
where
    &'t Item: AsRef<IterItem>,
    Item: AsRef<IterItem>,
{
    type Iter = std::iter::Map<
        std::slice::Iter<'t, Item>,
        fn(&'t Item) -> &'t IterItem,
    >;

    fn into_iter_with_bottom(self) -> Self::Iter {
        IntoIterator::into_iter(*self).map(AsRef::<IterItem>::as_ref)
    }
}

impl<'t, Item, IterItem: ?Sized> IntoIteratorWithBottom<&'t IterItem>
    for &'t [Item]
where
    &'t Item: AsRef<IterItem>,
    Item: AsRef<IterItem>,
{
    type Iter = std::iter::Map<
        std::slice::Iter<'t, Item>,
        fn(&'t Item) -> &'t IterItem,
    >;

    fn into_iter_with_bottom(self) -> Self::Iter {
        IntoIterator::into_iter(self).map(AsRef::<IterItem>::as_ref)
    }
}

impl<'t, Item, IterItem: ?Sized> IntoIteratorWithBottom<&'t IterItem>
    for &'t &'t [Item]
where
    &'t Item: AsRef<IterItem>,
    Item: AsRef<IterItem>,
{
    type Iter = std::iter::Map<
        std::slice::Iter<'t, Item>,
        fn(&'t Item) -> &'t IterItem,
    >;

    fn into_iter_with_bottom(self) -> Self::Iter {
        IntoIterator::into_iter(*self).map(AsRef::<IterItem>::as_ref)
    }
}

impl<Item> IntoIteratorWithBottom<Item> for Vec<Item> {
    type Iter = std::vec::IntoIter<Item>;

    fn into_iter_with_bottom(self) -> Self::Iter {
        IntoIterator::into_iter(self)
    }
}

impl<'t, Item, IterItem: ?Sized> IntoIteratorWithBottom<&'t IterItem>
    for &'t Vec<Item>
where
    &'t Item: AsRef<IterItem>,
    Item: AsRef<IterItem>,
{
    type Iter = std::iter::Map<
        std::slice::Iter<'t, Item>,
        fn(&'t Item) -> &'t IterItem,
    >;

    fn into_iter_with_bottom(self) -> Self::Iter {
        IntoIterator::into_iter(self).map(AsRef::<IterItem>::as_ref)
    }
}

impl<'t, Item, IterItem: ?Sized> IntoIteratorWithBottom<&'t IterItem>
    for &'t &'t Vec<Item>
where
    &'t Item: AsRef<IterItem>,
    Item: AsRef<IterItem>,
{
    type Iter = std::iter::Map<
        std::slice::Iter<'t, Item>,
        fn(&'t Item) -> &'t IterItem,
    >;

    fn into_iter_with_bottom(self) -> Self::Iter {
        IntoIterator::into_iter(*self).map(AsRef::<IterItem>::as_ref)
    }
}
