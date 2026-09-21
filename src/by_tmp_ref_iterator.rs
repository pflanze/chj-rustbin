/// Partially like `Iterator`, but handing out temporary references as
/// items

pub trait ByTmpRefIterator {
    type Item: ?Sized;
    type Error;

    fn next_tmp_ref<'s>(
        &'s mut self,
    ) -> Result<Option<&'s Self::Item>, Self::Error>;
}
