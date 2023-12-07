use crate::builtin_function;

// TODO: Traits and stuff
pub fn push() {
    // TODO: Basically implement this
    builtin_method!(
        Slice.push(value:Value) -> Slice,
        |this: Slice, args| {
                        let SliceValue(slice) = this;

                        let value_type = &slice[0].as_ref().into();
                        for value in values.into_vec() {
                                value.assert_type_of(value_type)?;
                                slice.push(Cow::Owned(value));
                        }

                        Ok(Value::None)
        }
    )
}
