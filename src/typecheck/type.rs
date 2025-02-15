use crate::parser::prelude::Identifier;

pub enum Type {
    // in the type lattice, all types are subtypes of Any.
    Any,
    // basically corresponds to an empty set.
    Empty,

    Atomic(AtomicType),
}

pub enum AtomicType {
    Integer,
    Float,
    String,
    Bool,
}