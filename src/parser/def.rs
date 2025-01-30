use super::*;

macro_rules! impl_from_for_def {
    ($($T:ident),+ $(,)?) => {
        $(
            impl From<$T> for Def {
                fn from(value: $T) -> Self {
                    Def::$T(value)
                }
            }
        )+
    };
}

impl_from_for_def!(FnDef, TypeDef, ClassDef, InstanceDef, ModuleDef);
