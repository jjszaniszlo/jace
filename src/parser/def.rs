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

impl From<OperatorMethodDef> for MethodDef {
    fn from(value: OperatorMethodDef) -> Self {
        MethodDef::Operator(value)
    }
}

impl From<NamedMethodDef> for MethodDef {
    fn from(value: NamedMethodDef) -> Self {
        MethodDef::Named(value)
    }
}
