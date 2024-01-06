pub(crate) enum Typ {
    Int,
    Bool,
    Var(String),
    Arrow(Box<ArrowType>),
}

pub(crate) struct ArrowType {
    pub(crate) in_typ: Typ,
    pub(crate) out_typ: Typ,
}



