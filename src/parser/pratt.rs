use std::collections::HashMap;

pub trait OpBindingPower {
    fn prefix_bp(&self, op: String) -> Option<usize>;
    fn infix_bp(&self, op: String) -> Option<(usize, usize)>;
    fn postfix_bp(&self, op: String) -> Option<usize>;
}

#[derive(Debug)]
pub struct PrattBindingPowers {
    prefix_bp: HashMap<String, usize>,
    infix_bp: HashMap<String, (usize, usize)>,
    postfix_bp: HashMap<String, usize>,
}

impl PrattBindingPowers {
    #[inline(always)]
    pub fn new() -> Self {
        Self {
            prefix_bp: HashMap::new(),
            infix_bp: HashMap::new(),
            postfix_bp: HashMap::new(),
        }
    }
}

impl OpBindingPower for PrattBindingPowers {
    #[inline(always)]
    fn prefix_bp(&self, op: String) -> Option<usize> {
        self.prefix_bp.get(&op).copied()
    }

    #[inline(always)]
    fn infix_bp(&self, op: String) -> Option<(usize, usize)> {
        self.infix_bp.get(&op).copied()
    }

    #[inline(always)]
    fn postfix_bp(&self, op: String) -> Option<usize> {
        self.postfix_bp.get(&op).copied()
    }
}
