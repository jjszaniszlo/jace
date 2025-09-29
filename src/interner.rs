use std::collections::HashMap;

/// String interner to reduce memory usage by deduplicating common strings
pub struct StringInterner {
    strings: Vec<String>,
    map: HashMap<String, usize>,
}

impl StringInterner {
    pub fn new() -> Self {
        let mut interner = Self {
            strings: Vec::with_capacity(256),
            map: HashMap::with_capacity(256),
        };
        
        // Pre-intern common keywords and identifiers
        interner.intern("main");
        interner.intern("print");
        interner.intern("Integer");
        interner.intern("String");
        interner.intern("Float");
        interner.intern("Bool");
        interner.intern("Option");
        interner.intern("Some");
        interner.intern("None");
        interner.intern("True");
        interner.intern("False");
        interner.intern("type");
        interner.intern("def");
        interner.intern("case");
        interner.intern("let");
        interner.intern("in");
        interner.intern("if");
        interner.intern("then");
        interner.intern("else");
        interner.intern("do");
        
        interner
    }
    
    pub fn intern(&mut self, s: &str) -> usize {
        if let Some(&id) = self.map.get(s) {
            id
        } else {
            let id = self.strings.len();
            self.strings.push(s.to_string());
            self.map.insert(s.to_string(), id);
            id
        }
    }
    
    pub fn get(&self, id: usize) -> Option<&str> {
        self.strings.get(id).map(|s| s.as_str())
    }
    
    pub fn get_string(&self, id: usize) -> Option<&String> {
        self.strings.get(id)
    }
}

impl Default for StringInterner {
    fn default() -> Self {
        Self::new()
    }
}