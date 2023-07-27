use std::collections::HashSet;
use std::collections::HashMap;

pub struct NFA {
    pub states: HashSet<String>,
    pub alphabet: HashSet<String>,
    pub transition_function:
    HashMap<String, HashMap<String, HashSet<String>>>,
    pub start_state: String,
    pub accept_states: HashSet<String>,
    pub is_deterministic: bool,
    pub dfa: Option<Box<NFA>>
}

impl NFA {
    pub fn new(is_deterministic: bool) -> NFA {
	NFA {
	    states: HashSet::new(),
	    alphabet: HashSet::new(),
	    transition_function: HashMap::new(),
	    start_state: String::new(),
	    accept_states: HashSet::new(),
	    is_deterministic,
	    dfa: None
	}
    }

    
}
