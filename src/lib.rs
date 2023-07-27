use std::collections::HashSet;
use std::collections::HashMap;

#[derive(Debug)]
pub struct NFA {
    pub states: HashSet<String>,
    pub alphabet: HashSet<String>,
    pub transition_function: HashMap<String, HashMap<String, HashSet<String>>>,
    pub start_state: String,
    pub accept_states: HashSet<String>,
    pub is_deterministic: bool,
    pub dfa: Option<Box<NFA>>
}

#[derive(Debug)]
pub enum ComputationResult {
    Accept, Reject
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

    pub fn write_state_symbols_map(&mut self, state: &str) -> &mut HashMap<String, HashSet<String>> {
	    self.transition_function
		.entry(state.to_string())
		.or_insert(HashMap::new())
    }

    pub fn write_symbol_states_set(&mut self, state: &str, symbol: &str) -> &mut HashSet<String> {
	self.write_state_symbols_map(state)
	    .entry(symbol.to_string())
	    .or_insert(HashSet::new())
    }

    pub fn read_state_symbols_map(&self, state: &str) -> Option<&HashMap<String, HashSet<String>>> {
	self.transition_function.get(state)
    }

    pub fn read_symbol_states_set(&self, state: &str, symbol: &str) -> Option<&HashSet<String>> {
	match self.read_state_symbols_map(state) {
	    Some(state_symbols_map) => state_symbols_map.get(symbol),
	    None => None
	}
    }

    pub fn add_transition<'a>(&'a mut self, state: &str, symbol: &str, output: impl Iterator<Item = &'a String>) {
	let destination = self.write_symbol_states_set(state, symbol);
	output.for_each(
	    |x| {
		destination.insert(x.to_string());
	    }
	);
    }
}
