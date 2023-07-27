use std::collections::HashSet;
use std::collections::HashMap;

#[derive(Debug)]
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

    pub fn state_symbols_map(&mut self, state: &str) ->
	&mut HashMap<String, HashSet<String>>
    {
	    self.transition_function
		.entry(state.to_string())
		.or_insert(HashMap::new())
    }

    pub fn symbol_states_set(&mut self, state: &str, symbol: &str) -> &mut HashSet<String> {
	if !self.alphabet.contains(symbol) {
	    eprintln!("Warning: Symbol {symbol} is not in alphabet {:?}", self.alphabet);
	}
	self.state_symbols_map(state)
	    .entry(symbol.to_string())
	    .or_insert(HashSet::new())
    }

    pub fn add_transition<'a>(&'a mut self, state: &str, symbol: &str, output: impl Iterator<Item = &'a String>) {
	output.for_each(
	    |x| {
		self.symbol_states_set(state, symbol).insert(x.to_string());
	    }
	);
    }
}
