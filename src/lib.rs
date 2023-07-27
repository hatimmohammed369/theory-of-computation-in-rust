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

    pub fn init_new(states: &[&str], alphabet: &[&str], transition_function: &[(&str, &str, &[&str])], start_state: &str, accept_states: &[&str], is_deterministic: bool) -> NFA {
	let mut states_set = states.iter().map(|x| String::from(*x)).collect::<HashSet<String>>();
	let mut alphabet_set = alphabet.iter().map(|x| String::from(*x)).collect::<HashSet<String>>();

	if !states.contains(&start_state) {
	    eprintln!("Warning: Start State `{start_state}` is not in the states array");
	}
	states_set.insert(String::from(start_state));

	accept_states.iter().for_each(
	    |x| {
		if !states.contains(x) {
		    eprintln!("Warning: Accept State `{x}` is not in the states array");
		}
		states_set.insert(String::from(*x));
	    }
	);

	let mut function = HashMap::<String, HashMap<String, HashSet<String>>>::new();
	for item in transition_function {
	    let (state, symbol, items) = (item.0, item.1, item.2);
	    if !states.contains(&state) {
		eprintln!("Warning: State `{state}` is not in the states array");
		eprintln!("Found in transition {:?}", item);
	    }

	    if !alphabet.contains(&symbol) {
		eprintln!("Warning: Symbol `{symbol}` is not in the alphabet array");
		eprintln!("Found in transition {:?}", item);
	    }
	    alphabet_set.insert(String::from(symbol));

	    let destination =
		function
		.entry(String::from(state)).or_insert(HashMap::new())
		.entry(String::from(symbol)).or_insert(HashSet::new());

	    items.iter().for_each(
		|x| {
		    destination.insert(String::from(*x));
		    if !states.contains(x) {
			eprintln!("Warning: State `{x}` is not in the states array");
			eprintln!("Found in transition {:?}", item);
		    }
		}
	    );
	}

	NFA {
	    states: states_set,
	    alphabet: alphabet_set,
	    transition_function: function,
	    start_state: String::from(start_state),
	    accept_states: accept_states.iter().map(|x| String::from(*x)).collect(),
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
