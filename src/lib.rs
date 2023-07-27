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
    fn new(is_deterministic: bool) -> NFA {
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

    pub fn init_new(states: &[&str], alphabet: &[&str], transition_function: &[(&str, &str, &[&str])], start_state: &str, accept_states: &[&str]) -> NFA {
	// Ignore additional elements in parameter `alphabet`
	// Ignore additional elements in paramater `states`

	let mut states_set = HashSet::<String>::new();
	let mut alphabet_set = HashSet::<String>::new();

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

	let mut is_deterministic = true;
	let mut processed_states = HashSet::<&str>::new();
	for item in transition_function {
	    let (state, symbol, items) = (item.0, item.1, item.2);
	    processed_states.insert(state);
	    alphabet_set.insert(String::from(symbol));
	    states_set.insert(String::from(state));

	    if is_deterministic {
		// If there is an empty string transition
		// then automaton is Nondeterministic.
		is_deterministic = !symbol.is_empty();
	    }

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
		    states_set.insert(String::from(*x));
		    if !states.contains(x) {
			eprintln!("Warning: State `{x}` is not in the states array");
			eprintln!("Found in transition {:?}", item);
		    }
		}
	    );
	}

	if is_deterministic {
	    // If some states do not have outgoing transitions
	    // then this automaton is an NFA
	    if processed_states.len() == states_set.len() {
		// All states have outgoing transitions.

		for (_, state_map) in &function {
		    if state_map.len() < alphabet_set.len() {
			// Some state does not have transitions for all alphabet symbols.
			is_deterministic = false;
			break;
		    }
		}
	    }

	}

	for state in states {
	    if !states_set.contains(*state) {
		eprintln!("Un-used state `{state}`");
	    }
	}

	for symbol in alphabet {
	    if !alphabet_set.contains(*symbol) {
		eprintln!("Un-used symbol `{symbol}`");
	    }
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

    fn expand(&self, set:&HashSet<String>) -> HashSet<String> {
	let mut out = set.clone();

	if self.is_deterministic || set.is_empty() {
	    return out;
	}

	for elem in set.iter() {
	    self.read_symbol_states_set(&elem, "")
		.unwrap_or(&HashSet::new())
		.iter()
		.for_each(
		    |x| {
			out.insert(x.to_string());
		    }
		);
	}

	loop {
	    let before = out.len();
	    for elem in set.clone() {
		self.read_symbol_states_set(&elem, "")
		    .unwrap_or(&HashSet::new())
		    .iter()
		    .for_each(
			|x| {
			    out.insert(x.to_string());
			}
		    );
	    }
	    if before == out.len() {
		break;
	    }
	}

	out
    }

    fn move_set(&self, set: &HashSet<String>, symbol: &str) -> HashSet<String> {
	let mut out = HashSet::new();
	for elem in set {
	    let mut x = &HashSet::new();
	    if let Some(value) = self.read_symbol_states_set(elem, symbol) {
		x = value;
	    }
	    let y = self.expand(x);
	    y.iter().for_each(
		|element| {
		    out.insert(element.to_string());
		}
	    )
	}
	out
    }

    pub fn compute(&mut self, input: &str, log: bool) -> ComputationResult {
	let mut automaton_states = HashSet::new();
	automaton_states.insert(self.start_state.to_string());

	automaton_states = self.expand(&automaton_states);

	if log {
	    println!("Computing on input `{input}` . . .");
	}

	for c in input.chars() {
	    if log {
		println!("{:?} reading `{c}`", &automaton_states);
	    }
	    if !self.alphabet.contains(&c.to_string()) {
		if log {
		    eprintln!("Warning: Symbol {c} is not in alphabet {:?}", self.alphabet);
		}
		break;
	    }
	    automaton_states = self.move_set(&mut automaton_states, &c.to_string());
	    if log {
		println!("=> {:?}", automaton_states);
		if automaton_states.is_empty() {
		    eprintln!("Early aborting computation because automaton lost all state");
		    break;
		}
	    }

	}

	let mut result = ComputationResult::Reject;
	for state in &self.accept_states {
	    if automaton_states.contains(state) {
		result = ComputationResult::Accept;
		break;
	    }
	}

	if log {
	    eprintln!("{:?}ed input `{input}`", result);
	}

	result
    }

}
