// Finite Automata module

use crate::automata::ComputationResult;
use std::collections::HashSet;
use std::collections::HashMap;
use std::cell::RefCell;
use std::rc::Rc;

// (Non-deterministic) Finite Automaton
#[derive(Debug, Clone)]
pub struct NFA {
    // automaton states
    states: HashSet<String>,

    // automaton alphabet
    alphabet: HashSet<String>,
    /*
    A Note on the alphabet:
    Even though any string is allowed to be an alphabet symbol
    when computing on some input, the automaton treats each single character of the input
    as a seperate symbol, thus symbols longer than one character will be ignored and hence
    be perceived as sequence of single-character symbols
    I know this is ill, but I will be fixing that soon, now we are here to comment code.
    */

    // the transition function, the heart of the automaton
    /*
    Keys in field (transition_function) are states and values are (state maps)
    For each state (q), its state map (the HashMap<String, HashSet<String>> part) has alphabet symbols as its keys
    and the values are those states you can reach from state (q) by reading a key from the state map of (q)
    For instance:
    let transition_function be {q: {0: {q, r, s}}, {r: {"": {t, v}}} ...}
    this means when in state (q) reading the alphabet symbol 0 we reach states {q, r, s}
    also when in state (r) we can do an empty string transition taking us to states {t, v}
    and so forth . . .
    */
    transition_function: HashMap<String, HashMap<String, HashSet<String>>>,

    // The distinguished starting state.
    start_state: String,

    // The accepting (final) states
    accept_states: HashSet<String>,

    /*
    This flag determines if the automaton is deterministic or not
    You can either pass its value or have the constructor infer its value
    */
    is_deterministic: bool,

    /*
    Cache field containg an equivalent DFA to this automaton
    When calling get_dfa (which returns the equivalent DFA) the code looks up the field
    Calling get_dfa on NFA which is already an DFA (is_deterministic = true) clones (self)
    thus it's not a good idea to call this function when is_deterministic flag is set
    */
    dfa: RefCell< Rc::< Option:: < Box::<NFA> >  > >
}

impl NFA {
    pub fn new(
	/*
	Ignore additional elements in parameter `states`
	If a state is in parameter (alphabet)
	but it is never used in parameter (transition_function) ignore it and emit a warning
	*/
	states: &[&str],

	/*
	Ignore additional elements in parameter `alphabet`
	If an alphabet symbol is in parameter (alphabet)
	but it is never used in parameter (transition_function) ignore it and emit a warning
	*/
	alphabet: &[&str],

	/*
	Each element is of the form
	(state-q, alphabet-symbol-x, &[states reachable from (q) when reading (x)])
	*/
	transition_function: &[(&str, &str, &[&str])],

	start_state: &str,
	accept_states: &[&str],
	infer_type: bool, // Infer the type of the new automaton based on parameter values.
	is_deterministic: bool // when paramter infer_type is false, just use this value in (is_deterministic) parameter.
    ) -> NFA {

	// Accumulate (used) states which are those appearing in parameter (transition_function)
	let mut states_set = HashSet::<String>::new();

	// Accumulate (used) alphabet symbols which are those appearing in parameter (transition_function)
	let mut alphabet_set = HashSet::<String>::new();

	if !states.contains(&start_state) {
	    eprintln!("Warning: Start State `{start_state}` is not in the states array");
	}
	states_set.insert(String::from(start_state)); // Do not forget adjoining the start state.

	// Issue warnings for each accepting state not in parameter `states`.
	accept_states.iter().for_each(
	    |x| {
		if !states.contains(x) {
		    eprintln!("Warning: Accept State `{x}` is not in the states array");
		}
		states_set.insert(String::from(*x));
	    }
	);

	// The final transition function of the new object.
	let mut function = HashMap::<String, HashMap<String, HashSet<String>>>::new();

	let mut computed_is_deterministic_flag = true;

	// This hash set is used in computing is_deterministic flag if infer_type flag is set.
	// if processed states, which are the first entry in each element in parameter (transition_function),
	// are less than the total number of states in variable (states_set)
	// this means that some states do not have outgoing transitions
	// and hence the new object is a strictly non-deterministic.
	let mut processed_states = HashSet::<&str>::new();

	for item in transition_function {
	    let (state, symbol, items) = (item.0, item.1, item.2);
	    processed_states.insert(state);
	    alphabet_set.insert(String::from(symbol)); // mark this symbol as used.
	    states_set.insert(String::from(state)); // mark this state as used.

	    if infer_type {
		// Try inferring the new object type if the function was intructed to do so.
		if computed_is_deterministic_flag {
		    // If there is an empty string transition
		    // then automaton is Nondeterministic.
		    computed_is_deterministic_flag = !symbol.is_empty();
		}
	    }

	    if !states.contains(&state) {
		eprintln!("Warning: State `{state}` is not in the states array");
		eprintln!("Found in transition {:?}", item);
	    }

	    if !alphabet.contains(&symbol) {
		eprintln!("Warning: Symbol `{symbol}` is not in the alphabet array");
		eprintln!("Found in transition {:?}", item);
	    }

	    // Automatically create new entries in transition function map when missing
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

	if infer_type {
	    if computed_is_deterministic_flag {
		// If some states do not have outgoing transitions
		// then this automaton is an NFA
		if processed_states.len() == states_set.len() {
		    // All states have outgoing transitions.

		    for (_, state_map) in &function {
			if state_map.len() < alphabet_set.len() {
			    // Some state does not have transitions for all alphabet symbols.
			    computed_is_deterministic_flag = false;
			    break;
			}
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

	let is_deterministic_flag =
	    if infer_type {computed_is_deterministic_flag} else {is_deterministic};
	NFA {
	    states: states_set,
	    alphabet: alphabet_set,
	    transition_function: function,
	    start_state: String::from(start_state),
	    accept_states: accept_states.iter().map(|x| String::from(*x)).collect(),
	    is_deterministic: is_deterministic_flag,
	    dfa: RefCell::new( Rc::new(None) )
	}
    }

    fn new_empty_nfa(is_deterministic: bool) -> NFA {
	// It will be useful somewhere.
	NFA {
	    states: HashSet::new(),
	    alphabet: HashSet::new(),
	    transition_function: HashMap::new(),
	    start_state: String::new(),
	    accept_states: HashSet::new(),
	    is_deterministic,
	    dfa: RefCell::new( Rc::new(None) )
	}
    }

    /*
    Access a state map for writing
    */
    pub fn write_state_symbols_map(&mut self, state: &str) -> &mut HashMap<String, HashSet<String>> {
	if self.states.insert(state.to_string()) {
	    eprintln!("Warning: new state `{state}`");
	}

	self.transition_function
	    .entry(state.to_string())
	    .or_insert(HashMap::new())
    }

    /*
    Access a particular value in a state map for writing
    This particular value is those states reachable from parameter (state) when reading parameter (symbol).
    */
    pub fn write_symbol_states_set(&mut self, state: &str, symbol: &str) -> &mut HashSet<String> {
	if self.states.insert(state.to_string()) {
	    eprintln!("Warning: new state `{state}`");
	}

	self.write_state_symbols_map(state)
	    .entry(symbol.to_string())
	    .or_insert(HashSet::new())
    }

    /*
    Access a state map for reading
    */
    pub fn read_state_symbols_map(&self, state: &str) -> Option<&HashMap<String, HashSet<String>>> {
	self.transition_function.get(state)
    }


    /*
    Access a particular value in a state map for reading
    This particular value is those states reachable from parameter (state) when reading parameter (symbol).
    */
    pub fn read_symbol_states_set(&self, state: &str, symbol: &str) -> Option<&HashSet<String>> {
	match self.read_state_symbols_map(state) {
	    Some(state_symbols_map) => state_symbols_map.get(symbol),
	    None => None
	}
    }

    /*
    Add a transition for a specific state
    */
    pub fn add_transition<'a>(&'a mut self, state: &str, symbol: &str, output: impl Iterator<Item = &'a String>) {
	if self.states.insert(state.to_string()) {
	    eprintln!("Warning: new state `{state}`");
	}

	let destination = self.write_symbol_states_set(state, symbol);
	output.for_each(
	    |x| {
		destination.insert(x.to_string());
	    }
	);
    }

    pub fn read_states(&self) -> &HashSet<String> {
	&self.states
    }

    pub fn read_alphabet(&self) -> &HashSet<String> {
	&self.alphabet
    }

    pub fn read_transition_function(&self) -> &HashMap<String, HashMap<String, HashSet<String>>> {
	&self.transition_function
    }

    pub fn read_start_state(&self) -> &str {
	&self.start_state
    }

    pub fn read_accept_states(&self) -> &HashSet<String> {
	&self.accept_states
    }

    pub fn is_deterministic(&self) -> bool {
	self.is_deterministic
    }

    /*
    Return all states reachable from parameter (set) using
    any number of empty string transitions

    any number: including 0, and thus value in parameter (set) is always part of return value
    thus return value of (expand) is never the empty set
    */
    fn expand(&self, set: &HashSet<String>) -> HashSet<String> {
	let mut out = set.clone();

	if self.is_deterministic || set.is_empty() {
	    // If this automaton object is deterministic (it has no empty string transitions) or input is nothing
	    // No work to be done
	    return out;
	}

	let mut new_items = out.clone();
	while !new_items.is_empty() {
	    let new_items_iter = new_items.clone();
	    let new_items_iter = new_items_iter.iter();
	    new_items.clear();

	    for elem in new_items_iter {
		if let Some(empty_string_set) = self.read_symbol_states_set(elem, "") {
		    empty_string_set
			.iter()
			.for_each(|s| {
			    if out.insert(s.to_string()) {
				new_items.insert(s.to_string());
			    }
			});
		}
	    }
	}

	out
    }

    /*
    Return all states reachable from each state (q) in parameter (set)
    when reading symbol in paramter (symbol)
    */
    fn move_set(&self, set: &HashSet<String>, symbol: &str) -> HashSet<String> {
	let mut out = HashSet::new();

	for q in set {
	    /*
	    Set (x) represents all states reachable from (q) when reading (sybmol)
	    */
	    let mut x = &HashSet::new();
	    if let Some(value) = self.read_symbol_states_set(q, symbol) {
		x = value;
	    }

	    // Exapnd (x) in case there are empty string transitions.
	    let y = self.expand(x);
	    y.iter().for_each(
		|element| {
		    out.insert(element.to_string());
		}
	    )
	}

	out
    }

    /*
    Compute on some input.
    */
    pub fn compute(&self, input: &str, log: bool) -> ComputationResult {
	let mut automaton_states = HashSet::new();

	// We start with ... the start state
	automaton_states.insert(self.start_state.to_string());

	// expand the start state
	automaton_states = self.expand(&automaton_states);

	if log {
	    println!("Computing on input `{input}` . . .");
	}

	for c in input.chars() {
	    /*
	    Here, unfortunately, we treat each character in the input as its own symbol
	    thus a transition with a symbol longer than one character will never be used
	    because we search in the (state maps) of those states in (automaton_states)
	    performing a simple == comparison which will clearly fail when lengthes differ.
	    I will be fixing this sooooooon!
	    */
	    if log {
		println!("{:?} reading `{c}`", &automaton_states);
	    }
	    if !self.alphabet.contains(&c.to_string()) {
		if log {
		    eprintln!("Warning: Symbol {c} is not in alphabet {:?}", self.alphabet);
		}
		break;
	    }

	    // Move according to the transition function.
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
		// There is a least one accepting state
		// This computation is successful
		// More fomally, the input is (accepted) by the automaton.
		result = ComputationResult::Accept;
		break;
	    }
	}

	if log {
	    eprintln!("{:?}ed input `{input}`", result);
	}

	result
    }

    /*
    Convert the invoking NFA to an equivalent DFA
    and cache that equivalent DFA for later use.

    The conversion as done as follow:
    Start with the start state in a singleton set, and then expand that singleton set
    Have a container prepared to store sets, insert the expanded start state set from above in that container
    then apply this:
    For each set (A) in this container move (A) with each symbol (x), of course excluding the empty string,
    remember that sets returned from method move_set are already expanded
    Add the resulting sets to your container
    If one resulting set was empty, instead of inserting it to your container
    insert a special unique singleton set containing the state in parameter (sink_state)
    If you got new sets in the container apply (move_set) to those new sets
    Otherwise stop

    Whenever set (A) on reading symbol (x) yields set (B)
    then in the resulting DFA, we say the state (set A) reading symbol (x) goes to state (set B)

    And thus states sets in the NFA represents single states in the equivalent DFA
    */
    pub fn to_dfa(&self, sink_state: &str) -> Rc<Option<Box<NFA>>> {
	if self.is_deterministic || self.dfa.borrow().is_some() {
	    // This automaton is deterministic or the cache is ready
	    // just return the underlying cache.
	    return self.get_dfa();
	}

	let mut dfa = NFA::new_empty_nfa(true);

	let mut dfa_alphabet = self.alphabet.clone();
	dfa_alphabet.remove("");

	dfa.alphabet = dfa_alphabet.clone();

	let mut strings = Vec::<(HashSet::<String>, String)>::new();
	let mut stringify_set = |x: &HashSet<String>| {
	    for (set, string) in &strings {
		if set.is_subset(x) && x.is_subset(set) {
		    return String::from(string);
		}
	    }
	    let mut s = String::new();
	    s.push('<');
	    s.push_str(&x.iter().map(|x| x.to_string()).collect::<Vec<String>>()[..].join(", "));
	    s.push('>');
	    strings.push((x.clone(), s.to_string()));
	    s
	};

	let start_set = self.expand(&HashSet::from([self.start_state.to_string()]));
	dfa.start_state = stringify_set(&start_set);

	let sink_state = String::from(sink_state);

	// The unique sink state set.
	let sink_state_set =
	    HashSet::from([sink_state.to_string()]);
	let sink_state_set_name =
	    stringify_set(&sink_state_set);

	{
	    dfa.states.insert( String::from(&sink_state_set_name) );
	    let destination =
		dfa.write_state_symbols_map(&sink_state_set_name);
	    for symbol in &dfa_alphabet {
		destination.insert(String::from(symbol), HashSet::from( [ String::from(&sink_state_set_name) ] ));
	    }
	}

	let mut new_items = vec![ start_set.clone() ];
	while !new_items.is_empty() {
	    let new_items_iter = new_items.clone();
	    let new_items_iter = new_items_iter.iter();
	    new_items.clear();

	    for new_set in new_items_iter {
		let name = stringify_set(new_set);
		dfa.states.insert(name.to_string());
		if new_set.intersection(&self.accept_states).next().is_some() {
		    // There's at least one accept state.
		    dfa.accept_states.insert( String::from(&name) );
		}

		let state_symbols_map =
		    dfa.write_state_symbols_map(&name);

		let has_empty_symbols_map =
		    state_symbols_map.is_empty();

		for symbol in &dfa_alphabet {
		    let moved_new_set = self.move_set(new_set, symbol);
		    let moved_new_set_name =
			stringify_set(&moved_new_set);

		    if has_empty_symbols_map {
			new_items.push(moved_new_set.clone());
		    }

		    state_symbols_map
			.entry( String::from(symbol) )
			.or_insert(HashSet::new())
			.insert({
			    if moved_new_set.is_empty() {
				String::from(&sink_state_set_name)
			    } else {
				String::from(&moved_new_set_name)
			    }
			});
		}
	    }
	}

	*self.dfa.borrow_mut() = Rc::new( Some(Box::new(dfa)) );
	self.get_dfa()
    }

    pub fn get_dfa(&self) -> Rc<Option<Box<NFA>>> {
	if self.is_deterministic && self.dfa.borrow().is_none() {
	    *self.dfa.borrow_mut() = Rc::new( Some( Box::new(self.clone()) ) );
	}

	let x = &self.dfa;

	let x = x.borrow();

	// Rc< Option< Box<NFA> > >
	let x = Rc::clone(&x);

	x
    }

    /*
    Convert the invoking automaton to an equivalent regular expression
    states are processed according to parameter (removal_sequence)
    parameters (g_start_state) and (g_accept_state) are those use during
    the construction of the hypothetical GNFA (Generalized NFA)
    */
    pub fn to_regular_expression(&self, removal_sequence: &[&str], g_start_state: &str, g_accept_state: &str) -> String {
	{
	    let fake_states =
		removal_sequence
		.iter()
		.filter(|s| {
		    let s = **s;
		    !self.states.contains(s)
		})
		.collect::<Vec<_>>();

	    if !fake_states.is_empty() {
		panic!("States `{:?}` do not exist!", fake_states);
	    }
	}
	if !self.is_deterministic {
	    panic!("Invoking automaton must be deterministic");
	}
	if self.states.contains(g_start_state) {
	    panic!("Choose another start state");
	}
	if self.states.contains(g_accept_state) {
	    panic!("Choose another accept state");
	}
	{
	    let missing =
		self.states
		.iter()
		.filter(|elem| {
		    !removal_sequence.contains(&elem.as_str())
		})
		.collect::<Vec<&String>>();
	    if !missing.is_empty() {
		panic!(
		    "States `{:?}` are missing from removal sequence",
		    missing
		);
	    }
	}

	use crate::generators::regular_expressions::star_string_regex as star;
	use crate::generators::regular_expressions::union_string_regexes as union;
	use crate::generators::regular_expressions::concat_string_regexes as concat;

	let mut function =
	    HashMap::<(&str, &str), RefCell<Option<String>>>::new();

	function.insert((g_start_state, g_accept_state), RefCell::new( Option::<String>::None ));

	for state in &(self.states) {
	    function.insert((g_start_state, state), RefCell::new( Option::<String>::None ));
	    function.insert((state, g_accept_state), RefCell::new( Option::<String>::None ));
	}
	function.insert((g_start_state, &self.start_state), RefCell::new( Some( String::new() ) ));

	for accept_state in &(self.accept_states) {
	    function.insert((accept_state.as_str(), g_accept_state), RefCell::new( Some( String::new() ) ));
	}

	for state in &(self.states) {
	    for another in &(self.states) {
		let mut transitions = Vec::<Option::<String>>::new();
		if let Some(state_map) = self.transition_function.get(state) {
		    transitions =
			state_map
			.iter()
			.filter(|(_, symbol_set)| {
			    symbol_set.contains(another)
			})
			.map(|(symbol, _)| {
			    Some(symbol.to_string())
			})
			.collect::<_>();
		} else {
		    transitions.push( Option::<String>::None );
		}

		let transitions =
		    transitions
		    .iter()
		    .map(
			|value| {
			    match value {
				Some(x) => Some(x.as_str()),
				None => None
			    }
			}
		    )
		    .collect::<Vec::<Option::<&str>>>();

		let combined = union(&transitions[..]);
		function.insert((state, another), RefCell::new(combined));
	    }
	}

	let phi = RefCell::new(Option::<String>::None);

	let mut senders = self.states.clone();
	senders.insert(g_start_state.to_string());

	let mut receivers = self.states.clone();
	receivers.insert(g_accept_state.to_string());

	for leaving in removal_sequence {
	    let leaving = *leaving;

	    senders.remove(leaving);
	    receivers.remove(leaving);

	    let leaving_self_loop = // &RefCell<Option<String>>
		function
		.get(&(leaving, leaving))
		.unwrap_or(&phi);
	    let leaving_self_loop =
		star(
		    &self,
		    &leaving_self_loop.borrow().as_deref()
		);
	    let leaving_self_loop =
		Some(leaving_self_loop);

	    for sender in &senders {
		let sender_to_leaving =
		    function
		    .get(&(sender.as_str(), leaving))
		    .unwrap_or(&phi);
		if sender_to_leaving.borrow().is_none() {
		    continue;
		}

		for receiver in &receivers {
		    let leaving_to_receiver =
			function
			.get(&(leaving, receiver.as_str()))
			.unwrap_or(&phi);
		    if leaving_to_receiver.borrow().is_none() {
			continue;
		    }

		    let sender_to_receiver =
			function
			.get(&(sender.as_str(), receiver.as_str()))
			.unwrap_or(&phi);

		    let through_leaving =
			concat(
			    &[
				sender_to_leaving.borrow().as_deref(),
				leaving_self_loop.as_deref(),
				leaving_to_receiver.borrow().as_deref()
			    ]
			);

		    let new_regex =
			union(
			    &[
				sender_to_receiver.borrow().as_deref(),
				through_leaving.as_deref()
			    ]
			);

		    let regex =
			function
			.get(&(sender.as_str(), receiver.as_str()))
			.unwrap_or(&phi);
		    *(regex.borrow_mut()) = new_regex;
		}
	    }
	}

	// Option<&RefCell<Option<String>>>
	let final_expression =
	    function
	    .get(&(g_start_state, g_accept_state));

	// &RefCell<Option<String>>
	let final_expression =
	    final_expression.unwrap();

	// Ref<_, Option<String>>
	let final_expression =
	    final_expression.borrow();

	// Option<&String>
	let final_expression =
	    final_expression.as_ref();

	// &String
	let final_expression =
	    final_expression.unwrap();

	let final_expression =
	    final_expression.to_string();

	final_expression
    }

    pub fn star(&self, star_state: &str) -> NFA {
	if self.states.contains(star_state) {
	    panic!("The star state `{star_state}` is already present");
	}

	let mut self_star = self.clone();

	// Leaving is_deterministic true will hold method (expand) from working
	// which is essential when we have empty string transitions
	self_star.is_deterministic = false;

	self_star.alphabet.insert( String::new() ); // in case missing
	self_star.states.insert( String::from(star_state) );
	self_star.start_state = String::from(star_state); // set the new start state
	self_star.accept_states.insert( String::from(star_state) ); // mark the new state as accepting

	// Create an empty string transition from the new (start) state
	// to the old start state
	self_star.add_transition(star_state, "", vec![self.start_state.to_string()].iter());

	self.accept_states.iter().for_each(
	    |state| {
		// For each original accepting state (q),
		// create an empty string transition from q to the old start state.
		self_star.add_transition(state, "", vec![self.start_state.to_string()].iter());
	    }
	);

	self_star
    }

    pub fn union<'a>(automata: impl Iterator<Item = &'a NFA>, union_nfa_start_state: &str) -> NFA {
	let mut union_nfa = NFA::new_empty_nfa(false);
	union_nfa.start_state = union_nfa_start_state.to_string();
	union_nfa.states.insert(
	    union_nfa_start_state.to_string()
	);

	let mut counter = 0usize;
	for automaton in automata {
	    let style = |s: &str, k: usize| {
		format!("(A{k}.{s})")
	    };

	    // Add an empty string transition from the new start state
	    // to the start state of (automaton).
	    union_nfa
		.add_transition(
		    union_nfa_start_state, "",
		    vec![ style(automaton.start_state.as_str(), counter) ].iter()
		);

	    automaton.alphabet.iter().for_each(|x| {
		union_nfa.alphabet.insert(x.to_string());
	    });

	    automaton.states.iter().for_each(|s| {
		let name = style(s, counter);

		union_nfa.states.insert(name.to_string());

		if let Some(state_map) = automaton.read_state_symbols_map(s) {
		    let mut adjusted_state_map =
			HashMap::<String, HashSet::<String>>::new();

		    for (symbol, symbol_set) in state_map {
			let symbol_set =
			    symbol_set
			    .iter()
			    .map(|elem| {
				style(elem.as_str(), counter)
			    })
			    .collect::<HashSet::<String>>();
			adjusted_state_map.insert(symbol.to_string(), symbol_set);
		    }

		    union_nfa.transition_function.insert(name, adjusted_state_map);
		}
	    });

	    automaton.accept_states.iter().for_each(|s| {
		let name = style(s, counter);
		union_nfa.accept_states.insert(name);
	    });

	    counter += 1;
	}

	union_nfa
    }
}
