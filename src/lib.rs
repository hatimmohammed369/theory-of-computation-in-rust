use std::collections::HashSet;
use std::collections::HashMap;
use std::cell::RefCell;
use std::rc::Rc;

#[derive(Debug, Clone)]
pub struct NFA {
    states: HashSet<String>,
    alphabet: HashSet<String>,
    transition_function: HashMap<String, HashMap<String, HashSet<String>>>,
    start_state: String,
    accept_states: HashSet<String>,
    is_deterministic: bool,
    dfa: RefCell<Rc<Option<Box<NFA>>>>
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
	    dfa: RefCell::new(Rc::new(None))
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
	    dfa: RefCell::new(Rc::new(None))
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

    pub fn compute(&self, input: &str, log: bool) -> ComputationResult {
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

    pub fn to_dfa(&self, sink_state: &str) -> Rc<Option<Box<NFA>>> {
	if self.is_deterministic || self.dfa.borrow().is_some() {
	    return self.get_dfa();
	}

	let mut dfa = NFA::new(true);

	let mut dfa_alphabet = self.alphabet.clone();
	dfa_alphabet.remove("");

	dfa.alphabet = dfa_alphabet.clone();

	let name_style = |x: &HashSet<String>| {
	    let mut s = String::new();
	    s.push('<');
	    s.push_str(&x.iter().map(|x| x.to_string()).collect::<Vec<String>>()[..].join(", "));
	    s.push('>');
	    s
	};

	let start_set = self.expand(&HashSet::from([self.start_state.to_string()]));
	let start_set = self.expand(&start_set);
	dfa.start_state = name_style(&start_set);

	let sink_state = sink_state.to_string();
	let sink_state_set = HashSet::from([sink_state.to_string()]);

	let mut sets: Vec<HashSet<String>> = vec![];
	sets.push(start_set);
	let mut begin = 0_usize;
	loop {
	    let before = sets.len();
	    for i in begin..before {
		let current_set = sets[i].clone();
		let current_set_name = if current_set.is_empty() {sink_state.to_string()} else {name_style(&current_set)};
		dfa.states.insert(current_set_name.to_string());
		for q in &current_set {
		    if self.accept_states.contains(q) {
			dfa.accept_states.insert(current_set_name.to_string());
			break;
		    }
		}
		for symbol in &dfa_alphabet {
		    let y = self.move_set(&current_set, symbol);
		    let mut add_set = true;
		    for s in &sets {
			if y.is_subset(s) && s.is_subset(&y) {
			    add_set = false;
			    break;
			}
		    }
		    if add_set {
			sets.push(y.clone());
		    }
		    if y.is_empty() {
			dfa.add_transition(&current_set_name, symbol, (&sink_state_set).iter());
			dfa.add_transition(&current_set_name, symbol, (&sink_state_set).iter());
		    } else {
			dfa.states.insert(name_style(&y));
			for q in &y {
			    if self.accept_states.contains(q) {
				dfa.accept_states.insert(name_style(&y));
				break;
			    }
			}
			dfa.add_transition(&current_set_name, symbol, HashSet::from([name_style(&y)]).iter());
		    }
		}
	    }

	    if before != sets.len() {
		begin = before;
	    } else {
		break;
	    }
	}

	*self.dfa.borrow_mut() = Rc::new(Some(Box::new(dfa)));
	self.get_dfa()
    }

    pub fn get_dfa(&self) -> Rc<Option<Box<NFA>>> {
	if self.dfa.borrow().is_none() {
	    *self.dfa.borrow_mut() = Rc::new(Some(Box::new(self.clone())));
	}
	let x = &self.dfa;
	let x = x.borrow();
	let x = Rc::clone(&x);
	x
    }

    pub fn star_regex(&self, expr: &Option<&str>) -> String {
	let mut expr = expr.unwrap_or_default().to_string();
	if expr.is_empty() {
	    return String::new();
	}

	if self.alphabet.contains(&expr) {
	    expr.push('*');
	} else if expr.starts_with("(") && expr.ends_with(")") {
	    let mut nesting = 0;
	    for (idx, ch) in expr.char_indices() {
		if ch == '(' {nesting += 1;}
		else if ch == ')' {nesting -= 1;}

		if nesting == 0 && idx != expr.len()-1 {
		    nesting = -1;
		    break;
		}
	    }

	    if nesting == -1 {
		expr = format!("({expr})*");
	    } else {
		expr.push('*');
	    }
	} else {
	    expr = format!("({expr})*");
	}
	expr
    }

    pub fn union_regexes(exprs: &[Option<&str>]) -> Option<String> {
	let mut union_expr = String::new();
	let mut all_none = true;
	let mut all_empty = true;
	for expr in exprs {
	    if expr.is_none() {
		continue;
	    }
	    all_none = false;

	    let mut expr = expr.as_ref().unwrap().to_string();
	    if expr.len() > 0 {
		all_empty = false;
	    }

	    if
		expr.len() >= 2 &&
		    &expr[expr.len()-2..] == "||" &&
		    expr.is_empty()
	    {
		continue;
	    }

	    let mut nesting = 0;
	    for ch in expr.chars() {
		if ch == '(' {nesting += 1;}
		else if ch == ')' {nesting -= 1;}

		if nesting == 0 && ch == '|' {
		    expr = format!("({expr})");
		    break;
		}
	    }

	    expr = format!("{expr}|");
	    union_expr.push_str(&expr);
	}

	if all_none {
	    return None;
	} else if all_empty {
	    return Some(String::new());
	}else {
	    if !union_expr.is_empty() {
		union_expr.pop();
	    }
	}

	Some(union_expr)
    }

    pub fn concat_regexes(exprs: &[Option<&str>]) -> Option<String> {
	let mut all_empty = true;
	let mut concat_expr = String::new();
	for expr in exprs {
	    if expr.is_none() {
		return None;
	    }

	    let mut expr = expr.as_ref().unwrap().to_string();
	    if expr.is_empty() {
		continue;
	    }
	    all_empty = false;

	    let mut nesting = 0;
	    for ch in expr.chars() {
		if ch == '(' {nesting += 1;}
		else if ch == ')' {nesting -= 1;}

		if nesting == 0 && ch == '|' {
		    expr = format!("({expr})");
		    break;
		}
	    }

	    concat_expr.push_str(&expr);
	}

	if all_empty {
	    return Some(String::new());
	}

	Some(concat_expr)
    }

    pub fn to_regular_expression(&self, removal_sequence: &[&str], g_start_state: &str, g_accept_state: &str) -> String {
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

		let combined =
		    NFA::union_regexes(
			&transitions[..]
		    );
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
		self.star_regex(
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
			NFA::concat_regexes(
			    &[
				sender_to_leaving.borrow().as_deref(),
				leaving_self_loop.as_deref(),
				leaving_to_receiver.borrow().as_deref()
			    ]
			);

		    let new_regex =
			NFA::union_regexes(
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
}
