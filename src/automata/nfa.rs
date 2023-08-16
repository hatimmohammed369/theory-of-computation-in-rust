#![allow(dead_code)]
#![allow(unused_variables)]
#![allow(unused_mut)]
// Finite Automata module

use crate::automata::ComputationResult;
use std::cell::RefCell;
use std::collections::HashMap;
use std::collections::HashSet;
use std::collections::LinkedList;
use std::rc::Rc;
use std::time::{SystemTime, UNIX_EPOCH};

// (Non-deterministic) Finite Automaton
#[derive(Debug, Clone)]
pub struct NFA {
    // automaton states
    states: HashSet<String>,

    // automaton alphabet
    alphabet: HashSet<char>,
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
    transition_function: HashMap<String, HashMap<char, HashSet<String>>>,

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
    dfa: RefCell<Option<Rc<NFA>>>,
}

impl NFA {
    // Generate time for to be used in states
    fn now() -> String {
        let now = SystemTime::now().duration_since(UNIX_EPOCH).ok().unwrap();
        let now = now.as_secs();
        format!("{now:#x}")
    }

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
        alphabet: &[char],

        /*
        Each element is of the form
        (state-q, alphabet-symbol-x, &[states reachable from (q) when reading (x)])
         */
        transition_function: &[(&str, char, &[&str])],

        start_state: &str,
        accept_states: &[&str],
        infer_type: bool, // Infer the type of the new automaton based on parameter values.
        is_deterministic: bool, // when paramter infer_type is false, just use this value in (is_deterministic) parameter.
    ) -> NFA {
        // Accumulate (used) states which are those appearing in parameter (transition_function)
        let mut states_set = HashSet::<String>::new();

        // Accumulate (used) alphabet symbols which are those appearing in parameter (transition_function)
        let mut alphabet_set = HashSet::<char>::new();

        if !states.contains(&start_state) {
            eprintln!("Warning: Start State `{start_state}` is not in the states array");
        }
        states_set.insert(start_state.to_string()); // Do not forget adjoining the start state.

        // Issue warnings for each accepting state not in parameter `states`.
        accept_states.iter().for_each(|x| {
            if !states.contains(x) {
                eprintln!("Warning: Accept State `{x}` is not in the states array");
            }
            states_set.insert(x.to_string());
        });

        // The final transition function of the new object.
        let mut function = HashMap::<String, HashMap<char, HashSet<String>>>::new();

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
            alphabet_set.insert(symbol);

            states_set.insert(state.to_string()); // mark this state as used.

            if infer_type {
                // Try inferring the new object type if the function was intructed to do so.
                if computed_is_deterministic_flag {
                    // If there is an empty string transition
                    // then automaton is Nondeterministic.
                    computed_is_deterministic_flag = symbol != '\0';
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
            let destination = function
                .entry(state.to_string())
                .or_insert(HashMap::new())
                .entry(symbol)
                .or_insert(HashSet::new());
            items.iter().for_each(|x| {
                destination.insert(x.to_string());
                states_set.insert(x.to_string());
                if !states.contains(x) {
                    eprintln!("Warning: State `{x}` is not in the states array");
                    eprintln!("Found in transition {:?}", item);
                }
            });
        }

        if infer_type && computed_is_deterministic_flag {
            // If some states do not have outgoing transitions
            // then this automaton is an NFA
            if processed_states.len() == states_set.len() {
                // All states have outgoing transitions.

                for state_map in function.values() {
                    if state_map.len() < alphabet_set.len() {
                        // Some state does not have transitions for all alphabet symbols.
                        computed_is_deterministic_flag = false;
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
            if !alphabet_set.contains(symbol) {
                eprintln!("Un-used symbol `{symbol}`");
            }
        }

        let is_deterministic_flag = if infer_type {
            computed_is_deterministic_flag
        } else {
            is_deterministic
        };

        NFA {
            states: states_set,
            alphabet: alphabet_set,
            transition_function: function,
            start_state: start_state.to_string(),
            accept_states: accept_states.iter().map(|x| x.to_string()).collect(),
            is_deterministic: is_deterministic_flag,
            dfa: RefCell::new(None),
        }
    }

    /*
    Directly fill private fields using pre-computed values.
     */
    pub fn raw_new(
        states: HashSet<String>,
        alphabet: HashSet<char>,
        transition_function: HashMap<String, HashMap<char, HashSet<String>>>,
        start_state: String,
        accept_states: HashSet<String>,
        is_deterministic: bool,
        dfa: RefCell<Option<Rc<NFA>>>,
    ) -> NFA {
        NFA {
            states,
            alphabet,
            transition_function,
            start_state,
            accept_states,
            is_deterministic,
            dfa,
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
            dfa: RefCell::new(None),
        }
    }

    /*
    Access a state map for writing
     */
    pub fn write_state_symbols_map(&mut self, state: &str) -> &mut HashMap<char, HashSet<String>> {
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
    pub fn write_symbol_states_set(&mut self, state: &str, symbol: char) -> &mut HashSet<String> {
        if self.states.insert(state.to_string()) {
            eprintln!("Warning: new state `{state}`");
        }

        self.write_state_symbols_map(state)
            .entry(symbol)
            .or_insert(HashSet::new())
    }

    /*
    Access a state map for reading
     */
    pub fn read_state_symbols_map(&self, state: &str) -> Option<&HashMap<char, HashSet<String>>> {
        self.transition_function.get(state)
    }

    /*
    Access a particular value in a state map for reading
    This particular value is those states reachable from parameter (state) when reading parameter (symbol).
     */
    pub fn read_symbol_states_set(&self, state: &str, symbol: char) -> Option<&HashSet<String>> {
        match self.read_state_symbols_map(state) {
            Some(state_symbols_map) => state_symbols_map.get(&symbol),
            None => None,
        }
    }

    /*
    Add a transition for a specific state
     */
    pub fn add_transition<'a>(
        &'a mut self,
        state: &str,
        symbol: char,
        output: impl Iterator<Item = &'a String>,
    ) {
        if self.states.insert(state.to_string()) {
            eprintln!("Warning: new state `{state}`");
        }

        let destination = self.write_symbol_states_set(state, symbol);
        output.for_each(|x| {
            destination.insert(x.to_string());
        });
    }

    pub fn read_states(&self) -> &HashSet<String> {
        &self.states
    }

    pub fn read_alphabet(&self) -> &HashSet<char> {
        &self.alphabet
    }

    pub fn read_transition_function(&self) -> &HashMap<String, HashMap<char, HashSet<String>>> {
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

        let mut new_items = out.iter().map(String::from).collect::<LinkedList<_>>();
        while !new_items.is_empty() {
            let end = new_items.len();
            for _ in 0..end {
                let elem = new_items.pop_front().unwrap();
                if let Some(empty_string_set) = self.read_symbol_states_set(&elem, '\0') {
                    empty_string_set.iter().for_each(|s| {
                        if out.insert(s.to_string()) {
                            new_items.push_back(s.to_string());
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
    fn move_set(&self, set: &HashSet<String>, symbol: char) -> HashSet<String> {
        let mut out = HashSet::new();

        set.iter().for_each(|q| {
            out.extend(
                self.read_symbol_states_set(q, symbol)
                    .unwrap_or(&HashSet::new())
                    .iter()
                    .map(String::from),
            )
        });
        out = self.expand(&out);

        out
    }

    /*
    Compute on some input.
     */
    pub fn compute(&self, input: &str, log: bool) -> Result<ComputationResult, String> {
        let mut automaton_states = HashSet::new();

        // We start with ... the start state
        automaton_states.insert(self.start_state.to_string());

        // expand the start state
        automaton_states = self.expand(&automaton_states);

        if log {
            eprintln!();
            eprintln!("########################################");
            eprintln!("Computing on input `{input}` . . .\n");
        }

        for next_symbol in input.chars() {
            /*
            Here, unfortunately, we treat each character in the input as its own symbol
            thus a transition with a symbol longer than one character will never be used
            because we search in the (state maps) of those states in (automaton_states)
            performing a simple == comparison which will clearly fail when lengthes differ.
            I will be fixing this sooooooon!
             */

            if log {
                eprintln!("{:?} reading `{next_symbol}`", &automaton_states);
            }
            if !self.alphabet.contains(&next_symbol) {
                return Err(format!(
                    "Warning: Symbol `{next_symbol}` is not in alphabet {:?}",
                    self.alphabet
                ));
            }

            // Move according to the transition function.
            automaton_states = self.move_set(&automaton_states, next_symbol);

            if log {
                eprintln!("=> {:?}", automaton_states);
            }
            if automaton_states.is_empty() {
                return Err(
                    "Early aborting computation because automaton lost all state".to_string(),
                );
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
            eprintln!("{:?}ed input `{input}`\n", result);

            if result == ComputationResult::Accept {
                eprintln!(
                    "Reached accepting states:\n{:?}",
                    automaton_states
                        .intersection(&self.accept_states)
                        .collect::<HashSet<_>>()
                );
            }
            eprintln!("########################################");
            eprintln!();
        }

        Ok(result)
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
    pub fn compute_equivalent_dfa(nfa: &NFA) -> NFA {
        if nfa.dfa.borrow().is_some() {
            return nfa.get_dfa().as_ref().clone();
        }

        /*
        Cache each set along with its string representation
        we can cache the string representation because iterating a HashSet is random
        thus the same HashSet will have difference strings each time closure stringift_set is invoked
        */
        let mut strings = Vec::<(HashSet<String>, String)>::new();
        let mut stringify_set = |x: &HashSet<String>| {
            for (set, string) in &strings {
                if set.is_subset(x) && x.is_subset(set) {
                    return string.to_string();
                }
            }
            let mut s = String::new();
            s.push('<');
            s.push_str(&{
                let mut elements = String::new();
                for elem in x {
                    elements.push_str(&format!("{elem}, "))
                }
                elements.pop();
                elements.pop();
                elements
            });
            s.push('>');
            strings.push((x.clone(), s.to_string()));
            s
        };

        let mut states = HashSet::<String>::new();
        let mut alphabet = nfa.alphabet.clone();
        alphabet.remove(&'\0');
        let mut transition_function = HashMap::<String, HashMap<char, HashSet<String>>>::new();

        let mut start_state = HashSet::<String>::from([nfa.start_state.to_string()]);
        start_state = nfa.expand(&start_state);

        let mut accept_states = HashSet::<String>::new();

        let sink_state = HashSet::from([format!(
            "({name}@{{{now}}})",
            name = "<DFA-SINK>",
            now = Self::now(),
        )]);

        let mut new_sets = LinkedList::from([start_state.clone()]);
        let start_state = stringify_set(&start_state);
        let mut used_sink_state = false;

        while let Some(new_set) = new_sets.pop_front() {
            let name = stringify_set(&new_set);
            let set_symbols_map = transition_function
                .entry(name.to_string())
                .or_insert(HashMap::new());
            if set_symbols_map.is_empty() {
                states.insert(name.to_string());
                if new_set.intersection(&nfa.accept_states).next().is_some() {
                    accept_states.insert(name.to_string());
                }
                for symbol in &alphabet {
                    let moved_new_set = {
                        let x = nfa.move_set(&new_set, *symbol);
                        if x.is_empty() {
                            used_sink_state = true;
                            sink_state.clone()
                        } else {
                            x
                        }
                    };
                    set_symbols_map.insert(*symbol, HashSet::from([stringify_set(&moved_new_set)]));
                    new_sets.push_back(moved_new_set);
                }
            }
        }

        if used_sink_state {
            let sink_state_name = stringify_set(&sink_state);
            states.insert(sink_state_name.to_string());
            let sink_state_map = transition_function
                .entry(sink_state_name)
                .or_insert(HashMap::new());
            for symbol in &alphabet {
                sink_state_map.insert(*symbol, sink_state.clone());
            }
        }

        let is_deterministic = true;
        let dfa = RefCell::new(None);

        NFA {
            states,
            alphabet,
            transition_function,
            start_state,
            accept_states,
            is_deterministic,
            dfa,
        }
    }

    pub fn transform_to_dfa(&mut self) -> &mut Self {
        if self.is_deterministic {
            return self;
        }

        let NFA {
            states,
            transition_function,
            start_state,
            accept_states,
            ..
        } = Self::compute_equivalent_dfa(self);

        self.states.clear();
        self.states.extend(states.into_iter());
        self.alphabet.remove(&'\0');
        self.transition_function.clear();
        self.transition_function
            .extend(transition_function.into_iter());
        self.start_state.clear();
        self.start_state.push_str(&start_state);
        self.accept_states.clear();
        self.accept_states.extend(accept_states.into_iter());
        self.is_deterministic = true;

        self
    }

    pub fn get_dfa(&self) -> Rc<NFA> {
        if self.dfa.borrow().is_none() {
            let dfa = {
                let automaton = {
                    if self.is_deterministic {
                        self.clone()
                    } else {
                        Self::compute_equivalent_dfa(self)
                    }
                };

                Some(Rc::new(automaton))
            };

            *self.dfa.borrow_mut() = dfa;
        }
        // Ref<Option<Rc<NFA>>>
        let dfa = self.dfa.borrow();
        // &Rc<NFA>
        let dfa = dfa.as_ref().unwrap();

        Rc::clone(dfa)
    }

    /*
    Convert the invoking automaton to an equivalent regular expression
    states are processed according to parameter (removal_sequence)
    parameters (g_start_state) and (g_accept_state) are those use during
    the construction of the hypothetical GNFA (Generalized NFA)
     */
    pub fn to_regular_expression(
        &self,
        removal_sequence: &[&str],
        g_start_state: &str,
        g_accept_state: &str,
    ) -> String {
        {
            let fake_states = removal_sequence
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
        if self.states.contains(g_start_state) {
            panic!("Choose another start state");
        }
        if self.states.contains(g_accept_state) {
            panic!("Choose another accept state");
        }
        {
            let missing = self
                .states
                .iter()
                .filter(|elem| !removal_sequence.contains(&elem.as_str()))
                .collect::<Vec<&String>>();
            if !missing.is_empty() {
                panic!("States `{:?}` are missing from removal sequence", missing);
            }
        }

        use crate::generators::regexp::concat_string_regexes as concat;
        use crate::generators::regexp::star_string_regex as star;
        use crate::generators::regexp::union_string_regexes as union;

        let mut function = HashMap::<(&str, &str), RefCell<Option<String>>>::new();

        function.insert(
            (g_start_state, g_accept_state),
            RefCell::new(Option::<String>::None),
        );

        for state in &(self.states) {
            function.insert((g_start_state, state), RefCell::new(Option::<String>::None));
            function.insert(
                (state, g_accept_state),
                RefCell::new(Option::<String>::None),
            );
        }
        function.insert(
            (g_start_state, &self.start_state),
            RefCell::new(Some(String::new())),
        );

        for accept_state in &(self.accept_states) {
            function.insert(
                (accept_state.as_str(), g_accept_state),
                RefCell::new(Some(String::new())),
            );
        }

        for state in &(self.states) {
            for another in &(self.states) {
                let mut transitions = Vec::<Option<String>>::new();
                if let Some(state_map) = self.transition_function.get(state) {
                    transitions = state_map
                        .iter()
                        .filter(|(_, symbol_set)| symbol_set.contains(another))
                        .map(|(symbol, _)| Some(symbol.to_string()))
                        .collect::<_>();
                } else {
                    transitions.push(Option::<String>::None);
                }

                let transitions = transitions
                    .iter()
                    .map(|value| value.as_ref().map(|x| x.as_str()))
                    .collect::<Vec<Option<&str>>>();

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
            let leaving_self_loop = star(&leaving_self_loop.borrow().as_deref());
            let leaving_self_loop = Some(leaving_self_loop);

            for sender in &senders {
                let sender_to_leaving = function.get(&(sender.as_str(), leaving)).unwrap_or(&phi);
                if sender_to_leaving.borrow().is_none() {
                    continue;
                }

                for receiver in &receivers {
                    let leaving_to_receiver =
                        function.get(&(leaving, receiver.as_str())).unwrap_or(&phi);
                    if leaving_to_receiver.borrow().is_none() {
                        continue;
                    }

                    let sender_to_receiver = function
                        .get(&(sender.as_str(), receiver.as_str()))
                        .unwrap_or(&phi);

                    let through_leaving = concat(&[
                        sender_to_leaving.borrow().as_deref(),
                        leaving_self_loop.as_deref(),
                        leaving_to_receiver.borrow().as_deref(),
                    ]);

                    let new_regex = union(&[
                        sender_to_receiver.borrow().as_deref(),
                        through_leaving.as_deref(),
                    ]);

                    let regex = function
                        .get(&(sender.as_str(), receiver.as_str()))
                        .unwrap_or(&phi);
                    *(regex.borrow_mut()) = new_regex;
                }
            }
        }

        // Option<&RefCell<Option<String>>>
        let final_expression = function.get(&(g_start_state, g_accept_state));

        // &RefCell<Option<String>>
        let final_expression = final_expression.unwrap();

        // Ref<_, Option<String>>
        let final_expression = final_expression.borrow();

        // Option<&String>
        let final_expression = final_expression.as_ref();

        // &String
        let final_expression = final_expression.unwrap();

        final_expression.to_string()
    }

    /*
    Compute the Kleene closure of this automaton
    The Kleene closure of an NFA (N) is defined as follow:
    Create a new distinguished start state S, mark S as accepting
    State (S) has an empty string transition leading to the start state of (N)
    For each accepting state in (N) add an empty string transition leading to
    the start state of (N)
     */
    pub fn kleene_star(nfa: &NFA, star_start_state: &str) -> NFA {
        // The new start state, kleene starred NFA start state
        let start_state = format!(
            "({name}@{{{now}}})",
            name = star_start_state,
            now = Self::now(),
        );

        let mut states = nfa.states.clone();
        states.insert(start_state.to_string());

        let mut alphabet = nfa.alphabet.clone();
        alphabet.insert('\0'); // if missing

        let mut transition_function = nfa.transition_function.clone();

        let mut accept_states = nfa.accept_states.clone();
        accept_states.insert(start_state.to_string());

        for accept_state in &accept_states {
            /*
            Add an empty string transition from all the accepting states (this includes the new start state)
            to the old start state (start state of the invoking NFA object)
             */

            transition_function
                .entry(accept_state.to_string())
                .or_insert(HashMap::<char, HashSet<String>>::new())
                .entry('\0')
                .or_insert(HashSet::<String>::new())
                .insert(nfa.start_state.to_string());
        }

        let is_deterministic = false;
        let dfa = RefCell::new(None);

        NFA {
            states,
            alphabet,
            transition_function,
            start_state,
            accept_states,
            is_deterministic,
            dfa,
        }
    }

    /*
    Compute the union of a finite set of NFAs
    Their union is defined like follow:
    Create a distinguished non-accepting start state S for the union automaton
    State S has a empty string transitions to each start state in each automaton in given the set
    and the accept states of the union automaton are exaclty those of the given automata
     */
    pub fn union<'a>(automata: impl Iterator<Item = &'a NFA>, union_start_state: &str) -> NFA {
        // The new start state for the union NFA
        let start_state = union_start_state.to_string();

        let mut states = HashSet::<String>::from([start_state.to_string()]);
        let mut alphabet = HashSet::<char>::from(['\0']);

        let mut accept_states = HashSet::<String>::new();
        let mut transition_function = HashMap::<String, HashMap<char, HashSet<String>>>::new();

        transition_function.insert(
            start_state.to_string(),
            HashMap::from([('\0', HashSet::new())]),
        );

        /*
        This closure is used to format states names to avoid name clashes
        Also such that when the final union automaton is computing over some input
        We can see how each of its original components is acting
         */
        let style = |s: &str, k: usize| format!("(A{k}.{s})");

        for (counter, automaton) in automata.enumerate() {
            // Add an empty string transition from the new start state
            // to the start state of currently processed automaton.
            transition_function
                .get_mut(&start_state)
                .unwrap()
                .get_mut(&'\0')
                .unwrap()
                .insert(style(&automaton.start_state, counter));

            // Add the alphabet of the currently processed automaton
            alphabet.extend(automaton.alphabet.iter());

            /*
            Add all states of the currenly processed automaton but with their names
            styled by closure (style) defined above this loop header
             */
            automaton.states.iter().for_each(|s| {
                // style state name
                let name = style(s, counter);

                // insert the styled name into the states set of the union automaton
                states.insert(name.to_string());

                /*
                If this state in currently process automaton has some transitions
                then also style the states in its transitions using the same index
                since they all belong to the currently processed automaton.
                 */
                if let Some(state_map) = automaton.read_state_symbols_map(s) {
                    let mut adjusted_state_map = HashMap::<char, HashSet<String>>::new();

                    for (symbol, symbol_set) in state_map {
                        let symbol_set = symbol_set
                            .iter()
                            .map(|elem| {
                                let elem = style(elem, counter);
                                states.insert(elem.to_string());
                                elem
                            })
                            .collect::<HashSet<String>>();
                        adjusted_state_map.insert(*symbol, symbol_set);
                    }

                    // Adjoin the (state symbols map) of currently processed state.
                    transition_function.insert(name, adjusted_state_map);
                }
            });

            /*
            Add all accepting states of the current automaton
            to the accept states set of the union automaton
            because the union automaton accepts only if at least
            one of its components do.
             */
            accept_states.extend(automaton.accept_states.iter().map(|q| style(q, counter)));
        }

        let is_deterministic = false;
        let dfa = RefCell::new(None);

        NFA {
            states,
            alphabet,
            transition_function,
            start_state,
            accept_states,
            is_deterministic,
            dfa,
        }
    }

    /*
    Compute the concatentation automaton of a sequence of NFAs
    Their concatentation is defined like follow:
    The start state of the concatenation automaton is the start state of the first NFA
    we can talk first, second, third because the input is sequence not a set like method (union)
    The concatenation automaton accepts only if its input is passed through
    the orginal NFAs sequence such that each NFA along the ways accepts a part of the input
    and the last NFA accepts a part of the input
    Put another way, the concatenation automaton of a sequence of N automata accepts its input
    only if the input can be broken into N piece with the i-th automaton from the sequence accepts
    the i-th part of the input for all 1 <= i <= N.
     */
    pub fn concatenate(automata: &[&NFA]) -> NFA {
        let style = |s: &str, k: usize| format!("(A{k}.{s})");

        let mut states = HashSet::<String>::new();
        let mut alphabet = HashSet::<char>::from(['\0']);

        let mut transition_function = HashMap::<String, HashMap<char, HashSet<String>>>::new();

        for (counter, automaton) in automata.iter().enumerate() {
            // Add the alphabet of the currently processed automaton
            alphabet.extend(automaton.alphabet.iter());

            /*
            Add all states of the currenly processed automaton but with their names
            styled by closure (style) defined above this loop header
             */
            automaton.states.iter().for_each(|s| {
                // style state name
                let name = style(s, counter);

                // insert the styled name into the states set of the union automaton
                states.insert(name.to_string());

                /*
                If this state in currently process automaton has some transitions
                then also style the states in its transitions using the same index
                since they all belong to the currently processed automaton.
                 */
                if let Some(state_map) = automaton.read_state_symbols_map(s) {
                    let mut adjusted_state_map = HashMap::<char, HashSet<String>>::new();

                    for (symbol, symbol_set) in state_map {
                        let symbol_set = symbol_set
                            .iter()
                            .map(|elem| {
                                let elem = style(elem, counter);
                                states.insert(elem.to_string());
                                elem
                            })
                            .collect::<HashSet<String>>();
                        adjusted_state_map.insert(*symbol, symbol_set);
                    }

                    // Adjoin the (state symbols map) of currently processed state.
                    transition_function.insert(name, adjusted_state_map);
                }
            });

            if counter + 1 < automata.len() {
                /*
                Create empty string transitions from the accept states
                of the current automaton to the start state of the next.
                 */
                let next_start_state = style(&automata[counter + 1].start_state, counter + 1);

                for accept_state in &automaton.accept_states {
                    let name = style(accept_state, counter);
                    transition_function
                        .entry(name)
                        .or_insert(HashMap::<char, HashSet<String>>::new())
                        .entry('\0')
                        .or_insert(HashSet::<String>::new())
                        .insert(next_start_state.to_string());
                }
            }
        }

        let start_state = style(&automata[0].start_state, 0);
        let is_deterministic = false;
        let dfa = RefCell::new(None);
        let accept_states = automata
            .last()
            .unwrap()
            .accept_states
            .iter()
            .map(|s| style(s, automata.len() - 1))
            .collect::<HashSet<String>>();

        NFA {
            states,
            alphabet,
            transition_function,
            start_state,
            accept_states,
            is_deterministic,
            dfa,
        }
    }

    pub fn invert(&mut self) -> &mut NFA {
        if !self.is_deterministic {
            eprintln!("NFA::invert: Invoking automaton MUST BE deterministic");
            std::panic::set_hook(Box::new(|_| {}));
            panic!();
        }
        self.accept_states = self
            .states
            .difference(&self.accept_states)
            .map(String::from)
            .collect::<_>();
        self
    }

    pub fn compute_complement(nfa: &NFA) -> NFA {
        let mut nfa_clone = nfa.clone();
        Self::invert(&mut nfa_clone);
        nfa_clone
    }

    pub fn has_empty_language(nfa: &NFA) -> bool {
        if !nfa.accept_states.is_empty() {
            let mut marked_states = HashSet::<String>::new();
            marked_states.extend(
                nfa.expand(&HashSet::from([nfa.start_state.to_string()]))
                    .into_iter(),
            );

            loop {
                let before = marked_states.len();
                for marked_state in marked_states.clone() {
                    for symbol in &nfa.alphabet {
                        marked_states.extend(
                            nfa.move_set(&HashSet::from([marked_state.to_string()]), *symbol)
                                .into_iter(),
                        );
                    }
                }

                if before == marked_states.len() {
                    break;
                }
            }

            /*
            Test if the intersection of marked states and accept states is empty
            Next element in the intersection iterator will be None if so
             */
            nfa.accept_states
                .intersection(&marked_states)
                .next()
                .is_none()
        } else {
            /*
            This automaton has no accept states
            Thus, it's (true) that it recognizes (The Empty Language)
             */
            true
        }
    }

    pub fn intersection(automata: &[&NFA]) -> NFA {
        {
            let mut error = String::new();
            for (idx, nfa) in automata.iter().enumerate() {
                if !nfa.is_deterministic {
                    error.push_str(&format!("NFA in index {idx} is not deterministic!\n"));
                }
            }
            if !error.is_empty() {
                eprintln!("NFA::intersection: {error}");
                std::panic::set_hook(Box::new(|_| {}));
                panic!();
            }
        }
        let stringify_set = |x: &Vec<String>| {
            let mut s = String::new();
            s.push('(');
            s.push_str(&{
                let mut elements = String::new();
                for elem in x {
                    elements.push_str(&format!("{elem}, "))
                }
                elements.pop();
                elements.pop();
                elements
            });
            s.push(')');
            s
        };

        let product = |sets: &[&HashSet<String>]| -> Vec<Vec<String>> {
            let mut product = LinkedList::<Vec<String>>::new();
            product.push_front(vec![]);
            for (tuple_size, set) in sets.iter().enumerate() {
                let end = product.len();
                for _ in 0..end {
                    let mut front = product.pop_front().unwrap();
                    let mut set = sets[tuple_size];
                    if set.is_empty() {
                        return vec![];
                    } else {
                        for element in set {
                            front.push(element.to_string());
                            product.push_back(front.clone());
                            front.pop();
                        }
                    }
                }
            }

            product.into_iter().collect::<Vec<_>>()
        };

        let mut states = {
            let states_sets_array = automata.iter().map(|nfa| &nfa.states).collect::<Vec<_>>();
            let states_sets_array = &states_sets_array[..];

            product(states_sets_array)
                .into_iter()
                .map(|tuple| {
                    let name = stringify_set(&tuple);
                    (tuple, name)
                })
                .collect::<Vec<_>>()
        };

        let the_dead_state = format!(
            "({name}@{{{now}}})",
            name = "<INTERSECTION-SINK>",
            now = Self::now()
        );
        let dead_state_set = HashSet::from([the_dead_state.to_string()]);
        let mut used_dead_state = false;

        let alphabet = {
            let mut alphabet = HashSet::<char>::new();
            for nfa in automata {
                alphabet.extend(nfa.alphabet.iter());
            }
            alphabet
        };

        let transition_function = {
            let mut transition_function = HashMap::<String, HashMap<char, HashSet<String>>>::new();
            for (state, name) in &states {
                let state_map = transition_function
                    .entry(name.to_string())
                    .or_insert(HashMap::new());
                for symbol in &alphabet {
                    /*
                    HashSet at position (i) is states reachable from state[i] in automata[i] when reading (symbol)
                     */
                    let outputs = state
                        .iter()
                        .enumerate()
                        .map(|(idx, elem_state)| {
                            automata[idx]
                                .move_set(&HashSet::from([elem_state.to_string()]), *symbol)
                        })
                        .collect::<Vec<_>>();
                    let outputs = outputs.iter().collect::<Vec<_>>();
                    let outputs = &outputs[..];
                    let outputs = product(outputs);
                    if outputs.is_empty() {
                        state_map.insert(*symbol, dead_state_set.clone());
                        used_dead_state = true;
                    } else {
                        let outputs = outputs
                            .iter()
                            .map(|tuple| stringify_set(tuple))
                            .collect::<HashSet<_>>();
                        state_map.insert(*symbol, outputs);
                    }
                }
            }

            if used_dead_state {
                let dead_state_name = the_dead_state.to_string();
                states.push((
                    vec![dead_state_name.to_string()],
                    dead_state_name.to_string(),
                ));
                transition_function
                    .entry(the_dead_state.to_string())
                    .or_insert(HashMap::new())
                    .extend(
                        alphabet
                            .iter()
                            .map(|symbol| (*symbol, dead_state_set.clone())),
                    );
            }

            transition_function
        };

        let start_state = {
            let mut start_state = Vec::<String>::new();
            for nfa in automata {
                start_state.push(nfa.start_state.to_string());
            }
            stringify_set(&start_state)
        };

        let accept_states = {
            let states_array = automata
                .iter()
                .map(|nfa| &nfa.accept_states)
                .collect::<Vec<_>>();
            let states_array = &states_array[..];
            let mut accept_states = product(states_array);
            if accept_states.len() == 1 && accept_states.last().as_ref().unwrap().is_empty() {
                accept_states.clear();
            }
            let accept_states = accept_states
                .iter()
                .map(|accepting_tuple| stringify_set(accepting_tuple))
                .collect::<HashSet<_>>();

            accept_states
        };

        let states = states
            .into_iter()
            .map(|(state, name)| name)
            .collect::<HashSet<_>>();

        let is_deterministic = true;
        let dfa = RefCell::new(None);

        NFA {
            states,
            alphabet,
            transition_function,
            start_state,
            accept_states,
            is_deterministic,
            dfa,
        }
    }

    pub fn eliminate_epsilon_transitions(nfa: &mut Self) -> &mut Self {
        let mut strings = Vec::<(HashSet<String>, String)>::new();
        let mut stringify_set = |x: &HashSet<String>| -> String {
            for (set, name) in &strings {
                if set.is_subset(x) && x.is_subset(set) {
                    return name.to_string();
                }
            }
            let mut s = String::new();
            s.push('{');
            s.push_str(&{
                let mut elements = String::new();
                for elem in x {
                    elements.push_str(&format!("{elem}, "))
                }
                elements.pop();
                elements.pop();
                elements
            });
            s.push('}');
            strings.push((x.clone(), s.to_string()));
            s
        };

        nfa.alphabet.remove(&'\0');

        let start_state_set = nfa.expand(&HashSet::from([nfa.start_state.to_string()]));
        let mut states_set = HashSet::<String>::new();
        let mut transitions = HashMap::<String, HashMap<char, HashSet<String>>>::new();
        let mut final_states = HashSet::<String>::new();

        let sink_state_set = HashSet::from([format!(
            "({name}@{{{now}}})",
            name = "ETESS", // EpsilonTransitionsEliminationSinkState
            now = Self::now()
        )]);
        let mut used_sink_state = false;

        let mut states_queue = LinkedList::from([start_state_set.clone()]);
        while let Some(front) = states_queue.pop_front() {
            let name = stringify_set(&front);
            let state_map = transitions
                .entry(name.to_string())
                .or_insert(HashMap::new());
            if state_map.is_empty() {
                states_set.insert(name.to_string());
                if nfa.accept_states.intersection(&front).next().is_some() {
                    final_states.insert(name.to_string());
                }
                state_map.extend(nfa.alphabet.iter().map(|symbol| {
                    (*symbol, {
                        let mut output = nfa.move_set(&front, *symbol);
                        if output.is_empty() {
                            used_sink_state = true;
                            output = sink_state_set.clone();
                        }

                        let set_name = stringify_set(&output);
                        states_set.insert(stringify_set(&output));

                        if !output.is_empty() {
                            states_queue.push_back(output);
                        }
                        HashSet::from([set_name])
                    })
                }));
            }
        }

        nfa.states.clear();
        nfa.states.extend(states_set.into_iter());

        nfa.transition_function.clear();
        nfa.transition_function.extend(transitions.into_iter());

        nfa.start_state.clear();
        nfa.start_state.push_str(&stringify_set(&start_state_set));

        nfa.accept_states.clear();
        nfa.accept_states.extend(final_states.into_iter());

        if used_sink_state {
            let sink_state_set_name = stringify_set(&sink_state_set);
            nfa.states.insert(sink_state_set_name.to_string());
            nfa.transition_function
                .entry(sink_state_set_name.to_string())
                .or_insert(HashMap::new())
                .extend(
                    nfa.alphabet
                        .iter()
                        .map(|symbol| (*symbol, sink_state_set.clone())),
                );
        }

        nfa
    }

    pub fn add_missing_transitions(nfa: &mut Self) -> &mut Self {
        let sink_state_name = format!("({name}@{{{now}}})", name = "SINK", now = Self::now());
        let sink_state_set = HashSet::from([sink_state_name.to_string()]);
        let mut alphabet = nfa.alphabet.clone();
        alphabet.remove(&'\0');

        let mut used_sink_state = false;
        for state in &nfa.states {
            let state_map = nfa
                .transition_function
                .entry(state.to_string())
                .or_insert(HashMap::new());
            if state_map.is_empty() {
                used_sink_state = true;
                state_map.extend(
                    alphabet
                        .iter()
                        .map(|symbol| (*symbol, sink_state_set.clone())),
                );
            } else {
                for symbol in &alphabet {
                    let symbol_set = state_map.entry(*symbol).or_insert(HashSet::new());
                    if symbol_set.is_empty() {
                        used_sink_state = true;
                        symbol_set.insert(sink_state_name.to_string());
                    }
                }
            }
        }

        if used_sink_state {
            nfa.states.insert(sink_state_name.to_string());
            let sink_state_map = nfa
                .transition_function
                .entry(sink_state_name.to_string())
                .or_insert(HashMap::new());
            sink_state_map.extend(
                alphabet
                    .iter()
                    .map(|symbol| (*symbol, sink_state_set.clone())),
            );
        }

        nfa
    }
}
