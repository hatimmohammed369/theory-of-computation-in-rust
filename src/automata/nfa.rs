#![allow(dead_code)]
#![allow(unused_variables)]
#![allow(unused_mut)]
// Finite Automata module

use crate::automata::ComputationResult;
use crate::generators::regexp::ExpressionBase;
use std::cell::RefCell;
use std::collections::HashMap;
use std::collections::HashSet;
use std::collections::LinkedList;
use std::fmt::Display;
use std::rc::Rc;
use std::time::{SystemTime, UNIX_EPOCH};

use super::ComputationStyle;

#[derive(Debug, Clone, Copy, Eq, PartialEq, Hash)]
pub enum AlphabetSymbol {
    Any, // Any (not the empty string) character, will be useful when implement `.` metacharacter in regular expressions.
    EmptyString, // The emtpy string, which is unique.
    Character(char), // A particular character
}

impl Display for AlphabetSymbol {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                AlphabetSymbol::Any => ".".to_string(),
                AlphabetSymbol::EmptyString => "".to_string(),
                AlphabetSymbol::Character(value) => format!("{value}"),
            }
        )
    }
}

// (Non-deterministic) Finite Automaton
#[derive(Debug, Clone)]
pub struct NFA {
    // automaton states
    states: HashSet<String>,

    // automaton alphabet
    alphabet: HashSet<AlphabetSymbol>,

    // the transition function, the heart of the automaton
    /*
    Keys in field (transition_function) are states and values are (state maps)
    For each state (q), its state map (the HashMap<AlphabetSymbol, HashSet<String>> part) has alphabet symbols as its keys
    and the values are those states you can reach from state (q) by reading a key from the state map of (q)
    For instance:
    let transition_function be {q: {AlphabetSymbol::Character('0') : {q, r, s}}, {r: {AlphabetSymbol::EmptyString, v}}} ...}
    this means when in state (q) reading the alphabet symbol 0 we reach states {q, r, s}
    also when in state (r) we can do an empty string transition taking us to states {t, v}
    and so forth . . .
     */
    transition_function: HashMap<String, HashMap<AlphabetSymbol, HashSet<String>>>,

    // The distinguished starting state.
    start_state: String,

    // The accepting (final) states
    accept_states: HashSet<String>,

    /*
    This flag determines if the automaton is deterministic or not
    You can either pass its value or have the constructor infer its value
     */
    computation_style: ComputationStyle,

    /*
    Cache field containg an equivalent DFA to this automaton
    When calling get_dfa (which returns the equivalent DFA) the code looks up the field
    Calling get_dfa on NFA which is already an DFA (computation_style = ComputationStyle::Deterministic) clones (self)
    thus it's not a good idea to call this function when (computation_style) field is ComputationStyle::Deterministic variant
     */
    dfa: RefCell<Option<Rc<NFA>>>,
}

impl NFA {
    pub fn new_random_state(name: &str) -> String {
        let now = SystemTime::now().duration_since(UNIX_EPOCH).ok().unwrap();
        let now = now.as_secs();
        format!("({name}@{now:#x})")
    }

    pub fn new(
        /*
        If a state is in parameter (alphabet)
        but it is never used in parameter (transition_function) ignore it and emit a warning
         */
        states: &[&str],

        /*
        If an alphabet symbol is in parameter (alphabet)
        but it is never used in parameter (transition_function) ignore it and emit a warning
         */
        alphabet: &[AlphabetSymbol],

        /*
        Each element is of the form
        (state-q, alphabet-symbol-x, &[states reachable from (q) when reading (x)])
         */
        transition_function: &[(&str, AlphabetSymbol, &[&str])],

        start_state: &str,
        accept_states: &[&str],
        infer_type: bool, // Infer the type of the new automaton based on parameter values.
        computation_style: &ComputationStyle,
    ) -> NFA {
        let mut states_set = states
            .iter()
            .map(ToString::to_string)
            .collect::<HashSet<_>>();
        let mut alphabet_set = alphabet.iter().copied().collect::<HashSet<_>>();

        if !states.contains(&start_state) {
            eprintln!(
                "Warning from [NFA::new]: Start State `{start_state}` is not in the states array\n"
            );
        }
        states_set.insert(start_state.to_string()); // Do not forget adjoining the start state.

        // Issue warnings for each accepting state not in parameter `states`.
        for x in accept_states {
            if !states.contains(x) {
                eprintln!(
                    "Warning from [NFA::new]: Accept State `{x}` is not in the states array\n"
                );
            }
            states_set.insert(x.to_string());
        }

        // The final transition function of the new object.
        let mut function = HashMap::new();

        let mut computed_computation_style = ComputationStyle::Nondeterministic;

        // This hash set is used in computing (computation_style) field if infer_type flag is set.
        // if processed states, which are the first entry in each element in parameter (transition_function),
        // are less than the total number of states in variable (states_set)
        // this means that some states do not have outgoing transitions
        // and hence the new object is a strictly non-deterministic.

        for item in transition_function {
            let (state, symbol, items) = (item.0, item.1, item.2);
            states_set.insert(state.to_string()); // mark this state as used.
            alphabet_set.insert(symbol);

            if infer_type && matches!(computed_computation_style, ComputationStyle::Deterministic) {
                // If there is an empty string transition
                // then automaton is Nondeterministic.
                if matches!(symbol, AlphabetSymbol::EmptyString) {
                    computed_computation_style = ComputationStyle::Nondeterministic;
                }
            }

            if !states.contains(&state) {
                eprintln!("Warning from [NFA::new]: State `{state}` is not in the states array\n");
                eprintln!("Found in transition {item:?}\n");
            }

            if !alphabet.contains(&symbol) {
                eprintln!(
                    "Warning from [NFA::new]: Symbol `{symbol}` is not in the alphabet array\n"
                );
                eprintln!("Found in transition {item:?}\n");
            }

            // Automatically create new entries in transition function map when missing
            let destination = function
                .entry(state.to_string())
                .or_insert(HashMap::new())
                .entry(symbol)
                .or_insert(HashSet::new());
            for x in items {
                states_set.insert(x.to_string());
                destination.insert(x.to_string());

                if !states.contains(x) {
                    eprintln!("Warning from [NFA::new]: State `{x}` is not in the states array\n");
                    eprintln!("Found in transition {item:?}\n");
                }
            }
        }

        let computation = if infer_type {
            Self::_infer_type(&states_set, &function, {
                let mut true_alphabet = alphabet_set.len();
                if alphabet_set.contains(&AlphabetSymbol::Any) {
                    true_alphabet -= 1;
                }
                if alphabet_set.contains(&AlphabetSymbol::EmptyString) {
                    true_alphabet -= 1;
                }
                true_alphabet
            })
        } else {
            computation_style.clone()
        };

        NFA {
            states: states_set,
            alphabet: alphabet_set,
            transition_function: function,
            start_state: start_state.to_string(),
            accept_states: accept_states.iter().map(ToString::to_string).collect(),
            computation_style: computation,
            dfa: RefCell::new(None),
        }
    }

    /*
    Directly fill private fields using pre-computed values.
     */
    pub fn raw_new(
        states: HashSet<String>,
        alphabet: HashSet<AlphabetSymbol>,
        transition_function: HashMap<String, HashMap<AlphabetSymbol, HashSet<String>>>,
        start_state: String,
        accept_states: HashSet<String>,
        computation_style: &ComputationStyle,
        dfa: RefCell<Option<Rc<NFA>>>,
    ) -> NFA {
        let computation_style = computation_style.clone();
        NFA {
            states,
            alphabet,
            transition_function,
            start_state,
            accept_states,
            computation_style,
            dfa,
        }
    }

    /*
    Access a state map for writing
     */
    pub fn write_state_symbols_map(
        &mut self,
        state: &str,
    ) -> &mut HashMap<AlphabetSymbol, HashSet<String>> {
        self.transition_function
            .entry(state.to_string())
            .or_insert(HashMap::new())
    }

    /*
    Access a particular value in a state map for writing
    This particular value is those states reachable from parameter (state) when reading parameter (symbol).
     */
    pub fn write_symbol_states_set(
        &mut self,
        state: &str,
        symbol: &AlphabetSymbol,
    ) -> &mut HashSet<String> {
        self.transition_function
            .entry(state.to_string())
            .or_insert(HashMap::new())
            .entry(*symbol)
            .or_insert(HashSet::new())
    }

    /*
    Access a state map for reading
     */
    pub fn read_state_symbols_map(
        &self,
        state: &str,
    ) -> Option<&HashMap<AlphabetSymbol, HashSet<String>>> {
        self.transition_function.get(state)
    }

    /*
    Access a particular value in a state map for reading
    This particular value is those states reachable from parameter (state) when reading parameter (symbol).
     */
    pub fn read_symbol_states_set(
        &self,
        state: &str,
        symbol: &AlphabetSymbol,
    ) -> Option<&HashSet<String>> {
        match self.transition_function.get(state) {
            Some(state_symbols_map) => state_symbols_map.get(symbol),
            None => None,
        }
    }

    /*
    Add a transition for a specific state
     */
    pub fn add_transition<'a>(
        &'a mut self,
        state: &str,
        symbol: &AlphabetSymbol,
        output: impl Iterator<Item = &'a String>,
    ) {
        self.transition_function
            .entry(state.to_string())
            .or_insert(HashMap::new())
            .entry(*symbol)
            .or_insert(HashSet::new())
            .extend(output.map(ToString::to_string));
    }

    pub fn read_states(&self) -> &HashSet<String> {
        &self.states
    }

    pub fn read_alphabet(&self) -> &HashSet<AlphabetSymbol> {
        &self.alphabet
    }

    pub fn read_transition_function(
        &self,
    ) -> &HashMap<String, HashMap<AlphabetSymbol, HashSet<String>>> {
        &self.transition_function
    }

    pub fn read_start_state(&self) -> &str {
        &self.start_state
    }

    pub fn read_accept_states(&self) -> &HashSet<String> {
        &self.accept_states
    }

    pub fn is_deterministic(&self) -> bool {
        matches!(self.computation_style, ComputationStyle::Deterministic)
    }

    pub fn read_computation_style(&self) -> &ComputationStyle {
        &self.computation_style
    }

    fn _infer_type(
        states: &HashSet<String>,
        transition_function: &HashMap<String, HashMap<AlphabetSymbol, HashSet<String>>>,
        true_alphabet: usize,
    ) -> ComputationStyle {
        for state in states {
            match transition_function.get(state) {
                Some(state_map) => {
                    if !state_map.contains_key(&AlphabetSymbol::Any)
                        && (state_map.contains_key(&AlphabetSymbol::EmptyString)
                            || state_map.len() < true_alphabet)
                    {
                        /*
                        This state either has an empty string transition(s) or
                        does not have transitions for some alphabet symbol (x)
                        where x != AlphabetSymbol::Any and x != AlphabetSymbol::EmptyString
                         */
                        // It is (false) that this automaton is deterministic
                        return ComputationStyle::Nondeterministic;
                    }
                }
                None => {
                    // This state has no transitions
                    // It is (false) that this automaton is deterministic
                    return ComputationStyle::Nondeterministic;
                }
            }
        }
        // It (true) that this automaton is deterministic
        ComputationStyle::Deterministic
    }

    pub fn infer_type(&mut self) -> &mut Self {
        self.computation_style = Self::_infer_type(&self.states, &self.transition_function, {
            let mut true_alphabet = self.alphabet.len();
            if self.alphabet.contains(&AlphabetSymbol::Any) {
                true_alphabet -= 1;
            }
            if self.alphabet.contains(&AlphabetSymbol::EmptyString) {
                true_alphabet -= 1;
            }
            true_alphabet
        });
        self
    }

    pub fn compute_renamed_states_nfa<'a>(
        nfa: &'a mut Self,
        aliases: &HashMap<String, String>, // Partial aliases map, not all states must have aliases.
    ) -> &'a mut Self {
        let mut aliases = aliases.clone();
        for state in &nfa.states {
            // States with no alias are mapped to themselves.
            aliases
                .entry(state.to_string())
                .or_insert(state.to_string());
        }
        let name = |state: &String| aliases.get(state).unwrap().to_string();
        nfa.states.clear();
        nfa.states.extend(aliases.values().map(ToString::to_string));
        // Renaming states in each key in each state map
        for (state, map) in nfa.transition_function.iter_mut() {
            for symbol_set in map.values_mut() {
                let set_clone = symbol_set.clone();
                symbol_set.clear();
                symbol_set.extend(
                    set_clone
                        .into_iter()
                        .map(|cloned_state| name(&cloned_state)),
                );
            }
        }
        // Rename keys themeselves
        for state in aliases.keys() {
            if let Some(map) = nfa.transition_function.remove(state) {
                nfa.transition_function.insert(name(state), map);
            }
        }
        nfa.start_state = name(&nfa.start_state);
        let accept_states_clone = nfa.accept_states.clone();
        nfa.accept_states.clear();
        nfa.accept_states.extend(
            accept_states_clone
                .into_iter()
                .map(|accepting| name(&accepting)),
        );
        nfa
    }

    pub fn rename_states<'a>(&'a mut self, aliases: &HashMap<String, String>) -> &'a mut Self {
        Self::compute_renamed_states_nfa(self, aliases)
    }

    pub fn rename_states_numerically(&mut self) -> &mut Self {
        let aliases = {
            let mut counter = 0usize;
            self.states
                .iter()
                .map(|state| {
                    let entry = (state.to_string(), counter.to_string());
                    counter += 1;
                    entry
                })
                .collect::<_>()
        };
        Self::compute_renamed_states_nfa(self, &aliases)
    }

    /*
    Return all states reachable from parameter (set) using
    any number of empty string transitions

    any number: including 0, and thus value in parameter (set) is always part of return value
    thus return value of (epsilon_closure) is never the empty set
     */
    pub fn epsilon_closure(&self, set: &HashSet<String>) -> HashSet<String> {
        let mut out = set.clone();

        let mut new_items = out
            .iter()
            .map(ToString::to_string)
            .collect::<LinkedList<_>>();
        while let Some(elem) = new_items.pop_front() {
            if let Some(empty_string_set) =
                self.read_symbol_states_set(&elem, &AlphabetSymbol::EmptyString)
            {
                for s in empty_string_set {
                    if out.insert(s.to_string()) {
                        new_items.push_back(s.to_string());
                    }
                }
            }
        }

        out
    }

    /*
    Return all states reachable from each state (q) in parameter (set)
    when reading symbol in paramter (symbol)
     */
    pub fn move_set(&self, set: &HashSet<String>, symbol: &AlphabetSymbol) -> HashSet<String> {
        let mut out = HashSet::new();

        for q in set {
            out.extend(
                self.read_symbol_states_set(q, symbol)
                    .unwrap_or(&HashSet::new())
                    .iter()
                    .map(ToString::to_string),
            );

            /*
            Also add all states reachable from (q) when reading any symbol,
            such states reside in hash set associated with key AlphabetSymbol::Any
             */
            out.extend(
                self.read_symbol_states_set(q, &AlphabetSymbol::Any)
                    .unwrap_or(&HashSet::new())
                    .iter()
                    .map(ToString::to_string),
            )
        }

        self.epsilon_closure(&out)
    }

    /*
    Compute on some input.
     */
    pub fn compute(&self, input: &str, log: bool) -> Result<ComputationResult, String> {
        let mut automaton_states = HashSet::new();

        // We start with ... the start state
        automaton_states.insert(self.start_state.to_string());

        // expand the start state
        automaton_states = self.epsilon_closure(&automaton_states);

        if log {
            eprintln!();
            eprintln!("########################################");
            eprintln!("Computing on input `{input}` . . .\n");
        }

        for next_symbol in input.chars() {
            let next_symbol = &AlphabetSymbol::Character(next_symbol);
            if log {
                eprintln!("{automaton_states:?} reading {next_symbol}");
            }
            if !self.alphabet.contains(next_symbol) && !self.alphabet.contains(&AlphabetSymbol::Any)
            {
                return Err(format!(
                    "Warning from [NFA::compute]: Symbol `{next_symbol}` is not in alphabet {:?}",
                    self.alphabet
                ));
            }

            // Apply the transition function.
            automaton_states = self.move_set(&automaton_states, next_symbol);

            if log {
                eprintln!("=> {automaton_states:?}");
            }
            if automaton_states.is_empty() {
                return Err(
                    "[NFA::compute]: Early aborting computation because automaton lost all state"
                        .to_string(),
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
            eprintln!("{result:?}ed input `{input}`");

            if result == ComputationResult::Accept {
                eprintln!();
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
        thus the same HashSet will have difference strings each time closure stringify_set is invoked
        */
        let mut strings = Vec::<(HashSet<String>, String)>::new();
        let mut stringify_set = |x: &HashSet<String>| -> String {
            for (set, string) in &strings {
                if set.is_subset(x) && x.is_subset(set) {
                    return string.to_string();
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

        // States possessing transitions labeled with AlphabetSymbol::Any.
        let sigma_transitions_states = nfa
            .transition_function
            .iter()
            .filter(|(_, map)| map.contains_key(&AlphabetSymbol::Any))
            .map(|(state, _)| state.to_string())
            .collect::<HashSet<_>>();

        let mut states = HashSet::new();
        let mut alphabet = nfa.alphabet.clone();
        alphabet.remove(&AlphabetSymbol::EmptyString);
        alphabet.remove(&AlphabetSymbol::Any);
        let mut transition_function = HashMap::new();

        let start_state = nfa.epsilon_closure(&HashSet::from([nfa.start_state.to_string()]));

        let mut accept_states = HashSet::new();

        let sink_state_name = Self::new_random_state("<DFA-SINK>");
        let sink_state_set = HashSet::from([sink_state_name.to_string()]);

        let mut sets_queue = LinkedList::from([start_state.clone()]);
        let mut used_sink_state = false;

        while let Some(new_set) = sets_queue.pop_front() {
            let name = stringify_set(&new_set);
            let set_symbols_map = transition_function
                .entry(name.to_string())
                .or_insert(HashMap::new());
            if set_symbols_map.is_empty() {
                states.insert(name.to_string());

                if new_set.intersection(&nfa.accept_states).next().is_some() {
                    accept_states.insert(name.to_string());
                }

                if new_set
                    .intersection(&sigma_transitions_states)
                    .next()
                    .is_some()
                {
                    /*
                    This (new_set) contains at least one state possessing a AlphabetSymbol::Any transition
                    in this case the only transition for this (new_set) will be AlphabetSymbol::Any
                    in other words, state map of (new_set) will have a single entry whose key is AlphabetSymbol::Any
                    and whose value is the set of (S) all states reachable from (q) when reading any symbol (or the empty string)
                    for each state (q) in (new_set), also we make S = epsilon closure of S
                    Of course if set (S) is empty we use the sink state set
                    */

                    // The set (S) defined above.
                    let mut sigma_transition_set = HashSet::new();
                    for q in new_set {
                        if let Some(state_q_map) = nfa.transition_function.get(&q) {
                            for symbol_set in state_q_map.values() {
                                sigma_transition_set
                                    .extend(symbol_set.iter().map(ToString::to_string));
                            }
                        }
                    }

                    set_symbols_map.insert(AlphabetSymbol::Any, {
                        if sigma_transition_set.is_empty() {
                            used_sink_state = true;
                            sink_state_set.clone()
                        } else {
                            // Take epsilon closure
                            sigma_transition_set = nfa.epsilon_closure(&sigma_transition_set);
                            let sigma_transition_set_name = stringify_set(&sigma_transition_set);
                            sets_queue.push_back(sigma_transition_set);
                            HashSet::from([sigma_transition_set_name])
                        }
                    });
                } else {
                    // No state in (new_set) has AlphabetSymbol::Any transition.
                    let mut all_go_sink_state = true;
                    for symbol in &alphabet {
                        set_symbols_map.insert(*symbol, {
                            // All sets returned by (move_set) are epsilon closures.
                            let moved_new_set = nfa.move_set(&new_set, symbol);
                            if moved_new_set.is_empty() {
                                used_sink_state = true;
                                sink_state_set.clone()
                            } else {
                                all_go_sink_state = false;
                                let moved_new_set_name = stringify_set(&moved_new_set);
                                sets_queue.push_back(moved_new_set);
                                HashSet::from([moved_new_set_name])
                            }
                        });
                    }

                    if all_go_sink_state {
                        set_symbols_map.clear();
                        set_symbols_map.insert(AlphabetSymbol::Any, sink_state_set.clone());
                    }
                }
            }
        }

        if !sigma_transitions_states.is_empty() {
            /*
            Some states in the DFA transition function use AlphabetSymbol::Any
            included AlphabetSymbol::Any in the DFA alphabet
            */
            alphabet.insert(AlphabetSymbol::Any);
        }

        if used_sink_state {
            states.insert(sink_state_name.to_string());
            alphabet.insert(AlphabetSymbol::Any);

            transition_function
                .entry(sink_state_name)
                .or_insert(HashMap::new())
                .insert(AlphabetSymbol::Any, sink_state_set.clone());
        }

        let start_state = stringify_set(&start_state);
        let computation_style = ComputationStyle::Deterministic;
        let dfa = RefCell::new(None);

        NFA {
            states,
            alphabet,
            transition_function,
            start_state,
            accept_states,
            computation_style,
            dfa,
        }
    }

    pub fn transform_to_dfa(&mut self) -> &mut Self {
        if self.is_deterministic() {
            return self;
        }

        let NFA {
            states,
            alphabet,
            transition_function,
            start_state,
            accept_states,
            ..
        } = Self::compute_equivalent_dfa(self);

        self.states.clear();
        self.states.extend(states.into_iter());

        if alphabet.contains(&AlphabetSymbol::Any) {
            self.alphabet.insert(AlphabetSymbol::Any);
        }
        self.alphabet.remove(&AlphabetSymbol::EmptyString);

        self.transition_function.clear();
        self.transition_function
            .extend(transition_function.into_iter());

        self.start_state.clear();
        self.start_state.push_str(&start_state);

        self.accept_states.clear();
        self.accept_states.extend(accept_states.into_iter());

        self.computation_style = ComputationStyle::Deterministic;

        self
    }

    pub fn get_dfa(&self) -> Rc<NFA> {
        if self.dfa.borrow().is_none() {
            let dfa = {
                let automaton = {
                    if self.is_deterministic() {
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
    ) -> Result<Rc<ExpressionBase>, String> {
        let g_start_state = NFA::new_random_state("S");
        let g_start_state = g_start_state.as_str();
        let g_accept_state = NFA::new_random_state("A");
        let g_accept_state = g_accept_state.as_str();

        {
            let mut error = String::new();
            let fake_states = removal_sequence
                .iter()
                .filter(|s| {
                    let s = **s;
                    !self.states.contains(s)
                })
                .collect::<Vec<_>>();

            if !fake_states.is_empty() {
                error.push_str(&format!(
                    "[NFA::to_regular_expression]: States `{fake_states:?}` do not exist!\n"
                ));
            }
            if self.states.contains(g_start_state) {
                error.push_str("Choose another start state\n");
            }
            if self.states.contains(g_accept_state) {
                error.push_str("Choose another accept state\n");
            }
            let missing = self
                .states
                .iter()
                .filter(|elem| !removal_sequence.contains(&elem.as_str()))
                .collect::<Vec<_>>();
            if !missing.is_empty() {
                error.push_str(&format!(
                    "States `{missing:?}` are missing from removal sequence\n"
                ));
            }

            if !error.is_empty() {
                return Err(error);
            }
        }

        use crate::generators::regexp::concat_string_regexes as concat;
        use crate::generators::regexp::star_string_regex as star;
        use crate::generators::regexp::union_string_regexes as union;

        let mut function = HashMap::<(&str, &str), RefCell<Rc<ExpressionBase>>>::new();

        function.insert(
            (g_start_state, g_accept_state),
            RefCell::new(Rc::new(ExpressionBase::EmptySet)),
        );

        for state in &(self.states) {
            function.insert(
                (g_start_state, state),
                RefCell::new(Rc::new(ExpressionBase::EmptySet)),
            );
            function.insert(
                (state, g_accept_state),
                RefCell::new(Rc::new(ExpressionBase::EmptySet)),
            );
        }

        function.insert(
            (g_start_state, &self.start_state),
            RefCell::new(Rc::new(ExpressionBase::EmptyString)),
        );

        for accept_state in &(self.accept_states) {
            function.insert(
                (accept_state.as_str(), g_accept_state),
                RefCell::new(Rc::new(ExpressionBase::EmptyString)),
            );
        }

        for state in &(self.states) {
            for another in &(self.states) {
                let mut transitions = Vec::new();
                if let Some(state_map) = self.transition_function.get(state) {
                    transitions = state_map
                        .iter()
                        .filter(|(_, symbol_set)| symbol_set.contains(another))
                        .map(|(symbol, _)| {
                            Rc::new({
                                match symbol {
                                    AlphabetSymbol::EmptyString => ExpressionBase::EmptyString,
                                    _ => ExpressionBase::Symbol(Rc::new(*symbol)),
                                }
                            })
                        })
                        .collect::<_>();
                } else {
                    transitions.push(Rc::new(ExpressionBase::EmptySet));
                }

                let transitions = transitions.iter().map(Rc::clone).collect::<Vec<_>>();

                let combined = union(&transitions[..]);
                function.insert((state, another), RefCell::new(combined));
            }
        }

        let phi = RefCell::new(Rc::new(ExpressionBase::EmptySet));

        let mut senders = self.states.clone();
        senders.insert(g_start_state.to_string());

        let mut receivers = self.states.clone();
        receivers.insert(g_accept_state.to_string());

        for leaving in removal_sequence {
            let leaving = *leaving;

            senders.remove(leaving);
            receivers.remove(leaving);

            let leaving_self_loop = function.get(&(leaving, leaving)).unwrap_or(&phi);
            let leaving_self_loop = Rc::new(star(&leaving_self_loop.borrow()));

            for sender in &senders {
                let sender_to_leaving = function.get(&(sender.as_str(), leaving)).unwrap_or(&phi);
                if matches!(
                    sender_to_leaving.borrow().as_ref(),
                    ExpressionBase::EmptySet
                ) {
                    continue;
                }

                for receiver in &receivers {
                    let leaving_to_receiver =
                        function.get(&(leaving, receiver.as_str())).unwrap_or(&phi);
                    if matches!(
                        leaving_to_receiver.borrow().as_ref(),
                        ExpressionBase::EmptySet
                    ) {
                        continue;
                    }

                    let sender_to_receiver = function
                        .get(&(sender.as_str(), receiver.as_str()))
                        .unwrap_or(&phi);

                    let through_leaving = concat(&[
                        Rc::clone(&sender_to_leaving.borrow()),
                        Rc::clone(&leaving_self_loop),
                        Rc::clone(&leaving_to_receiver.borrow()),
                    ]);

                    let new_regex =
                        union(&[Rc::clone(&sender_to_receiver.borrow()), through_leaving]);

                    let regex = function
                        .get(&(sender.as_str(), receiver.as_str()))
                        .unwrap_or(&phi);
                    *(regex.borrow_mut()) = new_regex;
                }
            }
        }

        // Option<&RefCell<Rc<ExpressionBase>>>
        let final_expression = function.get(&(g_start_state, g_accept_state));

        // &RefCell<Rc<ExpressionBase>>
        let final_expression = final_expression.unwrap();

        // Ref<_, Rc<ExpressionBase>
        let final_expression = final_expression.borrow();

        Ok(Rc::clone(&final_expression))
    }

    pub fn compute_an_equivalent_regular_expression(&self) -> Result<Rc<ExpressionBase>, String> {
        let removal_sequence = self
            .states
            .iter()
            .map(ToString::to_string)
            .collect::<Vec<_>>();
        let mut sequence = Vec::new();
        for state in &removal_sequence {
            sequence.push(state.as_str());
        }
        self.to_regular_expression(&sequence[..])
    }

    pub fn all_regular_expressions(&self) -> Result<Vec<Rc<ExpressionBase>>, String> {
        let states_vec = self.states.clone().into_iter().collect::<Vec<_>>();
        let mut sequences = LinkedList::from([Vec::<String>::new()]);
        for _ in 0..self.states.len() {
            let end = sequences.len();
            for _ in 0..end {
                let mut front_permutation = sequences.pop_front().unwrap();
                for state in &states_vec {
                    if !front_permutation.contains(state) {
                        front_permutation.push(state.to_string());
                        sequences.push_back(front_permutation.clone());
                        front_permutation.pop();
                    }
                }
            }
        }
        Ok(sequences
            .into_iter()
            .map(|seq| {
                let mut removal_sequence = vec![];
                for item in &seq {
                    removal_sequence.push(item.as_str());
                }
                self.to_regular_expression(&removal_sequence[..]).unwrap()
            })
            .collect::<_>())
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
        let start_state = star_start_state.to_string();

        let mut states = nfa.states.clone();
        states.insert(start_state.to_string());

        let mut alphabet = nfa.alphabet.clone();
        alphabet.insert(AlphabetSymbol::EmptyString); // if missing

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
                .or_insert(HashMap::new())
                .entry(AlphabetSymbol::EmptyString)
                .or_insert(HashSet::new())
                .insert(nfa.start_state.to_string());
        }

        let computation_style = ComputationStyle::Nondeterministic;
        let dfa = RefCell::new(None);

        NFA {
            states,
            alphabet,
            transition_function,
            start_state,
            accept_states,
            computation_style,
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

        let mut states = HashSet::from([start_state.to_string()]);
        let mut alphabet = HashSet::from([AlphabetSymbol::EmptyString]);

        let mut accept_states = HashSet::new();
        let mut transition_function = HashMap::new();

        transition_function.insert(
            start_state.to_string(),
            HashMap::from([(AlphabetSymbol::EmptyString, HashSet::new())]),
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
                .get_mut(&AlphabetSymbol::EmptyString)
                .unwrap()
                .insert(style(&automaton.start_state, counter));

            // Add the alphabet of the currently processed automaton
            alphabet.extend(automaton.alphabet.iter().copied());

            /*
            Add all states of the currenly processed automaton but with their names
            styled by closure (style) defined above this loop header
             */
            for s in &automaton.states {
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
                    let mut adjusted_state_map = HashMap::new();

                    for (symbol, symbol_set) in state_map {
                        let symbol_set = symbol_set
                            .iter()
                            .map(|elem| {
                                let elem = style(elem, counter);
                                states.insert(elem.to_string());
                                elem
                            })
                            .collect::<_>();
                        adjusted_state_map.insert(*symbol, symbol_set);
                    }

                    // Adjoin the (state symbols map) of currently processed state.
                    transition_function.insert(name, adjusted_state_map);
                }
            }

            /*
            Add all accepting states of the current automaton
            to the accept states set of the union automaton
            because the union automaton accepts only if at least
            one of its components do.
             */
            accept_states.extend(automaton.accept_states.iter().map(|q| style(q, counter)));
        }

        let computation_style = ComputationStyle::Nondeterministic;
        let dfa = RefCell::new(None);

        NFA {
            states,
            alphabet,
            transition_function,
            start_state,
            accept_states,
            computation_style,
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

        let mut states = HashSet::new();
        let mut alphabet = HashSet::from([AlphabetSymbol::EmptyString]);

        let mut transition_function = HashMap::new();

        for (counter, automaton) in automata.iter().enumerate() {
            // Add the alphabet of the currently processed automaton
            alphabet.extend(automaton.alphabet.iter().copied());

            /*
            Add all states of the currenly processed automaton but with their names
            styled by closure (style) defined above this loop header
             */
            for s in &automaton.states {
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
                    let mut adjusted_state_map = HashMap::new();

                    for (symbol, symbol_set) in state_map {
                        let symbol_set = symbol_set
                            .iter()
                            .map(|elem| {
                                let elem = style(elem, counter);
                                states.insert(elem.to_string());
                                elem
                            })
                            .collect::<_>();
                        adjusted_state_map.insert(*symbol, symbol_set);
                    }

                    // Adjoin the (state symbols map) of currently processed state.
                    transition_function.insert(name, adjusted_state_map);
                }
            }

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
                        .or_insert(HashMap::new())
                        .entry(AlphabetSymbol::EmptyString)
                        .or_insert(HashSet::new())
                        .insert(next_start_state.to_string());
                }
            }
        }

        let start_state = style(&automata[0].start_state, 0);
        let computation_style = ComputationStyle::Nondeterministic;
        let dfa = RefCell::new(None);
        let accept_states = automata
            .last()
            .unwrap()
            .accept_states
            .iter()
            .map(|s| style(s, automata.len() - 1))
            .collect::<_>();

        NFA {
            states,
            alphabet,
            transition_function,
            start_state,
            accept_states,
            computation_style,
            dfa,
        }
    }

    pub fn invert(&mut self) -> &mut NFA {
        if !self.is_deterministic() {
            eprintln!("[NFA::invert]: Invoking automaton MUST BE deterministic");
            std::panic::set_hook(Box::new(|_| {}));
            panic!();
        }
        self.accept_states = self
            .states
            .difference(&self.accept_states)
            .map(ToString::to_string)
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
            let mut marked_states = HashSet::new();
            marked_states.extend(
                nfa.epsilon_closure(&HashSet::from([nfa.start_state.to_string()]))
                    .into_iter(),
            );

            loop {
                let before = marked_states.len();
                for marked_state in marked_states.clone() {
                    for symbol in &nfa.alphabet {
                        marked_states.extend(
                            nfa.move_set(&HashSet::from([marked_state.to_string()]), symbol)
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
                if !nfa.is_deterministic() {
                    error.push_str(&format!("NFA in index {idx} is not deterministic!\n"));
                }
            }
            if !error.is_empty() {
                eprintln!("[NFA::intersection]:\n{error}");
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
            let mut product = LinkedList::new();
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

            product.into_iter().collect::<_>()
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

        let sink_state_name = Self::new_random_state("<INTERSECTION-SINK>");
        let sink_state_set = HashSet::from([sink_state_name.to_string()]);
        let mut used_sink_state = false;

        let mut alphabet = {
            let mut alphabet = HashSet::new();
            for nfa in automata {
                alphabet.extend(nfa.alphabet.iter().copied());
            }
            alphabet.remove(&AlphabetSymbol::Any);
            alphabet.remove(&AlphabetSymbol::EmptyString);
            alphabet
        };

        let transition_function = {
            let mut used_symbol_any = false;
            let mut transition_function = HashMap::new();
            for (state, name) in &states {
                let state_map = transition_function
                    .entry(name.to_string())
                    .or_insert(HashMap::new());
                let some_state_uses_symbol_any = {
                    let mut found = false;
                    for (idx, q) in state.iter().enumerate() {
                        if automata[idx]
                            .transition_function
                            .get(q)
                            .unwrap_or(&HashMap::new())
                            .contains_key(&AlphabetSymbol::Any)
                        {
                            found = true;
                            break;
                        }
                    }
                    found
                };
                if some_state_uses_symbol_any {
                    // A state in this tuple (loop variable `state`) has an AlphabetSymbol::Any transition

                    /*
                    HashSet at position (i) is states reachable from state[i] in automata[i] when reading (symbol)
                     */
                    used_symbol_any = true;
                    let outputs = state
                        .iter()
                        .enumerate()
                        .map(|(idx, elem_state)| {
                            automata[idx].move_set(
                                &HashSet::from([elem_state.to_string()]),
                                &AlphabetSymbol::Any,
                            )
                        })
                        .collect::<Vec<_>>();
                    let outputs = outputs.iter().collect::<Vec<_>>();
                    let outputs = &outputs[..];
                    state_map.insert(AlphabetSymbol::Any, {
                        // Choose output set
                        let p = product(outputs);
                        if p.is_empty() {
                            used_sink_state = true;
                            sink_state_set.clone()
                        } else {
                            // strigify each tuple and pack all these strings in one HashSet
                            p.iter()
                                .map(|tuple| stringify_set(tuple))
                                .collect::<HashSet<_>>()
                        }
                    });
                } else {
                    let mut all_go_to_sink_state = true;
                    for symbol in &alphabet {
                        /*
                        HashSet at position (i) is states reachable from state[i] in automata[i] when reading (symbol)
                         */
                        let outputs = state
                            .iter()
                            .enumerate()
                            .map(|(idx, elem_state)| {
                                automata[idx]
                                    .move_set(&HashSet::from([elem_state.to_string()]), symbol)
                            })
                            .collect::<Vec<_>>();
                        let outputs = outputs.iter().collect::<Vec<_>>();
                        let outputs = &outputs[..];
                        state_map.insert(*symbol, {
                            let p = product(outputs);
                            if p.is_empty() {
                                used_sink_state = true;
                                sink_state_set.clone()
                            } else {
                                all_go_to_sink_state = false;
                                p.iter().map(|tuple| stringify_set(tuple)).collect::<_>()
                            }
                        });
                    }

                    if all_go_to_sink_state {
                        state_map.clear();
                        state_map.insert(AlphabetSymbol::Any, sink_state_set.clone());
                    }
                }
            }

            if used_sink_state {
                states.push((
                    vec![sink_state_name.to_string()],
                    sink_state_name.to_string(),
                ));
                alphabet.insert(AlphabetSymbol::Any);

                transition_function
                    .entry(sink_state_name)
                    .or_insert(HashMap::new())
                    .insert(AlphabetSymbol::Any, sink_state_set);
            }

            if used_symbol_any {
                alphabet.insert(AlphabetSymbol::Any);
            }
            transition_function
        };

        let start_state = {
            let mut start_state = Vec::new();
            for nfa in automata {
                start_state.push(nfa.start_state.to_string());
            }
            stringify_set(&start_state)
        };

        let accept_states = {
            let accept_states_array = automata
                .iter()
                .map(|nfa| &nfa.accept_states)
                .collect::<Vec<_>>();
            let accept_states_array = &accept_states_array[..];
            let mut accept_states = product(accept_states_array);
            if accept_states.len() == 1 && accept_states.last().as_ref().unwrap().is_empty() {
                accept_states.clear();
            }
            let accept_states = accept_states
                .iter()
                .map(|accepting_tuple| stringify_set(accepting_tuple))
                .collect::<HashSet<_>>();

            accept_states
        };

        let states = states.into_iter().map(|(_, name)| name).collect::<_>();

        let computation_style = ComputationStyle::Deterministic;
        let dfa = RefCell::new(None);

        NFA {
            states,
            alphabet,
            transition_function,
            start_state,
            accept_states,
            computation_style,
            dfa,
        }
    }

    pub fn add_missing_transitions(nfa: &mut Self) -> &mut Self {
        let sink_state_name = Self::new_random_state("SINK");
        let sink_state_set = HashSet::from([sink_state_name.to_string()]);

        let mut alphabet = nfa.alphabet.clone();
        alphabet.remove(&AlphabetSymbol::EmptyString);
        alphabet.remove(&AlphabetSymbol::Any);

        let mut used_sink_state = false;
        for state in &nfa.states {
            let state_map = nfa
                .transition_function
                .entry(state.to_string())
                .or_insert(HashMap::new());
            if state_map.is_empty() {
                // This states has no transitions.
                used_sink_state = true;
                state_map.insert(AlphabetSymbol::Any, sink_state_set.clone());
            } else if !state_map.contains_key(&AlphabetSymbol::Any) {
                for symbol in &alphabet {
                    state_map.entry(*symbol).or_insert({
                        used_sink_state = true;
                        sink_state_set.clone()
                    });
                }
            }
        }

        if used_sink_state {
            nfa.alphabet.insert(AlphabetSymbol::Any);
            nfa.states.insert(sink_state_name.to_string());

            nfa.transition_function
                .entry(sink_state_name)
                .or_insert(HashMap::new())
                .insert(AlphabetSymbol::Any, sink_state_set);
        }

        nfa
    }

    pub fn symmetric_difference(dfa1: &NFA, dfa2: &NFA) -> Result<NFA, String> {
        let mut error = String::new();
        if !dfa1.is_deterministic() {
            error.push_str("[NFA::symmetric_difference]: First automaton is not deterministic!\n");
        }
        if !dfa2.is_deterministic() {
            error.push_str("[NFA::symmetric_difference]: Second automaton is not deterministic!\n");
        }
        if !error.is_empty() {
            return Err(error);
        }

        let not_dfa1 = Self::compute_complement(dfa1);
        let not_dfa2 = Self::compute_complement(dfa1);

        let one_not_two = Self::intersection(&[dfa1, &not_dfa2]);
        let two_not_one = Self::intersection(&[&not_dfa1, dfa2]);

        Ok(NFA::union([one_not_two, two_not_one].iter(), "<U>"))
    }

    pub fn are_equivalent(dfa1: &NFA, dfa2: &NFA) -> Result<bool, String> {
        Ok(Self::has_empty_language(&Self::symmetric_difference(
            dfa1, dfa2,
        )?))
    }

    pub fn compute_minimized_dfa(dfa: &Self) -> Result<Self, String> {
        if !dfa.is_deterministic() {
            return Err("[NFA::minimize_dfa]: Argument DFA is not deterministic!".to_string());
        }
        let alphabet = {
            let mut alphabet = dfa.alphabet.clone();
            alphabet.remove(&AlphabetSymbol::Any);
            alphabet.remove(&AlphabetSymbol::EmptyString);
            alphabet
        };
        let mut groups = LinkedList::from([
            dfa.accept_states.clone(),
            dfa.states
                .difference(&dfa.accept_states)
                .map(ToString::to_string)
                .collect::<HashSet<_>>(),
        ]);
        loop {
            let before = groups.len();
            for _ in 0..before {
                let group = groups.pop_front().unwrap();
                let mut partitions =
                    Vec::<(HashSet<String>, HashMap<AlphabetSymbol, HashSet<String>>)>::new();
                for state in &group {
                    let state_map = alphabet
                        .iter()
                        .map(|symbol| {
                            (
                                *symbol,
                                dfa.move_set(&HashSet::from([state.to_string()]), symbol)
                                    .intersection(&group)
                                    .map(ToString::to_string)
                                    .collect::<HashSet<_>>(),
                            )
                        })
                        .collect::<HashMap<_, _>>();

                    let mut equivalence_class = None;
                    for (peers, map) in &mut partitions {
                        let mut mismatch = false;
                        for symbol in &alphabet {
                            if let Some(state_set) = state_map.get(symbol) {
                                if let Some(map_set) = map.get(symbol) {
                                    if !(state_set.is_subset(map_set)
                                        && map_set.is_subset(state_set))
                                    {
                                        mismatch = true;
                                        break;
                                    }
                                } else {
                                    mismatch = true;
                                    break;
                                }
                            } else {
                                mismatch = true;
                                break;
                            }
                        }
                        if !mismatch {
                            equivalence_class = Some(peers);
                            break;
                        }
                    }

                    match equivalence_class {
                        Some(set) => {
                            set.insert(state.to_string());
                        }
                        None => partitions.push((HashSet::from([state.to_string()]), state_map)),
                    }
                }

                if partitions.len() > 1 {
                    for (peers, _) in partitions {
                        groups.push_back(peers);
                    }
                } else {
                    groups.push_back(group);
                }
            }
            if before == groups.len() {
                break;
            }
        }

        let representatives = dfa
            .states
            .iter()
            .map(|state| {
                let mut iter = groups.iter();
                let mut class = iter.next().unwrap();
                while !class.contains(state) {
                    class = iter.next().unwrap();
                }
                let class_string = {
                    let mut set_string = String::from("{");
                    for elem in class {
                        set_string.push_str(&format!("{elem}, "));
                    }
                    set_string.pop();
                    set_string.pop();
                    set_string.push('}');
                    set_string
                };
                (state, (class, class_string))
            })
            .collect::<HashMap<_, _>>();

        let find_by_class = |class: &HashSet<String>| {
            let mut iter = representatives.iter();
            let mut entry = iter.next().unwrap();
            loop {
                let entry_class = entry.1 .0;
                if entry_class.is_subset(class) && class.is_subset(entry_class) {
                    break;
                } else {
                    entry = iter.next().unwrap();
                }
            }
            entry
        };

        let find_by_dfa_state = |state: &String| representatives.get(state).unwrap();

        let states = groups
            .iter()
            .map(|class| find_by_class(class).1 .1.to_string())
            .collect::<_>();
        let transition_function = groups
            .iter()
            .map(|class| {
                let class_entry = find_by_class(class);
                let name = class_entry.1 .1.to_string();
                let name_map = alphabet
                    .iter()
                    .map(|symbol| {
                        let symbol = *symbol;

                        let class_representative_dfa_state = class_entry.0.to_string();
                        let dfa_state = dfa
                            .move_set(
                                &HashSet::from([class_representative_dfa_state.to_string()]),
                                &symbol,
                            )
                            .into_iter()
                            .next()
                            .unwrap();
                        let set = find_by_dfa_state(&dfa_state).1.clone();
                        let set = HashSet::from([set]);
                        (symbol, set)
                    })
                    .collect::<HashMap<_, _>>();
                (name, name_map)
            })
            .collect::<_>();
        let start_state = find_by_dfa_state(&dfa.start_state).1.to_string();
        let accept_states = dfa
            .accept_states
            .iter()
            .map(|f| find_by_dfa_state(f).1.to_string())
            .collect::<_>();
        let computation_style = ComputationStyle::Deterministic;
        let dfa = RefCell::new(None);

        Ok(NFA {
            states,
            alphabet,
            transition_function,
            start_state,
            accept_states,
            computation_style,
            dfa,
        })
    }

    pub fn accepts_empty_string(&self) -> bool {
        self.accept_states
            .intersection(&self.epsilon_closure(&HashSet::from([self.start_state.to_string()])))
            .next()
            .is_some()
    }

    pub fn is_accepting_set(&self, set: &HashSet<String>) -> bool {
        self.accept_states.intersection(set).next().is_some()
    }

    pub fn computation_history(&self, input: &str) -> ComputationHistory {
        ComputationHistory::new(self, input)
    }

    pub fn enumerate_strings(&self) {
        let mut strings = LinkedList::from([String::from("")]);
        loop {
            let back = strings.pop_front().unwrap();
            if let Ok(result) = self.compute(&back, false) {
                if result == ComputationResult::Accept {
                    println!("{}", back)
                }
            };
            for item in &self.alphabet {
                if let AlphabetSymbol::Character(ch) = item {
                    strings.push_back(format!("{back}{ch}"));
                }
            }
        }
    }
}

#[derive(Debug)]
pub struct ComputationHistory<'a> {
    nfa: &'a NFA,
    input: Vec<char>,
    state: HashSet<String>,
    position: usize,
}

impl<'a> ComputationHistory<'a> {
    pub fn new(nfa: &'a NFA, input: &str) -> ComputationHistory<'a> {
        let input = input.chars().collect::<_>();
        let state = nfa.epsilon_closure(&HashSet::from([nfa.start_state.to_string()]));
        let position = 0;
        ComputationHistory {
            nfa,
            input,
            state,
            position,
        }
    }

    pub fn read_state(&self) -> HashSet<String> {
        self.state.clone()
    }
}

impl<'a> Iterator for ComputationHistory<'a> {
    type Item = HashSet<String>;
    fn next(&mut self) -> Option<Self::Item> {
        if self.position < self.input.len() {
            if self.state.is_empty() {
                self.state = self
                    .nfa
                    .epsilon_closure(&HashSet::from([self.nfa.start_state.to_string()]));
            }
            let extension = self.nfa.move_set(
                &self.state,
                &AlphabetSymbol::Character(self.input[self.position]),
            );
            self.state.clear();
            self.state.extend(extension.into_iter());
            self.position += 1;
            Some(self.read_state())
        } else {
            None
        }
    }
}
