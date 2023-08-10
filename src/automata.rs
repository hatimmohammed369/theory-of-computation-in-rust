/*
Module nfa:
Contains struct NFA (representing both Deterministic and Nondeterministic Finite Automata)
The distinction between the NFA and the DFA is by the flag is_deterministic defined in struct NFA
Each automaton must define some function to perform its computation based on some input
 */
pub mod nfa;

/*
Each computation function in all automata must return this type
Accept: the automaton accepted its input
Reject: the automaton rejected its input
*/
#[derive(Debug, PartialEq)]
pub enum ComputationResult {
    Accept,
    Reject,
}
