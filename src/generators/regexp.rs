#![allow(dead_code)]
#![allow(unused_variables)]
#![allow(unused_mut)]
#![allow(unused_imports)]

pub mod compiler;

use crate::automata::nfa::{AlphabetSymbol, ComputationHistory, NFA};
use crate::automata::ComputationResult;
use std::collections::HashSet;

// Regular Expressions module.

#[derive(Debug)]
pub enum ExpressionBase {
    EmptySet,
    EmptyString,
    Symbol(Rc<AlphabetSymbol>), // AlphabetSymbol CAN NOT be the EmptyString variant.
    Grouping(Rc<ExpressionBase>),
    Star(Rc<ExpressionBase>),
    Union(Vec<Rc<ExpressionBase>>),
    Concat(Vec<Rc<ExpressionBase>>),
    CharacterClass {
        inverted: bool,
        ranges: HashSet<(u8, u8)>,
    },
}

impl Display for ExpressionBase {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                ExpressionBase::EmptySet => "{}".to_string(),
                ExpressionBase::EmptyString => "".to_string(),
                ExpressionBase::Symbol(value) => format!("{}", value.as_ref()),
                ExpressionBase::Grouping(grouped_expr) => format!("({})", grouped_expr.as_ref()),
                ExpressionBase::Star(starred_expr) => {
                    match starred_expr.as_ref() {
                        ExpressionBase::Symbol(value) => format!("{value}*"),
                        _ => format!("({starred_expr})*"),
                    }
                }
                ExpressionBase::Union(items) => {
                    let mut exprs = items.iter().map(|e| format!("{e}"));
                    let first = exprs.next().unwrap();
                    exprs.fold(first, |current, next| format!("{current}|{next}"))
                }
                ExpressionBase::Concat(items) => {
                    let mut exprs = items.iter().map(|e| {
                        // Parenthesize union expressions to perserve their (lower) precedence
                        match e.as_ref() {
                            ExpressionBase::Union(_) => format!("({e})"),
                            _ => format!("{e}"),
                        }
                    });
                    let first = exprs.next().unwrap();
                    exprs.fold(first, |current, next| format!("{current}{next}"))
                }
                ExpressionBase::CharacterClass { inverted, ranges } => {
                    let ranges = ranges
                        .iter()
                        .map(|(low, high)| {
                            if low < high {
                                format!("{}-{}", *low as char, *high as char)
                            } else {
                                format!("{}", *low as char)
                            }
                        })
                        .fold(String::new(), |cur, next| format!("{cur}{next}"));
                    let inverted = String::from(if *inverted { "^" } else { "" });
                    format!("[{inverted}{ranges}]")
                }
            }
        )
    }
}

// Compute the kleene star of a regular expression
pub fn star_string_regex(expr: &Rc<ExpressionBase>) -> Rc<ExpressionBase> {
    Rc::new(match expr.as_ref() {
        ExpressionBase::EmptySet => ExpressionBase::EmptyString,
        _ => ExpressionBase::Star(Rc::clone(expr)),
    })
}

// Compute the union of a group of regular expressions
pub fn union_string_regexes(exprs: &[Rc<ExpressionBase>]) -> Rc<ExpressionBase> {
    let mut exprs_vec = Vec::<Rc<ExpressionBase>>::new();

    let mut all_phi = true;
    let mut all_empty = true;
    let mut empty_string = None;
    for expr_rc in exprs {
        let expr = expr_rc.as_ref();

        if all_phi && !matches!(expr, ExpressionBase::EmptySet) {
            // Some thing is not phi.
            all_phi = false;
        }
        if all_empty {
            match expr {
                ExpressionBase::EmptyString => {}
                ExpressionBase::Symbol(value) => {
                    // Handle symbols
                    match value.as_ref() {
                        AlphabetSymbol::EmptyString => {}
                        _ => all_empty = false,
                    }
                }
                _ => all_empty = false,
            }
        }

        match expr {
            ExpressionBase::EmptySet => {} // Unioning the empty set has no effect.
            ExpressionBase::Symbol(value) => {
                // Extract AlphabetSymbol if it is not AlphabetSymbol::EmptyString or use ExpressionBase::EmptyString
                match value.as_ref() {
                    AlphabetSymbol::Any => {
                        return Rc::clone(expr_rc);
                    }
                    AlphabetSymbol::EmptyString => {
                        empty_string = Some(Rc::new(ExpressionBase::EmptyString));
                    }
                    AlphabetSymbol::Character(_) => {
                        exprs_vec.push(Rc::clone(expr_rc));
                    }
                }
            }
            _ => {
                if !matches!(expr, ExpressionBase::EmptyString) {
                    /*
                    When the vector is non-empty, check for the last inserted expression
                    if the last is empty string and current one is also, do not insert current
                    */
                    exprs_vec.push(Rc::clone(expr_rc));
                } else {
                    empty_string = Some(Rc::clone(expr_rc));
                }
            }
        }
    }

    if let Some(e) = empty_string {
        exprs_vec.push(e);
    }

    if exprs_vec.len() == 1 {
        return exprs_vec.pop().unwrap();
    }

    Rc::new({
        if all_phi {
            ExpressionBase::EmptySet
        } else if all_empty {
            ExpressionBase::EmptyString
        } else if exprs_vec.is_empty() {
            // Union of nothing
            ExpressionBase::EmptySet
        } else {
            ExpressionBase::Union(exprs_vec)
        }
    })
}

// Compute the concatenation of a group of regular expressions
pub fn concat_string_regexes(exprs: &[Rc<ExpressionBase>]) -> Rc<ExpressionBase> {
    let mut exprs_vec = Vec::<Rc<ExpressionBase>>::new();
    let mut all_empty = true;

    for expr_rc in exprs {
        let expr = expr_rc.as_ref();

        if all_empty && !matches!(expr, ExpressionBase::EmptyString) {
            // Some thing is not the empty string.
            all_empty = false;
        }

        match expr {
            ExpressionBase::EmptySet => {
                // Concatenting the empty set yields the empty set only.
                return Rc::clone(expr_rc);
            }
            ExpressionBase::EmptyString => {} // Concatentating the empty string has no effect.
            ExpressionBase::Symbol(value) => {
                // Extract AlphabetSymbol if it is not AlphabetSymbol::EmptyString, or use ExpressionBase::EmptyString
                match value.as_ref() {
                    AlphabetSymbol::EmptyString => {} // Concatenating the empty string (symbol) has no effect.
                    _ => exprs_vec.push(Rc::clone(expr_rc)),
                }
            }
            _ => exprs_vec.push(Rc::clone(expr_rc)),
        }
    }

    if exprs_vec.len() == 1 {
        return exprs_vec.pop().unwrap();
    }

    Rc::new({
        if all_empty {
            ExpressionBase::EmptyString
        } else if exprs_vec.is_empty() {
            ExpressionBase::EmptySet
        } else {
            ExpressionBase::Concat(exprs_vec)
        }
    })
}

use crate::generators::regexp::compiler::parser::Parser;
use crate::generators::regexp::compiler::scanner::Scanner;

#[derive(Debug)]
pub struct Regexp<'a> {
    pattern: &'a str,
    compiled_pattern: NFA,
}

use crate::generators::regexp::compiler::parser::Expression;
use std::collections::LinkedList;
use std::fmt::Display;
use std::rc::Rc;

fn debug_expression(root: &Rc<Expression>) {
    let mut expressions = LinkedList::from([Rc::clone(root)]);
    let mut pending_counters = LinkedList::from([1usize]);
    let mut level = 0u32;
    while !pending_counters.is_empty() {
        println!("++++++++++++++++++++++++++++++++++++++++");
        println!("Level {level}");
        let counter = pending_counters.pop_front().unwrap();
        let mut new_children = 0usize;
        for _ in 1..=counter {
            let left_most_expr = expressions.pop_front().unwrap();
            let left_most_expr = left_most_expr.as_ref();
            println!("Expression : {}", String::from(left_most_expr));
            println!("Full form : {left_most_expr}");
            expressions.extend({
                let children = left_most_expr.children.borrow();
                let children: &Vec<Rc<Expression>> = children.as_ref();
                new_children += children.len();
                let children = children
                    .iter()
                    .map(Rc::clone)
                    .collect::<Vec<Rc<Expression>>>();
                children.into_iter()
            });
        }
        if new_children > 0 {
            pending_counters.push_back(new_children);
        }
        level += 1;
        println!("++++++++++++++++++++++++++++++++++++++++");
    }
}

impl<'a> Regexp<'a> {
    pub fn new(pattern: &'a str, debug: bool) -> Result<Regexp, String> {
        let scanner = Scanner::new(pattern);
        let parser = Parser::new(scanner);
        let parsed_expression = parser.parse();
        match parsed_expression {
            Ok(expr) => {
                if debug {
                    debug_expression(&expr);
                }

                let compiled_pattern = expr.compile(&parser.scanner.borrow().alphabet);
                Ok(Regexp {
                    pattern,
                    compiled_pattern,
                })
            }
            Err(error) => Err(error),
        }
    }

    pub fn read_pattern(&self) -> &str {
        self.pattern
    }

    pub fn read_automaton(&self) -> &NFA {
        &self.compiled_pattern
    }

    pub fn fullmatch(&self, input: &str) -> bool {
        match self.compiled_pattern.compute(input, false) {
            Ok(result) => result == ComputationResult::Accept,
            Err(_) => false,
        }
    }

    pub fn find_iter<'c>(&'c self, input: &'c str, begin: usize, end: usize) -> MatchIterator {
        let target = &input[begin..end];
        let target_vec = input.chars().collect::<_>();
        let position = 0;
        let matched_empty_string = false;
        let regexp = &self;
        let computation_state = self.compiled_pattern.computation_history(input);
        MatchIterator {
            target,
            target_vec,
            position,
            matched_empty_string,
            regexp,
            computation_state,
        }
    }
}

pub struct Match<'a> {
    slice: &'a str,
    begin: usize,
    end: usize,
}

impl Display for Match<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "Match(slice=\"{}\", begin={}, end={})",
            self.slice, self.begin, self.end
        )
    }
}

#[derive(Debug)]
pub struct MatchIterator<'a, 'b> {
    target: &'a str,
    target_vec: Vec<char>,
    position: usize,
    matched_empty_string: bool,
    regexp: &'a Regexp<'b>,
    computation_state: ComputationHistory<'a>,
}

impl<'a, 'b> Iterator for MatchIterator<'a, 'b> {
    type Item = Match<'a>;
    fn next(&mut self) -> Option<Self::Item> {
        // Find the smallest next match
        if self.position <= self.target.len() {
            let nfa = &self.regexp.compiled_pattern;
            let empty_string_match = {
                if nfa.accepts_empty_string() {
                    Some(Match {
                        slice: "",
                        begin: self.position,
                        end: self.position,
                    })
                } else {
                    None
                }
            };
            let begin = self.position;
            let mut end = None;
            let non_empty_string_match = {
                for set in &self.computation_state.next() {
                    if nfa.is_accepting_set(set) {
                        end = Some(self.position);
                        break;
                    } else {
                        self.position += 1;
                    }
                }
                match end {
                    Some(end_pos) => {
                        self.position += 1;
                        let end_pos = end_pos + 1;
                        let slice = &self.target[begin..end_pos];
                        let end = end_pos;
                        Some(Match { slice, begin, end })
                    }
                    None => {
                        self.position = begin + 1;
                        None
                    }
                }
            };
            if non_empty_string_match.is_none() {
                self.matched_empty_string = true;
                empty_string_match
            } else {
                self.matched_empty_string = false;
                non_empty_string_match
            }
        } else {
            None
        }
    }
}
