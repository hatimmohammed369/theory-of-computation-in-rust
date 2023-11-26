#![allow(dead_code)]
#![allow(unused_variables)]
#![allow(unused_mut)]
#![allow(unused_imports)]

// Context-Free Grammar

use std::collections::{HashMap, HashSet, LinkedList};

pub type RuleString = Vec<String>;
pub type RuleList<'a> = &'a [&'a str];

#[derive(Debug)]
pub struct CFG {
    variables: HashSet<String>,
    terminals: HashSet<String>,
    rules: HashMap<String, Vec<RuleString>>,
    start_variable: String,
}

impl CFG {
    pub fn new(
        variables: &[&str],
        terminals: &[&str],
        rules: &[(&str, &[RuleList])],
        start_variable: &str,
    ) -> CFG {
        let variables = variables
            .iter()
            .map(ToString::to_string)
            .collect::<HashSet<_>>();
        let terminals = terminals.iter().map(ToString::to_string).collect::<_>();
        let i = variables.intersection(&terminals).collect::<HashSet<_>>();
        if !i.is_empty() {
            eprintln!("`{i:?}` are both variables and terminals");
            std::panic::set_hook(Box::new(|_| {}));
            panic!();
        }
        let rules = rules
            .iter()
            .map(|(var, rule)| {
                (
                    var.to_string(),
                    rule.iter()
                        .map(|r| r.iter().map(ToString::to_string).collect::<Vec<_>>())
                        .collect::<Vec<_>>(),
                )
            })
            .collect::<_>();
        let start_variable = start_variable.to_string();
        CFG {
            variables,
            terminals,
            rules,
            start_variable,
        }
    }

    pub fn rules_view(&self) -> &HashMap<String, Vec<RuleString>> {
        &self.rules
    }

    pub fn rules_mut_ref(&mut self) -> &mut HashMap<String, Vec<RuleString>> {
        &mut self.rules
    }

    /*
    Perform a right-most derivation from the start variable by replacing
    variables in parameter (replacement_rules).
     */
    pub fn right_derive(&self, replacement_rules: &[(&str, RuleList)]) -> String {
        let mut generated_string = LinkedList::new();
        generated_string.extend(
            self.rules.get(&self.start_variable).unwrap()[0]
                .iter()
                .map(ToString::to_string),
        );
        let mut temp_store = LinkedList::new();
        for var in replacement_rules {
            let (var, var_replacement) = (var.0.to_string(), var.1);
            while generated_string.front().unwrap() != &var {
                temp_store.push_back(generated_string.pop_front().unwrap());
            }
            if !generated_string.is_empty() && generated_string.front().unwrap() == &var {
                generated_string.pop_front();
                var_replacement.iter().rev().for_each(|item| {
                    generated_string.push_front(item.to_string());
                });
            }
            while !temp_store.is_empty() {
                generated_string.push_front(temp_store.pop_back().unwrap());
            }
        }
        generated_string
            .into_iter()
            .fold(String::new(), |a, b| format!("{a}{b}"))
    }

    /*
    Perform a left-most derivation from the start variable by replacing
    variables in parameter (replacement_rules).
     */
    pub fn left_derive(&self, replacement_rules: &[(&str, RuleList)]) -> String {
        let mut generated_string = LinkedList::new();
        generated_string.extend(
            self.rules.get(&self.start_variable).unwrap()[0]
                .iter()
                .map(ToString::to_string),
        );
        let mut temp_store = LinkedList::new();
        for var in replacement_rules {
            println!("{:?}", var);
            println!("{:?}\n", generated_string);
            let (var, var_replacement) = (var.0.to_string(), var.1);
            while generated_string.back().unwrap() != &var {
                temp_store.push_front(generated_string.pop_back().unwrap());
            }
            if !generated_string.is_empty() && generated_string.back().unwrap() == &var {
                generated_string.pop_back();
                var_replacement.iter().for_each(|item| {
                    generated_string.push_back(item.to_string());
                });
            }
            while !temp_store.is_empty() {
                generated_string.push_back(temp_store.pop_front().unwrap());
            }
        }
        generated_string
            .into_iter()
            .fold(String::new(), |a, b| format!("{a}{b}"))
    }
}
