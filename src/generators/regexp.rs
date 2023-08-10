mod compiler;

use crate::automata::nfa::NFA;
use crate::automata::ComputationResult;

// Regular Expressions module.

/*
Add parenthesis to (expr) when needed, that's when:
1 - (expr) is in form A|B|C|...
2 - in (expr) first occurence of ( does not matche last )
 */
pub fn parenthesize(expr: &mut String) -> &mut String {
    if expr.len() == 1
        && (expr == "|"
            || expr == "\\"
            || expr == "("
            || expr == ")"
            || expr == "*"
            || expr == "["
            || expr == "]"
            || expr == ".")
    {
        /*
        Treat those characters specially such that return value
        can be use as input to (regexp) module.
         */
        expr.insert(0, '\\');
        return expr;
    }

    let mut stars = 0u32;
    while expr.ends_with('*') {
        expr.pop();
        stars += 1;
    }

    let mut nesting = 0u128;
    for (idx, ch) in expr.char_indices() {
        if ch == '(' {
            nesting += 1
        } else if ch == ')' {
            nesting -= 1
        }

        if nesting == 0 && (idx != expr.len() - 1 || ch == '|') {
            if stars > 0 {
                for _ in 1..=stars {
                    expr.push('*')
                }
                stars = 0;
            }
            expr.insert(0, '(');
            expr.push(')');
            break;
        }
    }
    if stars > 0 {
        for _ in 1..=stars {
            expr.push('*')
        }
    }
    expr
}

// Option::None REPRESENTS THE PHI REGULAR EXPRESSION, THE EMPTY LANGUAGE

// Compute the kleene star of a regular expression
pub fn star_string_regex(expr: &Option<&str>) -> String {
    if expr.is_none() {
        /*
        Starring the empty language yields the empty string
        thus return an empty string.
        */
        return String::new();
    }

    let mut expr = expr.unwrap().to_string();
    parenthesize(&mut expr);
    expr
}

// Compute the union of a group of regular expressions
pub fn union_string_regexes(exprs: &[Option<&str>]) -> Option<String> {
    let mut union_expr = String::new();

    /*
    All expressions are Option::None, equivalently, all are the phi regular expression
    and thus we are computing the union of empty sets which just yields the empty set.
    */
    let mut all_none = true;

    /*
    All expressions are Option::Some(""), equivalently, all are the empty string
    and thus we are computing the union of empty string which just yields the empty string.
    */
    let mut all_empty = true;

    for expr in exprs {
        if expr.is_none() {
            /*
            Union with Option::None (aka phi regular expression) is like adding 0
            Its has no effect, so just continue to the next iteration.
            */
            continue;
        }

        // Now we know at least one expression is not phi.
        all_none = false;

        let mut expr = expr.as_ref().unwrap().to_string();
        if !expr.is_empty() {
            // Now we know at one expression is not the empty string.
            all_empty = false;
        }

        if expr.len() >= 2 && &expr[expr.len() - 2..] == "||" && expr.is_empty() {
            /*
            The last expression we unioned was the empty string, and current one is also the empty string
            do not append another union operator `|` so that the output is little pretty
            Otherwise if we could end up with return value `a|||||||`
            which is not really nice to look at!.
            */
            continue;
        }

        parenthesize(&mut expr);
        expr.push('|');

        union_expr.push_str(&expr);
    }

    if all_none {
        // All expressions are phi, just return phi (None)
        return None;
    } else if all_empty {
        // All expressions are empty, just return the empty string
        return Some(String::new());
    } else if !union_expr.is_empty() {
        /*
        In each iteration of the above loop, we append the expression followed by `|`
        thus the last expression will append a redundant `|` which must be removed
        otherwise this redundant `|` will imply this expression unions the empty string
         */
        union_expr.pop();
    }

    Some(union_expr)
}

// Compute the concatenation of a group of regular expressions
pub fn concat_string_regexes(exprs: &[Option<&str>]) -> Option<String> {
    /*
    All expressions are Option::Some(""), equivalently, all are the empty string
    and thus we are computing the union of empty string which just yields the empty string.
    */
    let mut all_empty = true;

    let mut concat_expr = String::new();

    for expr in exprs {
        if expr.is_none() {
            /*
            If one expression is phi (the empty language)
            the the whole concatination of the input is the empty language
            because concatination the empty language yields the empty langauge
            Like multiply by zero.
            */
            return None;
        }

        let mut expr = expr.as_ref().unwrap().to_string();
        if expr.is_empty() {
            /*
            Contination with the empty string has no effect
            Like multiplying by one or adding zero
            */
            continue;
        }
        // Now we know at one expression is not the empty string.
        all_empty = false;

        parenthesize(&mut expr);
        concat_expr.push_str(&expr);
    }

    if all_empty {
        /*
        Concatinating a group of empty strings,
        the final expression is just the empty string
        */
        return Some(String::new());
    }

    Some(concat_expr)
}

use crate::generators::regexp::compiler::{Parser, Scanner};

pub struct Regexp<'a> {
    pattern: &'a str,
    matcher: NFA,
}

use crate::generators::regexp::compiler::Expression;
use std::collections::LinkedList;
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
                    .map(|rc| Rc::clone(rc))
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
    pub fn new(pattern: &'a str) -> Result<Regexp, String> {
        let scanner = Scanner::new(pattern);
        let parser = Parser::new(scanner);
        let parsed_expression = parser.parse();
        match parsed_expression {
            Ok(expr) => {
                debug_expression(&expr);

                let matcher = expr.compile(&parser.scanner.borrow().alphabet);
                Ok(Regexp { pattern, matcher })
            }
            Err(error) => Err(error),
        }
    }

    pub fn read_pattern(&self) -> &str {
        self.pattern
    }

    pub fn fullmatch(&self, input: &str) -> bool {
        match self.matcher.compute(input, false) {
            Ok(result) => result == ComputationResult::Accept,
            Err(_) => false,
        }
    }
}
