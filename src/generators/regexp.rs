mod compiler;

use crate::automata::ComputationResult;
use crate::automata::nfa::NFA;

// Regular Expressions module.

/*
Add parenthesis to (expr) when needed, that's when:
1 - (expr) is in form A|B|C|...
2 - in (expr) first occurence of ( does not matche last )
 */
pub fn parenthesize(expr: &mut String) -> &mut String {
    if expr.len() == 1 &&
	(
	    expr == "|" ||
		expr == "\\" ||
		expr == "(" ||
		expr == ")" ||
		expr == "*" ||
		expr == "[" ||
		expr == "]" ||
		expr == "."
	)
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
	if ch == '(' {nesting += 1}
	else if ch == ')' {nesting -= 1}

	if nesting == 0 && (idx != expr.len() - 1 || ch == '|') {
	    if stars > 0 {
		for _ in 1..=stars {expr.push('*')}
		stars = 0;
	    }
	    expr.insert(0, '(');
	    expr.push(')');
	    break;
	}
    }
    if stars > 0 {for _ in 1..=stars {expr.push('*')}}
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

	if
	    expr.len() >= 2 &&
	    &expr[expr.len()-2..] == "||" &&
	    expr.is_empty()
	{
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

use crate::generators::regexp::compiler::{Scanner, Parser};

pub struct Regexp<'a> {
    pattern: &'a str,
    matcher: NFA
}

use std::rc::Rc;
use crate::generators::regexp::compiler::Expression;

impl<'a> Regexp<'a> {
    pub fn new(pattern: &'a str) -> Result<Regexp, String> {
	let scanner = Scanner::new(pattern);
	let parser = Parser::new(scanner);
	let parsed_expression = parser.parse();
	match parsed_expression {
	    Ok(expr) => {
		let mut stack = vec![Rc::clone(&expr)];
		let mut level = 0;
		while !stack.is_empty() {
		    let top = stack.pop().unwrap();
		    let top = top.as_ref();
		    println!("Level {level}");
		    println!("Expression: {}", String::from(top));
		    println!("Full      : {top}");
		    level += 1;
		    println!("    Children: ");
		    let children = &top.children;
		    let children = children.borrow();
		    let children: &Vec<Rc<Expression>> = children.as_ref();
		    children
			.iter()
			.for_each(|child| {
			    stack.push(Rc::clone(child));
			    let child = child.as_ref();
			    print!("    ");
			    print!("    ");
			    println!("Child: {}", String::from(child));
			    print!("    ");
			    print!("    ");
			    println!("Full : {child}");
			});
		    println!();
		    println!("########################################");
		}

		let matcher =
		    expr.compile(&parser.scanner.borrow().alphabet);
		Ok(Regexp {pattern, matcher})
	    }
	    Err(error) => Err(error)
	}
    }

    pub fn read_pattern(&self) -> &str {
	self.pattern
    }

    pub fn fullmatch(&self, input: &str) -> bool {
	match self.matcher.compute(input, false) {
	    Ok(result) => result == ComputationResult::Accept,
	    Err(_) => false
	}
    }
}
