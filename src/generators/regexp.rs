mod compiler;

use crate::automata::ComputationResult;
use crate::automata::nfa::NFA;

// Regular Expressions module.

// Option::None REPRESENTS THE PHI REGULAR EXPRESSION, THE EMPTY LANGUAGE

// Compute the kleene star of a regular expression
pub fn star_string_regex(automaton: &NFA, expr: &Option<&str>) -> String {
    if expr.is_none() {
	/*
	Starring the empty language yields the empty string
	thus return an empty string.
	*/
	return String::new();
    }

    let mut expr = expr.unwrap().to_string();
    if expr.is_empty() {
	// Starring the empty string yields the empty string.
	return String::new();
    }

    if automaton.read_alphabet().contains(&expr.chars().next().unwrap()) {
	/*
	If this regular expression is just an alphabet symbol
	just append the `*`.
	*/
	expr.push('*');
    } else if expr.starts_with('(') && expr.ends_with(')') {
	/*
	If the regular expressions starts and ends with `(` and `)`
	then we must check if the `(` matches the `)`
	if so we just append the star operator `*`

	Otherwise we must surround the orginal regular expression
	with parentheses to perserve the precedence of its parts
	and then append the star operator `*`
	 */

	let mut nesting = 0;
	for (idx, ch) in expr.char_indices() {
	    if ch == '(' {nesting += 1;}
	    else if ch == ')' {nesting -= 1;}

	    if nesting == 0 && idx != expr.len()-1 {
		/*
		We reached the most outer level before the end of the expression
		thus first `(` is closed before the end and hence it does not
		match last `)` at the end of the expression
		Hence we must surround the orginal expression with parentheses
		*/
		nesting = -1;
		break;
	    }
	}

	if nesting == -1 {
	    /*
	    First `(` does NOT match last `)`
	    Surround the orginal expression with parentheses to
	    perserve the precedence of its parts
	    */
	    expr = format!("({expr})*");
	} else {
	    /*
	    First `(` DOES match last `)`
	    Just append the star operator `*`
	    */
	    expr.push('*');
	}
    } else {
	/*
	This expression is neither an alphabet symbol
	nor its starts and ends with parentheses
	It must be parenthesised to perserve the precedence of its inner parts
	then appending the star operator `*`
	*/
	expr = format!("({expr})*");
    }

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

	let mut nesting = 0;
	for ch in expr.chars() {
	    if ch == '(' {nesting += 1;}
	    else if ch == ')' {nesting -= 1;}

	    if nesting == 0 && ch == '|' {
		/*
		We reached the outer most level in this particular expression received from the input
		and this expression is union expression (A|B|C|...)
		thus we must parenthesize it to perserve its precendence
		*/
		expr = format!("({expr})");
		break;
	    }
	}

	// Append the union operator `|`
	expr = format!("{expr}|");

	union_expr.push_str(&expr);
    }

    if all_none {
	// All expressions are phi, just return phi (None)
	return None;
    } else if all_empty {
	// All expressions are empty, just return the empty string
	return Some(String::new());
    }else if !union_expr.is_empty() {
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

	let mut nesting = 0;
	for ch in expr.chars() {
	    if ch == '(' {nesting += 1;}
	    else if ch == ')' {nesting -= 1;}

	    if nesting == 0 && ch == '|' {
		/*
		We reached the outer most level in this particular expression received from the input
		and this expression is union expression (A|B|C|...)
		thus we must parenthesize it to perserve its precendence
		*/
		expr = format!("({expr})");
		break;
	    }
	}

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

impl<'a> Regexp<'a> {
    pub fn new(pattern: &'a str) -> Result<Regexp, String> {
	Err(String::new())
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
