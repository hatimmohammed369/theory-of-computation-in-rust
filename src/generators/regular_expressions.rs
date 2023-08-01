use crate::automata::nfa::NFA;

pub fn star_string_regex(automaton: &NFA, expr: &Option<&str>) -> String {
    let mut expr = expr.unwrap_or_default().to_string();
    if expr.is_empty() {
	return String::new();
    }

    if automaton.read_alphabet().contains(&expr) {
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

pub fn union_string_regexes(exprs: &[Option<&str>]) -> Option<String> {
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

pub fn concat_string_regexes(exprs: &[Option<&str>]) -> Option<String> {
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

