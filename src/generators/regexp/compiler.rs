use std::collections::HashSet;

#[derive(Debug, PartialEq, Clone)]
pub enum TokenType {
    EmptyString, Symbol,
    RightParen, LeftParen,
    Star, Pipe
}

#[derive(Debug, Clone)]
pub struct Token {
    pub name: TokenType,
    pub lexeme: char,
    pub position: usize
}

pub struct Scanner {
    chars: Vec<char>,
    pub alphabet: HashSet<char>,
    empty_string_found: bool,
    current: usize,
}

impl Scanner {
    pub fn new(source: &str) -> Scanner {
	Scanner {
	    chars: source.chars().collect::<_>(),
	    alphabet: HashSet::new(),
	    empty_string_found: false,
	    current: 0
	}
    }
}

impl Iterator for Scanner {
    type Item = Token;

    fn next(&mut self) -> Option<Token> {
	let mut next = None;

	let peek = {
	    if self.current >= self.chars.len() {
		'\0'
	    } else {
		self.chars[self.current]
	    }
	};

	let prev = {
	    if self.current == 0 {
		'\0'
	    } else {
		self.chars[self.current-1]
	    }
	};

	if !self.empty_string_found {
	    if (prev == '\0' && (peek == '\0' || peek == '|')) ||
		(prev == '(' && (peek == '|' || peek == ')')) ||
		(prev=='|' && (peek=='|' || peek==')' || peek=='\0')) {
		    self.alphabet.insert('\0');
		    next = Some(
			Token {
			    name: TokenType::EmptyString,
			    lexeme: '\0',
			    position: self.current
			}
		    );
		}
	    self.empty_string_found = true;
	}

	if next.is_none() && self.current < self.chars.len() {
	    self.empty_string_found = false;

	    match peek {
		'(' => {
		    next = Some(
			Token {
			    name: TokenType::LeftParen,
			    lexeme: '(',
			    position: self.current
			}
		    );
		}
		')' => {
		    next = Some(
			Token {
			    name: TokenType::RightParen,
			    lexeme: ')',
			    position: self.current
			}
		    );
		}
		'*' => {
		    next = Some(
			Token {
			    name: TokenType::Star,
			    lexeme: '*',
			    position: self.current
			}
		    );
		}
		'|' => {
		    next = Some(
			Token {
			    name: TokenType::Pipe,
			    lexeme: '|',
			    position: self.current
			}
		    );
		}
		c => {
		    self.alphabet.insert(c);
		    next = Some(
			Token {
			    name: TokenType::Symbol,
			    lexeme: c,
			    position: self.current
			}
		    );
		}
	    }

	    self.current += 1;
	}

	next
    }
}

pub enum ExpressionBase {
    EmptyString,
    Symbol {value: char},
    Grouping {inner_expr: Box<ExpressionBase>},
    Star {inner_expr: Box<ExpressionBase>},
    Union {items: Vec<ExpressionBase>},
    Concat {items: Vec<ExpressionBase>},
}

impl From<&ExpressionBase> for String {
    fn from(value: &ExpressionBase) -> Self {
	match value {
	    ExpressionBase::EmptyString { .. } => {
		String::from("{\"\"}")
	    },
	    ExpressionBase::Symbol { value, .. } => {
		String::from(*value)
	    },
	    ExpressionBase::Grouping { inner_expr, .. } => {
		String::from(inner_expr.as_ref())
	    },
	    ExpressionBase::Star { inner_expr, .. } => {
		format!("({})*", String::from(inner_expr.as_ref()))
	    },
	    ExpressionBase::Union { items, .. } => {
		items
		    .iter()
		    .map(String::from)
		    .collect::<Vec<String>>()
		    .join("|")
	    },
	    ExpressionBase::Concat { items, .. } => {
		items
		    .iter()
		    .fold(String::new(), |a, b| {
			format!("{}{}", a, String::from(b))
		    })
	    }
	}
    }
}

impl std::fmt::Display for &ExpressionBase {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", String::from(*self))
    }
}

use std::collections::HashMap;
use std::cell::RefCell;
use std::rc::Rc;
use crate::automata::nfa::NFA;

static mut COUNTER: usize = 0usize;

impl ExpressionBase {
    fn formatted(&self) -> String {
	format!("{}", self)
    }

    pub fn compile(&self, alphabet: &HashSet<char>) -> NFA {
	let id = format!("{:?}", self as *const ExpressionBase);
	let self_string = self.formatted();

	let start_state = unsafe {
	    let temp = COUNTER;
	    COUNTER += 1;
	    format!("(#{temp}<{self_string}>@{id}).<START>")
	};
	let mut states = HashSet::new();
	states.insert(String::from(&start_state));

	let mut transition_function =
	    HashMap::<String, HashMap<char, HashSet<String>>>::new();
	let mut accept_states = HashSet::new();

	match self {
	    Self::EmptyString { .. } => {
		accept_states.insert(String::from(&start_state));
	    },
	    ExpressionBase::Symbol { value, .. } => {
		let accept_state = unsafe {
		    let temp = COUNTER;
		    COUNTER += 1;
		    format!("(#{temp}<{self_string}>@{id}).<ACCEPT>")
		};
		states.insert(String::from(&accept_state));

		transition_function
		    .entry(String::from(&start_state))
		    .or_insert(HashMap::new())
		    .entry(*value)
		    .or_insert(HashSet::new())
		    .insert(String::from(&accept_state));

		accept_states.insert(String::from(&accept_state));
	    },
	    Self::Grouping { inner_expr, .. } => {
		return inner_expr.compile(alphabet);
	    },
	    ExpressionBase::Star { inner_expr, .. } => {
		let expr_nfa = inner_expr.compile(alphabet);
		return NFA::kleene_star(&expr_nfa, &start_state);
	    },
	    Self::Union { items , .. } => {
		let automata =
		    items
		    .iter()
		    .map(|expr| expr.compile(alphabet))
		    .collect::<Vec<NFA>>();
		return NFA::union(automata.iter(), &start_state);
	    },
	    Self::Concat { items , .. } => {
		let automata =
		    items
		    .iter()
		    .map(|expr| expr.compile(alphabet))
		    .collect::<Vec<NFA>>();

		let automata =
		    automata
		    .iter()
		    .collect::<Vec<&NFA>>();

		return NFA::concatenate(&automata[..]);
	    }
	}

	let alphabet = alphabet.clone();
	let is_deterministic = false;
	let dfa = RefCell::new(Rc::new(None));

	NFA::raw_new(
	    states,
	    alphabet,
	    transition_function,
	    start_state,
	    accept_states,
	    is_deterministic,
	    dfa
	)
    }
}

pub struct Parser {
    pub scanner: RefCell<Scanner>,
    previous: RefCell<Option<Token>>,
    current: RefCell<Option<Token>>
}

impl Parser {
    pub fn new(scanner: Scanner) -> Parser {
	let scanner = RefCell::new(scanner);
	let previous = RefCell::new(None);
	let current = RefCell::new(None);
	Parser {
	    scanner,
	    previous,
	    current
	}
    }

    fn read_current(&self) -> Option<Token> {
	self.current.borrow().clone()
    }

    fn read_previous(&self) -> Option<Token> {
	self.previous.borrow().clone()
    }

    fn advance(&self) -> Option<Token> {
	*self.previous.borrow_mut() = self.current.take();
	*self.current.borrow_mut() = self.scanner.borrow_mut().next();
	self.read_previous()
    }

    fn consume(
	&self,
	kind: TokenType,
	message: &str
    ) -> Result<Option<Token>, String> {
	if self.check(kind) {
	    Ok(self.advance())
	} else {
	    Err(String::from(message))
	}
    }

    fn match_current(&self, expected: TokenType) -> bool {
	if self.check(expected) {
	    self.advance();
	    true
	} else {
	    false
	}
    }

    fn check(&self, expected: TokenType) -> bool {
	match self.read_current() {
	    Some(tok) => tok.name == expected,
	    None => false
	}
    }
    /*
    Expression => Union
    Union => Concat ( '|' Concat )? ( '|' Concat )*
    Concat => Star Star*
    Star => Primary ( '*' )?
    Primary => SYMBOL | '(' Expression ')'
     */
    pub fn parse(&self) -> Result<Option<ExpressionBase>, String> {
	self.advance();
	let expr = self.expression();
	if let Some(tok) = self.read_current() {
	    if tok.lexeme == ')' {
		return Err(String::from("Un-matched ')'"));
	    }
	}
	expr
    }

    // ExpressionBase => union
    fn expression(&self) -> Result<Option<ExpressionBase>, String> {
	self.union()
    }

    // Union => Concat ( '|' Concat )? ( '|' Concat )*
    fn union(&self) -> Result<Option<ExpressionBase>, String> {
	let first = self.concat()?;
	if first.is_none() {
	    return Ok(None);
	}
	let first = first.unwrap();
	let mut exprs = Vec::new();
	exprs.push(first);

	if self.match_current(TokenType::Pipe) {
	    let second = self.concat()?;
	    if let Some(expr) = second {
		exprs.push(expr);
	    }
	}

	while self.match_current(TokenType::Pipe) {
	    let new_expr = self.concat()?;
	    if let Some(expr) = new_expr {
		exprs.push(expr);
	    }
	}

	if exprs.len() <= 1 {
	    return Ok(exprs.pop());
	}

	let union =
	    ExpressionBase::Union {
		items : exprs
	    };

	Ok(Some(union))
    }

    // Concat => Star Star*
    fn concat(&self) -> Result<Option<ExpressionBase>, String> {
	let mut exprs = Vec::new();
	loop {
	    let expr = self.star()?;
	    match expr {
		Some(value) => exprs.push(value),
		None => break
	    }
	}

	if exprs.len() == 0 {
	    return Ok(None);
	} else if exprs.len() == 1 {
	    let parsed_expr = exprs.pop();
	    return Ok(parsed_expr);
	}

	let concat =
	    ExpressionBase::Concat {
		items: exprs
	    };

	Ok(Some(concat))
    }

    // Star => Primary ( '*' )?
    fn star(&self) -> Result<Option<ExpressionBase>, String> {
	let expr = self.primary()?;
	if expr.is_none() {
	    return Ok(None);
	}
	
	let expr = expr.unwrap();

	if self.match_current(TokenType::Star) {
	    let star =
		ExpressionBase::Star {
		    inner_expr: Box::new(expr)
		};

	    return Ok(Some(star));
	}

	Ok(Some(expr))
    }

    // Primary => SYMBOL | '(' ExpressionBase ')'
    fn primary(&self) -> Result<Option<ExpressionBase>, String> {
	match self.read_current() {
	    Some(peek) => {
		if self.match_current(TokenType::LeftParen) {
		    let expr = self.expression()?;
		    self.consume(
			TokenType::RightParen,
			"Expected ')' after expression"
		    )?;

		    let expr = expr.unwrap();

		    let grouping =
			ExpressionBase::Grouping {
			    inner_expr: Box::new(expr)
			};

		    Ok(Some(grouping))
		} else if
		    self
		    .scanner
		    .borrow()
		    .alphabet
		    .contains(&peek.lexeme)
		{
		    self.advance();
		    Ok(
			Some(
			    ExpressionBase::Symbol {
				value : peek.lexeme
			    }
			)
		    )
		} else {
		    Ok(None)
		}
	    },
	    None => Ok(None)
	}
    }
}