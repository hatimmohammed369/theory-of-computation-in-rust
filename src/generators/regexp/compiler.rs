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

#[derive(Debug)]
pub enum ExpressionBase {
    EmptyString,
    Symbol {value: char},
    Grouping {inner_expr: Rc<ExpressionBase>},
    Star {inner_expr: Rc<ExpressionBase>},
    Union {items: Vec<Rc<ExpressionBase>>},
    Concat {items: Vec<Rc<ExpressionBase>>},
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
		format!(
		    "({})",
		    String::from(inner_expr.as_ref())
		)
	    },
	    ExpressionBase::Star { inner_expr, .. } => {
		let inner_expr = inner_expr.as_ref();
		match inner_expr {
		    ExpressionBase::Symbol {value} => {
			format!("{value}*")
		    },
		    _ => {
			format!(
			    "({})*",
			    String::from(inner_expr)
			)
		    }
		}
	    },
	    ExpressionBase::Union { items, .. } => {
		items
		    .iter()
		    .map(|item| {
			String::from(item.as_ref())
		    })
		    .collect::<Vec<String>>()
		    .join("|")
	    },
	    ExpressionBase::Concat { items, .. } => {
		items
		    .iter()
		    .fold(
			String::new(),
			|a, b| {
			    format!(
				"{}{}", a,
				String::from(b.as_ref())
			    )
			}
		    )
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
}
