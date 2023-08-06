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
    alphabet: HashSet<char>,
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

enum Expression<'a> {
    EmptyString {parent: Option<&'a Expression<'a>>,},
    Symbol {parent: Option<&'a Expression<'a>>, value: char},
    Grouping {parent: Option<&'a Expression<'a>>, inner_expr: Box<Expression<'a>>},
    Star {parent: Option<&'a Expression<'a>>, inner_expr: Box<Expression<'a>>},
    Union {parent: Option<&'a Expression<'a>>, items: Vec<Expression<'a>>},
    Concat {parent: Option<&'a Expression<'a>>, items: Vec<Expression<'a>>},
}

impl From<&Expression<'_>> for String {
    fn from(value: &Expression<'_>) -> Self {
	match value {
	    Expression::EmptyString { .. } => {
		String::from("{\"\"}")
	    },
	    Expression::Symbol { value, .. } => {
		String::from(*value)
	    },
	    Expression::Grouping { inner_expr, .. } => {
		String::from(inner_expr.as_ref())
	    },
	    Expression::Star { inner_expr, .. } => {
		format!("({})*", String::from(inner_expr.as_ref()))
	    },
	    Expression::Union { items, .. } => {
		items
		    .iter()
		    .map(String::from)
		    .collect::<Vec<String>>()
		    .join("|")
	    },
	    Expression::Concat { items, .. } => {
		items
		    .iter()
		    .fold(String::new(), |a, b| {
			format!("{}{}", a, String::from(b))
		    })
	    }
	}
    }
}

impl std::fmt::Display for Expression<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
	let parent = self.read_parent();
	let self_string = String::from(self);
	let display =
	    match parent {
		Some(expr) => {
		    let expr = *expr;
		    let expr = String::from(expr);
		    format!("({expr} <- {self_string})")
		},
		None => self_string
	    };

        write!(f, "{}", display)
    }
}

use std::collections::HashMap;
use std::cell::RefCell;
use std::rc::Rc;
use crate::automata::nfa::NFA;

impl<'a> Expression<'a> {
    fn read_parent(&self) -> &Option<&'a Expression> {
	match self {
	    Self::EmptyString { parent } => parent,
	    Expression::Symbol { parent, .. } => parent,
	    Self::Grouping { parent, .. } => parent,
	    Expression::Star { parent, .. } => parent,
	    Self::Union { parent, .. } => parent,
	    Self::Concat { parent, .. } => parent
	}
    }

    fn write_parent(&self) -> Option<&'a Expression> {
	match self {
	    Self::EmptyString {parent } => *parent,
	    Expression::Symbol {parent, .. } => *parent,
	    Self::Grouping {parent, .. } => *parent,
	    Expression::Star {parent, .. } => *parent,
	    Self::Union {parent, .. } => *parent,
	    Self::Concat {parent, .. } => *parent
	}
    }

    fn formatted(&self) -> String {
	format!("{}", self)
    }

    pub fn compile(&self, alphabet: &HashSet<char>) -> NFA {
	let id = format!("{:?}", self as *const Expression<'a>);
	let self_string = self.formatted();

	let mut counter = 0usize;
	let mut name_gen = || {
	    let temp = counter;
	    counter += 1;
	    format!("({self_string}@{id}).{temp}")
	};

	let start_state = name_gen();
	let mut states = HashSet::new();
	states.insert(String::from(&start_state));

	let mut transition_function =
	    HashMap::<String, HashMap<char, HashSet<String>>>::new();
	let mut accept_states = HashSet::new();

	match self {
	    Self::EmptyString { .. } => {
		states.insert(String::from(&start_state));
		accept_states.insert(String::from(&start_state));
	    },
	    Expression::Symbol { value, .. } => {
		let accept_state = name_gen();

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
	    Expression::Star { inner_expr, .. } => {
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
