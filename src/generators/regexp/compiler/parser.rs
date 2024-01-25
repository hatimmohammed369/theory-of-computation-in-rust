use crate::automata::nfa::AlphabetSymbol;
use crate::automata::nfa::NFA;
use crate::automata::ComputationStyle;
use crate::generators::regexp::compiler::scanner::*;
use crate::generators::regexp::ExpressionBase;
use std::cell::RefCell;
use std::collections::HashMap;
use std::collections::HashSet;
use std::rc::Rc;

static mut COUNTER: usize = 0usize;

impl ExpressionBase {
    fn formatted(&self) -> String {
        format!("{}", self)
    }

    pub fn compile(&self, alphabet: &HashSet<AlphabetSymbol>) -> NFA {
        let start_state =
            unsafe { NFA::new_random_state(&format!("({COUNTER},<{}>.S)", self.formatted())) };
        unsafe {
            COUNTER += 1;
        }
        let mut states = HashSet::new();
        states.insert(String::from(&start_state));

        let mut transition_function = HashMap::new();
        let mut accept_states = HashSet::new();

        match self {
            Self::EmptyString { .. } => {
                accept_states.insert(String::from(&start_state));
            }
            Self::Symbol(value) => {
                let accept_state = unsafe {
                    NFA::new_random_state(&format!("({COUNTER},<{}>.A)", self.formatted()))
                };
                states.insert(String::from(&accept_state));

                transition_function
                    .entry(String::from(&start_state))
                    .or_insert(HashMap::new())
                    .entry(*value.as_ref())
                    .or_insert(HashSet::new())
                    .insert(String::from(&accept_state));

                accept_states.insert(String::from(&accept_state));
            }
            Self::Grouping(grouped_expr) => {
                return grouped_expr.compile(alphabet);
            }
            Self::Star(starred_expr) => {
                let expr_nfa = starred_expr.compile(alphabet);
                return NFA::kleene_star(&expr_nfa, &start_state);
            }
            Self::Union(items) => {
                let automata = items
                    .iter()
                    .map(|expr| expr.compile(alphabet))
                    .collect::<Vec<NFA>>();
                return NFA::union(automata.iter(), &start_state);
            }
            Self::Concat(items) => {
                let automata = items
                    .iter()
                    .map(|expr| expr.compile(alphabet))
                    .collect::<Vec<NFA>>();

                let automata = automata.iter().collect::<Vec<&NFA>>();

                return NFA::concatenate(&automata[..]);
            }
            ExpressionBase::CharacterClass { inverted, ranges } => {
                panic!();
            }
            ExpressionBase::EmptySet => {
                // compiling an actual regular expression can never yield the empty set.
                panic!();
            }
        }

        let alphabet = alphabet.clone();
        let computation_style = crate::automata::ComputationStyle::Nondeterministic;
        let dfa = RefCell::new(None);

        NFA::raw_new(
            states,
            alphabet,
            transition_function,
            start_state,
            accept_states,
            &computation_style,
            dfa,
        )
    }
}

use std::rc::Weak;

#[derive(Debug)]
pub struct Expression {
    parent: RefCell<Option<Weak<Expression>>>,
    base: RefCell<Option<Rc<ExpressionBase>>>,
    pub children: RefCell<Vec<Rc<Expression>>>,
}

impl From<&Expression> for String {
    fn from(value: &Expression) -> Self {
        let base = value.base.borrow();
        let base = base.as_ref().unwrap();
        let base = base.as_ref();
        format!("{base}")
    }
}

impl Default for Expression {
    fn default() -> Self {
        Expression {
            parent: RefCell::new(None),
            base: RefCell::new(None),
            children: RefCell::new(Vec::new()),
        }
    }
}

impl std::fmt::Display for &Expression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let base = self.base.borrow();
        let base = base.as_ref().unwrap();
        let base = format!("{base}");

        match self.parent.borrow().as_ref() {
            Some(parent) => {
                let parent = parent.upgrade().unwrap();
                let parent = parent.base.borrow();
                let parent = parent.as_ref().unwrap();
                let parent = format!("{parent}");
                write!(f, "({base} => {parent})")
            }
            None => {
                write!(f, "{base}")
            }
        }
    }
}

impl Expression {
    pub fn compile(&self, alphabet: &HashSet<AlphabetSymbol>) -> NFA {
        self.base.borrow().as_ref().unwrap().compile(alphabet)
    }
}

pub struct RegexpParseError {
    message: String,
    position: usize,
    post_message: String,
}

impl RegexpParseError {
    fn format_error(&self, pattern: &str) -> String {
        let msg = format!("{}\n", self.message);

        let position = self.position;
        let mut caret = String::new();
        while caret.len() < position {
            caret.push(' ')
        }
        caret.push('^');
        caret.push('\n');
        let post_message = &self.post_message;

        format!("[Parsing Error at {position}]: {msg}{pattern}\n{caret}\n{post_message}")
    }
}

enum GroupingType {
    Parentheses,    // ()
    CharacterClass, // []
    Multiplier,     // {m, n}
}

pub struct Parser {
    pub scanner: RefCell<Scanner>,
    previous: RefCell<Option<Token>>,
    current: RefCell<Option<Token>>,
    groupings: RefCell<Vec<GroupingType>>,
}

use TokenName::*;
impl Parser {
    pub fn new(scanner: Scanner) -> Parser {
        let scanner = RefCell::new(scanner);
        let previous = RefCell::new(None);
        let current = RefCell::new(None);
        let groupings = RefCell::new(Vec::new());
        Parser {
            scanner,
            previous,
            current,
            groupings,
        }
    }

    fn read_current(&self) -> Option<Token> {
        self.current.borrow().clone()
    }

    fn read_previous(&self) -> Option<Token> {
        self.previous.borrow().clone()
    }

    fn advance(&self) {
        *self.previous.borrow_mut() = self.current.take();
        *self.current.borrow_mut() = self.scanner.borrow_mut().next();
    }

    fn consume(&self, name: TokenName, message: &str) -> Result<(), RegexpParseError> {
        let message = String::from(message);
        match self.read_current() {
            Some(tok) => {
                if tok.name != name {
                    let position = tok.position;
                    let post_message = String::new();
                    Err(RegexpParseError {
                        message,
                        position,
                        post_message,
                    })
                } else {
                    self.advance();
                    Ok(())
                }
            }
            None => {
                let position = &self.scanner.borrow().chars.len() - 1;
                let post_message = String::new();
                Err(RegexpParseError {
                    message,
                    position,
                    post_message,
                })
            }
        }
    }

    fn match_current(&self, expected: TokenName) -> bool {
        if self.check(expected) {
            self.advance();
            true
        } else {
            false
        }
    }

    fn check(&self, expected: TokenName) -> bool {
        match self.read_current() {
            Some(tok) => tok.name == expected,
            None => false,
        }
    }

    /*
    AFTER PARSING EXPRESSION (E), FIELD (current) MUST POINT TO THE FIRST
    TOKEN AFTER EXPRESSION (E)
     */
    /*
    Expression => Union
    Union => Concat ( '|' Concat )? ( '|' Concat )*
    Concat => Star Star*
    Star => Primary ( '*' )?
    Primary => EMPTY_STRING | DOT | SYMBOL | '(' Expression ')' | CharacterClass
    CharacterClass => '[' ( ('^')? Range+ )? ']'
    Range => SYMBOL ( '-' SYMBOL )?
     */
    pub fn parse(&self) -> Result<Rc<Expression>, String> {
        self.advance();
        match self.expression() {
            Ok(expr) => Ok(expr),
            Err(error) => {
                let pattern = self
                    .scanner
                    .borrow()
                    .chars
                    .iter()
                    .fold(String::new(), |a, b| format!("{a}{b}"));
                Err(error.format_error(&pattern))
            }
        }
    }

    // Expression => Union
    pub fn expression(&self) -> Result<Rc<Expression>, RegexpParseError> {
        let parsed_expression = self.union()?;
        if let Some(tok) = self.read_current() {
            if self.groupings.borrow().is_empty() // No active group
		&& tok.name == RightParen
            {
                let message = String::from("Un-matched `)`");
                let position = tok.position;
                let post_message = String::new();
                return Err(RegexpParseError {
                    message,
                    position,
                    post_message,
                });
            }
        }
        Ok(parsed_expression)
    }

    // Union => Concat ( '|' Concat )? ( '|' Concat )*
    pub fn union(&self) -> Result<Rc<Expression>, RegexpParseError> {
        let mut concats = Vec::<Rc<Expression>>::new();
        loop {
            let concat_expr = self.concat()?;
            if concat_expr.base.borrow().is_some() {
                concats.push(Rc::clone(&concat_expr));
            } else {
                // Can't parse another expression, stop
                break;
            }

            if !self.match_current(Pipe) {
                break;
            }
        }

        if concats.is_empty() {
            // Could not parse any expression.
            Ok(Rc::new(Expression::default()))
        } else if concats.len() == 1 {
            Ok(concats.pop().unwrap())
        } else {
            let union = ExpressionBase::Union(
                concats
                    .iter()
                    .map(|expr| Rc::clone(expr.base.borrow().as_ref().unwrap()))
                    .collect::<_>(),
            );
            let union = Rc::new(union);

            let returned_expression = {
                let parent = RefCell::new(None);
                let base = RefCell::new(Some(union));
                let children = RefCell::new(vec![]);

                Expression {
                    parent,
                    base,
                    children,
                }
            };
            let returned_expression = Rc::new(returned_expression);

            concats.iter().for_each(|concat_expr| {
                *concat_expr.parent.borrow_mut() = Some(Rc::downgrade(&returned_expression));
                returned_expression
                    .children
                    .borrow_mut()
                    .push(Rc::clone(concat_expr));
            });

            Ok(returned_expression)
        }
    }

    // Concat => Star Star*
    pub fn concat(&self) -> Result<Rc<Expression>, RegexpParseError> {
        let mut stars = Vec::<Rc<Expression>>::new();
        loop {
            let star_expr = self.star()?;
            if star_expr.base.borrow().is_some() {
                stars.push(Rc::clone(&star_expr));
            } else {
                // Could not parse another expression, stop
                break;
            }
        }

        if stars.is_empty() {
            // Could not parse any expression
            Ok(Rc::new(Expression::default()))
        } else if stars.len() <= 1 {
            Ok(stars.pop().unwrap())
        } else {
            let concat = ExpressionBase::Concat(
                stars
                    .iter()
                    .map(|expr| Rc::clone(expr.base.borrow().as_ref().unwrap()))
                    .collect::<_>(),
            );
            let concat = Rc::new(concat);

            let returned_expression = {
                let parent = RefCell::new(None);
                let base = RefCell::new(Some(concat));
                let children = RefCell::new(vec![]);

                Expression {
                    parent,
                    base,
                    children,
                }
            };
            let returned_expression = Rc::new(returned_expression);

            stars.iter().for_each(|star_expr| {
                *star_expr.parent.borrow_mut() = Some(Rc::downgrade(&returned_expression));
                returned_expression
                    .children
                    .borrow_mut()
                    .push(Rc::clone(star_expr));
            });

            Ok(returned_expression)
        }
    }

    // Star => Primary ( '*' )?
    pub fn star(&self) -> Result<Rc<Expression>, RegexpParseError> {
        let primary = self.primary()?;
        if self.match_current(Star) {
            if primary.base.borrow().is_none() {
                /*
                Could not parse an expression
                Error: Using `*` preceeded by nothing
                 */
                let message = String::from("Expected expression before `*`");
                let position = self.read_previous().unwrap().position;
                let post_message = String::from("Use \\* to match a literal `*`");
                return Err(RegexpParseError {
                    message,
                    position,
                    post_message,
                });
            }
            let star = ExpressionBase::Star(Rc::clone(primary.base.borrow().as_ref().unwrap()));
            let star = Rc::new(star);

            let returned_expression = {
                let parent = RefCell::new(None);
                let base = RefCell::new(Some(star));
                let children = RefCell::new(vec![]);

                Expression {
                    parent,
                    base,
                    children,
                }
            };
            let returned_expression = Rc::new(returned_expression);

            *primary.parent.borrow_mut() = Some(Rc::downgrade(&returned_expression));

            returned_expression.children.borrow_mut().push(primary);

            Ok(returned_expression)
        } else {
            Ok(primary)
        }
    }

    // Primary => EMPTY_STRING | DOT | SYMBOL | '(' Expression ')' | CharacterClass
    // CharacterClass => '[' ( ('^')? Range+ )? ']'
    // Range => SYMBOL ( '-' SYMBOL )?
    pub fn primary(&self) -> Result<Rc<Expression>, RegexpParseError> {
        match self.read_current() {
            Some(peek) => {
                if peek.name == LeftParen {
                    self.groupings.borrow_mut().push(GroupingType::Parentheses);
                    self.advance();
                    let parsed_expr = self.expression()?;
                    self.consume(RightParen, "Expected `)` after expression.")?;
                    self.groupings.borrow_mut().pop();

                    let grouping = ExpressionBase::Grouping(Rc::clone(
                        parsed_expr.base.borrow().as_ref().unwrap(),
                    ));
                    let grouping = Rc::new(grouping);

                    let returned_expression = {
                        let parent = RefCell::new(None);
                        let base = RefCell::new(Some(grouping));
                        let children = RefCell::new(vec![]);

                        Expression {
                            parent,
                            base,
                            children,
                        }
                    };
                    let returned_expression = Rc::new(returned_expression);

                    *parsed_expr.parent.borrow_mut() = Some(Rc::downgrade(&returned_expression));

                    returned_expression.children.borrow_mut().push(parsed_expr);

                    Ok(returned_expression)
                } else if peek.name == Dot || peek.name == Symbol || Self::is_escaped_token(&peek) {
                    // Single-symbol expression
                    self.advance();
                    let parent = RefCell::new(None);
                    let symbol = ExpressionBase::Symbol(Rc::new({
                        // Choose symbol
                        if peek.name == Dot {
                            AlphabetSymbol::Any
                        } else {
                            AlphabetSymbol::Character(peek.lexeme)
                        }
                    }));
                    let symbol = Rc::new(symbol);
                    let base = RefCell::new(Some(symbol));
                    let children = RefCell::new(Vec::new());
                    Ok(Rc::new(Expression {
                        parent,
                        base,
                        children,
                    }))
                } else if peek.name == EmptyString {
                    // Empty string expression
                    // Something like (), (|), ()|(), ...
                    self.advance();
                    let parent = RefCell::new(None);
                    let symbol = ExpressionBase::EmptyString;
                    let symbol = Rc::new(symbol);
                    let base = RefCell::new(Some(symbol));
                    let children = RefCell::new(Vec::new());
                    Ok(Rc::new(Expression {
                        parent,
                        base,
                        children,
                    }))
                } else if peek.name == LeftBracket {
                    // A character class expression
                    self.groupings
                        .borrow_mut()
                        .push(GroupingType::CharacterClass);
                    self.advance();
                    let inverted = {
                        // Check if this character class is inverted
                        if self.read_current().as_ref().unwrap().name == Caret {
                            self.advance();
                            true
                        } else {
                            false
                        }
                    };
                    let mut ranges = Vec::<(u8, u8)>::new();
                    use crate::generators::regexp::compiler::scanner::RangePart;
                    let mut range_part = RangePart::None;
                    while self.read_current().unwrap().name != RightBracket {
                        let current_token = self.read_current().unwrap();
                        let lexeme = current_token.lexeme;
                        let current_name = current_token.name;
                        match current_name {
                            EmptyString => {
                                // nothing to be done.
                            }
                            Symbol => {
                                // Ordinary characters
                                if lexeme.is_alphanumeric() {
                                    match range_part {
                                        RangePart::Hyphen => {
                                            ranges.last_mut().unwrap().1 = lexeme as u8;
                                            range_part = RangePart::UpperLimit;
                                        }
                                        _ => {
                                            ranges.push((lexeme as u8, lexeme as u8));
                                            range_part = RangePart::LowerLimit;
                                        }
                                    }
                                } else {
                                    ranges.push((lexeme as u8, lexeme as u8));
                                    range_part = RangePart::None;
                                }
                            }
                            RightParen | LeftParen | RightBracket | LeftBracket | Star | Pipe
                            | Dot => {
                                panic!(
                                    "TokenName::{current_name:?} was emitted inside character class"
                                );
                            }
                            RangeHyphen => {
                                range_part = RangePart::Hyphen;
                            }
                            Caret => {
                                panic!(
                                    "TokenName::Caret was emitted in the middle of a character class"
                                );
                            }
                            escaped => {
                                ranges.push((lexeme as u8, lexeme as u8));
                            }
                        }
                        self.advance();
                    }
                    self.consume(RightBracket, "Expected `]` after character class.")?;
                    self.groupings.borrow_mut().pop();

                    let ranges = ranges.into_iter().collect::<_>();
                    let character_class = ExpressionBase::CharacterClass { inverted, ranges };
                    let parent = RefCell::new(None);
                    let base = RefCell::new(Some(Rc::new(character_class)));
                    let children = RefCell::new(Vec::new());
                    Ok(Rc::new(Expression {
                        parent,
                        base,
                        children,
                    }))
                } else {
                    /*
                    No expression found
                    When we have more functionality this else branch will change.
                     */
                    Ok(Rc::new(Expression::default()))
                }
            }
            None => {
                // End of input
                Ok(Rc::new(Expression::default()))
            }
        }
    }

    fn is_escaped_token(token: &Token) -> bool {
        matches!(
            token.name,
            EscapedSlash
                | EscapedRightParen
                | EscapedLeftParen
                | EscapedStar
                | EscapedPipe
                | EscapedRightBracket
                | EscapedLeftBracket
                | EscapedMinus
                | EscapedCaret
        )
    }
}
