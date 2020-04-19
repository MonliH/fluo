use std::collections::HashMap;
use crate::parser::ast::{ NameID, Node };
use crate::logger::logger::Error;
use crate::parser::parser::Parser;
use crate::parser::ast;
use crate::helpers;
use std::fmt;

pub enum NamespaceObj<'a> {
    ParserNamespace(HashMap<NameID, NamespaceObj<'a>>),
    // Transforms the impl (hence taking a mutable refrence)
    Metasyntax(Metasyntax),
    Syntax(&'a Impl)  // Patterns are really just syntaxes that use the pattern metaclass
}

#[derive(Debug)]
/// User defined syntax ast
pub struct Custom {
    /// Type of syntax, i.e. syntax::statement::statement (aka metasyntax)
    pub custom_type: ast::Namespace,

    /// Values, i.e. { $left = ast::Integer(10), $right = ast::Integer(1) }
    pub values: HashMap<NameID, Node>,

    /// Name of custom syntax used (i.e. useless_print, syntax::statement::parse, my_file::my_syntax)
    pub name: ast::Namespace,
    pub pos: helpers::Pos,

    /// String representation of statement
    pub rep: String,

    /// Scopes which it can be used
    pub scope: ast::Scope
}


/// Impl block (for changeable syntax at compile time definition)
pub struct Impl {
    pub name: NameID,
    pub metasyntax: ast::Namespace,
    pub patterns: HashMap<String, Custom>,  // Custom should have all the values needed, 
                                            // parse and run functions to be used by the 
                                            // metasyntax
    pub pos: helpers::Pos
}

pub struct Metasyntax {
    pub rules: HashMap<String, Custom>,
    pub pos: helpers::Pos
}

impl fmt::Display for Impl {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f, 
            "Impl {{\n{:#?}\n{}\n{:#?}\n{:#?}\n{:?}\n}}", 
            self.name, 
            self.metasyntax, 
            self.patterns, 
            self.pos, 
            if let Some(operations) = &self.operations {
                operations.keys().collect()
            } else {
                vec![]
            }
        )
    }
}

impl fmt::Debug for Impl {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Impl")
         .field("name", &self.name)
         .field("pos", &self.pos)
         .field("patterns", &self.patterns)
         .field("pos", &self.pos)
         .field(
            "operations", 
            &if let Some(operations) = &self.operations {
                operations.keys().collect()
            } else {
                vec![]
            }
        )
         .finish()
    }
}

#[derive(Debug)]
/// Pattern class
pub struct Pattern {
    pub prototype: ast::Namespace,
    pub contents: Vec<Node>,
    pub pos: helpers::Pos
}

#[derive(Debug)]
/// Terminal node in pattern
pub struct Terminal {
    pub contents: ast::StringLiteral,
    pub pos: helpers::Pos
}

impl Terminal {
    pub fn generate(self, parser: &mut Parser) -> Result<Box<dyn Fn (&mut Parser) -> Result<Node, Error>>, Error> {
        Ok(Box::new(|parser| { 
            Ok(Node::Empty(ast::Empty { pos: helpers::Pos::new(0, 0) }))
        }))
    }
}

#[derive(Debug)]
/// Non-terminal node in pattern
pub struct NonTerminal {
    pub name: DollarID,
    pub grammar: GrammarExpr,
    pub pos: helpers::Pos
}

impl NonTerminal {
    pub fn generate(self, parser: &mut Parser) -> Result<Box<dyn Fn (&mut Parser) -> Result<Node, Error>>, Error> {
        Ok(Box::new(|parser| { 
            Ok(Node::Empty(ast::Empty { pos: helpers::Pos::new(0, 0) }))
        }))
    }
}

#[derive(Debug)]
/// Dollar sign id (i.e. `$myvar`) node
pub struct DollarID {
    pub value: NameID,
    pub pos: helpers::Pos
}

#[derive(Debug)]
pub enum GrammarExpr {
    Plus(Box<Plus>),
    Star(Box<Star>),
    Production(Box<Production>),
    Or(Box<Or>)
}

#[derive(Debug)]
pub struct Plus {
    pub non_terminal: GrammarExpr,
    pub trailing: TrailingElement,
    pub pos: helpers::Pos,
}

#[derive(Debug)]
pub struct Star {
    pub non_terminal: GrammarExpr,
    pub trailing: TrailingElement,
    pub pos: helpers::Pos
}

#[derive(Debug)]
pub struct Or {
    pub left: GrammarExpr,
    pub right: GrammarExpr,
    pub pos: helpers::Pos
}

#[derive(Debug)]
pub struct Group {
    pub items: Vec<Production>,
    pub pos: helpers::Pos
}

#[derive(Debug)]
pub enum Production {
    Terminal(Terminal),
    NonTerminal(ast::Namespace)
}

/// Trailing marker, specified like
/// 
/// $expr: ( syntax::nonterminal | syntax::terminal ) * -> "," 
///                                                   ^^^^^^^^
///                                                   Note the single star. Doing a
///                                                   double star results in no 
///                                                   trailing marker allowed
#[derive(Debug)]
pub struct TrailingElement {
    pub trailing: bool,
    pub separator: String,
    pub pos: helpers::Pos
}
