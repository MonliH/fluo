use crate::parser::parser::Parser;
use crate::parser::{ custom_syntax::NamespaceObj, custom_syntax };
use crate::parser::std::{ pattern, statement };
use std::collections::HashMap;
use crate::parser::gen_utils as g;

// Tools for generating std

macro_rules! map (
    { $($key:expr => $value:expr),+ } => {
        {
            let mut m = HashMap::new();
            $(
                m.insert($key, $value);
            )+
            m
        }
     };
);

// When we generate code for a pattern:
// 
// ```fluo
// pattern syntax::statement::parse {
//    $expr: syntax::expr
// }
// ```
//
// We would have a `parser` and `run` for each of the different pattern types.
// In fact, each of these `parser` and `run` also each have their own 
// `parse` and `run` values. Typically, the `run` value inside the individual
// patterns (i.e. in `parse_patterns` and `run_patterns`) just returns the value
// verbatim. It is the one level higher method that actually changed the parse
// int compiler recognizable code (in reality an ast).
//
//
// NOTE:
// The pattern template definition (syntax::pattern::parse) and pattern parser 
// (and evaler, although usually we just fill in values verbatim) ARE NOT THE
// SAME. We need two separate std objects for this; a pattern template parser
// (for overloading and creating patterns), and a complete object that does
// already have the overloadable functions.

fn generate_statement_lib<'a>() -> NamespaceObj<'a> {
    // The std lib should be able to parse these on their own, as well as generate the appropriate things, 
    // that means they have to return 

    // Parsing the syntax::statement::parse pattern
    let parse_patterns = map![
        "parse".to_string() => pattern::generate_pattern_parse(), // Parser with normal pattern syntax
        "run".to_string() => pattern::generate_pattern_run() // Keep it the same way we parsed it.
    ];

    // Parsing the syntax::statement::runs pattern
    let run_patterns = map![
        "parse".to_string() => statement::generate_statement_run_parser(), // Use block (i.e. normal code) to parse
        "run".to_string() => pattern::generate_pattern_run() // Keep it the same way we parsed it.
    ];

    let objects = map![
        g::new_name_id("parse") => NamespaceObj::Syntax(
            &custom_syntax::Impl {
                name: g::new_name_id("parse"),
                metasyntax: g::new_namespace(vec!["syntax", "pattern", "pattern"]),

                patterns: parse_patterns,
                pos: g::gp(),

                operations: None
            }
        ),
        g::new_name_id("run") => NamespaceObj::Syntax(
            &custom_syntax::Impl {
                name: g::new_name_id("run"),
                metasyntax: g::new_namespace(vec!["syntax", "pattern", "pattern"]),

                patterns: run_patterns,
                pos: g::gp(),

                operations: None
            }
        ),
        g::new_name_id("statement") => NamespaceObj::Metasyntax(&Parser::std_statement_statement)
    ];

    NamespaceObj::ParserNamespace(objects)
}

fn generate_pattern_lib<'a>() -> NamespaceObj<'a> {
    // We want to impelment these methods so they can be generated on the fly by our pattern metasyntax

    // Parsing the syntax::statement::parse pattern
    let parse_patterns = map![
        "parse".to_string() => pattern::generate_pattern_parse(), // Parser with normal pattern syntax
        "run".to_string() => pattern::generate_pattern_run() // Keep it the same way we parsed it.
    ];

    // Parsing the syntax::statement::runs pattern
    let run_patterns = map![
        "parse".to_string() => statement::generate_statement_run_parser(), // Use block (i.e. normal code) to parse
        "run".to_string() => pattern::generate_pattern_run() // Keep it the same way we parsed it.
    ];

    let objects = map![
        // The parse and run for a pattern
        g::new_name_id("parse") => NamespaceObj::Syntax(
            &custom_syntax::Impl {
                name: g::new_name_id(""),
                metasyntax: g::new_namespace(vec!["syntax", "pattern", "pattern"]),

                patterns: parse_patterns,
                pos: g::gp(),

                operations: None
            }
        ),
        g::new_name_id("run") => NamespaceObj::Syntax(
            &custom_syntax::Impl {
                name: g::new_name_id(""),
                metasyntax: g::new_namespace(vec!["syntax", "pattern", "pattern"]),

                patterns: run_patterns,
                pos: g::gp(),

                operations: None
            }
        ),
        g::new_name_id("pattern") => NamespaceObj::Metasyntax(&Parser::std_pattern_pattern)
    ];

    NamespaceObj::ParserNamespace(objects)
}

fn generate_syntax_lib<'a>() -> NamespaceObj<'a> {
    let objects = map![
        g::new_name_id("statement") => generate_statement_lib(),
        g::new_name_id("pattern") => generate_pattern_lib()
    ];

    NamespaceObj::ParserNamespace(objects)
}

pub fn generate_std_lib<'a>() -> NamespaceObj<'a> {
    let objects = map![
        g::new_name_id("syntax") => generate_syntax_lib()
    ];

    NamespaceObj::ParserNamespace(objects)
}
