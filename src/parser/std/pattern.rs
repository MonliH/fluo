use crate::lexer;
use crate::parser::{ ast, parser::Parser, custom_syntax };
use std::collections::HashMap;
use crate::parser::gen_utils as g;
use crate::logger::logger::{ ErrorAnnotation, Error, ErrorDisplayType, ErrorType };

// Pattern custom object spec:
// Every pattern should have the `custom_type`, and it is the name of the pattern metasyntax used.
// (typically syntax::pattern::pattern)
//
// A `parse` pattern should be of type `ast::Node::Nodes`, using the `g` (gen_util) functions when necessary.
// It should containt a list of `ast::Node::NonTerminal` and `ast::Node::Terminal`
// 
// A `run` pattern should contain an `ast::Node::Block`, as it is what will sub for the `{ /* my rules */ }`
// It can really be anything. Note however if it turns the code into something that is not valid, errors 
// will be reported.
// 
// The `run` pattern will not be transformed; it is quite trivial. Just store the current parsed objects 
// (i.e. `pattern_rules`), and since its an expression, when it is parsed that way, we can just replace 
// every occurrence of any DollarID to the matching value on the symtab.
// 
// `name` is the "syntax" of the "metasyntax"
// i.e. syntax::pattern::parse, syntax:::statement::parse, my_dumb_pattern

// Patterns for parsing patterns
pub fn generate_pattern_parse() -> custom_syntax::Custom {
    let mut rule: HashMap<ast::NameID, ast::Node> = HashMap::new();
    
    rule.insert(
        // expr is the thing the metasyntax looks for, don't touch this
        // (has nothing to do with the non terminal below)
        g::new_name_id("expr"), 

        // Generate something like this `$pattern_rules: ( syntax::nonterminal | syntax::terminal ) * -> ","`
        ast::Node::Nodes(
            g::new_nodes(vec![ 
                ast::Node::Terminal(
                    g::new_terminal("{")
                ), 
                ast::Node::NonTerminal(
                    g::new_non_terminal(
                        g::new_dollar_sign("pattern_rules"), // this is the `$pattern_rules`
                        g::new_plus_grammar(
                            g::new_or_grammar(
                                g::new_production_grammar_expr(g::new_namespace_production(g::new_namespace(vec!["syntax", "terminal"]))),
                                g::new_production_grammar_expr(g::new_namespace_production(g::new_namespace(vec!["syntax", "nonterminal"])))
                            ),
                            g::new_trailing_grammar(
                                ",",
                                true
                            )
                        )
                    )
                ),
                ast::Node::Terminal(
                    g::new_terminal("}")
                )
            ])
        )
    );

    custom_syntax::Custom {
        custom_type: g::new_namespace(vec!["syntax", "pattern", "pattern"]),

        /// Values, i.e. { $left = ast::Integer(10), $right = ast::Integer(1) }
        values: rule,

        // syntax::pattern::parse
        name: g::new_namespace(vec!["syntax", "pattern", "parse"]),
        pos: g::gp(),

        rep: "pattern std definition".to_string(),

        scope: ast::Scope::Outer,
    }
}

pub fn generate_pattern_run() -> custom_syntax::Custom {
    // Note we want to keep the pattern the same as the way we parsed it.
    let mut rule: HashMap<ast::NameID, ast::Node> = HashMap::new();
    
    rule.insert(
        // expr is the thing the metasyntax looks for, don't touch this
        // (has nothing to do with the non terminal below)
        g::new_name_id("expr"), 

        // Access `pattern_rules` from pattern_parse implementation
        ast::Node::Block( ast::Block {
            nodes: vec![ast::Statement::DollarID(g::new_dollar_sign("pattern_rules"))],
            pos: g::gp()
        })
    );

    custom_syntax::Custom {
        custom_type: g::new_namespace(vec!["syntax", "pattern", "pattern"]),

        values: rule,

        // syntax::pattern::parse
        name: g::new_namespace(vec!["syntax", "pattern", "run"]),
        pos: g::gp(),

        rep: "pattern std definition".to_string(),

        scope: ast::Scope::Outer,
    }
}

impl Parser<'_> {
    pub fn parse_pattern<'a>(parser: &'a mut Parser<'a>) -> Result<custom_syntax::Custom, Error> {
        // Should parse any pattern in impl
        // Need to call the `std_pattern_pattern` on the prototype.

        let position = parser.lexer.get_pos();

        parser.next(lexer::TokenType::PATTERN, position, true)?;

        let prototype = parser.namespace()?;

        match parser.get_syntax(&prototype) {
            Ok(parser_impl) => {
                // Parser func should be an impl with parse and run functions
                // They should return 

                let parser_impl = parser.get_metasyntax(&parser_impl.metasyntax)?(parser, parser_impl)?;
                match parser_impl.operations.unwrap().get("parse") {
                    Some(parse_func) => {
                        let mut nodes = HashMap::new();
                        nodes.insert(g::new_name_id("expr"), parse_func(parser)?);

                        Ok(custom_syntax::Custom {
                            custom_type: g::new_namespace(vec!["syntax", "pattern", "pattern"]),

                            /// Values, i.e. { $left = ast::Integer(10), $right = ast::Integer(1) }
                            values:  nodes,

                            name: prototype,
                            pos: g::gp(),

                            rep: "pattern std definition".to_string(),

                            scope: ast::Scope::Outer,
                        })
                    },
                    None => { Err( Error::new (
                        format!("`parse` is not defined on pattern `{}`", prototype),
                        ErrorType::SyntaxTypeError,
                        prototype.pos,
                        ErrorDisplayType::Error,
                        parser.lexer.filename.clone(),
                        vec![
                            ErrorAnnotation::new(Some("`parse` is not defined".to_string()), prototype.pos, ErrorDisplayType::Error, parser.lexer.filename.clone())
                        ],
                        true  // High priority error
                    ))},
                }
            },
            Err(e) => Err(e)
        }
    }

    // Turns raw impl into impl with methods
    // It should take in impl with parse and run patterns (patterns are really just custom statements)
    // It should generate the parser for the Pattern from the parse function in the raw impl
    // Then it should output an impl, which is capable of parsing to a Custom Statement node
    // Meta, I know right. 
    pub fn std_pattern_pattern<'a>(parser: &mut Parser<'a>, impl_curr: &'a custom_syntax::Impl) -> Result<&'a custom_syntax::Impl, Error> {
        let mut impl_curr = impl_curr;
        match impl_curr.patterns.get("parse") {
            Some(parse_instructions) => {
                match impl_curr.patterns.get("run") {
                    Some(run_instruction) => {
                        match parse_instructions.values.get(&g::new_name_id("parse")) {
                            Some(parse_inner) => match parse_inner {
                                ast::Node::Nodes(nodes) => {
                                    let mut operations = HashMap::new();
                                    
                                    operations.insert("parse".to_string(), nodes.generate(parser)?);
                                    impl_curr.operations = Some(
                                        operations
                                    );

                                    Ok(impl_curr)
                                }
                                _ => {
                                    Err(
                                        Error::new(
                                            format!("wrong format for pattern"), 
                                            ErrorType::UndefinedSyntax, 
                                            impl_curr.pos, 
                                            ErrorDisplayType::Error, 
                                            parser.lexer.filename.clone(), 
                                            vec![
                                                ErrorAnnotation::new(Some("wrong format".to_string()), impl_curr.pos, ErrorDisplayType::Error, parser.lexer.filename.clone())
                                            ], 
                                            true
                                        )
                                    )
                                }
                            }
                            None => {
                                Err(Error::new(
                                    format!("no entry `parse` for pattern"), 
                                    ErrorType::UndefinedSyntax, 
                                    parse_instructions.pos, 
                                    ErrorDisplayType::Error, 
                                    parser.lexer.filename.clone(), 
                                    vec![
                                        ErrorAnnotation::new(Some("`parse` is not defined on this pattern".to_string()), parse_instructions.pos, ErrorDisplayType::Error, parser.lexer.filename.clone())
                                    ], 
                                    true
                                ))
                            }
                        }
                    },
                    None => {
                        Err(Error::new(
                            format!("no entry `run` found"), 
                            ErrorType::UndefinedSyntax, 
                            impl_curr.pos, 
                            ErrorDisplayType::Error, 
                            parser.lexer.filename.clone(), 
                            vec![
                                ErrorAnnotation::new(Some("`parse` is not defined on this impl".to_string()), impl_curr.pos, ErrorDisplayType::Error, parser.lexer.filename.clone())
                            ], 
                            true
                        ))
                    }
                }
            },
            None => {
                Err(Error::new(
                    format!("no entry `parse` found"), 
                    ErrorType::UndefinedSyntax, 
                    impl_curr.pos, 
                    ErrorDisplayType::Error, 
                    parser.lexer.filename.clone(), 
                    vec![
                        ErrorAnnotation::new(Some("`parse` is not defined on this impl".to_string()), impl_curr.pos, ErrorDisplayType::Error, parser.lexer.filename.clone())
                    ], 
                    true
                ))
            }
        }
    }
}
