use crate::parser::ast;
use crate::lexer;
use crate::logger::logger::{Error, Logger};
use crate::parser::{ ast::{ Node }, parser::Parser, custom_syntax::Impl, custom_syntax };
use std::collections::HashMap;
use crate::parser::gen_utils as g;

pub fn generate_statement_run_parser() -> custom_syntax::Custom {
    // Note we want to keep the pattern the same as the way we parsed it.
    let mut rule: HashMap<ast::NameID, ast::Node> = HashMap::new();
    
    rule.insert(
        // expr is the thing the metasyntax looks for, don't touch this
        // (has nothing to do with the non terminal below)
        g::new_name_id("expr"), 

        // Parse a block
        ast::Node::NonTerminal(
            g::new_non_terminal(
                g::new_dollar_sign("pattern_rules"), // this is the `$pattern_rules`
                g::new_production_grammar_expr(g::new_namespace_production(g::new_namespace(vec!["syntax", "block"])))
            )
        )
    );

    custom_syntax::Custom {
        custom_type: g::new_namespace(vec!["syntax", "pattern", "pattern"]),

        values: rule,

        // syntax::pattern::parse
        name: g::new_namespace(vec!["syntax", "statement", "run"]),
        pos: g::gp(),

        rep: "pattern std definition".to_string(),

        scope: ast::Scope::Outer,
    }
}

impl<'a> Parser<'a> {
    // Should return impl with patterns complete, that way the std_pattern_pattern can use it and parse it.
    pub fn std_statement_parse(parser: &'a mut Parser<'a>) -> Result<ast::Node, Error> {
        let position = parser.lexer.get_pos();
        let mut items: Vec<Node> = Vec::new();
        let mut errors: Vec<Error> = Vec::new();
        
        parser.next(lexer::TokenType::LCP, position, false)?;

        loop {
            if parser.lexer.peek()?.token == lexer::TokenType::RCP {
                break
            }
            let fail;
            match parser.parse_non_terminal() {
                Ok(non_term) => { fail = false; items.push(ast::Node::NonTerminal(non_term)) },
                Err(e) => { 
                    errors.push(e);
                    match parser.parse_terminal() {
                        Ok(term) => { fail = false; items.push(ast::Node::Terminal(term)) },
                        Err(e) => { fail = true; errors.push(e) }
                    }
                }
            }

            if fail {
                return Err( Logger::longest(errors) );
            }

            if parser.lexer.peek()?.token == lexer::TokenType::COMMA {
                parser.lexer.advance()?;
            } else {
                break
            }
        }

        parser.next(lexer::TokenType::RCP, position, false)?;

        Ok(ast::Node::Nodes( ast::Nodes {
            nodes: items,
            pos: parser.position(position)
        }))
    }

    // Should return impl with patterns complete
    pub fn std_statement_run(parser: &'a mut Parser<'a>) -> Result<ast::Node, Error> {
        let position = parser.lexer.get_pos();

        let mut items: Vec<Node> = Vec::new();
        for node in parser.block()?.nodes {
            items.push(node.into_node());
        }

        Ok(ast::Node::Nodes( ast::Nodes {
            nodes: items,
            pos: parser.position(position)
        }))
    }

    // Returns impl object, generates parse and replace functions
    pub fn std_statement_statement(parser: &mut Parser<'a>, impl_curr: &'a Impl) -> Result<&'a Impl, Error> {
        Ok(impl_curr)
    }
}
