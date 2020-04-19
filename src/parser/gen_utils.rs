use crate::parser::custom_syntax;
use crate::parser::ast;
use crate::helpers;

pub fn new_name_id(name: &str) -> ast::NameID {
    ast::NameID {
        value: name.to_string(), 
        pos: gp()
    }
}

pub fn new_string_literal(string: &str) -> ast::StringLiteral {
    ast::StringLiteral {
        value: string.to_string(),
        pos: gp()
    }
}

pub fn new_dollar_sign(name: &str) -> custom_syntax::DollarID {
    custom_syntax::DollarID {
        value: new_name_id(name),
        pos: gp()
    }
}

pub fn new_non_terminal(name: custom_syntax::DollarID, grammar: custom_syntax::GrammarExpr) -> custom_syntax::NonTerminal {
    custom_syntax::NonTerminal {
        name,
        grammar,
        pos: gp()
    }
}

pub fn new_terminal(content: &str) -> custom_syntax::Terminal {
    custom_syntax::Terminal {
        contents: new_string_literal(content),
        pos: gp()
    }
}

pub fn new_or_grammar(left: custom_syntax::GrammarExpr, right: custom_syntax::GrammarExpr) -> custom_syntax::GrammarExpr {
    custom_syntax::GrammarExpr::Or(
        Box::new(custom_syntax::Or {
            left,
            right,
            pos: gp()
        })
    )
}

pub fn new_plus_grammar(non_terminal: custom_syntax::GrammarExpr, trailing: custom_syntax::TrailingElement) -> custom_syntax::GrammarExpr {
    custom_syntax::GrammarExpr::Plus(
        Box::new(custom_syntax::Plus {
            non_terminal,
            trailing,
            pos: gp()
        })
    )
}

pub fn new_star_grammar(non_terminal: custom_syntax::GrammarExpr, trailing: custom_syntax::TrailingElement) -> custom_syntax::GrammarExpr {
    custom_syntax::GrammarExpr::Star(
        Box::new(custom_syntax::Star {
            non_terminal,
            trailing,
            pos: gp()
        })
    )
}

pub fn new_nodes(nodes: Vec<ast::Node>) -> ast::Nodes {
    ast::Nodes {
        nodes,
        pos: gp()
    }
}

pub fn new_namespace(items: Vec<&str>) -> ast::Namespace {
    ast::Namespace {
        scopes: items.iter().map(|x| new_name_id(&x)).collect(),
        pos: gp()
    }
}

pub fn new_namespace_production(namespace: ast::Namespace) -> custom_syntax::Production {
    custom_syntax::Production::NonTerminal(namespace)
}

pub fn new_trailing_grammar(separator: &str, trailing: bool) -> custom_syntax::TrailingElement {
    custom_syntax::TrailingElement {
        separator: separator.to_string(),
        trailing,
        pos: gp()
    }
}

pub fn new_production_grammar_expr(production: custom_syntax::Production) -> custom_syntax::GrammarExpr {
    custom_syntax::GrammarExpr::Production(Box::new(production))
}

// Generate placeholder position
pub fn gp() -> helpers::Pos {
    helpers::Pos::new(0, 0)
}

