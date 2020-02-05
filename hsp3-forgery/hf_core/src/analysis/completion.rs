use crate::ast::*;
use crate::syntax::*;
use crate::token::*;
use std::rc::Rc;

pub(crate) fn get_completion_list(syntax_root: Rc<SyntaxRoot>, position: Position) -> Vec<String> {
    syntax_root
        .node()
        .descendant_elements()
        .filter_map(|e| AAssignStmt::cast(&SyntaxElement::cast_node(e)?))
        .flat_map(|assign_stmt| {
            assign_stmt
                .syntax()
                .child_tokens()
                .filter(|t| t.kind() == Token::Ident)
        })
        .map(|token| token.text().to_string())
        .collect()
}

pub(crate) struct SignatureHelp {
    pub(crate) params: Vec<String>,
    pub(crate) active_param_index: usize,
}

pub(crate) fn get_signature_help(
    syntax_root: Rc<SyntaxRoot>,
    position: Position,
) -> Option<SignatureHelp> {
    fn go_node(node: &SyntaxNode, p: Position, out: &mut Option<SignatureHelp>) -> bool {
        for child in node.child_nodes() {
            if !child.range().contains_loosely(p) {
                continue;
            }

            if go_node(&child, p, out) {
                return true;
            }

            let command_stmt = match ACommandStmt::cast(&child) {
                None => continue,
                Some(x) => x,
            };

            let mut arg_index = 0;

            for element in command_stmt.syntax().child_elements() {
                match element {
                    SyntaxElement::Token(token) => {
                        if token.kind() == Token::Ident {
                            // コマンド
                        } else {
                            continue;
                        }
                    }
                    SyntaxElement::Node(node) => match AArg::cast(&node) {
                        None => continue,
                        Some(arg) => {
                            arg_index += 1;

                            let syntax = arg.syntax();
                            if !syntax.range().contains_loosely(p) {
                                continue;
                            }

                            // 引数
                            if go_node(syntax, p, out) {
                                return true;
                            }

                            *out = Some(SignatureHelp {
                                params: vec!["x", "y"]
                                    .into_iter()
                                    .map(ToString::to_string)
                                    .collect(),
                                active_param_index: arg_index - 1,
                            });
                            return true;
                        }
                    },
                }
            }
        }

        false
    }

    let mut help = None;
    go_node(&syntax_root.node(), position, &mut help);
    help
}
