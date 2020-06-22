use super::{
    a_scope::{ADefFunc, ADefFuncData, AModule, AModuleData},
    a_symbol::{ASymbolData, ASymbolOutline},
    integrate::AWsScope,
    ADoc, ALoc, APos, AScope, ASymbol, ASymbolKind,
};
use crate::{
    parse::*,
    token::TokenKind,
    utils::{id::Id, rc_str::RcStr},
};
use std::mem::{replace, take};

pub(crate) type ACandidate = Id<ACandidateData>;

#[derive(Copy, Clone, Debug)]
pub(crate) enum ACandidateKind {
    Label,
    Command,
    VarOrArray,
    ArrayOrFunc,
}

#[derive(Debug)]
pub(crate) struct ACandidateData {
    kind: ACandidateKind,
    name: RcStr,
    loc: ALoc,
    scope: AScope,
}

#[derive(Copy, Clone)]
pub(crate) enum ASymbolOrCandidate {
    Symbol(ASymbol),
    DefCandidate(ACandidate),
    UseCandidate(ACandidate),
}

/// Analysis context.
#[derive(Default)]
struct Ax {
    eof_loc: ALoc,
    symbols: Vec<ASymbolData>,
    def_candidates: Vec<ACandidateData>,
    use_candidates: Vec<ACandidateData>,
    deffuncs: Vec<ADefFuncData>,
    deffunc_opt: Option<ADefFunc>,
    modules: Vec<AModuleData>,
    module_opt: Option<AModule>,
}

impl Ax {
    fn new() -> Self {
        Self::default()
    }

    fn current_scope(&self) -> AScope {
        AScope {
            deffunc_opt: self.deffunc_opt,
            module_opt: self.module_opt,
            is_global: false,
        }
    }

    fn add_symbol(
        &mut self,
        kind: ASymbolKind,
        token: &PToken,
        privacy: PPrivacy,
        definer: &PToken,
    ) -> ASymbol {
        let comments = definer
            .leading
            .iter()
            .filter_map(|t| {
                if t.kind == TokenKind::Comment && !str_is_ornament_comment(&t.text) {
                    Some(t.text.clone())
                } else {
                    None
                }
            })
            .collect();

        let scope = {
            let scope = self.current_scope();
            match privacy {
                PPrivacy::Global => AScope {
                    is_global: true,
                    ..scope
                },
                PPrivacy::Local => scope,
            }
        };

        let symbol_id = self.symbols.len();
        self.symbols.push(ASymbolData {
            kind,
            name: token.body.text.clone(),
            def_sites: vec![token.body.loc.clone()],
            use_sites: vec![],
            comments,
            scope,
        });
        ASymbol::from(symbol_id)
    }
}

/// 装飾コメント (// ---- とか) や空行など
fn str_is_ornament_comment(s: &str) -> bool {
    s.chars()
        .all(|c| c.is_control() || c.is_whitespace() || c.is_ascii_punctuation())
}

fn get_privacy_or_local(privacy_opt: &Option<(PPrivacy, PToken)>) -> PPrivacy {
    match privacy_opt {
        Some((privacy, _)) => *privacy,
        None => PPrivacy::Local,
    }
}

fn on_symbol_def(name: &PToken, kind: ACandidateKind, ax: &mut Ax) {
    ax.def_candidates.push(ACandidateData {
        kind,
        name: name.body.text.clone(),
        loc: name.body.loc.clone(),
        scope: ax.current_scope(),
    });
}

fn on_symbol_use(name: &PToken, kind: ACandidateKind, ax: &mut Ax) {
    ax.use_candidates.push(ACandidateData {
        kind,
        name: name.body.text.clone(),
        loc: name.body.loc.clone(),
        scope: ax.current_scope(),
    });
}

fn on_compound_def(compound: &PCompound, ax: &mut Ax) {
    match compound {
        PCompound::Name(name) => on_symbol_def(name, ACandidateKind::VarOrArray, ax),
        PCompound::Paren(PNameParen { name, args, .. }) => {
            on_symbol_def(name, ACandidateKind::ArrayOrFunc, ax);

            for arg in args {
                on_expr_opt(arg.expr_opt.as_ref(), ax);
            }
        }
        PCompound::Dots(PNameDot { name, args }) => {
            on_symbol_def(name, ACandidateKind::ArrayOrFunc, ax);

            for arg in args {
                on_expr_opt(arg.expr_opt.as_ref(), ax);
            }
        }
    }
}

fn on_compound_use(compound: &PCompound, ax: &mut Ax) {
    match compound {
        PCompound::Name(name) => on_symbol_use(name, ACandidateKind::VarOrArray, ax),
        PCompound::Paren(PNameParen { name, args, .. }) => {
            on_symbol_use(name, ACandidateKind::ArrayOrFunc, ax);

            for arg in args {
                on_expr_opt(arg.expr_opt.as_ref(), ax);
            }
        }
        PCompound::Dots(PNameDot { name, args }) => {
            on_symbol_use(name, ACandidateKind::ArrayOrFunc, ax);

            for arg in args {
                on_expr_opt(arg.expr_opt.as_ref(), ax);
            }
        }
    }
}

fn on_expr(expr: &PExpr, ax: &mut Ax) {
    match expr {
        PExpr::Literal(_) => {}
        PExpr::Label(PLabel { star: _, name_opt }) => {
            if let Some(name) = name_opt {
                on_symbol_use(name, ACandidateKind::Label, ax);
            }
        }
        PExpr::Compound(compound) => on_compound_use(compound, ax),
        PExpr::Group(PGroupExpr { body_opt, .. }) => on_expr_opt(body_opt.as_deref(), ax),
        PExpr::Prefix(PPrefixExpr { prefix: _, arg_opt }) => on_expr_opt(arg_opt.as_deref(), ax),
        PExpr::Infix(PInfixExpr {
            infix: _,
            left,
            right_opt,
        }) => {
            on_expr(left, ax);
            on_expr_opt(right_opt.as_deref(), ax);
        }
    }
}

fn on_expr_opt(expr_opt: Option<&PExpr>, ax: &mut Ax) {
    if let Some(expr) = expr_opt {
        on_expr(expr, ax);
    }
}

fn on_args(args: &[PArg], ax: &mut Ax) {
    for arg in args {
        on_expr_opt(arg.expr_opt.as_ref(), ax);
    }
}

fn on_stmt(stmt: &PStmt, ax: &mut Ax) {
    match stmt {
        PStmt::Label(PLabel { star, name_opt }) => {
            if let Some(name) = name_opt {
                ax.add_symbol(ASymbolKind::Label, name, PPrivacy::Local, star);
            }
        }
        PStmt::Assign(PAssignStmt {
            left,
            op_opt: op,
            args,
        }) => {
            // FIXME: def/use は演算子の種類による
            // on_compound_def(left, ax);
            on_args(args, ax);
        }
        PStmt::Command(PCommandStmt {
            command,
            jump_modifier_opt: _,
            args,
        }) => {
            on_symbol_def(&command, ACandidateKind::Command, ax);
            on_args(&args, ax);
        }
        PStmt::Invoke(PInvokeStmt {
            left,
            arrow_opt: _,
            method_opt,
            args,
        }) => {
            on_compound_use(left, ax);
            on_expr_opt(method_opt.as_ref(), ax);
            on_args(&args, ax);
        }
        PStmt::Const(PConstStmt {
            hash,
            privacy_opt,
            name_opt,
            ..
        }) => {
            if let Some(name) = name_opt {
                let privacy = get_privacy_or_local(privacy_opt);
                ax.add_symbol(ASymbolKind::Const, name, privacy, hash);
            }
        }
        PStmt::Define(PDefineStmt {
            hash,
            privacy_opt,
            name_opt,
            ..
        }) => {
            // FIXME: ctype などをみて kind を決定する。

            if let Some(name) = name_opt {
                let privacy = get_privacy_or_local(privacy_opt);
                ax.add_symbol(ASymbolKind::Const, name, privacy, hash);
            }
        }
        PStmt::Enum(PEnumStmt {
            hash,
            privacy_opt,
            name_opt,
            ..
        }) => {
            if let Some(name) = name_opt {
                let privacy = get_privacy_or_local(privacy_opt);
                ax.add_symbol(ASymbolKind::Const, name, privacy, hash);
            }
        }
        PStmt::DefFunc(PDefFuncStmt {
            hash,
            keyword,
            kind,
            privacy_opt,
            name_opt,
            onexit_opt: _,
            params,
            stmts,
            behind,
            ..
        }) => {
            let deffunc = ADefFunc::new(ax.deffuncs.len());
            ax.deffuncs.push(ADefFuncData {
                kind: *kind,
                symbol_opt: None,
                keyword_loc: keyword.body.loc.clone(),
                content_loc: hash.body.loc.unite(behind),
            });

            if let Some(name) = name_opt {
                let privacy = match privacy_opt {
                    Some((privacy, _)) => *privacy,
                    None => PPrivacy::Global,
                };

                let symbol = ax.add_symbol(ASymbolKind::CommandOrFunc, name, privacy, hash);
                ax.deffuncs[deffunc.get()].symbol_opt = Some(symbol);
            }

            let parent_deffunc = replace(&mut ax.deffunc_opt, Some(deffunc));

            for param in params {
                if let Some(name) = &param.name_opt {
                    ax.add_symbol(ASymbolKind::Param, name, PPrivacy::Local, hash);
                }
            }

            for stmt in stmts {
                on_stmt(stmt, ax);
            }

            ax.deffunc_opt = parent_deffunc;
        }
        PStmt::UseLib(_) => {}
        PStmt::LibFunc(PLibFuncStmt {
            hash,
            privacy_opt,
            name_opt,
            onexit_opt,
            ..
        }) => {
            if let Some(name) = name_opt {
                if onexit_opt.is_none() {
                    let privacy = get_privacy_or_local(privacy_opt);
                    ax.add_symbol(ASymbolKind::CommandOrFunc, name, privacy, hash);
                }
            }
        }
        PStmt::UseCom(PUseComStmt {
            hash,
            privacy_opt,
            name_opt,
            ..
        }) => {
            if let Some(name) = name_opt {
                let privacy = get_privacy_or_local(privacy_opt);
                ax.add_symbol(ASymbolKind::Const, name, privacy, hash);
            }
        }
        PStmt::ComFunc(PComFuncStmt {
            hash,
            privacy_opt,
            name_opt,
            ..
        }) => {
            if let Some(name) = name_opt {
                let privacy = match privacy_opt {
                    Some((privacy, _)) => *privacy,
                    None => PPrivacy::Global,
                };
                ax.add_symbol(ASymbolKind::Command, name, privacy, hash);
            }
        }
        PStmt::RegCmd(_) => {}
        PStmt::Cmd(PCmdStmt {
            hash,
            privacy_opt,
            name_opt,
            ..
        }) => {
            if let Some(name) = name_opt {
                let privacy = get_privacy_or_local(privacy_opt);
                ax.add_symbol(ASymbolKind::CommandOrFuncOrVar, name, privacy, hash);
            }
        }
        PStmt::Module(PModuleStmt {
            hash,
            keyword,
            name_opt,
            fields,
            stmts,
            behind,
            ..
        }) => {
            let module = AModule::from(ax.modules.len());
            ax.modules.push(AModuleData {
                symbol_opt: None,
                keyword_loc: keyword.body.loc.clone(),
                content_loc: hash.body.loc.unite(&behind),
            });

            let parent_deffunc_opt = take(&mut ax.deffunc_opt);
            let parent_module_opt = replace(&mut ax.module_opt, Some(module));

            if let Some(name) = name_opt {
                let symbol_opt = match name.kind() {
                    TokenKind::Ident => {
                        Some(ax.add_symbol(ASymbolKind::Module, name, PPrivacy::Global, hash))
                    }
                    TokenKind::Str => {
                        // FIXME: 識別子として有効な文字列ならシンボルとして登録できる。
                        None
                    }
                    _ => None,
                };

                ax.modules[module.get()].symbol_opt = symbol_opt;
            }

            for field in fields.iter().filter_map(|param| param.name_opt.as_ref()) {
                ax.add_symbol(ASymbolKind::Field, field, PPrivacy::Local, field);
            }

            for stmt in stmts {
                on_stmt(stmt, ax);
            }

            ax.deffunc_opt = parent_deffunc_opt;
            ax.module_opt = parent_module_opt;
        }
        PStmt::Global(_) => {}
        PStmt::Include(_) => {}
        PStmt::UnknownPreProc(_) => {}
    }
}

#[derive(Debug, Default)]
pub(crate) struct AAnalysis {
    symbols: Vec<ASymbolData>,
    def_candidates: Vec<ACandidateData>,
    use_candidates: Vec<ACandidateData>,
    deffuncs: Vec<ADefFuncData>,
    modules: Vec<AModuleData>,
}

impl AAnalysis {
    pub(crate) fn locate_symbol(&self, loc: ALoc) -> Option<ASymbolOrCandidate> {
        None
    }

    pub(crate) fn collect_nonlocal_symbols(
        &self,
        doc: ADoc,
        symbols: &mut Vec<(ASymbol, ASymbolKind, AWsScope)>,
    ) {
        for (i, symbol_data) in self.symbols.iter().enumerate() {
            let ws_scope = AWsScope::from_scope(doc, &symbol_data.scope);
            if ws_scope.is_local_to_file() {
                continue;
            }

            let symbol = ASymbol::new(i);
            symbols.push((symbol, symbol_data.kind, ws_scope));
        }
    }
}

pub(crate) fn analyze(root: &PRoot) -> AAnalysis {
    let mut ax = Ax::new();
    ax.eof_loc = root.eof.behind();

    for stmt in &root.stmts {
        on_stmt(stmt, &mut ax);
    }

    AAnalysis {
        symbols: ax.symbols,
        def_candidates: ax.def_candidates,
        use_candidates: ax.use_candidates,
        deffuncs: ax.deffuncs,
        modules: ax.modules,
    }
}
