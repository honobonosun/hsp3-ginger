use super::{
    a_scope::{ADefFunc, AModule},
    analyze::AAnalysis,
    ADoc, ALoc, APos, AScope, ASymbol,
};
use crate::utils::rc_str::RcStr;
use std::{
    collections::{HashMap, HashSet},
    rc::Rc,
};

#[derive(Default)]
pub(crate) struct AWsSymbol {
    doc: ADoc,
    symbol: ASymbol,
}

pub(crate) enum AWsScope {
    Global,
    TopLevelStatic,
    TopLevelDefFunc {
        doc: ADoc,
        deffunc: ADefFunc,
    },
    ModuleStatic {
        doc: ADoc,
        module: AModule,
    },
    ModuleDefFunc {
        doc: ADoc,
        module: AModule,
        deffunc: ADefFunc,
    },
}

impl AWsScope {
    pub(crate) fn is_local_to_file(&self) -> bool {
        match self {
            AWsScope::Global | AWsScope::TopLevelStatic => true,
            _ => false,
        }
    }

    pub(crate) fn from_scope(doc: ADoc, scope: &AScope) -> AWsScope {
        match scope {
            AScope {
                is_global: true, ..
            } => AWsScope::Global,
            AScope {
                module_opt: None,
                deffunc_opt: None,
                ..
            } => AWsScope::TopLevelStatic,
            AScope {
                module_opt: None,
                deffunc_opt: Some(deffunc),
                ..
            } => AWsScope::TopLevelDefFunc {
                doc,
                deffunc: *deffunc,
            },
            AScope {
                module_opt: Some(module),
                deffunc_opt: None,
                ..
            } => AWsScope::ModuleStatic {
                doc,
                module: *module,
            },
            AScope {
                module_opt: Some(module),
                deffunc_opt: Some(deffunc),
                ..
            } => AWsScope::ModuleDefFunc {
                doc,
                module: *module,
                deffunc: *deffunc,
            },
        }
    }
}

pub(crate) struct AWorkspaceAnalysis {
    pub(crate) dirty_docs: HashSet<ADoc>,
    pub(crate) doc_texts: HashMap<ADoc, RcStr>,
    pub(crate) doc_analysises: HashMap<ADoc, AAnalysis>,
}

impl AWorkspaceAnalysis {
    // pub(crate) fn add_hs_symbols(&mut self, doc: ADoc, hs_symbols: Vec<Rc<Symbol>>) {
    // self.is_dirty = true;
    // self.docs.insert(
    //     doc,
    //     DocSem {
    //         doc,
    //         text: "".to_string().into(),
    //         lines: vec![],
    //         line_count: 0,
    //         module_ranges: HashMap::new(),
    //         command_ranges: HashMap::new(),
    //         pp_symbols: hs_symbols
    //             .into_iter()
    //             .map(|symbol| (symbol.symbol_id, symbol))
    //             .collect(),
    //         pp_symbol_defs: HashMap::new(),
    //     },
    // );
    // }

    pub(crate) fn update_doc(&mut self, doc: ADoc, text: RcStr) {
        self.dirty_docs.insert(doc);
        self.doc_texts.insert(doc, text);
        self.doc_analysises.remove(&doc);
    }

    pub(crate) fn close_doc(&mut self, doc: ADoc) {
        self.dirty_docs.insert(doc);
        self.doc_texts.remove(&doc);
        self.doc_analysises.remove(&doc);
    }

    fn compute(&mut self) {
        for doc in self.dirty_docs.drain() {
            let text = match self.doc_texts.get(&doc) {
                Some(text) => text,
                None => continue,
            };

            let analysis = {
                let tokens = crate::token::tokenize(doc, text.clone());
                let tokens = crate::parse::PToken::from_tokens(tokens);
                let root = crate::parse::parse_root(tokens);
                super::analyze::analyze(&root)
            };
            self.doc_analysises.insert(doc, analysis);
        }

        for (&doc, analysis) in &self.doc_analysises {
            analysis;
        }
    }

    pub(crate) fn get_symbol_list(&mut self, doc: ADoc, pos: APos, symbols: &mut Vec<AWsSymbol>) {
        self.compute();
    }

    pub(crate) fn locate_symbol(&mut self, doc: ADoc, pos: APos) -> Option<(AWsSymbol, ALoc)> {
        self.compute();

        let analysis = self.doc_analysises.get(&doc)?;

        None
    }

    pub(crate) fn get_symbol_defs(&mut self, symbol: AWsSymbol, locs: &mut Vec<ALoc>) {
        self.compute();
    }

    pub(crate) fn get_symbol_uses(&mut self, symbol_id: usize, locs: &mut Vec<ALoc>) {
        self.compute();
    }
}
