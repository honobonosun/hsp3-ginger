use crate::{
    parse::{PDefFuncKind, PToken},
    source::{DocId, Loc},
    token::TokenKind,
    utils::{id::Id, rc_str::RcStr},
};

#[derive(Clone, Default, PartialEq, Eq, Hash)]
pub(crate) struct ALocalScope {
    pub(crate) module_opt: Option<AModule>,

    /// `#deffunc` 系命令の下の部分。(このスコープに属して定義されるのはパラメータだけ。)
    pub(crate) deffunc_opt: Option<ADefFunc>,
}

impl ALocalScope {
    pub(crate) fn is_public(&self) -> bool {
        self.module_opt.is_none() && self.deffunc_opt.is_none()
    }

    pub(crate) fn is_outside_module(&self) -> bool {
        self.module_opt.is_none()
    }

    /// スコープselfで定義されたシンボルが、スコープotherにおいてみえるか？
    pub(crate) fn is_visible_to(&self, other: &ALocalScope) -> bool {
        // 異なるモジュールに定義されたものはみえない。
        // deffuncの中で定義されたものは、その中でしかみえないが、外で定義されたものは中からもみえる。
        self.module_opt == other.module_opt
            && (self.deffunc_opt.is_none() || self.deffunc_opt == other.deffunc_opt)
    }
}

#[derive(Clone)]
pub(crate) enum AScope {
    Global,
    Local(ALocalScope),
}

pub(crate) type ADefFunc = Id<ADefFuncData>;

#[derive(Debug)]
pub(crate) struct ADefFuncData {
    pub(crate) kind: PDefFuncKind,
    pub(crate) name_opt: Option<RcStr>,
    pub(crate) keyword_loc: Loc,
    pub(crate) content_loc: Loc,
}

#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub(crate) struct AModule {
    pub(crate) doc: DocId,
    pub(crate) index: usize,
    pub(crate) name_opt: Option<RcStr>,
}

impl AModule {
    pub(crate) fn new(doc: DocId, index: &mut usize, name_opt: &Option<PToken>) -> AModule {
        // FIXME: 識別子として有効な文字列なら名前として使える。
        let name_opt = match name_opt {
            Some(token) if token.kind() == TokenKind::Ident => Some(token.body.text.clone()),
            _ => None,
        };

        let module = AModule {
            doc,
            index: *index,
            name_opt,
        };
        *index += 1;

        module
    }
}

pub(crate) struct AModuleData {
    #[allow(unused)]
    pub(crate) keyword_loc: Loc,
    pub(crate) content_loc: Loc,
}
