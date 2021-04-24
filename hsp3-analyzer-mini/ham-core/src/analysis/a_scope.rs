use super::ALoc;
use crate::{
    parse::PDefFuncKind,
    utils::{id::Id, rc_str::RcStr},
};

#[derive(Copy, Clone, Debug, Default, PartialEq)]
pub(crate) struct ALocalScope {
    pub(crate) module_opt: Option<AModule>,
    pub(crate) deffunc_opt: Option<ADefFunc>,
}

impl ALocalScope {
    pub(crate) fn is_outside_module(self) -> bool {
        self.module_opt.is_none()
    }

    /// スコープselfで定義されたシンボルが、スコープotherにおいてみえるか？
    pub(crate) fn is_visible_to(self, other: ALocalScope) -> bool {
        // 異なるモジュールに定義されたものはみえない。
        // deffuncの中で定義されたものは、その中でしかみえないが、外で定義されたものは中からもみえる。
        self.module_opt == other.module_opt
            && (self.deffunc_opt.is_none() || self.deffunc_opt == other.deffunc_opt)
    }
}

#[derive(Copy, Clone, Debug)]
pub(crate) enum AScope {
    Global,
    Local(ALocalScope),
}

pub(crate) type ADefFunc = Id<ADefFuncData>;

#[derive(Debug)]
pub(crate) struct ADefFuncData {
    pub(crate) kind: PDefFuncKind,
    pub(crate) name_opt: Option<RcStr>,
    pub(crate) keyword_loc: ALoc,
    pub(crate) content_loc: ALoc,
}

pub(crate) type AModule = Id<AModuleData>;

#[derive(Debug, Default)]
pub(crate) struct AModuleData {
    pub(crate) name_opt: Option<RcStr>,
    pub(crate) keyword_loc: ALoc,
    pub(crate) content_loc: ALoc,
}
