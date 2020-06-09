use crate::{
    canonical_uri::CanonicalUri,
    docs::Docs,
    sem::{self, ProjectSem},
    syntax,
};
use lsp_types::{CompletionItem, CompletionItemKind, CompletionList, Documentation, Position, Url};

fn to_loc(uri: &Url, position: Position, docs: &Docs) -> Option<syntax::Loc> {
    let uri = CanonicalUri::from_url(uri)?;
    let doc = docs.find_by_uri(&uri)?;

    // FIXME: position は UTF-16 ベース、pos は UTF-8 ベースなので、マルチバイト文字が含まれている場合は変換が必要
    let pos = syntax::Pos {
        row: position.line as usize,
        col: position.character as usize,
    };

    Some(syntax::Loc {
        doc,
        start: pos,
        end: pos,
    })
}

pub(crate) fn incomplete_completion_list() -> CompletionList {
    CompletionList {
        is_incomplete: true,
        items: vec![],
    }
}

pub(crate) fn completion(
    uri: &Url,
    position: Position,
    docs: &Docs,
    sem: &mut ProjectSem,
) -> Option<CompletionList> {
    let mut items = vec![];
    let mut symbols = vec![];

    let loc = to_loc(uri, position, docs)?;

    sem.get_symbol_list(loc.doc, loc.start, &mut symbols);

    for symbol in symbols {
        let kind = match symbol.kind {
            sem::SymbolKind::Macro { ctype: true, .. }
            | sem::SymbolKind::Command { ctype: true, .. } => CompletionItemKind::Function,
            sem::SymbolKind::Label | sem::SymbolKind::Macro { .. } => CompletionItemKind::Constant,
            sem::SymbolKind::Command { .. } => CompletionItemKind::Method, // :thinking_face:
            sem::SymbolKind::Param { .. } | sem::SymbolKind::Static => CompletionItemKind::Variable,
        };

        items.push(CompletionItem {
            kind: Some(kind),
            label: symbol.name.to_string(),
            detail: symbol.details.description.as_ref().map(|s| s.to_string()),
            documentation: if symbol.details.documentation.is_empty() {
                None
            } else {
                Some(Documentation::String(
                    symbol.details.documentation.join("\r\n\r\n"),
                ))
            },
            filter_text: if symbol.name.as_str().starts_with("#") {
                Some(symbol.name.as_str().chars().skip(1).collect::<String>())
            } else {
                None
            },
            data: Some(serde_json::to_value(&symbol.symbol_id).unwrap()),
            ..CompletionItem::default()
        })
    }

    Some(CompletionList {
        is_incomplete: false,
        items,
    })
}
