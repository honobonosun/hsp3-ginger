use super::loc_to_range;
use crate::{
    analysis::integrate::AWorkspaceAnalysis, assists::from_document_position,
    lang_service::docs::Docs,
};
use lsp_types::{DocumentHighlight, DocumentHighlightKind, Position, Url};

pub(crate) fn document_highlight(
    uri: Url,
    position: Position,
    docs: &Docs,
    wa: &mut AWorkspaceAnalysis,
) -> Option<Vec<DocumentHighlight>> {
    let (doc, pos) = from_document_position(&uri, position, docs)?;
    let (ws_symbol, _) = wa.locate_symbol(doc, pos)?;

    let mut locs = vec![];
    let mut highlights = vec![];

    wa.collect_symbol_defs(ws_symbol, &mut locs);
    highlights.extend(
        locs.drain(..)
            .map(|loc| (DocumentHighlightKind::Write, loc)),
    );

    wa.collect_symbol_uses(ws_symbol, &mut locs);
    highlights.extend(locs.drain(..).map(|loc| (DocumentHighlightKind::Read, loc)));

    highlights.retain(|(_, loc)| loc.doc == doc);

    Some(
        highlights
            .into_iter()
            .map(|(kind, loc)| DocumentHighlight {
                kind: Some(kind),
                range: loc_to_range(loc),
            })
            .collect(),
    )
}
