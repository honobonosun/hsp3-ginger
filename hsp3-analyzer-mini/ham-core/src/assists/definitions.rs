use super::{loc_to_location, to_loc};
use crate::{analysis::integrate::AWorkspaceAnalysis, lang_service::docs::Docs};
use lsp_types::{Location, Position, Url};

pub(crate) fn definitions(
    uri: Url,
    position: Position,
    docs: &Docs,
    wa: &mut AWorkspaceAnalysis,
) -> Option<Vec<Location>> {
    let loc = to_loc(&uri, position, docs)?;
    let (symbol, _) = wa.locate_symbol(loc.doc, loc.start())?;

    let mut locs = vec![];

    wa.get_symbol_defs(symbol, &mut locs);

    Some(
        locs.into_iter()
            .filter_map(|loc| loc_to_location(loc, docs))
            .collect(),
    )
}
