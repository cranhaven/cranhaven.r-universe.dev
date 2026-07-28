#' "Serialize" a data frame or (pre)registration specification
#'
#' When exporting a (pre)registration specification to YAML or JSON, the most
#' human-readable format differs from the way data frames are comprised of
#' lists. Data frames are lists that are bound together as columns; and so,
#' when saving a data frame to YAML or JSON, the data in each column is
#' combined (e.g. first all item identifiers, then all item labels, then all
#' item descriptions, etc). However, for humans, it makes more sense to
#' have all data belonging to the same item close together. These functions
#' do that processing.
#'
#' @param x For `serialize_df`, a data frame; for
#' `structure_for_serialization`, the (pre)registration specification
#' @param idCol If not `NULL`, the name of a column in the data frame to use
#' as names for the lists.
#'
#' @return The restructured list
#' @rdname serialization
#' @export
structure_for_serialization <- function(x) {

  if (!(inherits(x, "preregr") && inherits(x, "preregr_spec"))) {
    stop("As `x`, you have to pass an initialized {preregr} object (see\n\n",
         "  ?preregr::prereg_initialize()\n\n",
         "for more information).");
  }

  if ("form" %in% names(x)) {
    serializedForm <-
      list(
        instructions = serialize_df(x$form$instructions, idCol = "heading"),
        metadata = serialize_df(x$form$metadata, idCol = "field"),
        items = serialize_df(x$form$items, idCol = "item_id"),
        sections = serialize_df(x$form$sections, idCol = "section_id"),
        valueTemplates = serialize_df(x$form$valueTemplates, idCol = "identifier")
      );
  } else {
    serializedForm <- NULL;
  }

  if ("jstf" %in% names(x)) {
    jstfToSave <- x$jstf
  } else {
    jstfToSave <- NULL;
  }

  if ("config" %in% names(x)) {
    configToSave <- x$config
  } else {
    configToSave <- NULL;
  }

  res <- list(
    specs = x$specs,
    jstf = jstfToSave,
    form = serializedForm,
    config = configToSave);

  return(res);

}
