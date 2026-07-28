#' Show which items in a (pre)registration have been completed
#'
#' This function shows which items in a (pre)registration, or in one or more
#' sections, have been completed - or, more accurately, contain at least some
#' content that is different from the default content.
#'
#' @param x The (pre)registration object (as produced by a call to
#' [preregr::prereg_initialize()]).
#' @param section The section(s) to show; pass `NULL` (the default) to show
#' everything.
#'
#' @return x, invisibly
#' @export
#'
#' @examples ### Load an example (pre)registration specification
#' data("examplePrereg_1", package = "preregr");
#'
#' ### Show which items were completed
#' examplePrereg_1 |>
#'   preregr::prereg_show_item_completion(
#'     section="metadata"
#'   );
prereg_show_item_completion <- function(x,
                                        section = NULL) {

  if (!(inherits(x, "preregr") && inherits(x, "preregr_spec"))) {
    stop("As `x`, you have to pass an initialized {preregr} object (see\n\n",
         "  ?preregr::prereg_initialize()\n\n",
         "for more information).");
  }

  cli::cli_h1("Items in (pre)registration specification");

  cli::cli_text();

  cli::cli_alert_info(
    paste0(
      cli::col_cyan("Form: "),
      x$form$metadata[x$form$metadata$field == "title", "content"]
    )
  );
  cli::cli_alert_info(
    paste0(
      cli::col_cyan("Version: "),
      x$form$metadata[x$form$metadata$field == "version", "content"]
    )
  );

  if (is.null(section)) {
    sectionsToShow <- x$form$sections$section_id;
  } else {
    sectionsToShow <-
      intersect(
        x$form$sections$section_id,
        section
      );
  }

  for (section in sectionsToShow) {

    cli::cli_h2(
      paste(
        "Section:",
        x$form$sections[
          x$form$sections$section_id==section,
          "section_label"]
      )
    );

    item_ids <- x$form$items$item_id[x$form$items$section_id == section];
    item_labels <- x$form$items$item_label[x$form$items$section_id == section];
    names(item_labels) <- item_ids;

    for (currentItemId in item_ids) {
      if (x$specs[[currentItemId]] == x$config$initialText) {
        cli::cli_alert_danger(
          paste0(item_labels[currentItemId],
                 cli::col_silver(" [", currentItemId, "]"))
        );
      } else {
        cli::cli_alert_success(
          paste0(item_labels[currentItemId],
                 cli::col_silver(" [", currentItemId, "]"))
        );
      }
    }

  }

  return(invisible(x));

}
