#' Show a (pre)registration form
#'
#' This function shows (parts of) a (pre)registration form.
#'
#' @param x The (pre)registration form (as produced by a call to
#' [preregr::form_create()]).
#' @param section The section(s) to show; pass `NULL` (the default) to show
#' everything.
#'
#' @return x, invisibly
#' @export
#'
#' @examples ### An empty form
#' preregr::form_create(
#'   "Example form",
#'   version = "1"
#' ) |>
#'   preregr::form_show();
#'
#' ### A complete form
#' preregr::prereg_initialize("inclSysRev_v0_92") |>
#'   preregr::form_show();
form_show <- function(x,
                      section = NULL) {

  x <- retrieve_form(x);

  cli::cli_h1("(Pre)registration form");

  title <- x$metadata$content[x$metadata$field=="title"];
  author <- x$metadata$content[x$metadata$field=="author"];
  date <- x$metadata$content[x$metadata$field=="date"];
  nrOfItems <- nrow(x$items);
  nrOfSections <- nrow(x$sections);

  cli::cli_text();

  cli::cli_alert_info(paste0(cli::col_cyan("Title:"), " {title}"));
  cli::cli_alert_info(paste0(cli::col_cyan("Author:"), " {author}"));
  cli::cli_alert_info(paste0(cli::col_cyan("Date:"), " {date}"));

  if (nrow(x$instructions) > 0) {

    cli::cli_h1("Instructions");

    for (i in 1:nrow(x$instructions)) {

      cli::cli_h2(x$instructions[i, "heading"]);

      cli::cli_text(x$instructions[i, "description"]);

    }

  }

  if (is.null(section)) {
    sectionsToShow <- x$sections$section_id;
  } else {
    sectionsToShow <-
      intersect(
        x$sections$section_id,
        section
      );
  }

  cli::cli_h1("Sections and items");

  for (section in sectionsToShow) {

    cli::cli_h2(
      paste(
        "Section:",
        x$sections[
          x$sections$section_id==section,
          "section_label"]
      )
    );

    item_ids <- x$items$item_id[x$items$section_id == section];
    item_labels <- x$items$item_label[x$items$section_id == section];
    item_descriptions <- x$items$item_description[x$items$section_id == section];
    names(item_labels) <- item_ids;
    names(item_descriptions) <- item_ids;

    for (currentItemId in item_ids) {
      cli::cli_alert_info(
        paste0(cli::col_cyan(item_labels[currentItemId]),
               cli::col_silver(" [", currentItemId, "]"),
               cli::col_cyan(": "),
               item_descriptions[currentItemId])
      );
      cli::cli_text();
    }

  }

  return(invisible(x));

}
