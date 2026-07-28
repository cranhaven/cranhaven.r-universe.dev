#' Show the next item to specify content for
#'
#' This function shows the next item (or items) in a (pre)registration
#' for which to specify content (searching through all sections or through
#' a selection of sections).
#'
#' @param x The (pre)registration object (as produced by a call to
#' [preregr::prereg_initialize()]).
#' @param section The section(s) to search; pass `NULL` (the default) to show
#' everything.
#' @param nrOfItems The number of items to complete to show.
#'
#' @return x, invisibly
#' @export
#'
#' @examples ### Load an example (pre)registration specification
#' data("examplePrereg_1", package = "preregr");
#'
#' ### Check next item
#' examplePrereg_1 |>
#'   preregr::prereg_next_item();
#'
#' ### Specify content for this item
#' examplePrereg_1 <-
#'   preregr::prereg_specify(
#'     examplePrereg_1,
#'     funding = paste0(
#'       "No funding. There's never any ",
#'       "funding for this kind of stuff."
#'     )
#'   );
#'
#' ### Get the next three items
#' preregr::prereg_next_item(
#'   examplePrereg_1,
#'   nrOfItems = 3
#' );
#'
prereg_next_item <- function(x,
                             nrOfItems = 1,
                             section = NULL) {

  objectName <- deparse(substitute(x));

  if (!(inherits(x, "preregr") && inherits(x, "preregr_spec"))) {
    stop("As `x`, you have to pass an initialized {preregr} object (see\n\n",
         "  ?preregr::prereg_initialize()\n\n",
         "for more information).");
  }

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

  cli::cli_text();

  if (nrOfItems == 1) {
    introText <- paste0("The next item to complete is:");
  } else if (nrOfItems > 1) {
    introText <- paste0("The next ", nrOfItems, " items to complete are:");
  } else {
    stop("For `nrOfItems`, specify a positive integer!");
  }

  cli::cli_text(introText);

  cli::cli_text();

  if (is.null(section)) {
    sectionsToShow <- x$form$sections$section_id;
  } else if (any(section %in% x$form$sections$section_id)) {
    sectionsToShow <-
      intersect(
        x$form$sections$section_id,
        section
      );
  } else {
    stop("None of the section(s) you specified exists in the form! ",
         "You specified ", vecTxtQ(section),
         ", and the form has sections: ",
         vecTxtQ(unique(x$form$sections$section_id)), ".");
  }

  itemsToCheck <-
    x$form$items$item_id[x$form$items$section_id %in% sectionsToShow];

  uncompletedItems <-
    lapply(
      itemsToCheck,
      function(currentItem) {
        if (x$specs[[currentItem]]$text == x$config$initialText) {
          return(currentItem);
        } else {
          return(NULL);
        }
      }
    );

  uncompletedItems <- unlist(uncompletedItems);

  if (nrOfItems > length(uncompletedItems)) {
    nrOfItems <- length(uncompletedItems);
  }

  if (nrOfItems == 0) {

    cli::cli_alert_success(
      paste0("You have specified something for all items, well done!")
    );

  } else {

    nextItems <- uncompletedItems[1:nrOfItems];

    for (currentItemId in nextItems) {

      currentLabel <-
        x$form$items$item_label[x$form$items$item_id == currentItemId];
      currentDescription <-
        x$form$items$item_description[x$form$items$item_id == currentItemId];

      cli::cli_alert_success(
        paste0(currentLabel,
               ": ",
               ifelse(is.na(currentDescription), "", currentDescription),
               " ",
               cli::col_silver("[", currentItemId, "]"))
      );

      cli::cli_text();

    }

    cli::cli_text(
      paste0(
        "To specify preregistration content, you can use the following command:\n\n",
        cli::col_green(
          "  preregr::prereg_specify(",
          objectName,
          ", ", nextItems[1],
          " = \"The content to specify\");"
        )
      )
    );

    if (nrOfItems > 1) {
      cli::cli_text();
      cli::cli_text(
        paste0(
          "Note that in this example, the item identifier for the first ",
          "item was used; replace this with the item identifiers for the ",
          "other items to specify content for those (shown between square ",
          "brackets after each item's description)."
        )
      );
    }

    cli::cli_text();

  }

  return(invisible(x));

}
