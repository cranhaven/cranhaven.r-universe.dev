#' Initialize a (pre)registration
#'
#' To initialize a (pre)registration, pass the URL to a Google Sheet holding
#' the (pre)registration form specification (in {preregr} format), see the
#' "[Creating a form from a spreadsheet](https://preregr.opens.science/articles/creating_form_from_spreadsheet.html)"
#' vignette), the path to a file with a spreadsheet holding such a
#' specification, or a loaded or imported {preregr} (pre)registration form.
#'
#' For an introduction to working with {preregr} (pre)registrations,
#' see the
#' "[Specifying preregistration content](https://preregr.opens.science/articles/specifying_prereg_content.html)"
#'  vignette.
#'
#' @param x The (pre)registration form specification, as a URL to a Google
#' Sheet or online file or as the path to a locally stored file.
#' @param initialText The text to initialize every field with.
#'
#' @return The empty (pre)registration specification.
#' @export
#'
#' @examples preregr::prereg_initialize(
#'   "inclSysRev_v0_92"
#' );
prereg_initialize <- function(x,
                              initialText = "Unspecified") {

  formSpec <- retrieve_form(x);

  res <- list(
    specs =
      as.list(
        stats::setNames(
          lapply(
            formSpec$items$item_id,
            function(x) {
              return(list(text = initialText))
            }
          ),
          nm = formSpec$items$item_id
        )
      ),
    jstf = list(),
    form =
      formSpec,
    config =
      list(initialText = initialText)
  );

  if (requireNamespace('justifier', quietly = TRUE)) {
    res$jstf <-
      structure(
        list(
          sources = structure(list(),
                              class=c("justifier", "justifierStructured", "justifierSource", "list")),
          assertions = structure(list(),
                                 class=c("justifier", "justifierStructured", "justifierAssertion", "list")),
          justifications = structure(list(),
                                     class=c("justifier", "justifierStructured", "justifierJustification", "list")),
          decisions = structure(list(),
                                class=c("justifier", "justifierStructured", "justifierDecision", "list")),
          justifier = structure(list(),
                                class=c("justifier", "justifierStructured", "justifierJustifier", "list"))
        ),
        class = c("justifier", "justifierStructuredObject", "list")
      );
  }

  res$form$metadata$content[res$form$metadata$field == "date"] <-
    format(
      number_as_xl_date(
        res$form$metadata$content[res$form$metadata$field == "date"]
      ),
      "%Y-%m-%d"
    );

  class(res) <- c("preregr", "preregr_spec", "list");

  return(res);

}
