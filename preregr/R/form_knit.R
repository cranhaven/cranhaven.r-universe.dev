#' Knit a (pre)registration form into an Rmd file
#'
#' This function inserts a (pre)registration form, or one or more
#' sections, into an R Markdown file.
#'
#' @param x The (pre)registration form (as produced by a call
#' to [preregr::form_create()]) or initialized `preregr` object
#' (as produced by a call to [preregr::prereg_initialize()]).
#' @param section The section(s) to show; pass `NULL` (the default) to show
#' everything.
#' @param headingLevel The level to use for the top-most heading.
#'
#' @return x, invisibly
#' @export
#'
#' @examples preregr::form_create(
#'   title = "Example form",
#'   version = "0.1.0"
#' ) |>
#'   preregr::form_knit();
form_knit <- function(x,
                      section = NULL,
                      headingLevel = 2) {

  x <- retrieve_form(x);

  rmdpartials::partial(
    system.file("partials",
                "_preregr_form_clean_partial.Rmd",
                package = "preregr")
  );

}
