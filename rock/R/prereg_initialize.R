#' Initialize a (pre)registration
#'
#' To initialize a (pre)registration, pass the URL to a Google Sheet holding
#' the (pre)registration form specification (in \{preregr\} format), see the
#' "[Creating a form from a spreadsheet](https://r-packages.gitlab.io/preregr/articles/creating_form_from_spreadsheet.html)"
#' vignette), the path to a file with a spreadsheet holding such a
#' specification, or a loaded or imported \{preregr\} (pre)registration form.
#'
#' For an introduction to working with \{preregr\} (pre)registrations,
#' see the
#' "[Specifying preregistration content](https://r-packages.gitlab.io/preregr/articles/specifying_prereg_content.html)"
#'  vignette.
#'
#' @param x The (pre)registration form specification, as a URL to a Google
#' Sheet or online file or as the path to a locally stored file.
#' @param initialText The text to initialize every field with.
#'
#' @return The empty (pre)registration specification.
#' @export
#'
#' @examples rock::prereg_initialize(
#'   "preregQE_v0_95"
#' );
prereg_initialize <- function(x,
                              initialText = "Unspecified") {

  if (!requireNamespace("preregr", quietly = TRUE)) {
    stop("To work with (pre)registrations, you must have the {preregr} ",
         "package installed. To install it, use:\n\n  ",
         "install.packages(\"preregr\");");
  }

  return(
    preregr::prereg_initialize(x,
                               initialText = initialText)
  );

}
