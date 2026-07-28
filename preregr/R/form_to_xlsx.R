#' Export a (pre)registration form to an Excel spreadsheet
#'
#' @param x The (pre)registration form (as produced by a call
#' to [preregr::form_create()]) or initialized `preregr` object
#' (as produced by a call to [preregr::prereg_initialize()]).
#' @param file The file to write the spreadsheet to.
#'
#' @return x, invisibly
#' @export
form_to_xlsx <- function(x,
                         file) {

  x <- retrieve_form(x);

  if (!requireNamespace("writexl", quietly = TRUE)) {
    stop("To write spreadsheets, you need to have `writexl` installed!");
  }

  writexl::write_xlsx(
    x,
    file
  );

  return(invisible(x));

}

