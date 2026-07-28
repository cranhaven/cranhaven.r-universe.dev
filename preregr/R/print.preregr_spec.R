#' @export
#' @method print preregr_spec
print.preregr_spec <- function(x, ...) {

  cli::cli_h1("(Pre)registration specification");

  nrOfFields <- length(x$specs);
  emptyFields <- sum(unlist(x$specs) == x$config$initialText);
  completedFields <- nrOfFields - emptyFields;

  cli::cli_alert_info(
    paste0(
      cli::col_cyan("Form: "),
      x$form$metadata[x$form$metadata$field == "title", "content"]
    )
  );

  cli::cli_alert_info(
    paste0(
      nrOfFields, " fields (",
      completedFields, " completed, ",
      emptyFields, " empty)"
    )
  );

  return(invisible(x));

}
