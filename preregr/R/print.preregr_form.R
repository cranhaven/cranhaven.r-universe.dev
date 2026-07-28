#' @export
#' @method print preregr_form
print.preregr_form <- function(x, ...) {

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

  cli::cli_alert_info(
    "This form has {nrOfItems} item{?s} in {nrOfSections} section{?s}."
  );

  return(invisible(x));

}
