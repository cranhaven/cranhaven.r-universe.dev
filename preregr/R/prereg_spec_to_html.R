#' Convert a (pre)registration specification to html
#'
#' Use this function to export your (pre)registration specification to an
#' HTML file. To instead embed it in an R Markdown file, use
#' [preregr::prereg_knit_item_content()].
#'
#' @param x The (pre)registration object (as produced by a call to
#' [preregr::prereg_initialize()]).
#' @param file Optionally, a file to save the html to.
#' @param section Optionally, one or multiple sections to include (if `NULL`,
#' all sections are included).
#' @param headingLevel The level of the top-most headings.
#' @param silent Whether to be silent or chatty.
#'
#' @return The produced HTML, which will print in the viewer in RStudio.
#' @export
#'
#' @examples ### Load an example (pre)registration specification
#' data("examplePrereg_1", package = "preregr");
#'
#' ### Convert it to HTML and show the result
#' preregr::prereg_spec_to_html(
#'   examplePrereg_1
#' );
prereg_spec_to_html <- function(x,
                                file = NULL,
                                section = NULL,
                                headingLevel = 1,
                                silent = preregr::opts$get('silent')) {

  res <-
    rmdpartials::partial(
      system.file("partials",
                  "_preregr_spec_full_partial.Rmd",
                  package = "preregr")
    );

  #Encoding(res) <- "UTF-8";

  if (!is.null(file)) {

    if (!requireNamespace('knitr', quietly=TRUE)) {
      stop("You need to have 'knitr' installed to export HTML to a file!");
    }

    if (dir.exists(dirname(file))) {
      knittedFile <-
        knitr::knit2html(
          text = as.character(res)
        );
      writeLines(
        knittedFile,
        file
      );
      msg("Knitted the (pre)registration specification to HTML file '",
          file, "'.\n",
          silent = silent);
    } else {
      stop("The path that you specified to save the file in ('",
           dirname(file), "') does not exist.");
    }
  }

  return(res);

}
