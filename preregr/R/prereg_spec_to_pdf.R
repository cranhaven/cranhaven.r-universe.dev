#' Convert a (pre)registration specification to PDF
#'
#' Use this function to export your (pre)registration specification to a PDF
#' file. To embed it in an R Markdown file, use
#' [preregr::prereg_knit_item_content()] instead.
#'
#' @param x The (pre)registration object (as produced by a call to
#' [preregr::prereg_initialize()]).
#' @param file The filename to save the (pre)registration to.
#' @param author The author to specify in the PDF.
#' @param section Optionally, one or multiple sections to include (if `NULL`,
#' all sections are included).
#' @param headingLevel The level of the top-most headings.
#' @param silent Whether to be silent or chatty.
#'
#' @return x, invisibly
#' @export
#'
#' @examples ### Use a temporary file to write to
#' tmpFile <- tempfile(fileext = ".pdf");
#'
#' ### Load an example (pre)registration specification
#' data("examplePrereg_1", package = "preregr");
#'
#' ### Only run this if you have a functional LaTeX installation
#' if (FALSE) {
#'   preregr::prereg_spec_to_pdf(
#'     examplePrereg_1,
#'     file = tmpFile
#'   );
#' }
prereg_spec_to_pdf <- function(x,
                               file,
                               author = NULL,
                               section = NULL,
                               headingLevel = 1,
                               silent = preregr::opts$get('silent')) {

  if (!requireNamespace('rmarkdown', quietly=TRUE)) {
    stop("You need to have 'rmarkdown' installed to export to a PDF!");
  }

  if (is.null(author)) {
    author <- "";
  }

  paramsToPass <-
    list(title = x$form$metadata[x$form$metadata$field == "title", "content"],
         author = author);

  if (dir.exists(dirname(file))) {
    rmarkdown::render(
      input = system.file("templates",
                          "_preregr_spec_full_template_for_pdf.Rmd",
                          package = "preregr"),
      params = paramsToPass,
      output_file = file,
      quiet = TRUE
    );
    msg("Exported the (pre)registration specification to PDF file '",
        file, "'.\n",
        silent = silent);
  } else {
    stop("The path that you specified to save the file in ('",
         dirname(file), "') does not exist.");
  }

  return(invisible(x));

}
