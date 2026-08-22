#' Convert a codebook specification to PDF
#'
#' Use this function to export your codebook specification to a PDF
#' file. To embed it in an R Markdown file, use
#' !!! CREATE rock::knit_codebook() !!!
# #' [rock::knit_codebook()] instead.
#'
#' @param x The codebook object (as produced by a call to
#' [rock::codebook_fromSpreadsheet()]).
#' @param file The filename to save the codebook to.
#' @param author The author to specify in the PDF.
#' @param headingLevel The level of the top-most headings.
#' @param silent Whether to be silent or chatty.
#'
#' @return x, invisibly
#' @export
#'
#' @examples \donttest{
#' ### Use a temporary file to write to
#' tmpFile <- tempfile(fileext = ".pdf");
#'
#' ### Load an example (pre)registration specification
#' data("exampleCodebook_1", package = "rock");
#'
#' rock::codebook_to_pdf(
#'   exampleCodebook_1,
#'   file = tmpFile
#' );
#' }
codebook_to_pdf <- function(x,
                            file,
                            author = NULL,
                            headingLevel = 1,
                            silent = rock::opts$get('silent')) {

  if (!inherits(x, "rock_codebook_spec")) {
    stop("As `x`, you must pass a code book as created with the {rock} ",
         "package, for example through `rock::codebook_fromSpreadsheet()`. ",
         "However, you passed an object of class(es) ",
         vecTxtQ(class(x)), ".");
  }

  if (!requireNamespace('rmarkdown', quietly=TRUE)) {
    stop("You need to have 'rmarkdown' installed to export to a PDF!");
  }

  if (is.null(author)) {
    author <- "";
  }

  paramsToPass <-
    list(title = x$metadata[x$metadata$field == "title", "content"],
         author = author);

  if (dir.exists(dirname(file))) {
    rmarkdown::render(
      input = system.file("templates",
                          "_rock_codebook_full_template_for_pdf.Rmd",
                          package = "rock"),
      params = paramsToPass,
      output_file = file,
      quiet = TRUE
    );
    msg("Exported the codebook specification to PDF file '",
        file, "'.\n",
        silent = silent);
  } else {
    stop("The path that you specified to save the file in ('",
         dirname(file), "') does not exist.");
  }

  return(invisible(x));

}
