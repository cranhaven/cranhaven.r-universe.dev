#' Create a templated report for cognitive interviews
#'
#' Use this function to export a templated report for cognitive interviews.
#' To embed it in an R Markdown file, use
#' !!! CREATE rock::knit_codebook() !!!
# #' [rock::knit_codebook()] instead.
#'
#' @param x The codebook object (as produced by a call to
#' [rock::parse_sources()]).
#' @param file The filename to save the codebook to.
#' @param title The title to use.
#' @param author The author to specify in the PDF.
#' @param caption The caption for the heatmap.
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
#' ### Load an example CI
#' examplePath <- file.path(system.file(package="rock"), 'extdata');
#' parsedCI <- parse_source(file.path(examplePath,
#'                                    "ci_example_1.rock"));
#'
#' rock::template_ci_heatmap_1_to_pdf(
#'   parsedCI,
#'   file = tmpFile
#' );
#' }
template_ci_heatmap_1_to_pdf <- function(x,
                                         file,
                                         title = "Cognitive Interview: Heatmap and Coded Fragments",
                                         author = NULL,
                                         caption = "Heatmap",
                                         headingLevel = 1,
                                         silent = rock::opts$get('silent')) {

  if (!(inherits(x, "rock_parsedSource") || inherits(x, "rock_parsedSources"))) {
    stop("As `x`, you must pass an object with one or more parsed sources ",
         "as created with the {rock} ",
         "package, for example through `rock::parse_sources()`. ",
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
    list(title = title,
         author = author,
         caption = caption);

  if (dir.exists(dirname(file))) {
    rmarkdown::render(
      input = system.file("templates",
                          "_rock_ci_heatmap_and_fragments_template_for_pdf.Rmd",
                          package = "rock"),
      params = paramsToPass,
      output_file = file,
      quiet = TRUE
    );
    msg("Exported the template to PDF file '",
        file, "'.\n",
        silent = silent);
  } else {
    stop("The path that you specified to save the file in ('",
         dirname(file), "') does not exist.");
  }

  return(invisible(x));

}
