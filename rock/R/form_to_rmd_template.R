#' Convert a (pre)registration form to an R Markdown template
#'
#' This function creates an R Markdown template from a \{preregr\}
#' (pre)registrations form specification. Pass it the URL to a Google
#' Sheet holding the (pre)registration form specification (in \{preregr\}
#' format), see the
#' "[Creating a form from a spreadsheet](https://r-packages.gitlab.io/preregr/articles/creating_form_from_spreadsheet.html)"
#' vignette), the path to a file with a spreadsheet holding such a
#' specification, or a loaded or imported \{preregr\} (pre)registration form.
#'
#' @param x The (pre)registration form (as produced by a call
#' to `preregr::form_create()` or `preregr::import_from_html()`) or
#' initialized `preregr` object (as produced by a call to
#' `preregr::prereg_initialize()` or `preregr::import_from_html()`); or, for
#' the printing method, the R Markdown template produced by a call to
#' `preregr::form_to_rmd_template()`.
#' @param title The title to specify in the template's YAML front matter.
#' @param author The author to specify in the template's YAML front matter.
#' @param date The date to specify in the template's YAML front matter.
#' @param output The output format to specify in the template's YAML
#' front matter.
#' @param yaml It is also possible to specify the YAML front matter directly
#' using this argument. If used, it overrides anything specified in `title`,
#' `author`, `date` and `output`.
#' @param includeYAML Whether to include the YAML front matter or omit it.
#' @param file Optionally, a file to save the html to.
#' @param headingLevel The level of the top-most heading to use (the
#' title of the (pre)registration form).
#' @param showSpecification Whether to show the specification in the Rmd
#' output. When `FALSE`, the `preregr` option `silent` is set to `TRUE` at
#' the start of the Rmd template; otherwise, it is set to `FALSE`.
#' @param chunkOpts The chunk options to set for the chunks in the template.
#' @param justify Whether to use `preregr::prereg_specify()` as function for
#' specifying the (pre)registration content (if `FALSE`), or
#' `preregr::prereg_justify()` (if `TRUE`).
#' @param preventOverwriting Set to `FALSE` to override overwrite prevention.
#' @param silent Whether to be silent or chatty.
#'
#' @return x, invisibly
#' @export
#' @rdname rmd_templates
#'
#' @examples newForm <-
#'   preregr::form_create(
#'     title = "Example form",
#'     version = "0.1.0"
#'   );
#'
#' preregr::form_to_rmd_template(
#'   newForm
#' );
form_to_rmd_template <- function(x,
                                 file = NULL,
                                 title = NULL,
                                 author = NULL,
                                 date = '`r format(Sys.time(), "%H:%M:%S on %Y-%m-%d %Z (UTC%z)")`',
                                 output = "html_document",
                                 yaml = list(title = title,
                                             author = author,
                                             date = date,
                                             output = output),
                                 includeYAML = TRUE,
                                 chunkOpts = "echo=FALSE, results='hide'",
                                 justify = FALSE,
                                 headingLevel = 1,
                                 showSpecification = FALSE,
                                 preventOverwriting = rock::opts$get('preventOverwriting'),
                                 silent = rock::opts$get('silent')) {


  if (!requireNamespace("preregr", quietly=TRUE)) {
    stop("To work with (pre)registrations, you must have the {preregr} ",
         "package installed. To install it, use:\n\n  ",
         "install.packages(\"preregr\");");
  }

  return(
    preregr::form_to_rmd_template(
      x = x,
      file = file,
      title = title,
      author = author,
      date = date,
      output = output,
      yaml = yaml,
      includeYAML = includeYAML,
      chunkOpts = chunkOpts,
      justify = justify,
      headingLevel = headingLevel,
      showSpecification = showSpecification,
      preventOverwriting = preventOverwriting,
      silent = silent
    )
  );

}
