#' @name fml_example
#' @author Sebastian Malkusch
#' @title fml_example
#' @description path to flowml examples data
#' @details flowml comes bundled with a number of sample files in its
#' `inst/extdata` directory. This function allows to access them.
#'
#' @param file Name of file. If `NULL`, the example files will be listed.
#' @return The path of to an example file, if file is defined.
#' Else, a list of example files.
#' @export
#' @examples
#' \dontrun{
#' fml_example()
#' fml_example(file = "reg_config.json")
#' }
#'

fml_example <- function(file = NULL) {
  if (is.null(file)) {
    dir(system.file("extdata", package = "flowml"))
  } else {
    system.file("extdata", file, package = "flowml", mustWork = TRUE)
  }
}
