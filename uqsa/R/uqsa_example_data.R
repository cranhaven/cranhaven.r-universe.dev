#' Load an example model for this package
#'
#' This function finds the path to an example model, given by name.
#' In the SBtab format, model and data travel together (in different
#' tables, but the same documents).
#'
#' By default this function returns the names of the tsv files
#' belonging to the named model. If no modelName is provided it
#' returns possible names (contents of the top-level example
#' directory).
#'
#' @param modelName name of model, e.g.: "AKAR4", "AKAP79", "CaMKII";
#'     if empty, this function lists all available examples.
#' @param full.names return full paths to files - defaults to TRUE
#' @param pattern pattern to find specific files; if `NULL`, this function
#'     returns the directory of the example
#' @param f file ending, search for file endings in `f`, alternative to `pattern`
#' @return The location of the examples in the current environment if
#'     called with no arguments, the paths to the model files if a
#'     modelName was provided or the full path to the example if the
#'     file pattern _pattern_ is unset
#' @examples
#' uqsa_example()
#' uqsa_example("AKAR4",full.names=FALSE)
#' uqsa_example("AKAP79",f='R',full.names=FALSE)
#' uqsa_example("AKAP79",pat="^run.*R$")
#' @export
uqsa_example<-function(modelName=NULL, full.names=TRUE, pattern='[.]tsv$', f=NULL) {
	if (!is.null(f)){
		pattern <- sprintf("%s$",gsub("[.]","[.]",f))
	}
	if (is.null(modelName)) {
		return(dir(system.file("extdata", package = "uqsa")))
	} else if (!is.null(pattern) && is.character(pattern)) {
		return(dir(system.file("extdata", modelName, package = "uqsa", mustWork = TRUE), pattern=pattern, full.names=full.names))
	} else {
		return(system.file("extdata", modelName, package = "uqsa", mustWork = TRUE))
	}
}
