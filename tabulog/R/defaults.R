# List of formatters provided "out-of-the-box" for use with default classes
.default_formatters <- function(){
  list(
    int = as.integer,
    double = as.numeric
  )
}

#' Default parser classes
#'
#' List or parser classes provided 'out-of-the-box'. These can be used without
#' further definition in any templates, or can be overridden.
#'
#' Parser classes are provided for the following
#' \itemize{
#'     \item ip: For matching ip addresses
#'     \item quote: For matching any string quoted by double-quotes
#'     \item url: For matching a standard http(s) url
#'     \item int: For matching any integer
#'     \item double: For matching any numeric value (including integers)
#' }
#'
#' @param file Yaml file of parser classes to load. Defaults to included package file.
#' @param formatters Named list of formatter functions to be associated with
#'                   parsers. Default formatters are provided for default parser classes
#'
#' @return A named list of the default parser classes provided "out of the box". Users
#'         should not need to use this in their code, and is mostly used for use in 
#'         other internal functions. It is only visible to users so they can call
#'         it and see what classes are available by default.
#' 
#' @examples
#' default_classes()
#'
#' @importFrom yaml yaml.load_file
#' @export
default_classes <- function(file=system.file('config/parser_classes.yml', package = 'tabulog'),
                            formatters = .default_formatters()){
  classes <- yaml::yaml.load_file(file)
  parsers <- parser(classes, formatters)
  names(parsers) <- names(classes)
  parsers
}
