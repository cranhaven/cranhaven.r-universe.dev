#' execute string command
#' This command just execute in the paraent frame.
#' @param string one string
#' @param envir the environment in which sting is to be evaluated.
#' @return execute string command
#' @export
#'
#' @examples
#' a=2
#' exec('a = 1')
#' a
exec <- function(string,envir = parent.frame()){
    eval(expr = parse(text = string),envir = envir)
}