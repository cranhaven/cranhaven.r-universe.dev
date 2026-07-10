#' Check legal character
#' Whether the character is legal for names in dataframe or formula
#' @param ... one or more string
#'
#' @return logical, TRUE means legal.
#' @export
#'
#' @examples
#' legal('a','b','a b')
legal <- function(...){
    # one or more string
    string <- c(...)
    sapply(string,function(i) i == make.names(i))
}