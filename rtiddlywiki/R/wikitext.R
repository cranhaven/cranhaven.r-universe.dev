
wikitext_formatting <- function(x, f, sep = NULL) {

    attributes(x) <- NULL
    if (!is.vector(x)) {
        stop('Only support vector')
    }
    x <- paste0(f, x, f)
    if (!is.null(sep)) {
        x <- paste(x, collapse = sep)
    }
    return(x)

}

#' backticks
#'
#' backticks for wikitext
#' @param x character vector
#' @return By default this function outputs (see: \code{cat}) the result. Call the function ending in \code{.return} to catch the result instead.
#' @export
#' @aliases wikitext_backticks

#' @examples
#' wikitext_backticks('FOO')
#' wikitext_backticks_return('FOO')
wikitext_backticks_return <- function(x)
    wikitext_formatting(x, '`', sep = ", ")

#' @export
wikitext_backticks <- function(x) {
    cat(wikitext_backticks_return(x))
}
