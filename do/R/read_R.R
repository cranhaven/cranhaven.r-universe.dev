#' Read R file
#'
#' @param R path of R file
#' @param pattern pattern
#' @return one vector of R command with names of R file
#' @export
#'
read_R <- function(R, pattern){
    r <- list.files(path = R,pattern = '\\.R',full.names = TRUE)
    x <- sapply(r, function(i){
        readLines(i) |> paste0(collapse = '\n')
    })
    names(x) <- file.name(r)
    if (!missing(pattern)) x <- x[grepl(pattern,x)]
    x
}
