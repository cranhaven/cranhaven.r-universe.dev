#' Extract file name
#'
#' @param ... one or more file path
#' @param extension whether include extension, default is TRUE
#' @return file names
#' @export
#'
#' @examples
#' file.name('f:/dir/1.txt')
#' file.name('f:/dir/1.txt', 'f:/dir/1.txt')
#' file.name('f:/dir/1.txt', 'f:/dir/1.txt', 'f:/dir/')
file.name <- function(...,extension=TRUE){
    fn <- c(...) |> 
        sapply(reverse) |> 
        Replace0('/.*','\\\\.*') |> 
        sapply(reverse)
    names(fn) <- NULL
    if (extension) fn[!grepl('\\.',fn)] <- NA
    fn
}

