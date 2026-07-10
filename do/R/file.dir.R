#' up level directory
#'
#' @param path path of file
#' @param end.slash logical. Whether to end with slash
#' @param extension logical. whether file name include extension
#'
#' @return upper directory
#' @export
#'
file.dir <- function(path,end.slash=TRUE,extension=TRUE){
    do::Replace0(path,do::file.name(path,extension = extension)) |> 
        formal_dir(end.slash)
}