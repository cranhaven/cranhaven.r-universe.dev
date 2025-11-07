#' Combine miRNA vectors into one
#'
#' Combine miRNA vectors into one.
#'
#' Combine miRNA vectors into one. miRNA names occurring more than once
#' are reduced to one instance.
#'
#' @param ... Character vectors. Character vectors containing miRNA names.
#'
#' @return Combined character vector containing miRNA names.
#'
#' @seealso [get_mir()]
#'
#' @family combine functions
#'
#' @export
combine_mir <- function(...) {

    mir_combined <- c(...) %>%
        unique()

    return(mir_combined)
}
