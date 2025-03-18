#' \code{greatR} package
#'
#' See the README on
#' \href{https://github.com/ruthkr/greatR/}{GitHub}
#'
#' @docType package
#' @name greatR
#' @aliases greatR-package
#' @noRd
"_PACKAGE"

## quiets concerns of R CMD check re: the .'s that appear in pipelines
if (getRversion() >= "2.15.1") {
  utils::globalVariables(
    c(".", "..colnames_id_table", "..ids_data_ref_colnames", "..colnames_wanted")
  )
}
