#' Get the names in the 'identified.by' column
#'
#' This function facilitates the search for non-taxonomist strings in the
#' 'identified.by' column of occurrence records data set
#'
#' @param occ data frame with occurrence records information.
#' @param identified.by column name of \code{occ} with the name of who
#'  determined the species.
#' @param determined.by deprecated, use \code{identified.by} instead.
#' @param freq logical. If \code{TRUE} output contain the number of times each
#'  string is repeated in the \code{identified.by} column. Default = \code{FALSE}
#' @param decreasing logical. sort strings in decreasing order of frequency.
#'  Default = \code{TRUE}.
#'
#' @return character vector containing the strings in \code{identified.by}
#'     column of \code{occ}. If \code{freq = TRUE} it return a data frame with
#'     two columns: 'strings' and 'frequency'.
#'
#' @examples
#' data("A.setosa")
#' get_det_names(A.setosa, freq = TRUE)
#'
#' @export

get_det_names <- function(occ,
                          identified.by = "identifiedBy",
                          freq = FALSE,
                          decreasing = TRUE,
                          determined.by) { # deprecated

  if (!missing(determined.by)) {
    warning("argument 'determined.by' is deprecated; please use 'identified.by' instead.",
      call. = FALSE
    )
    identified.by <- determined.by
  }

  det.table <- table(occ[, identified.by])
  det.string <- names(det.table)
  if(freq){
    ord <- order(det.table, decreasing = decreasing)
    det.string <- as.data.frame(det.table[ord])
    names(det.string) <- c("string", "frequency")

  }

  return(det.string)
}
