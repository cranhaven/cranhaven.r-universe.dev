
#' @title Create New Graphviz Data.Frame (grPipe Nodes)
#'
#' @description
#' if nrow or ncol parameters are equal zero, then the output will be an empty data.frame.
#'
#' @param nrow integer
#' @param ncol integer
#'
#' @return Returns a data.frame with 3 columns (id, id_next and text) where:
#' \itemize{
#'     \item if nrow==0 or ncol==0, then return an empty data.frame;
#'     \item if nrow>0 and ncol>0, then return a data.frame with one row:
#'     \itemize{
#'         \item id = paste0(LETTERS[nrow], ncol)
#'         \item id_next = NA
#'         \item text = NA
#'     }
#' }
#'
#' @author Daniel Gaspar Gonçalves
#'
#' @examples
#' nodes = grPipe.create()
#' nodes = grPipe.create(nrow = 2, ncol = 5)
#'
#' @export

grPipe.create = function(nrow = 0, ncol = 0) {
  if (nrow==0 | ncol==0) {
    nodes = data.frame(
      id = character(0),
      id_next = character(0),
      text = character(0)
    )
  } else {
    nodes = data.frame(
      id = paste0(LETTERS[nrow], ncol),
      id_next = NA,
      text = NA
    )
  }

  return(nodes)
}
