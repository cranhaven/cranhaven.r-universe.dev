#' Merging 2 XML node sets
#'
#' @param ns1 First XMLNodeSet
#' @param ns2 Second XMLNodeSet
#'
#' @return An XMLNodeSet
#'
#'
#' @keywords internal
#'
#' @noRd
#'
# @examples
merge_nodesets <- function(ns1, ns2) {
  new_ns <- ns1
  nodes_nb1 <- length(ns1)
  nodes_nb2 <- length(ns2)

  if (!nodes_nb2) {
    return(ns1)
  }

  for (i in 1:nodes_nb2) {
    new_ns[[nodes_nb1 + i]] <- ns2[[i]]
  }
  return(new_ns)
}
