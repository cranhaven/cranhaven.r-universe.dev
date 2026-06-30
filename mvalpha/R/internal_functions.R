#' Efficient Set Operations
#'
#' Find the intersection and set difference(s) of two sets all at once and more
#' efficiently than calling `base::intersect()` and `base::setdiff()` separately.
#' Based on this stackoverflow answer <https://stackoverflow.com/a/72631719>
#' @param A,B sets (vectors) of elements
#' @inheritParams mvalpha

set_ops <-
  function(A, B, type){
    ind <- match(B, A, nomatch = 0)
    A_intersect_B <- A[ind]
    A_diff_B <- A[-c(ind, length(A) + 1)]
    if(type == "nominal"){B_diff_A <- NULL}
    else{
      ind2 <- match(A, B, nomatch = 0)
      B_diff_A <- B[-c(ind2, length(B) + 1)]
      }
    return(
      list(
        A_intersect_B = A_intersect_B,
        A_diff_B = A_diff_B,
        B_diff_A = B_diff_A
      )
    )
  }



