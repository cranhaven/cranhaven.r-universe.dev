#' @title Permutate a vector
#'
#' @description Internal function to permutate a vector of size n using the function \code{\link{shuffleSet}}.
#'
#' @encoding UTF-8
#' @importFrom permute how Within Plots shuffleSet
#' @param n The length of vector.
#' @param strata A vector to specify restricting permutations.
#' @param nset The number of permutations to generate for the set (Default strata = NULL).
#' @return A matrix of permutations, where each row is a separate permutation.
#' @author Vanderlei Julio Debastiani <vanderleidebastiani@@yahoo.com.br>
#' @seealso \code{\link{syncsa}}, \code{\link{permut.row.matrix}}
#' @keywords SYNCSA
#' @export
permut.vector <- function(n, strata = NULL, nset = 999)
{
  if(is.null(strata)){
    CTRL <- permute::how(within = permute::Within(type = "free"), plots = permute::Plots(type = "free"))
    samp <- permute::shuffleSet(n = n, nset = nset, control = CTRL, check = FALSE)
  }
  if(!is.null(strata)){
    if(n != length(strata)){
      stop("\nstrata must be the length of n\n")
    }
    CTRL <- permute::how(within = permute::Within(type = "free"), plots = permute::Plots(strata = strata, type = "none"))
    samp <- permute::shuffleSet(n = n, nset = nset, control = CTRL, check = FALSE)
  }
  res <- as.matrix(samp)
  return(res)
}
