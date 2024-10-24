#' @title A function to create symbols
#' @usage cr_symb(k = k, m = m)
#' @description This function obtains the set of symbols to get the Q-statistic
#' @param k number of categories
#' @param m length of the m-surrounding
#' @return A list with two types of symbols. Permutation and Combinations-totals
#'  \tabular{ll}{
#'     \code{p_symb} \tab Matrix with symbols (permutations)\cr
#'     \code{c_symb} \tab  Matrix with symbols (combinations)\cr
#'     }
#' @details ...
#' @seealso
#' \code{\link{m.surround}}
#' @author
#'   \tabular{ll}{
#'   Fernando López  \tab \email{fernando.lopez@@upct.es} \cr
#'   Román Mínguez  \tab \email{roman.minguez@@uclm.es} \cr
#'   Antonio Páez \tab \email{paezha@@gmail.com} \cr
#'   Manuel Ruiz \tab \email{manuel.ruiz@@upct.es} \cr
#'   }
#' @references
#'   \itemize{
#'     \item Ruiz M, López FA, A Páez. (2010). \emph{Testing for spatial association of qualitative
#'     data using symbolic dynamics}. Journal of Geographical Systems. 12 (3) 281-309
#'   }
#' @export
#' @examples
#'
#' # Example 1: Obtain symbols for k=2 classes and m-surroundings of size 5
#' symb25 <- cr_symb(2,5)
#' symb25$p_symb # Permutations symbols
#' symb25$c_symb # Combinations-totals symbols

cr_symb <- function(k = k, m = m) {
  p_symb <- gtools::permutations(k, m, repeats.allowed = TRUE)  #Symbols by permutation
  rownames(p_symb) <- paste("Perm", 1:nrow(p_symb), sep="")
  c_symb0 <- gtools::combinations(k, m, repeats.allowed = TRUE)  #Symbols by combination
  rownames(c_symb0) <- paste("Comb", 1:nrow(c_symb0), sep="")
  c_symb <- matrix(rep(0, nrow(c_symb0) * k), ncol = k)
  colnames(c_symb) <- paste("level",1:ncol(c_symb), sep="")
  for (i in 1:nrow(c_symb0)) {
    for (j in 1:k) {
      c_symb[i, j] = sum(c_symb0[i, ] == j)
    }
  }
  symb <- list(p_symb, c_symb, c_symb0)
  names(symb)[1] <- "p_symb"
  names(symb)[2] <- "c_symb"
  names(symb)[3] <- "c_symb0"
  return(symb)
}
