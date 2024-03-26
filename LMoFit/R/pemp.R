#' Emperical cumulative distribution function
#'
#' @param data quantile/s
#'
#' @return A dataframe containing two columns as the sorted observations and the corresponding empirical probability of non-exceedance
#' @author Mohanad Zaghloul [aut, cre], Simon Michael Papalexiou [aut, ths], Amin Elshorbagy [aut, ths]
#' @export
#'
#' @examples
#' 
#' output <- pemp(data = runif(n = 50, min = 10, max = 100))
#' 
pemp <- function(data){
  sorted <- sort(data)
  ranked <- rank(sorted)
  u <- ranked/(max(ranked) + 1) #probability of non-exceedance
  output <- data.frame(Q = sorted, empcdf = u)
  return(output)
}
