#' @title Function to integrate for mutual information.
#' @param u value for u.
#' @param v value for v.
#' @param cop a copula with parameter \eqn{\theta}.

int.mi <- function(u,v,cop){
  den <- dCopula(cbind(as.numeric(u),as.numeric(v)),cop)
  den <- dCopula(cbind(as.numeric(u),as.numeric(v)),cop)
  
  den <- ifelse(den == 0,1e-300, den)
  res <- as.numeric(den) * 
    as.numeric(log(den))
  return(res)
}
