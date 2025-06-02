#' @title Grid copula joint density
#' @description Returns the joint density in tree for grid copula.
#' @param theta Copula parameters.
#' @param arbol Dataframe with the results of the minimum expansion tree.
#' @param U Values of u and v.

join.grid <- function(theta,arbol,U){
  from <- arbol$from
  to <- arbol$to
  ncop <- dim(arbol)[1]
  
  join <- matrix(nrow = nrow(U), ncol = ncop)
  
  par <- theta[[from[1]]][[to[1]]]
  join[,1] <- d.grid(U = U[,c(from[1],to[1])],gc = par)

  if(ncop > 1){
      for (c in 2:ncop) {
        par <- theta[[from[c]]][[to[c]]]
        join[,c] <- d.grid(U = U[,c(from[c],to[c])],gc = par)
      }
    }

  
  join[join == 0] <- 1e-200
  den <- apply(join, 1, function(x)  sum(log(x)) )
  return(den)
}



