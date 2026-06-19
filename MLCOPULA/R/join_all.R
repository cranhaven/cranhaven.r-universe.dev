join.all <- function(theta,arbol,U,cop){
  from <- arbol$from
  to <- arbol$to
  ncop <- dim(arbol)[1]
  join <- matrix(nrow = nrow(U), ncol = ncop)
  
  
  par <- theta[from[1],to[1]]
  copula <- copulas(theta = par, cop = cop[from[1],to[1]])
  join[,1] <- dCopula(U[,c(from[1],to[1])],copula)

  if(ncop > 1){
      for (c in 2:ncop) {
        par <- theta[from[c],to[c]]
        copula <- copulas(theta = par, cop = cop[from[c],to[c]])
        join[,c] <- dCopula(U[,c(from[c],to[c])],copula)
      }
    }

  join[join == 0] <- 1e-200
  den <- apply(join, 1, function(x)  sum(log(x)) )
  return(den)
  
}
