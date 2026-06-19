#' @title Build the weight matrix for a grid copula.
#' @description Returns a list with the estimated parameters
#'  of the copula, the log likelihood, the mutual information
#'   (optional) and the weight matrix: log likelihood or mutual
#'    information.
#' @param U Values of u and v.
#' @param k positive integer indicating the 
#' number of subintervals for the U2 variable.
#' @param m positive integer indicating the number
#'  of subintervals for the U1 variable.
#' @param method Method that uses, least squares "ls" or
#'  maximum likelihood "ml", by default "ml".
#' @param weights Character with the weight construction method:
#'  "likelihood" or "mutual_information", by default likelihood.


build.weightsGRID <- function(U,k,m, method,weights){
  ll.grid <- function(x, y) {
    if( (0 < sum(x<0)) | (0<sum((x==0) & (0<y))) ) {
      value <- -1*sum((x-1)^2) #-1e100
    } else {
      position <- (0<x) & (0<y)
      value <- sum(y[position] * log(x[position]))
    }
    return(value)
  }
  
  n_col <- dim(U)[2]
  ll <- matrix(nrow = n_col,ncol = n_col)
  colnames(ll) <- colnames(U)
  rownames(ll) <- colnames(U)
  
  mi <- matrix(nrow = n_col,ncol = n_col)
  colnames(mi) <- colnames(U)
  rownames(mi) <- colnames(U)
  
  names <- rownames(ll)
  
  theta <- list()
  for (i in 1:(n_col - 1)) {
    theta[[names[i]]] <- list()
    for (j in (i + 1):n_col) {
      gc <- estimate.gridCopula(U = U[,c(i,j)],k = k, m = m,
                                method =  method)
      ll[i,j] <- ll.grid(x = as.vector(gc$Density), 
                         y = as.vector(gc$Quantity))
      theta[[names[i]]][[names[j]]] <- gc
      
      if(weights == "mutual_information"){
        #mi[i,j] <- mi.grid(gc = theta[[names[i]]][[names[j]]])
        mi[i,j] <- measures.grid(gc = theta[[names[i]]][[names[j]]], measures = c("mi"))$mi
      }
    }
  }
  
  if(weights == "mutual_information"){
    w <- mi
  }else{
    w <- ll
  }
  lower_triangle <- lower.tri(w)
  w[lower_triangle] <- t(w)[lower_triangle]
  diag(w) <- 0
  
  res <- list(w = w, theta = theta)
}
