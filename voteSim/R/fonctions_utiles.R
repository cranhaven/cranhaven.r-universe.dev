######################################
# générateur de données suivant
# une loi de distribution empirique
# et avec copule
######################################


## Function: fpx
## Description : Obtain the ranks of a vector (A factor of
##               0.5 is used to avoid extremes values of ranks).
fpx <- function(x) (rank(x, ties.method = "max") - 0.5) / length(x)


#' Rename_rows
#' @param preferences voters preferences
#' @returns preferences
rename_rows <- function(preferences) {
  n_candidates <- nrow(preferences)
  n_voters <- ncol(preferences)
  rownames(preferences) <- paste0("Candidate ", seq_len(n_candidates))
  return(preferences)
}

#' Generalized inverse of the empirical cumulative function.
#'
#' @param u a numeric vector of quantiles to be transformed.
#' @param x a numeric vector of data values.
#' @param n a positive integer specifying the length of the output vector.
#'
#' @return a numeric vector of transformed quantiles.
#' @details Computes the generalized inverse of the empirical cumulative function,
#' which transforms quantiles \code{u} to the corresponding values of \code{x} based on
#' the frequency distribution of \code{x}.
#'
#' @importFrom stats splinefun
#' @importFrom stats uniroot
#'
icdf <- function(u, x, n) {
  freq <- fpx(x)
  Fn   <- splinefun(x, freq, method = "monoH.FC")
  xstar <- numeric(n)
  for(i in seq_along(xstar)){
    xstar[i] <- uniroot(function(x) Fn(x) - u[i],
                        range(x), extendInt = "upX",
                        f.lower = - u[i], f.upper = 1 - u[i])$root
  }
  return(xstar)
}

#' Distance formula
#' @param votant array
#' @param candidats array
#' @returns distance
distance<-function(votant, candidats){apply(candidats, 1, function(x) sqrt(sum((votant-x)^2)))}

#' Score to distance
#' @param x score
#' @param dim dimension int
#' @param method method string
#' @returns distance
ScoresToDist<-function(x, dim=2, method="linear")
{
  if (method=="linear")
  {T<-dim*(1-x)
  }else{#transformation sigmoïde
    lambda<-5
    x_min<-1/(1+exp(lambda*(2*sqrt(dim)-1)))
    x_max<-1/(1+exp(-lambda))
    x[x<x_min]<-x_min
    x[x>x_max]<-x_max
    T<-((log(1/x-1))/lambda+1)/2
    T[T<0]<-0
  }

  return(as.data.frame(T))
}

#' Distance to score
#' @param dist int
#' @param dim dimension int
#' @param method method string
#' @param lambda lambdad int
#' @returns score
DistToScores <- function(dist, dim=2, method="linear", lambda=5)
{
  if (method=="linear"){
    x <- 1-2*dist
    x[x<0]<-0
  }else{#transformation sigmoide
    x <- 1/(1+exp(lambda*(4*dist-1)))
  }

  return(as.data.frame(x))
}


######################################
#Metric Unfolding Using the MLSMU6 Procedure
######################################
doubleCenterRect <- function(T){ # version score d'appétence
  n <- nrow(T)
  q <- ncol(T)
  (T-matrix(apply(T,1,mean), nrow=n, ncol=q) - t(matrix(apply(T,2,mean), nrow=q, ncol=n)) + mean(T))/2
}


MLSMU6 <- function(df, ndim=2){

  T <- as.matrix(df)
  n <- nrow(T)
  T <- (1-T)*2
  TTSQ <- T*T
  TTSQ[is.na(T)] <- (mean(T,na.rm=TRUE))^2
  TTSQDC <- doubleCenterRect(TTSQ)
  xsvd <- svd(TTSQDC)
  zz <- xsvd$v[,1:ndim]
  xx <- matrix(0, nrow=n, ncol=ndim)
  for (i in 1:ndim){
    zz[,i] <- zz[,i] * sqrt(xsvd$d[i])
    xx[,i] <- xsvd$u[,i] * sqrt(xsvd$d[i])
  }
  MLSMU6<-list(X=xx, Z=zz)
}

#' Preferences_to_ranks
#' @param preferences voters preferences
#' @returns ranks
preferences_to_ranks <- function(preferences) {
  # Calculer les rangs de chaque élément sur chaque colonne
  ranks <- apply(preferences, 2, rank)
  # Inverser les rangs pour que le candidat le plus préféré soit le numéro 1
  ranks <- nrow(ranks) + 1 - ranks
  # Retourner les rangs
  return(ranks)
}

#' Distance formula
#' @param distance_matrix distance_matrix
#' @returns mat_inverse
distance_to_pref<- function(distance_matrix){
  #mat_inverse <- apply(distance_matrix, 2, rev)
  #mat_inverse <- apply(distance_matrix, 2, function(x) x[order(x, decreasing = TRUE)])
  mat_inverse_rangs <- apply(distance_matrix, 2, order, decreasing = TRUE)
  # Réarrangement des valeurs en utilisant les rangs inversés
  #mat_inversee <- apply(mat_inverse_rangs, 2, function(x) mat[x])

  return(mat_inverse_rangs)
}



