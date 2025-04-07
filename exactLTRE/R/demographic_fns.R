## Demographic functions -------------------------------------------------------

#' Generation time
#'
#' Generation time has been defined in multiple ways. This calculation comes
#' from Bienvenu and Legendre (2015, The American Naturalist;
#' \doi{10.1086/681104}). They define the generation time as
#' the average time between two events in the genealogy of the population.
#'
#' @param Amat The full population projection matrix
#' @param Fmat The fertility elements of the population projection matrix.
#'
#' @return The generation time, as a single number, given in the same units as
#' the projection time step. If the projection interval is two weeks, the
#' generation time will be the number of two-week intervals. You may wish to
#' convert to a standard time step, like days or years.
#' @export
#'
#' @examples
#' A1<- matrix(data=c(0,0.8,0, 0,0,0.7, 5,0,0.2), nrow=3, ncol=3)
#' F1<- matrix(0, nrow=3, ncol=3)
#' F1[1,3]<- A1[1,3]
#' #F1 is all zeros, except the upper right corner which matches A1 for adult fertility
#' T<- generation_time(A1, F1)
generation_time<- function(Amat, Fmat){
  # This uses the Bienvenu and Legendre (Am Nat 2015) definition of generation time.
  # T = lambda*v*w/(v*F*w)

  eigz<- eigen(Amat) # get eigenvalues and eigenvectors
  ilambda<- which(Re(eigz$values)==max(Re(eigz$values)))
  lambda<- max(Re(eigz$values))
  w<- Re(eigz$vectors[,ilambda]) # right eigenvector of the matrix
  v<- Re(eigen(t(Amat))$vectors[,ilambda]) # left eigenvector of the matrix

  gentime<- lambda*v%*%w/(v%*%Fmat%*%w)
  return(gentime)
}

#' R0, the net reproductive output
#'
#' The net reproductive output, R_0, is the expected number of offspring for one
#' individual across their expected lifespan. It is calculated as the largest
#' eigenvalue of the matrix product of the fertility matrix and the fundamental
#' matrix. The fundamental matrix, generally referred to as __N__, contains the
#' expected number of timesteps that an individual will spend in each age,
#' stage, or size class of the matrix.
#'
#' @param Amat The full population projection matrix
#' @param Fmat The fertility elements of the population projection matrix.
#'
#' @return The net reproductive output, a single value, is the number of
#' offspring that an individual is expected to have over their lifespan.
#' @export
#'
#' @examples
#' A1<- matrix(data=c(0,0.8,0, 0,0,0.7, 5,0,0.2), nrow=3, ncol=3)
#' F1<- matrix(0, nrow=3, ncol=3)
#' F1[1,3]<- A1[1,3]
#' #F1 is all zeros, except the upper right corner which matches A1 for adult fertility
#' R0<- r_nought(A1, F1)
r_nought<- function(Amat, Fmat){
  Umat<- Amat-Fmat
  Nmat<- fundamental_matrix(Umat)
  R0<- max(Re(eigen(Fmat%*%Nmat, only.values = TRUE)$values))
  return(R0)
}

#' Expected lifespan
#'
#' The expected lifespan vector is calculated by multiplying a column
#' of ones by the fundamental matrix. The fundamental matrix, generally
#' referred to as __N__, contains the expected number of timesteps that an
#' individual will spend in each age, stage, or size class of the matrix, given
#' their current state.
#'
#' @param Umat The survival components of the population projection matrix
#' @param all_ages User specifies whether the function should return the
#' expected lifespan remaining for all ages (all_ages="T") or only the
#' expected lifespan at birth (all_ages="F").
#'
#' @return The expected lifespan is either a vector (if all_ages="T") or a
#'   single number for the expected lifespan of a newly born individual (if
#'   all_ages="F"). Expected lifespan is given in the same units as the
#'   projection time step. If the projection interval is two weeks, the lifespan
#'   will be the number of two-week intervals that an individual is expected to
#'   survive. You may wish to convert to a standard time step, like days or
#'   years.
#' @export
#'
#' @details The expected lifespan vector contains the expected lifespan remaining for
#'   an individual in each age, stage, or size class of the population. If the
#'   user requests only the expected lifespan from birth, then only the first
#'   entry of the expected lifespan vector is returned.
#'
#' @examples
#' A1<- matrix(data=c(0,0.8,0, 0,0,0.7, 5,0,0.2), nrow=3, ncol=3)
#' U1<- A1
#' U1[1,3]<- 0
#' # the upper right corner represents adult fertility in this model. U1, the
#' # survival matrix, contains all the transitions *except* for fertility.
#' eta<- lifespan(U1, all_ages=TRUE)
#' eta_1<- lifespan(U1, all_ages=FALSE) # eta_1 should match the first entry of eta
lifespan<- function(Umat, all_ages=TRUE){
  # expected lifespan (in timesteps of the model, not years!) is t(e)*Nmat
  # if all.ages=TRUE, then return the vector of expected lifespan remaining
  # if all.ages=FALSE, then return only the first entry (expected lifespan at the beginning of life)
  Nmat<- fundamental_matrix(Umat)
  eT<- t(rep(1, dim(Umat)[1])) # column of ones
  eta<- eT%*%Nmat
  if (all_ages==FALSE){
    eta<- eta[1]
  }
  return(eta)
}


#' Calculate the mean matrix
#'
#' Calculate the mean matrix from a list of matrices. In the mean matrix, each
#' element of the matrix is the mean value at that indexed position, across all
#' the provided matrices.
#'
#' @param Aobj A list of matrix population models, which must all have the same
#' dimensions.
#'
#' @return A single population projection matrix, with the same dimensions as
#' the provided ones, where all vital rate entries are the mean across all
#' provided matrices at the respective matrix index.
#' @export
#'
#' @examples
#' A1<- matrix(data=c(0,0.8,0, 0,0,0.7, 5,0,0.2), nrow=3, ncol=3)
#' A2<- matrix(data=c(0,0.9,0, 0,0,0.5, 4,0,0.3), nrow=3, ncol=3)
#' A3<- matrix(data=c(0,0.4,0, 0,0,0.6, 6,0,0.25), nrow=3, ncol=3)
#' Amean<- mean_matrix(list(A1,A2,A3))
mean_matrix<- function(Aobj){
  # Aobj is a list of matrices. We want to calculate the mean of all indices:

  Aobj_flat<- collapse_mat_list(Aobj)
  meanmat_flat<- apply(Aobj_flat, MARGIN=2, FUN=mean)
  meanmat<- reMat(meanmat_flat)
  return(meanmat)
}

#' Fundamental matrix
#'
#' Calculate the fundamental matrix. The fundamental matrix, generally referred
#' to as \strong{N}, contains the expected number of timesteps that an individual
#' will spend in each age, stage, or size class of the matrix throughout their
#' lifespan, given their current class. In the fundamental matrix, the current
#' state is the column, and the future states are in the rows. For an
#' age-classified matrix, this should be lower triangular.
#'
#' @param Umat The survival components of the population projection matrix.
#'
#' @return The fundamental matrix, containing the expected number of timesteps
#' that an individual will spend in each class of the population, given their
#' current state. This matrix will have the same size as the input matrix.
#' @export
#'
#' @examples
#' A1<- matrix(data=c(0,0.8,0, 0,0,0.7, 5,0,0.2), nrow=3, ncol=3)
#' U1<- A1
#' U1[1,3]<- 0
#' # the upper right corner represents adult fertility in this model. U1, the
#' # survival matrix, contains all the transitions *except* for fertility.
#' N1<- fundamental_matrix(U1)
fundamental_matrix<- function(Umat){
  # Umat contains all the transitions for individuals (so Amat-Fmat)

  # the fundamental matrix, Nmat, is inv(I-U))
  Nmat<- matrixcalc::matrix.inverse(diag(dim(Umat)[1])-Umat)
  return(Nmat)
}
