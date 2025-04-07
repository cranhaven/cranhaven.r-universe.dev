## Auxiliary functions ---------------------------------------------------------


#' A function for variance assuming a complete sample
#'
#' The variance, assuming a complete sample, is the mean of the squared
#' deviations. When you assume an incomplete sample (the standard assumption in
#' statistics), the variance is calculated as the sum of squared deviations,
#' divided by (N-1), where N is the number of observations in the sample. As
#' such, the output of \code{variance_complete} will always be smaller than the
#' output of \code{var}.
#'
#' @param x A numeric vector that represents a complete sample.
#'
#' @return The variance of the entries of x, calculated with the assumption that
#' x represents a complete sample. Compare to the output of
#' \code{\link[stats]{var}}.
#' @export
#'
#' @seealso \code{\link[stats]{var}}
#'
#' @examples
#' test<- c(5, 6, 8, 10, 25)
#' Vc<- variance_complete(test)
#' # compare this output with that of var()
variance_complete <- function(x) {
  xx<- x[!is.na(x)]
  mean((xx - mean(xx))^2)
}

#' The variance-covariance matrix for a set of population projection matrices
#'
#' Calculate the variance-covariance matrix for the vital rates of a series of
#' population projection matrices.
#'
#' @param Aobj A matrix where each row is the column-wise vectorization of a
#' population projection matrix.
#'
#' @return The variance-covariance matrix for the vital rates of the population
#' projection matrices. If the dimensions of each matrix in \code{Aobj} are n-by-n,
#' then the variance-covariance matrix will have dimensions n^2-by-n^2.
#' @export
#'
#' @details The diagonal entries of the variance-covariance matrix give the
#' variance of the entries in each index of the matrix. The off-diagonal entries
#' of the variance-covariance matrix give the covariance of entries in each index
#' of the matrix. The variance-covariance matrix is symmetrical, and the
#' covariance of a given pair of vital rates is the sum of the two corresponding
#' indices.
#'
#' @examples
#' A1<- matrix(data=c(0,0.8,0, 0,0,0.7, 5,0,0.2), nrow=3, ncol=3)
#' A2<- matrix(data=c(0,0.9,0, 0,0,0.5, 4,0,0.3), nrow=3, ncol=3)
#' A3<- matrix(data=c(0,0.4,0, 0,0,0.6, 6,0,0.25), nrow=3, ncol=3)
#' A_all<- collapse_mat_list(list(A1, A2, A3))
#' covmat<- cov_matrix(A_all)
cov_matrix<- function(Aobj){
  # covariance matrix is: C = Expected[vec(A)*t(vec(A))] - vec(Amean)*t(vec(Amean))
  Amean<- apply(Aobj, 2, mean) # calculate the mean matrix (in vector form)
  second_term<- Amean%*%t(Amean)
  first_term_elements<- list() # initialize the holder for the part inside square brackets
  for (i in 1:dim(Aobj)[1]){
    first_term_elements[[i]]<- Aobj[i,]%*%t(Aobj[i,])
  }
  # first_term_elements is a list of matrices. We want the average matrix from those:
  first_term<- apply(simplify2array(first_term_elements), 1:2, mean)
  covmat<- first_term - second_term
  return(covmat)
}

# A function to reconstruct a matrix that was collapsed column-wise into a row vector:
#' Reconstruct a matrix that was collapsed into a row vector
#'
#' Reconstruct a matrix that was collapsed into a row vector. This function
#' assumes that the matrix is square (as population projection matrices are).
#' This function also assumes that the matrix was originally collapsed column-wise.
#'
#' @param vecM Either a single row containing a vectorized matrix, or a matrix
#' where each row is a column-wise vectorized matrix.
#' @param j Row-index of the target matrix to re-construct, if \code{vecM} is a
#' matrix. If \code{vecM} is a single row vector, then this input can be
#' neglected.
#'
#' @return A single square matrix
#' @export
#'
#' @examples
#' A1<- matrix(data=c(0,0.8,0, 0,0,0.7, 5,0,0.2), nrow=3, ncol=3)
#' A2<- matrix(data=c(0,0.9,0, 0,0,0.5, 4,0,0.3), nrow=3, ncol=3)
#' A3<- matrix(data=c(0,0.4,0, 0,0,0.6, 6,0,0.25), nrow=3, ncol=3)
#' A1_remat<- reMat(as.vector(A1))
#' A_all<- rbind(as.vector(A1), as.vector(A2), as.vector(A3))
#' A3_remat<- reMat(A_all, j=3)
reMat<- function(vecM,j=NULL){
  # vecM is a matrix where each row is vec(Aj), and j is the row index to be re-constructed

  # if vecM is a single row, we need to write it a little differently:
  if (is.null(dim(vecM))){
    matdim<- sqrt(length(vecM)) # this is only for making square matrices, so
    # the matrix dimensions are the square root of the length of the row vector.
    matted<- matrix(vecM, matdim, matdim)
  } else {
    if(is.null(j)){
      warning("You have not specified which row to reMat, assuming j=1.")
      j<- 1
    }
    matdim<- sqrt(length(vecM[j,]))
    matted<- matrix(vecM[j,], matdim, matdim)
  }
  return(matted)
}

# A function to collapse a list of matrices into rows:
#' Collapse a list of matrices
#'
#' Collapse a list of square matrices into a matrix where each row contains the
#' column-wise vectorization of one of the original matrices.
#'
#' @param Alist A list of matrices, of any length.
#'
#' @return A matrix where each row contains the column-wise vectorization of one
#' of the original matrices.
#' @export
#'
#' @examples
#' A1<- matrix(data=c(0,0.8,0, 0,0,0.7, 5,0,0.2), nrow=3, ncol=3)
#' A2<- matrix(data=c(0,0.9,0, 0,0,0.5, 4,0,0.3), nrow=3, ncol=3)
#' A3<- matrix(data=c(0,0.4,0, 0,0,0.6, 6,0,0.25), nrow=3, ncol=3)
#' A_all<- collapse_mat_list(list(A1, A2, A3))
collapse_mat_list<- function(Alist){
  # check what size the Aobj matrix needs to be:
  matdim<- dim(Alist[[1]])[1]
  # initialize the Aobj matrix:
  Aobj<-matrix(NA, nrow=length(Alist), ncol=matdim^2)

  for (i in 1:length(Alist)){
    Aobj[i,]<- as.vector(Alist[[i]])
  }
  return(Aobj)
}

# A function to compute the variance in lambda across a set of matrices, with some parameters fixed at their mean value:
#' Variance in lambda
#'
#' In population projection matrices, the eigenvalue with the largest magnitude
#' is the asymptotic population growth rate, referred to as lambda. This function
#' calculates the variance in lambda among a group of population projection
#' matrices, which must all be the same size. This function also has the option
#' to hold some of the vital rates at their mean values across all the provided
#' matrices. The resulting calculation is the variance of lambda with all the
#' non-fixed vital rates are varying. For example, if all the vital rates are
#' held fixed except for adult fertility, then the output is the variance in
#' lambda due to variance in adult fertility.
#'
#' @param Aobj An object containing all the population projection matrices to be
#'  included in the analysis. It should either be a list, or a matrix where each
#'  row is the column-wise vectorization of a matrix.
#' @param which.fixed The column-wise indices (single-value index) of the vital
#' rates to be held at their mean values across all matrices in \code{Aobj}.
#'
#' @return A single value of variance.
#' @export
#'
#' @examples
#' A1<- matrix(data=c(0,0.8,0, 0,0,0.7, 5,0,0.2), nrow=3, ncol=3)
#' A2<- matrix(data=c(0,0.9,0, 0,0,0.5, 4,0,0.3), nrow=3, ncol=3)
#' A3<- matrix(data=c(0,0.4,0, 0,0,0.6, 6,0,0.25), nrow=3, ncol=3)
#' A_all<- collapse_mat_list(list(A1, A2, A3))
#' var_all_vary<- lamVar(A_all)
#' var_fert_vary<- lamVar(A_all, which.fixed=c(2,6,9))
lamVar<- function(Aobj, which.fixed=NULL) {

  if (is.list(Aobj)){
    vecM<- collapse_mat_list(Aobj)
  } else {
    vecM<- Aobj
  }

  Mfix<- vecM
  # for each of the parameters to be held fixed, set it equal to the mean for
  # all matrices in the dataset
  if (length(which.fixed)>0){
    for(j in which.fixed) {
      Mfix[,j]<- mean(vecM[,j])
    }
  }

  lambdas<- numeric(nrow(Mfix)) # initialize lambdas
  # calculate the lambda of each population matrix (with the fixed parameters
  # held constant at their means)
  for(j in 1:nrow(Mfix)) {
    Mj<- reMat(Mfix,j)
    lambdas[j]<- max(Re(eigen(Mj, only.values = T)$values))
  }
  # calculate the variance in lambda
  return(variance_complete(lambdas))
}

# A function to compute the difference in lambda across a set of matrices, with some parameters fixed at their baseline value:
#' Difference in lambda
#'
#' In population projection matrices, the eigenvalue with the largest magnitude
#' is the asymptotic population growth rate, referred to as lambda. This
#' function calculates the difference in lambda between two population
#' projection matrices, which must have the same dimensions. This function also
#' has the option to hold some of the vital rates at the value in the baseline
#' matrix. The resulting calculation is the difference in lambda when all the
#' non-fixed vital rates are varying. For example, if all the vital rates are
#' held fixed except for adult fertility, then the output is the difference in
#' lambda due to difference in adult fertility. The difference is taken as
#' \eqn{baseline matrix - observed matrix}, and the function assumes that the
#' provided matrices are ordered \[baseline, observed\].
#'
#' This function differs from \code{lamDiff_symmetric} because it uses the first
#' matrix in \code{Aobj} as the baseline matrix. So fixed parameters are set to
#' the values in the baseline matrix. In \code{lamDiff_symmetric}, the fixed
#' parameters would be set to their mean values.
#'
#' \code{lamDiff} is most appropriate for comparisons between a control and
#' treatment population in a controlled experiment or other settings where one
#' of the populations can be considered as a standard-of-reference.
#' \code{lamDiff_symmetric} is more appropriate for comparisons where none of
#' the population matrices are obviously suitable as a baseline or
#' standard-of-reference (for example, when comparing a wet and a dry year).
#'
#' @param Aobj An object containing the population projection matrices to be
#'   included in the analysis. It should either be a list, or a matrix where
#'   each row is the column-wise vectorization of a matrix. Exactly 2 matrices
#'   should be provided. If more than 2 matrices are provided, the function will
#'   assume that the first is the baseline and the second is the observed matrix
#'   to be compared. Matrices beyond the first two will be ignored.
#'
#' @param which.fixed The column-wise indices (single-value index) of the vital
#' rates to be held at their baseline values across the matrices in \code{Aobj}.
#'
#' @return A single value for the difference in lambda.
#' @export
#'
#' @seealso \code{\link{lamDiff_symmetric}} \code{\link{lamVar}}
#'
#' @examples
#' Abaseline<- matrix(data=c(0,0.8,0, 0,0,0.7, 5,0,0.2), nrow=3, ncol=3)
#' Aobserved<- matrix(data=c(0,0.9,0, 0,0,0.5, 4,0,0.3), nrow=3, ncol=3)
#' A_all<- list(Abaseline,Aobserved)
#' diff_all_vary<- lamDiff(A_all)
#' diff_fert_vary<- lamDiff(A_all, which.fixed=c(2,6,9))
lamDiff<- function(Aobj, which.fixed=NULL) {
  # Aobj can either be a list of matrices, where Aobj[[1]] is Abaseline and
  # Aobj[[2]] is Aobserved or have 2 rows that are the vec(Abaseline) and
  # vec(Aobserved)

  if (is.list(Aobj)){
    if (length(Aobj)>2){
      warning("Aobj contains more than 2 matrices. lamDiff assumes Aobj[[1]] is Abaseline and Aobj[[2]] is Aobserved.")
    }
    else if (length(Aobj)<2){
      stop("Aobj contains fewer than 2 matrices, so we cannot compute the difference in lambda.")
    }
    Aobj<- rbind(as.vector(Aobj[[1]]), as.vector(Aobj[[2]])) # convert to the vec'ed format
    Mtest<- Aobj # make a copy so that we can do which.fixed and then calculate lambda's
  } else { # if Aobj isn't a list, then we assume it's a matrix.
    if (dim(Aobj)[1]>2){
      warning("Aobj contains more than 2 matrices. lamDiff assumes Aobj[1,] is vec(Abaseline) and Aobj[2,] is vec(Aobserved).")
    } else if (dim(Aobj)[1]<2){
      stop("Aobj contains fewer than 2 matrices, so we cannot compute the difference in lambda.")
    }
    Mtest<- Aobj[1:2,]
  }

  # for each of the parameters to be held fixed, set it equal to the corresponding
  # value in the baseline matrix
  if (length(which.fixed)>0){
    for(j in which.fixed) {
      Mtest[2,j]<- Aobj[1,j]
    }
  }

  lambdas<- numeric(nrow(Mtest)) # initialize lambdas
  # calculate the lambda of each population matrix (with the fixed parameters
  # held constant at their means)
  for(j in 1:nrow(Mtest)) {
    Mj<- reMat(Mtest,j)
    lambdas[j]<- max(Re(eigen(Mj, only.values = T)$values))
  }
  # calculate the difference in lambda as Aobserved-Abaseline:
  return(lambdas[2]-lambdas[1])
}

#' Difference in lambda, with the mean as baseline
#'
#' In population projection matrices, the eigenvalue with the largest magnitude
#' is the asymptotic population growth rate, referred to as lambda. This
#' function calculates the difference in lambda between two population
#' projection matrices, which must have the same dimensions. This function also
#' has the option to hold some of the vital rates at their mean values across
#' the provided matrices. The resulting calculation is the difference in lambda
#' when all the non-fixed vital rates are varying. For example, if all the vital
#' rates are held fixed except for adult fertility, then the output is the
#' difference in lambda due to difference in adult fertility. The difference is
#' taken as \eqn{observed matrix 1 - observed matrix 2}, where the provided
#' matrices are ordered \[observed matrix 1, observed matrix 2\].
#'
#' This function differs from \code{lamDiff} because it uses the mean
#' matrix as the baseline. So fixed parameters are set to their mean values. In
#' \code{lamDiff}, the fixed parameters would be set to their respective values
#' given by the baseline matrix.
#'
#' \code{lamDiff} is most appropriate for comparisons between a control and
#' treatment population in a controlled experiment or other settings where one
#' of the populations can be considered as a standard-of-reference.
#' \code{lamDiff_symmetric} is more appropriate for comparisons where none of
#' the population matrices are obviously suitable as a baseline or
#' standard-of-reference (for example, when comparing a wet and a dry year).
#'
#' @param Aobj An object containing the population projection matrices to be
#'   included in the analysis. It should either be a list, or a matrix where
#'   each row is the column-wise vectorization of a matrix. Exactly 2 matrices
#'   should be provided. If more than 2 matrices are provided, the function will
#'   only use the first two.
#'
#' @param which.fixed The column-wise indices (single-value index) of the vital
#' rates to be held at their mean values across the matrices in \code{Aobj}.
#'
#' @return A single value for the difference in lambda.
#' @export
#'
#' @seealso \code{\link{lamDiff}} \code{\link{lamVar}}
#'
#' @examples
#' Aobs1<- matrix(data=c(0,0.8,0, 0,0,0.7, 5,0,0.2), nrow=3, ncol=3)
#' Aobs2<- matrix(data=c(0,0.9,0, 0,0,0.5, 4,0,0.3), nrow=3, ncol=3)
#' A_all<- list(Aobs1,Aobs2)
#' diff_all_vary<- lamDiff_symmetric(A_all)
#' diff_fert_vary<- lamDiff_symmetric(A_all, which.fixed=c(2,6,9))
lamDiff_symmetric<- function(Aobj, which.fixed=NULL) {
  # Aobj can either be a list of matrices, where Aobj[[1]] is Aobs1 and Aobj[[2]] is Aobs2
  # or have 2 rows that are the vec(Aobs1) and vec(Aobs2)

  if (is.list(Aobj)){
    if (length(Aobj)>2){
      warning("Aobj contains more than 2 matrices. lamDiff will compare the first two, and disregard the rest.")
    }
    else if (length(Aobj)<2){
      stop("Aobj contains fewer than 2 matrices, so we cannot compute the difference in lambda.")
    }
    Mtest<- rbind(as.vector(Aobj[[1]]), as.vector(Aobj[[2]])) # convert to the vec'ed format
    # calculate the mean matrix
    Amean<- mean_matrix(Aobj)
  } else { # if Aobj isn't a list, then we assume it's a matrix.
    if (dim(Aobj)[1]>2){
      warning("Aobj contains more than 2 matrices. lamDiff will compare the first two, and disregard the rest.")
    } else if (dim(Aobj)[1]<2){
      stop("Aobj contains fewer than 2 matrices, so we cannot compute the difference in lambda.")
    }
    Mtest<- Aobj[1:2,]
    Amean<- apply(Mtest, 2, FUN=mean)
  }

  # for each of the parameters to be held fixed, set it equal to the corresponding
  # value in the mean matrix
  if (length(which.fixed)>0){
    for(j in which.fixed) {
      Mtest[2,j]<- Amean[j]
      Mtest[1,j]<- Amean[j]
    }
  }

  lambdas<- numeric(nrow(Mtest)) # initialize lambdas
  # calculate the lambda of each population matrix (with the fixed parameters
  # held constant at their means)
  for(j in 1:nrow(Mtest)) {
    Mj<- reMat(Mtest,j)
    lambdas[j]<- max(Re(eigen(Mj, only.values = T)$values))
  }
  # calculate the difference in lambda as Aobs2-Aobs1:
  return(lambdas[2]-lambdas[1])
}

# recursive function for making a Gmatrix (from Poelwijk, Krishna, Ranganathan 2016 paper):
#' G-matrix
#'
#' The G-matrix is the operator used for calculating a vector of effects from a
#' vector of responses, up to arbitrary interaction order. This recursive function
#' for building up the G-matrix is presented in a paper about genetic epistasis
#' from Poelwijk, Krishna, and Ranganathan (2016, PLOS Comp Bio
#' \doi{10.1371/journal.pcbi.1004771}). When the G-matrix is
#' multiplied on the right by a column vector of the observed responses, it will
#' produce a column vector of the effects. In other words, the G-matrix adds and
#' subtracts off the appropriate lower-order terms to arrive at the correct values
#' of interaction effects.
#'
#' @param n The number of observed parameters, mutation sites, etc.
#'
#' @return A matrix that is 2^n-by-2^n.
#' @export
#'
#' @examples
#' Gmat<- make.Gmatrix(3)
make.Gmatrix<-function(n) {
  G<- 1;
  for(k in 1:n) {
    topMat<- cbind(G, 0*G)
    botMat<- cbind(-G, G)
    G<- rbind(topMat, botMat)}
  return(G)
}

# A lower-level function for calculating the responses of a matrix, according to some other function:
#' Responses to changing entries of a matrix
#'
#' Calculate the responses of varying certain entries of a matrix. This function
#' takes a set of matrix population models, the indices of parameters that vary
#' in those matrices, and a response function. For example, difference or variance
#' in lambda (the leading eigenvalue of a population projection matrix).
#'
#' @param Aobj An object containing all the population projection matrices to be
#'  included in the analysis. It should either be a list, or a matrix where each
#'  row is the column-wise vectorization of a matrix.
#' @param ind_vary A vector containing the column-wise (single-value) indices of
#' the population projection matrices that vary.
#' @param FUN The name of the function to be used for calculating responses. For
#' example, \code{\link{lamVar}}, \code{\link{lamDiff}}, and \code{\link{lamDiff_symmetric}}
#' @param maxint The maximum interaction order to be evaluated. The default input
#' is "all" but this input can take any integer value. If maxint=3, then the
#' output will include contributions terms up to 3-way interactions.
#'
#' @return This returns a list object, with 2 items: (1) a
#' list of the indices varying for each of the responses terms; and (2)
#' a vector of responses.
#'
#'  \code{list_ind_vary} is a list object, where each entry is a vector
#'  containing the indices (the combinations of the elements of `ind_vary`, an
#'  input parameter) that varied (were *not* held fixed) for the corresponding
#'  entry in the `nu` vector.
#'
#'  \code{nus} is a vector of responses, calculated using the function provided
#'  in \code{FUN}.
#' @export
#'
#' @seealso \code{\link{lamVar}}, \code{\link{lamDiff}}, and \code{\link{lamDiff_symmetric}}
#'
#' @examples
#' A1<- matrix(data=c(0,0.8,0, 0,0,0.7, 5,0,0.2), nrow=3, ncol=3)
#' A2<- matrix(data=c(0,0.9,0, 0,0,0.5, 4,0,0.3), nrow=3, ncol=3)
#' A3<- matrix(data=c(0,0.4,0, 0,0,0.6, 6,0,0.25), nrow=3, ncol=3)
#' A_all<- collapse_mat_list(list(A1, A2, A3))
#' nu_var<- calc_matrix_responses(A_all, c(2,6,7,9), FUN=lamVar, maxint="all")
#' nu_diff<- calc_matrix_responses(list(A1,A2), c(2,6,7,9), FUN=lamDiff_symmetric, maxint="all")
calc_matrix_responses<- function(Aobj, ind_vary, FUN, maxint="all"){
  # count how many indices vary:
  n_vary<- length(ind_vary)

  if (maxint=="all"){ # use expand.grid:
    # find all the combinations of ind_vary for generating variances (or differences) in lambda (the nu's):
    bin_order<- expand.grid(replicate(n_vary, c(0,1), simplify=FALSE), KEEP.OUT.ATTRS = FALSE)
  } else {
    if (!is.numeric(maxint)){
      stop("Input variable maxint, the maximum interaction order to calculate, must either be ''all'' or a numeric variable.")
    } else if (maxint>=n_vary){
      warning("You have requested an interaction order that is greater than or equal to the number of parameters that vary, defaulting to maxint='all'.")
      # find all the combinations of ind_vary for generating variances (or differences) in lambda (the nu's):
      bin_order<- expand.grid(replicate(n_vary, c(0,1), simplify=FALSE), KEEP.OUT.ATTRS = FALSE)
      maxint<- "all"
    } else {
      list_ind_vary<- list()
      list_ind_vary[[1]]<- vector() # row 1 is empty, fix all of the variables
      for (i in 1:maxint){
        newrows<- t(utils::combn(ind_vary, i))
        newrows<- lapply(seq_len(nrow(newrows)), function(i) newrows[i,])
        list_ind_vary<- c(list_ind_vary, newrows)
      }
    }
  }
  # okay, now we have either bin_order or list_ind_vary, both of which have the
  # necessary information to calculate the nu vector

  if (maxint=="all"){ # this is the bin_order method.
    # convert bin_order to list_ind_vary:
    list_ind_vary<- list()
    list_ind_vary[[1]]<- vector()
    for (i in 2:dim(bin_order)[1]){
      newlistitem<- ind_vary[which(bin_order[i,]==1)]
      list_ind_vary[[i]]<- newlistitem
    }
    rm(bin_order)
  }

  # the number of nus is the number of list entries in list_ind_vary
  n_nus<- length(list_ind_vary)
  # initialize the holder for the nu values:
  nus<- numeric(length=n_nus)

  # loop for calculating the nus:
  for (i in 1:n_nus){
    ind_i<- list_ind_vary[[i]]
    # fix.these should be the indices that vary that are not in ind_i:
    fix.these<- ind_vary[!(ind_vary %in% ind_i)]
    nus[i]<- FUN(Aobj, fix.these)
  }
  # need to return the nus and the list_ind_vary:
  results<- list(nus=nus, list_ind_vary=list_ind_vary)
  return(results)

}

# An alternative to using the Gmatrix approach, this converts from responses to effects:
#' Effects of matrix parameters
#'
#' Calculate the effects of differences or variance in vital rate entries in a
#' set of matrix population models, for a given set of interaction terms.
#'
#' @param responses A vector of the responses due to the parameters of a matrix
#' population model and their interactions.
#' @param list_ind_vary A list object, where each entry is a vector
#'  containing the indices (the combinations of the elements of `ind_vary`, an
#'  input parameter) that varied (were *not* held fixed) for the corresponding
#'  entry in the `responses` vector.
#'
#' @return A vector of effects, which are the responses due to a given interaction
#' order, controlling for the lower-level interaction terms. These are the epsilon
#' terms in an fANOVA decomposition.
#' @export
#'
#' @examples
#' A1<- matrix(data=c(0,0.8,0, 0,0,0.7, 5,0,0.2), nrow=3, ncol=3)
#' A2<- matrix(data=c(0,0.9,0, 0,0,0.5, 4,0,0.3), nrow=3, ncol=3)
#' A3<- matrix(data=c(0,0.4,0, 0,0,0.6, 6,0,0.25), nrow=3, ncol=3)
#' A_all<- collapse_mat_list(list(A1, A2, A3))
#' nu_var<- calc_matrix_responses(A_all, c(2,6,7,9), FUN=lamVar, maxint="all")
#' nu_diff<- calc_matrix_responses(list(A1,A2), c(2,6,7,9), FUN=lamDiff_symmetric, maxint="all")
#' epsilon_var<- calc_matrix_effects(nu_var$nus, nu_var$list_ind_vary)
#' epsilon_diff<- calc_matrix_effects(nu_diff$nus, nu_diff$list_ind_vary)
calc_matrix_effects<- function(responses, list_ind_vary){
  effects<- vector(length=length(list_ind_vary))
  effects[1]<- responses[1]
  term_order<- sapply(list_ind_vary, length)

  for (i in 2:length(effects)){# go row-wise, starting with row 2
    ind_i<- list_ind_vary[[i]] # which indices were varying for this row?

    thisrow<- rep(0, length=length(effects))
    thisrow[i]<- 1 # always need to include the response of the term of interest

    # we only need to check the entries that are lower order than term_order[i]
    tocheck<- which(term_order<term_order[i])

    for (j in tocheck){
      ind_j<- list_ind_vary[[j]]

      if (sum(ind_j %in% ind_i)==length(ind_j)){
        thisrow[j]<- ifelse(term_order[i]%%2 == term_order[j]%%2, 1, -1)
      }
    }
    # multiply this row by the response vector to get the effects entry
    effects[i]<- thisrow%*%responses

  }
  return(effects)
}

# Run matrix checks on an Aobj array (each row is a matrix, collapsed column-wise):
#' Automated checks for a set of matrices
#'
#' Run automated checks for a set of population projection matrices. This code will
#' check if the matrices are square, strictly non-negative, ergodic, irreducible,
#' and primitive. The last check is for whether the column sums are greater than 1
#' for presumed survival terms. For this portion, the code assumes that the first
#' row represents only fertility, and that all other matrix entries represent
#' only survival.
#'
#' @param Aobj An object containing the population projection matrices to be
#'  included in the analysis. It should either be a list, or a matrix where each
#'  row is the column-wise vectorization of a matrix.
#'
#' @return If all the checks pass, then nothing is returned. If one of the checks
#' fails, then an error or warning message will be returned.
#' @export
#'
#' @examples
#' A1<- matrix(data=c(0,0.8,0, 0,0,0.7, 5,0,0.2), nrow=3, ncol=3)
#' A2<- matrix(data=c(0,0.9,0, 0,0,0.5, 4,0,0.3), nrow=3, ncol=3)
#' A3<- matrix(data=c(0,0.4,0, 0,0,0.6, 6,0,0.25), nrow=3, ncol=3)
#' run_matrix_checks(list(A1,A2,A3))
#' # A couple of examples that would throw errors or warnings:
#' # run_matrix_checks(c(0,0.8,0, -1,0,0.7, 5,0,0.2)) # has a negative value
#' # run_matrix_checks(c(0,0.8,0, 0,0.5,0.7, 5,0,0.2)) # has a column-sum greater than 1
run_matrix_checks<- function(Aobj){
  # If Aobj is a list, collapse to run the checks:
  if (is.list(Aobj)){
    Aobj<- collapse_mat_list(Aobj)
  }
  if (is.null(dim(Aobj))){
    Aobj<- matrix(Aobj, nrow=1)
  }

  # Check 1: are they all square matrices?
  n_elements<- length(Aobj[1,])
  if (sqrt(n_elements)%%1!=0){
    stop("Your matrices do not appear to be square.", call.=FALSE)
  }
  # Check 2: Are all elements non-negative?
  if (sum(Aobj<0)>0){
    stop("Population matrices must be strictly non-negative.", call.=FALSE)
  }

  # A few checks that we have to run after reshaping each matrix:
  for (i in 1:dim(Aobj)[1]){
    this_matrix<- reMat(Aobj, i)

    # Check 3: Is the matrix ergodic?
    if (popdemo::isErgodic(this_matrix)==FALSE){
      warning(paste("Matrix", i, "is not ergodic."), call.=FALSE)
    }

    # Check 4: is the matrix irreducible?
    if (popdemo::isIrreducible(this_matrix)==FALSE){
      warning(paste("Matrix", i, "is reducible."), call.=FALSE)
    }
    # Check 5: is the matrix primitive?
    if (popdemo::isPrimitive(this_matrix)==FALSE){
      warning(paste("Matrix", i, "is imprimitive."), call.=FALSE)
    }

    # Check 5: Check survival sums are not greater than 1.
    # assume that the first row is fecundity and all other entries represent survival/growth
    survival<- this_matrix[-1,]
    # if this_matrix is 2x2, the survival will be a vector. need to treat that differently than a matrix.
    if (is.null(dim(survival))){
      surv_sums<- survival
    } else { # if survival is still a vector, then we take the column sums:
      surv_sums<- apply(survival, 2, FUN=sum, na.rm=TRUE)
    }
    if (sum(surv_sums>1)>0){
      warning(paste("Survival terms may be misspecified. Matrix", i, "has column sums greater than 1 (assuming row 1 and only row 1 represents fecundity)."),
              call.=FALSE)
    }
  }
}
