## Main LTRE functions


# a wrapper function for classical LTRE:
#' Classical LTRE analysis
#'
#' Life Table Response Experiments (LTREs) are a method of comparative demographic
#' analysis. The purpose is to quantify how the difference or variance in vital
#' rates (stage-specific survival, growth, and fertility) among populations
#' contributes to difference or variance in the population growth rate, "lambda."
#' The equations and descriptions for the classical methods of LTRE analysis
#' can be found in Caswell's 2001 textbook. The function we provide here can
#' perform a one-way fixed design LTRE, or a random design LTRE.
#'
#' @param Aobj An object containing all the population projection matrices to be
#'  included in the analysis. It should either be a list, or a matrix where each
#'   row is the column-wise vectorization of a matrix.
#'
#'  For one-way fixed design, exactly 2 matrices must be provided, ordered as
#'  `[reference matrix, treatment matrix`]. For random design, any set of 2 or
#'  more matrices can be provided. The set of matrices passed in to this
#'  function must all have the same dimensions.
#'
#' @param method Either "random" or "fixed." The default behavior is "random."
#' See details for more information.
#'
#' @return A matrix of contributions to variance (random design) or difference
#' (one-way fixed design) in lambda. Lambda is the asymptotic population growth
#' rate, defined as the largest eigenvalue of the population projection matrix.
#'
#' @export
#'
#'@details  Lambda is the asymptotic population growth rate, defined as the
#'  largest eigenvalue of the population projection matrix. A one-way fixed
#'  design LTRE decomposes the difference in lambda due to differences at each
#'  position of the matrices. It should be used when the particular treatment
#'  levels are of interest. For a one-way fixed design LTRE, exactly 2 matrices
#'  must be provided, ordered as `[reference matrix, treatment matrix`]. The
#'  matrix of contributions returned from a classical method fixed design LTRE
#'  will have the same shape as the provided matrices.
#'
#'  A random design LTRE decomposes the variance in lambda due to variance and
#'  covariance in the entries at each position in the matrices. It assumes that
#'  the matrices being analyzed come from a population of similar matrices,
#'  without the particular treatment levels or population conditions being of
#'  interest in themselves. For a random design LTRE, at least 2 matrices must be
#'  provided. The matrix of contributions returned from a classical method
#'  random design LTRE will include both first-order terms (due to variance) and
#'  interaction terms (due to covariance). Therefore, if the provided matrix is
#'  3x3, the matrix of contributions will be 9x9 (the size of the
#'  variance-covariance matrix is the square of the size of the original
#'  matrix). The contributions of variances are found on the diagonal of the
#'  contribution matrix, and the contributions of covariances are symmetric. So
#'  the contribution of covariance between two vital rate parameters is the sum
#'  of the two corresponding off-diagonal terms.
#'
#'  The equations and descriptions for the classical methods of LTRE analysis
#'  can be found in Caswell's 2001 textbook.
#'
#' @examples
#' A1<- matrix(data=c(0,0.8,0, 0,0,0.7, 5,0,0.2), nrow=3, ncol=3)
#' A2<- matrix(data=c(0,0.9,0, 0,0,0.5, 4,0,0.3), nrow=3, ncol=3)
#' A3<- matrix(data=c(0,0.4,0, 0,0,0.6, 6,0,0.25), nrow=3, ncol=3)
#' cont_diff<- classicalLTRE(list(A1,A2), method='fixed') # contributions to the difference in lambda
#' cont_var<- classicalLTRE(list(A1,A2,A3), method='random') # contributions to the variance of lambda
classicalLTRE<- function(Aobj, method="random"){

  # if Aobj is passed in as a list, collapse into the row-format:
  if (is.list(Aobj)){
    Aobj<- collapse_mat_list(Aobj)
  }

  if (method=="random"){
    output<- classicalLTRE_random(Aobj)
  } else if (method=="fixed"){
    # For fixed LTRE, it is important that Aobj is 2 rows. Row 1 contains vec(Aref), and Row 2 contains vec(Atreatment)
    Aref<- reMat(Aobj,1)
    Atreatment<- reMat(Aobj,2)
    output<- classicalLTRE_fixed(Aref, Atreatment)
  }
  return(output)
}

# A wrapper function for exact LTRE:
#' Exact LTRE analysis
#'
#' Life Table Response Experiments (LTREs) are a method of comparative demographic
#' analysis. The purpose is to quantify how the difference or variance in vital
#' rates (stage-specific survival, growth, and fertility) among populations
#' contributes to difference or variance in the population growth rate, "lambda."
#' The exact method of LTRE is based on the principles of functional ANOVA.
#' The equations and descriptions for the exact method will be published in a
#' forthcoming paper, which we will link to.
#'
#' @param Aobj An object containing all the population projection matrices to be
#'  included in the analysis. It should either be a list, or a matrix where each
#'   row is the column-wise vectorization of a matrix.
#'
#'  For one-way fixed design, exactly 2 matrices must be provided. For a
#'  "directional" analyses, one of the provided matrices will serve as the
#'  baseline state (for example, a control or standard-of-reference). In this
#'  case, the matrices need to be ordered as `[baseline matrix, observed matrix`].
#'  For random design, any set of 2 or more matrices can be provided.
#'  The set of matrices passed in must all have the same dimensions.
#'
#' @param method Either "random" or "fixed." The default behavior is "random."
#' See details for more information.
#'
#' @param maxint The maximum interaction order to be evaluated. The default input
#' is "all" but this input can take any integer value. If maxint=3, then the
#' output will include contributions terms up to 3-way interactions.
#'
#' @param fixed.directional A true/false switch that allows the user to specify
#' whether a directional LTRE should be used. The default behavior is to calculate
#' a symmetric LTRE, where the mean matrix is used as the baseline matrix. See
#' details for more guidance.
#'
#' @return This returns a list object, with 3 items: (1) a vector of the matrix
#' indices where the parameters vary between/among the matrices in Aobj; (2) a
#' list of the indices varying for each of the contribution terms provided; (3)
#' a vector of the contribution terms.If the method is "fixed" then these are
#' contributions to the difference in lambda. If the method is "random" then
#' these are the contributions to the variance in lambda.
#'
#' `indices.varying` is a vector with the indices of parameters that vary. The
#'  numeric indices count down the columns of a given population projection
#'  matrix. For example, in a 3x3 matrix, the (2,2) position would be identified
#'  with a 5.
#'
#'  `varying.indices.list` is a list object, where each entry is a vector
#'  containing the indices (matching the `indices.varying` part of the output)
#'  that differed or varied for the corresponding entry in the `epsilon` vector.
#'
#'  `epsilon` is a vector of contributions to the variance or difference in
#'  lambda due to the observed values of the various life history parameters.
#'  For example, the contribution of adult survival to the Var(lambda) is
#'  determined by setting all parameters *except* adult survival to their mean
#'  values, and then calculating the variance of lambda across this manipulated
#'  set of matrices.
#'
#' @details  Lambda is the asymptotic population growth rate, defined as the
#'  largest eigenvalue of the population projection matrix.
#'
#'  In a one-way fixed design LTRE, the particular treatment levels or
#'  conditions faced by a population are of interest, so one-way fixed design
#'  LTRE decomposes the difference in lambda due to differences at each position
#'  of the matrices. For a fixed design LTRE, exactly 2 matrices must be
#'  provided.
#'
#'  A random design LTRE treats the different matrices being compared as random
#'  samples from a set of population conditions, without specific focus on the
#'  treatments or conditions that each population experienced. This analysis
#'  decomposes the variance in lambda due to variance and covariance in the
#'  entries at each position in the matrices. For a random design LTRE, at least
#'  2 matrices must be provided.
#'
#'  \code{fixed.directional=FALSE} is most appropriate for comparisons where
#'  none of the matrices are appropriate for use as a baseline or
#'  standard-of-reference. For example, if we want to ask which vital rates
#'  drive the difference in population growth rate for two populations of fish
#'  in separate but similar lakes, we want to use a *symmetric* analysis. In
#'  this case, the difference in lambda is decomposed using the mean matrix as
#'  the baseline. The decomposition is symmetric, meaning that if the treatment
#'  and reference matrix are swapped, the contributions from the vital rates
#'  will be equal in magnitude, but positive contributions will become negative
#'  and vice versa. In this case, it does not matter which order you provide the
#'  matrices in \code{Aobj}, but interpretation will require that you pay
#'  attention to the fact that the sum of contributions will be equal to the
#'  observed difference in lambda between the two matrices in \code{Aobj},
#'  evaluated as lambda(A1) - lambda(A2).
#'
#'  \code{fixed.directional=TRUE} is most appropriate for comparisons between a
#'  control and treatment population in a controlled experiment, or for other
#'  cases where one of the populations can serve as a standard of reference (for
#'  example, the lowest elevation population, or one near the range center). In
#'  this case, the first matrix in \code{Aobj} is used as the baseline. This is
#'  a directional analysis, meaning that if the order in which the two matrices
#'  are provided were to be swapped (switching the baseline matrix and other
#'  observed matrix), the contributions of the vital rates would change. If you
#'  choose a directional analysis, be sure to provide the matrices in
#'  \code{Aobj} ordered as `[baseline matrix, observed matrix`].
#'
#'  We set \code{fixed.directional=FALSE} as the default behavior because most
#'  population projection models are built with field-collected data rather than
#'  controlled experiment data.
#'
#'
#' @export
#'
#' @examples
#' A1<- matrix(data=c(0,0.8,0, 0,0,0.7, 5,0,0.2), nrow=3, ncol=3)
#' A2<- matrix(data=c(0,0.9,0, 0,0,0.5, 4,0,0.3), nrow=3, ncol=3)
#' A3<- matrix(data=c(0,0.4,0, 0,0,0.6, 6,0,0.25), nrow=3, ncol=3)
#' cont_diff<- exactLTRE(list(A1,A2), method='fixed') # contributions to the difference in lambda
#' cont_var<- exactLTRE(list(A1,A2,A3), method='random') # contributions to the variance of lambda
exactLTRE<- function(Aobj, method="random", maxint="all", fixed.directional=FALSE){

  # if Aobj is passed in as a list, collapse into the row-format:
  if (is.list(Aobj)){
    Aobj<- collapse_mat_list(Aobj)
  }

  if (method=="random"){
    output<- exactLTRE_random(Aobj, maxint)
  } else if (method=="fixed"){
    output<- exactLTRE_fixed(Aobj, maxint, fixed.directional)
  }
  return(output)
}


#' Classical LTRE analysis: one-way fixed design
#'
#' Life Table Response Experiments (LTREs) are a method of comparative demographic
#' analysis. The purpose is to quantify how the difference or variance in vital
#' rates (stage-specific survival, growth, and fertility) among populations '
#' contributes to the difference or variance in the population growth rate,
#' "lambda." ' The equations and descriptions for the classical methods of LTRE
#' analysis ' can be found in Caswell's 2001 textbook.
#'
#' @param Aref The population projection matrix of the reference population.
#' Depending on the experimental or observational dataset, this may be the
#' control treatment, the first time period, the unharvested population, etc.
#'
#' @param Atreatment The population projection matrix of a treatment population.
#'
#' @return A matrix of contributions to the difference in lambda. Lambda is the
#' asymptotic population growth rate, defined as the largest eigenvalue of the
#' population projection matrix.
#'
#' @export
#'
#'@details  Lambda is the asymptotic population growth rate, defined as the
#'  largest eigenvalue of the population projection matrix. A fixed design LTRE
#'  decomposes the difference in lambda due to differences at each position of
#'  the matrices. For a fixed design LTRE, exactly 2 matrices must be provided,
#'  ordered as `[reference matrix, treatment matrix`]. The matrix of contributions
#'  returned from a classical method fixed design LTRE will have the same shape
#'  as the provided matrices.
#'
#'  In some cases, it may not be obvious how to identify the reference and the
#'  treatment matrix. The sum of contributions will be approximately equal to the
#'  observed difference in lambda between these two matrices, evaluated as
#'  lambda(Atreatment) - lambda(Aref). In cases where it doesn't 'matter' which way
#'  you, as a user, input these matrices, it is important to understand how to
#'  interpret positive and negative contributions.
#'
#'  The equations and descriptions for the classical methods of LTRE analysis
#'  can be found in Caswell's 2001 textbook.
#'
#' @examples
#' A1<- matrix(data=c(0,0.8,0, 0,0,0.7, 5,0,0.2), nrow=3, ncol=3)
#' A2<- matrix(data=c(0,0.9,0, 0,0,0.5, 4,0,0.3), nrow=3, ncol=3)
#' cont_diff<- classicalLTRE_fixed(A1, A2) # contributions to the difference in lambda
classicalLTRE_fixed<- function(Aref, Atreatment){

  # run some matrix checks and return warnings as needed:
  run_matrix_checks(rbind(as.vector(Aref), as.vector(Atreatment)))

  Amean<- (Atreatment+Aref)/2 # define the mean matrix
  eigz<- eigen(Amean) # get eigenvalues and eigenvectors
  ilambda<- which(Re(eigz$values)==max(Re(eigz$values)))
  wmean<- Re(eigz$vectors[,ilambda]) # right eigenvector of the mean matrix
  vmean<- Re(eigen(t(Amean))$vectors[,ilambda]) # left eigenvector of the mean matrix
  sensmat<- vmean%*%t(wmean)/as.vector(vmean%*%wmean)
  diffmat<- Atreatment-Aref
  C_m<- diffmat*sensmat
  return(C_m)
}

#' Classical LTRE analysis: random design
#'
#' Life Table Response Experiments (LTREs) are a method of comparative demographic
#' analysis. The purpose is to quantify how the difference or variance in vital
#' rates (stage-specific survival, growth, and fertility) among populations
#' contributes to the difference or variance in the population growth rate, "lambda."
#' The equations and descriptions for the classical methods of LTRE analysis
#' can be found in Caswell's 2001 textbook.
#'
#' @param Aobj An object containing all the population projection matrices to be
#'  included in the analysis. It should either be a list, or a matrix where each
#'  row is the column-wise vectorization of a matrix. Any set of 2 or more
#'  matrices can be provided. The set of matrices passed in must all have the
#'  same dimensions.
#'
#' @return A matrix of contributions to variance in lambda.
#'
#' @export
#'
#'@details  Lambda is the asymptotic population growth rate, defined as the
#'  largest eigenvalue of the population projection matrix.
#'
#'  A random design LTRE decomposes the variance in lambda due to variance and
#'  covariance in the entries at each position in the matrices. For a random
#'  design LTRE, at least 2 matrices must be provided. The matrix of
#'  contributions returned from a classical method random design LTRE will
#'  include both first-order terms (due to variance) and interaction terms (due
#'  to covariance). Therefore, if the provided matrix is 3x3, the matrix of
#'  contributions will be 9x9 (the size of the variance-covariance matrix is the
#'  square of the size of the original matrix). The contributions of variances
#'  are found on the diagonal of the contribution matrix, and the contributions
#'  of covariances are symmetric. So the contribution of covariance between two
#'  vital rate parameters is the sum of the two corresponding off-diagonal terms.
#'
#'  The equations and descriptions for the classical methods of LTRE analysis
#'  can be found in Caswell's 2001 textbook.
#'
#' @examples
#' A1<- matrix(data=c(0,0.8,0, 0,0,0.7, 5,0,0.2), nrow=3, ncol=3)
#' A2<- matrix(data=c(0,0.9,0, 0,0,0.5, 4,0,0.3), nrow=3, ncol=3)
#' A3<- matrix(data=c(0,0.4,0, 0,0,0.6, 6,0,0.25), nrow=3, ncol=3)
#' # contributions to the variance of lambda
#' cont_var<- classicalLTRE(list(A1,A2,A3), method='random')
classicalLTRE_random<- function(Aobj){
  # each row of Aobj should be the elements of an individual population matrix, collapsed column-wise
  # if Aobj is passed in as a list, collapse into the row-format:
  if (is.list(Aobj)){
    Aobj<- collapse_mat_list(Aobj)
  }

  # run some matrix checks and return warnings as needed:
  run_matrix_checks(Aobj)

  # covariance matrix for the parameters in Aobj:
  Cmat<- cov_matrix(Aobj)

  # calculate the mean parameter values and reshape into a square matrix:
  Amean<- matrix(apply(Aobj,2,mean),sqrt(dim(Aobj)[2]),sqrt(dim(Aobj)[2]))

  # calculate the sensitivity, evaluated at the mean matrix:
  eigz<- eigen(Amean) # get eigenvalues and eigenvectors
  ilambda<- which(Re(eigz$values)==max(Re(eigz$values)))
  wmean<- Re(eigz$vectors[,ilambda]) # right eigenvector of the mean matrix
  vmean<- Re(eigen(t(Amean))$vectors[,ilambda]) # left eigenvector of the mean matrix
  sensmat<- vmean%*%t(wmean)/as.vector(vmean%*%wmean)

  # calculate contributions according to: Cov(aij,akl)*sij*skl
  s<- as.vector(sensmat) # this is vec
  contmat<- Cmat*(s%*%t(s)) # Cmat *hadamard* vec(s)*t(vec(s))
  return(contmat)
}


#' Exact LTRE analysis: random design
#'
#' Life Table Response Experiments (LTREs) are a method of comparative demographic
#' analysis. The purpose is to quantify how the difference or variance in vital
#' rates (stage-specific survival, growth, and fertility) among populations
#' contributes to difference or variance in the population growth rate, "lambda."
#' The exact method of LTRE is based on the principles of functional ANOVA.
#' The equations and descriptions for the exact method will be published in a
#' forthcoming paper, which we will link to.
#'
#' @param Aobj An object containing all the population projection matrices to be
#' included in the analysis. It should either be a list, or a matrix where each
#' row is the column-wise vectorization of a matrix. For random design, any
#' set of 2 or more matrices can be provided. The set of matrices passed in must
#' all have the same dimensions.
#'
#' @param maxint The maximum interaction order to be evaluated. The default input
#' is "all" but this input can take any integer value. If maxint=3, then the
#' output will include contributions terms up to 3-way interactions.
#'
#' @return This returns a list object, with 3 items: (1) a vector of the matrix
#' indices where the parameters vary between/among the matrices in Aobj; (2) a
#' list of the indices varying for each of the contribution terms provided; (3)
#' a vector of the contribution terms.If the method is "fixed" then these are
#' contributions to the difference in lambda. If the method is "random" then
#' these are the contributions to the variance in lambda.
#'
#' `indices.varying` is a vector with the indices of parameters that vary. The
#'  numeric indices count down the columns of a given population projection
#'  matrix. For example, in a 3x3 matrix, the (2,2) position would be identified
#'  with a 5.
#'
#'  `varying.indices.list` is a list object, where each entry is a vector
#'  containing the indices (matching the `indices.varying` part of the output)
#'  that varied for the corresponding entry in the `epsilon` vector.
#'
#'  `epsilon` is a vector of contributions to the variance in lambda due to the
#'  observed values of the various life history parameters. For example, the
#'  contribution to the variance in lambda of adult survival is determined by
#'  setting all parameters *except* adult survival to their mean values, and
#'  then calculating the variance in lambda in this manipulated set of matrices.
#'
#' @details  Lambda is the asymptotic population growth rate, defined as the
#'  largest eigenvalue of the population projection matrix. A random design LTRE
#'  decomposes the variance in lambda due to variance and covariance in the
#'  entries at each position in the matrices. For a random design LTRE, at least
#'  2 matrices must be provided.
#'
#' @export
#'
#' @examples
#' A1<- matrix(data=c(0,0.8,0, 0,0,0.7, 5,0,0.2), nrow=3, ncol=3)
#' A2<- matrix(data=c(0,0.9,0, 0,0,0.5, 4,0,0.3), nrow=3, ncol=3)
#' A3<- matrix(data=c(0,0.4,0, 0,0,0.6, 6,0,0.25), nrow=3, ncol=3)
#' cont_var<- exactLTRE_random(list(A1,A2,A3), maxint='all') # contributions to the variance of lambda
exactLTRE_random<- function(Aobj, maxint="all"){
  # each row of Aobj should be the elements of an individual population matrix, collapsed column-wise
  # if Aobj is passed in as a list, collapse into the row-format:
  if (is.list(Aobj)){
    Aobj<- collapse_mat_list(Aobj)
  }

  # run some matrix checks and return warnings as needed:
  run_matrix_checks(Aobj)

  #find the indices of the columns in Aobj that have non-zero variance. In other
  # words, which matrix entries vary among populations?
  ind_vary<- which(apply(Aobj, MARGIN=2, FUN=variance_complete)>0)

  # calculate the responses, the vector nu: aka the variance in lambda,
  # given each possible combination of parameters varying or held at their mean:
  responses<- calc_matrix_responses(Aobj, ind_vary, FUN=lamVar, maxint)

  # Calculate the effects or "epsilons," depending on maxint.
  if (maxint=="all" & length(ind_vary)<=15){ # use Poelwijk approach

    Gmatrix<- make.Gmatrix(length(ind_vary)) # order of Gmatrix is the number of parameters that vary
    # calculate the epsilon values:
    epsilons<- Gmatrix%*%responses$nus

  } else { # directly calculate the epsilons:

    epsilons<- calc_matrix_effects(responses$nus, responses$list_ind_vary)

  }

  # make the data to return:
  output<- list(indices.varying = ind_vary,
                varying.indices.list = responses$list_ind_vary,
                epsilons = epsilons)
  return(output)

}

#' Exact LTRE analysis: fixed design
#'
#' Life Table Response Experiments (LTREs) are a method of comparative demographic
#' analysis. The purpose is to quantify how the difference or variance in vital
#' rates (stage-specific survival, growth, and fertility) among populations
#' contributes to difference or variance in the population growth rate, "lambda."
#' The exact method of LTRE is based on the principles of functional ANOVA.
#' The equations and descriptions for the exact method will be published in a
#' forthcoming paper, which we will link to.
#'
#' @param Aobj An object containing all the population projection matrices to be
#'  included in the analysis. It should either be a list, or a matrix where each
#'  row is the column-wise vectorization of a matrix. For fixed design, exactly
#'  2 matrices must be provided, ordered as `[reference matrix, treatment matrix`].
#'
#' @param maxint The maximum interaction order to be evaluated. The default input
#' is "all" but this input can take any integer value. If maxint=3, then the
#' output will include contributions terms up to 3-way interactions.
#'
#' @param fixed.directional A true/false switch that allows the user to specify
#' whether a directional LTRE should be used. The default behavior is to calculate
#' a symmetric LTRE, where the mean matrix is used as the baseline. See details
#' for more guidance.
#'
#' @return This returns a list object, with 3 items: (1) a vector of the matrix
#' indices where the parameters vary between/among the matrices in Aobj; (2) a
#' list of the indices varying for each of the contribution terms provided; (3)
#' a vector of the contribution terms. For fixed design LTRE these are
#' contributions to the difference in lambda.
#'
#' `indices.varying` is a vector with the indices of parameters that vary. The
#'  numeric indices count down the columns of a given population projection
#'  matrix. For example, in a 3x3 matrix, the (2,2) position would be identified
#'  with a 5.
#'
#'  `varying.indices.list` is a list object, where each entry is a vector
#'  containing the indices (matching the `indices.varying` part of the output)
#'  that differed or varied for the corresponding entry in the `epsilon` vector.
#'
#'  `epsilon` is a vector of contributions to the difference in lambda due to
#'  the observed values of the various life history parameters. For example, the
#'  contribution to the difference in lambda of adult survival is determined by
#'  setting all parameters *except* adult survival to their mean values, and
#'  then calculating the difference in lambda in this manipulated set of
#'  matrices.
#'
#' @details  Lambda is the asymptotic population growth rate, defined as the
#'  largest eigenvalue of the population projection matrix. A fixed design LTRE
#'  decomposes the difference in lambda due to differences at each position of
#'  the matrices. For a fixed design LTRE, exactly 2 matrices must be provided,
#'  ordered as `[reference matrix, treatment matrix`].
#'
#'  \code{fixed.directional=FALSE} is most appropriate for comparisons where it is
#'  not entirely obvious which population should be the reference and which
#'  should be the treatment (for example, when comparing a wet and a dry year). In
#'  this case, the difference in lambda is decomposed using the mean matrix as
#'  the baseline. The decomposition is symmetric, meaning that if the treatment and
#'  reference matrix are swapped, the contributions from the vital rates will be
#'  equal in magnitude, but positive contributions will become negative and vice
#'  versa.
#'
#'  \code{fixed.directional=TRUE} is most appropriate for comparisons between a
#'  control and treatment population in a controlled experiment. In this case,
#'  the reference matrix is treated as the baseline. This is a directional analysis,
#'  meaning that if the reference and treatment matrices were to be swapped, the
#'  contributions of the vital rates would change.
#'
#'  We set \code{fixed.directional=FALSE} as the default behavior because most
#'  population projection models are built with field-collected data rather than
#'  controlled experiment data.
#'
#' @export
#'
#' @examples
#' A1<- matrix(data=c(0,0.8,0, 0,0,0.7, 5,0,0.2), nrow=3, ncol=3)
#' A2<- matrix(data=c(0,0.9,0, 0,0,0.5, 4,0,0.3), nrow=3, ncol=3)
#' A3<- matrix(data=c(0,0.4,0, 0,0,0.6, 6,0,0.25), nrow=3, ncol=3)
#' cont_diff<- exactLTRE_fixed(list(A1,A2), maxint="all") # contributions to the difference in lambda
#' cont_diff<- exactLTRE_fixed(list(A1,A2), maxint=2) # only first- and second-order terms
#'
#' # if A1 represents a control and A2 is a treatment:
#' cont_diff<- exactLTRE_fixed(list(A1,A2), maxint="all", fixed.directional=TRUE)
exactLTRE_fixed<- function(Aobj, maxint="all", fixed.directional=FALSE){
  # each row of Aobj should be the elements of an individual population matrix, collapsed column-wise
  # It is important that Aobj is 2 rows. Row 1 contains vec(Aref), and Row 2 contains vec(Atreatment)

  # if Aobj is passed in as a list, collapse into the row-format:
  if (is.list(Aobj)){
    Aobj<- collapse_mat_list(Aobj)
  }

  # run some matrix checks and return warnings as needed:
  run_matrix_checks(Aobj)

  # get rid of column and row names, just in case:
  colnames(Aobj)<- NULL; rownames(Aobj)<- NULL

  #find the indices of the columns in Aobj that have non-zero difference. In other
  # words, which matrix entries vary between populations?
  ind_vary<- which(abs(Aobj[1,]-Aobj[2,])>0)

  # checks on maxint:
  if (maxint!="all"){
    if (!is.numeric(maxint)){
      stop("Input variable maxint, the maximum interaction order to calculate, must either be ''all'' or a numeric variable.")
    } else if (maxint>=length(ind_vary)){
      warning("You have requested an interaction order that is greater than or equal to the number of parameters that vary, defaulting to maxint='all'.")
      maxint="all"
    }
  }

  # calculate the matrix responses. We use a different function depending on whether fixed.directional=TRUE
  if (fixed.directional==FALSE){
    responses<- calc_matrix_responses(Aobj, ind_vary, FUN=lamDiff_symmetric, maxint)
  } else if (fixed.directional==TRUE){
    responses<- calc_matrix_responses(Aobj, ind_vary, FUN=lamDiff, maxint)
  }

  # Calculate the effects or "epsilons," depending on maxint.
  if (maxint=="all" & length(ind_vary)<=15){ # use Poelwijk approach

    Gmatrix<- make.Gmatrix(length(ind_vary)) # order of Gmatrix is the number of parameters that vary
    # calculate the epsilon values:
    epsilons<- Gmatrix%*%responses$nus

  } else { # directly calculate the epsilons:

    epsilons<- calc_matrix_effects(responses$nus, responses$list_ind_vary)

  }

  # make the data to return:
  output<- list(indices.varying = ind_vary,
                varying.indices.list = responses$list_ind_vary,
                epsilons = epsilons)
  return(output)

}

