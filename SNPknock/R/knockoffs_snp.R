#' Group-knockoffs of unphased genotypes
#'
#' This function efficiently constructs group-knockoffs of {0,1,2} variables distributed
#' according to the Li and Stephens model for unphased genotypes.
#'
#' @param X a {0,1,2} matrix of size n-by-p containing the original variables.
#' @param r a vector of length p containing the "r" parameters estimated by fastPHASE.
#' @param alpha a matrix of size p-by-K containing the "alpha" parameters estimated by fastPHASE.
#' @param theta a matrix of size p-by-K containing the "theta" parameters estimated by fastPHASE.
#' @param groups a vector of length p containing group memberships for each variable. Indices
#' are assumed to be monotone increasing, starting from 1 (default: NULL).
#' @param seed an integer random seed (default: 123).
#' @param cluster a computing cluster object created by \link[parallel]{makeCluster} (default: NULL).
#' @param display_progress whether to show progress bar (default: FALSE).
#' @return A {0,1,2} matrix of size n-by-p containing the knockoff variables.
#'
#' @family knockoffs
#'
#' @details
#' Generate group-knockoffs of unphased genotypes according to the Li and Stephens HMM.
#' The required model parameters can be obtained through fastPHASE and loaded with \link{loadHMM}.
#' This function is more efficient than \link{knockoffHMM} for haplotype data.
#'
#' @references
#'   \insertRef{sesia2019multi}{SNPknock}
#'
#' @examples
#' # Problem size
#' p = 10
#' n = 100
#' # Load HMM to generate data
#' r_file = system.file("extdata", "haplotypes_rhat.txt", package = "SNPknock")
#' alpha_file = system.file("extdata", "haplotypes_alphahat.txt", package = "SNPknock")
#' theta_file = system.file("extdata", "haplotypes_thetahat.txt", package = "SNPknock")
#' char_file = system.file("extdata", "haplotypes_origchars", package = "SNPknock")
#' hmm.data = loadHMM(r_file, alpha_file, theta_file, char_file, compact=FALSE, phased=FALSE)
#' hmm.data$Q = hmm.data$Q[1:(p-1),,]
#' hmm.data$pEmit = hmm.data$pEmit[1:p,,]
#' # Sample X from this HMM
#' X = sampleHMM(hmm.data$pInit, hmm.data$Q, hmm.data$pEmit, n=n)
#' # Load HMM to generate knockoffs
#' hmm = loadHMM(r_file, alpha_file, theta_file, char_file)
#' hmm$r = hmm$r[1:p]
#' hmm$alpha = hmm$alpha[1:p,]
#' hmm$theta = hmm$theta[1:p,]
#' # Generate knockoffs
#' Xk = knockoffGenotypes(X, hmm$r, hmm$alpha, hmm$theta)
#' # Generate group-knockoffs for groups of size 3
#' groups = rep(seq(p), each=3, length.out=p)
#' Xk = knockoffGenotypes(X, hmm$r, hmm$alpha, hmm$theta, groups=groups)

#' @export
knockoffGenotypes <- function(X, r, alpha, theta, groups=NULL, seed=123, cluster=NULL, display_progress=FALSE) {
  # If groups are not provided, define singleton groups
  if(is.null(groups)) {
    groups = seq(1, ncol(X))
  }

  # Verify dimensions are compatible
  stopifnot(dim(X)[2]==length(r))
  stopifnot(dim(X)[2]==dim(alpha)[1])
  stopifnot(dim(X)[2]==dim(theta)[1])
  stopifnot(dim(alpha)[2]==dim(theta)[2])
  stopifnot(dim(X)[2]==length(groups))

  # Verify contents are compatible
  stopifnot(is.integer(X))
  stopifnot(is.numeric(r))
  stopifnot(is.numeric(alpha))
  stopifnot(is.numeric(theta))
  stopifnot(is.numeric(seed))
  stopifnot(seed==floor(seed))
  stopifnot( min(X)>=0 )
  stopifnot( max(X)<=2 )
  seed = as.integer(seed)
  stopifnot(is.integer(seed))
  stopifnot(is.logical(display_progress))
  stopifnot(is.integer(groups))

  # Extract dimensions
  n = dim(X)[1]
  p = dim(X)[2]
  K = dim(alpha)[2]

  # Split non-contigous groups
  groups.cont = rep(1,p)
  group.count = 1
  for(j in 2:p) {
    if(groups[j]!=groups[j-1]) {
      group.count = group.count + 1
    }
    groups.cont[j] = group.count
  }

  if( (!is.null(cluster)) & (length(cluster)>1) ) {
    if(requireNamespace("doParallel", quietly = TRUE))
    {
      # Count number of workers in the cluster
      ncores = length(cluster)

      # Assign rows to workers
      splits <- cut(1:nrow(X),breaks=ncores,labels=FALSE)

      # Sample knockoffs in parallel
      Xk = do.call(rbind, parallel::parLapply(cluster, 1:ncores, function(i) {
        n.split = sum(splits==i)
        display_progress = (i==1)*display_progress
        GroupGenotypes_wrapper(X[splits==i,], r, alpha, theta, groups.cont-1, n.split, ncol(X),
                               seed+(i-1), display_progress)
      }))
    } else {
      warning("To enable multithreading, please install the doParallel package ")
      # Sample knockoffs sequentially
      Xk = GroupGenotypes_wrapper(X, r, alpha, theta, groups.cont-1, n, p, seed, display_progress)
    }
  } else {
    # Sample knockoffs sequentially
    Xk = GroupGenotypes_wrapper(X, r, alpha, theta, groups.cont-1, n, p, seed, display_progress)
  }
  storage.mode(Xk) = "integer"
  return(Xk)
}

#' Group-knockoffs of phased haplotypes
#'
#' This function efficiently constructs group-knockoffs of binary variables distributed
#' according to the Li and Stephens model for phased haplotypes.
#'
#' @param X a binary matrix of size n-by-p containing the original variables.
#' @param r a vector of length p containing the "r" parameters estimated by fastPHASE.
#' @param alpha a matrix of size p-by-K containing the "alpha" parameters estimated by fastPHASE.
#' @param theta a matrix of size p-by-K containing the "theta" parameters estimated by fastPHASE.
#' @param groups a vector of length p containing group memberships for each variable. Indices
#' are assumed to be monotone increasing, starting from 1 (default: NULL).
#' @param seed an integer random seed (default: 123).
#' @param cluster a computing cluster object created by \link[parallel]{makeCluster} (default: NULL).
#' @param display_progress whether to show progress bar (default: FALSE).
#' @return A binary matrix of size n-by-p containing the knockoff variables.
#'
#' @family knockoffs
#'
#' @details
#' Generate group-knockoffs of phased haplotypes according to the Li and Stephens HMM.
#' The required model parameters can be obtained through fastPHASE and loaded with \link{loadHMM}.
#' This function is more efficient than \link{knockoffHMM} for haplotype data.
#'
#' @references
#'   \insertRef{sesia2019multi}{SNPknock}
#'
#' @examples
#' # Problem size
#' p = 10
#' n = 100
#' # Load HMM to generate data
#' r_file = system.file("extdata", "haplotypes_rhat.txt", package = "SNPknock")
#' alpha_file = system.file("extdata", "haplotypes_alphahat.txt", package = "SNPknock")
#' theta_file = system.file("extdata", "haplotypes_thetahat.txt", package = "SNPknock")
#' char_file = system.file("extdata", "haplotypes_origchars", package = "SNPknock")
#' hmm.data = loadHMM(r_file, alpha_file, theta_file, char_file, compact=FALSE, phased=TRUE)
#' hmm.data$Q = hmm.data$Q[1:(p-1),,]
#' hmm.data$pEmit = hmm.data$pEmit[1:p,,]
#' # Sample X from this HMM
#' X = sampleHMM(hmm.data$pInit, hmm.data$Q, hmm.data$pEmit, n=n)
#' # Load HMM to generate knockoffs
#' hmm = loadHMM(r_file, alpha_file, theta_file, char_file)
#' hmm$r = hmm$r[1:p]
#' hmm$alpha = hmm$alpha[1:p,]
#' hmm$theta = hmm$theta[1:p,]
#' # Generate knockoffs
#' Xk = knockoffHaplotypes(X, hmm$r, hmm$alpha, hmm$theta)
#' # Generate group-knockoffs for groups of size 3
#' groups = rep(seq(p), each=3, length.out=p)
#' Xk = knockoffHaplotypes(X, hmm$r, hmm$alpha, hmm$theta, groups=groups)
#'
#' @export
knockoffHaplotypes <- function(X, r, alpha, theta, groups=NULL, seed=123, cluster=NULL, display_progress=FALSE) {
  # If groups are not provided, define singleton groups
  if(is.null(groups)) {
    groups = seq(1, ncol(X))
  }

  # Verify dimensions are compatible
  stopifnot(dim(X)[2]==length(r))
  stopifnot(dim(X)[2]==dim(alpha)[1])
  stopifnot(dim(X)[2]==dim(theta)[1])
  stopifnot(dim(alpha)[2]==dim(theta)[2])
  stopifnot(dim(X)[2]==length(groups))

  # Verify contents are compatible
  stopifnot(is.integer(X))
  stopifnot(is.numeric(r))
  stopifnot(is.numeric(alpha))
  stopifnot(is.numeric(theta))
  stopifnot(is.numeric(seed))
  stopifnot(seed==floor(seed))
  stopifnot( min(X)>=0 )
  stopifnot( max(X)<=1 )
  seed = as.integer(seed)
  stopifnot(is.integer(seed))
  stopifnot(is.logical(display_progress))
  stopifnot(is.integer(groups))

  # Extract dimensions
  n = dim(X)[1]
  p = dim(X)[2]
  K = dim(alpha)[2]

  # Split non-contigous groups
  groups.cont = rep(1,p)
  group.count = 1
  for(j in 2:p) {
    if(groups[j]!=groups[j-1]) {
      group.count = group.count + 1
    }
    groups.cont[j] = group.count
  }

  if( (!is.null(cluster)) & (length(cluster)>1) ) {
    if(requireNamespace("doParallel", quietly = TRUE))
    {
      # Count number of workers in the cluster
      ncores = length(cluster)

      # Assign rows to workers
      splits <- cut(1:nrow(X),breaks=ncores,labels=FALSE)

      # Sample knockoffs in parallel
      Xk = do.call(rbind, parallel::parLapply(cluster, 1:ncores, function(i) {
        n.split = sum(splits==i)
        display_progress = (i==1)*display_progress
        GroupHaplotypes_wrapper(X[splits==i,], r, alpha, theta, groups.cont-1, n.split, p,
                                seed+(i-1), display_progress)
      }))
    } else {
      warning("To enable multithreading, please install the doParallel package ")
      # Sample knockoffs sequentially
      Xk = GroupHaplotypes_wrapper(X, r, alpha, theta, groups.cont-1, n, p, seed, display_progress)
    }
  } else {
    # Sample knockoffs sequentially
    Xk = GroupHaplotypes_wrapper(X, r, alpha, theta, groups.cont-1, n, p, seed, display_progress)
  }
  storage.mode(Xk) = "integer"
  return(Xk)
}
