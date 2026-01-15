#########################
# R wrapper for hmm_cpp #
#########################

#' Train a hidden Markov model with multivariate normal state distributions.
#'
#' @param Xs List of nsequences matrices; each matrix represents one observation
#'   sequence and is of dimension nobs x nfeatures. For a single observation
#'   sequence, a single matrix can be provided
#' @param weights Optional vector of weights, one for each observation sequence
#' @param nstates Integer; number of states
#' @param par List of initialization parameters; see 'Details'
#' @param control List of control parameters for EM steps
#' @param labels List of observation labels for supervised training, with each
#'   element corresponding to an observation sequence. Element i can either be
#'   an vector of integer state labels in \code{1:nstates} or a matrix of
#'   dimension \code{nstates} x \code{nrow(Xs[[i]])} with columns summing to 1.
#'   If labels are supplied, E-step is suppressed.
#'
#' @return An object of class hmm. Contains fitted values of model
#'     parameters, along with input values for hyperparameters and
#'     features.
#'
#' @details The \code{par} argument is a list of initialization parameters.
#'   Can supply any of the following components:
#' \itemize{
#'   \item{\code{method}}{
#'     Name of method used to automatically initialize EM run. Currently only
#'     \code{'dirichlet'} and \code{'random-spherical'} are implemented. If
#'     provided, user-specified state distributions are ignored.
#'     \code{'dirichlet'} randomly generates responsibilities which are in turn
#'     used to calculate starting distributions. \code{'random-spherical'} randomly draws
#'     nstates observations and uses their features as state means; all state covariance matrices are set to a
#'     diagonal matrix with entries \code{method_arg} (default=1).
#'   }
#'   \item{\code{method_arg}}{
#'     Argument to supply to \code{method}. For \code{method='dirichlet'}, this
#'     is a scalar concentration \code{alpha} (same value used for all states). For \code{method='random-spherical'}, this is a
#'     scalar for diagonal entries of the spherical covariance matrices of the
#'     starting distributions (after features are standardized).
#'     \code{'dirichlet'} is implemented. If provided, all other arguments are ignored.
#'   }
#'   \item{\code{resp}}{
#'     Matrix or list of nsequences matrices with rows summing to 1; each matrix
#'     represents one observation sequence and is of dimension nobs x nstates,
#'     with the (t,k)-th entry giving the initial probability that the t-th
#'     observation belongs to state k. If either \code{resp} or both \code{mus}
#'     and \code{Sigmas} are not provided, responsibilities are randomly
#'     initialized using \code{rdirichlet} with all shape parameters set to 10.
#'   }
#'   \item{\code{mus}}{
#'     List of nstates vectors with length nfeatures, each corresponding to the mean of
#'     a state distribution
#'   }
#'   \item{\code{Sigmas}}{
#'     List of nstates matrices with dimension nfeatures x nfeatures, each
#'     corresponding to the covariance matrix of a state distribution
#'   }
#'   \item{\code{Gamma}}{
#'     Matrix of transition probabilities with dimension nstates x nstates, with
#'     row k representing the probabilities of each transition out of k and
#'     summing to 1. If not supplied, each row is randomly drawn from
#'     \code{rdirichlet} with all shape parameters set to 10.
#'   }
#'   \item{\code{delta}}{
#'     Vector of initial state probabilities, of length nstates and summing to
#'     1. If not supplied, \code{delta} is set to the stationary distribution of
#'     \code{Gamma}, i.e. the normalized first left eigenvector.
#'   }
#' }
#'
#' The \code{control} argument is a list of EM control parameters that can supply
#' any of the following components
#' \itemize{
#'   \item{\code{lambda}}{
#'     Ridge-like regularization parameter. \code{lambda} is added to each
#'     \code{diag(Sigmas[[k]])} to stabilize each state's covariance matrix,
#'     which might otherwise be numerically singular, before inverting to
#'     calculate multivariate normal densities. Note that regularization is
#'     applied after all features are standardized, so \code{diag(Sigmas[[k]])}
#'     is unlikely to contain elements greater than 1. This parameter should be
#'     selected through cross-validation.
#'   }
#'   \item{\code{tol}}{
#'     EM terminates when the improvement in the log-likelihood between
#'     successive steps is \code{< tol}. Defaults to 1e-6.
#'   }
#'   \item{\code{maxiter}}{
#'     EM terminates with a warning if \code{maxiter} iterations are reached
#'     without convergence as defined by \code{tol}. Defaults to 100.
#'   }
#'   \item{\code{uncollapse}}{
#'     Threshold for detecting and resetting state distribution when they
#'     collapse on a single point. State distributions are uncollapsed by
#'     re-drawing \code{mus[[k]]} from a standard multivariate normal and
#'     setting \code{Sigmas[[k]]} to the nfeatures-dimensional identity matrix.
#'     Note that this distribution is with respect to the standardized features.
#'   }
#'   \item{\code{standardize}}{
#'     Whether features should be standardized. Defaults to \code{TRUE}. This
#'     option also adds a small amount of noise , equal to .01 x feature
#'     standard deviation, to observation-features that have been zeroed out
#'     (e.g. f0 during unvoiced periods). If set to \code{FALSE}, it is assumed
#'     that features have been externally standardized and zeroed-out values
#'     handled. scaling$feature_means and scaling$feature_sds are set to 0s and
#'     1s, respectively, and no check is done to ensure this is correct. If
#'     features are in fact not standardized and zeroes handled, bad things will
#'     happen and nobody will feel sorry for you.
#'   }
#'   \item{\code{verbose}}{
#'     Integer in 0:1. If \code{1}, information on the EM process is reported.
#'     Defaults to 1.
#'   }
#' }
#'
#' @examples
#' data('audio')
#' \dontrun{
#' mod <- hmm(audio$data, nstates = 2, control = list(verbose = TRUE))
#' }
#'
#' @export
#' @import Rcpp 
#' @useDynLib communication
#' 
hmm = function(Xs,              # data
               weights=NULL,    # weight on each element of Xs
               nstates,         # number of states
               par=list(),      # initialization
               control=list(),  # EM control parameters
               labels=list()    # labels for supervised training
){

  if (class(Xs) == 'matrix'){
      Xs = list(Xs)
  }

  if (any(unique(diff(sapply(Xs, ncol))) != 0)){
      stop('all observation sequences must have same number of observed features')
  }

  ## if (is.null(weights)){
  ##     weights <- rep(weights, length(Xs))
  ## }

  # indices

  K = floor(nstates)
  N = length(Xs)
  M = ncol(Xs[[1]])      # number of features
  Ts = sapply(Xs, nrow)  # number of obs in each sequence

  K_digits = ceiling(log(K+1, 10))  # for verbose
  N_digits = ceiling(log(N+1, 10))

  if (K != nstates | nstates < 2){
      stop('nstates must be integer >= 2')
  }

  if (is.null(weights)){
      weights = rep(1, N)
  }

  # try to read derivatives off attr, otherwise use names _d*

  if (is.null(attr(Xs[[1]], 'derivatives'))){
    derivatives = 0
    ind_d1 = grep('_d1$', colnames(Xs[[1]]))                 # 1st deriv
    ind_d2 = grep('_d2$', colnames(Xs[[1]]))                 # 2nd deriv
    if (length(ind_d1) > 0)
      derivatives = 1
    if (length(ind_d2) > 0)
      derivatives = 2
  } else {
    derivatives = attr(Xs[[1]], 'derivatives')
  }

  ## # set aside leading obs where derivatives cannot be calculated
  ## if (derivatives > 0){
  ##   for (i in 1:N){
  ##     Xs[[i]] = Xs[[i]][-(1:derivatives),]
  ##   }
  ## }
  ## Ts = Ts - derivatives

  # defaults

  if (!'lambda' %in% ls(control)){
    control$lambda = 0
  } else {
    if (control$lambda < 0)
      stop('regularization parameter lambda must be nonnegative')
  }

  if (!'tol' %in% ls(control))
    control$tol = 1e-6

  if (!'maxiter' %in% ls(control))
    control$maxiter = 100

  if (!'uncollapse' %in% ls(control))
    control$uncollapse = .01^M

  if (!'verbose' %in% ls(control))
    control$verbose = 1

  # process labels for supervised hmm

  supervised = FALSE

  if (length(labels) > 0){
    supervised = TRUE

    if (N==1 & is.atomic(labels))
      labels = list(labels)

    if (all(sapply(labels, is.numeric))){

      if (length(labels) != N)
        stop('length(labels) != length(Xs)')

      if (all(sapply(labels, is.matrix))){

        if (any(sapply(labels, ncol) != K))
          stop('ncol of labels[[i]] must equal nstates for all i')
        if (any(sapply(labels, nrow) != Ts))
          stop('nrow(labels[[i]]) must equal nrow(Xs[[i]]) for all i')
        if (any(unlist(sapply(labels, rowSums)) - 1 > 4 * .Machine$double.eps))
          stop('labels for each observation must sum to 1')

        par$resp = labels

      } else if (all(sapply(labels, is.vector))){

        if (any(sapply(labels, length) != Ts))
          stop('length of labels[[i]] must equal nrow(Xs[[i]]) for all i')
        if (any(!unique(unlist(labels)) %in% 1:K))
          stop('all integer labels must be in 1:nstates')

        par$resp = lapply(labels, function(lab){
          out = matrix(0, length(lab), nstates)
          out[cbind(1:length(lab), lab)] = 1
          return(out)
        })

      } else {
        stop('labels for supervised training must be either (a) list of integer vectors with ',
             'length(labels)==length(Xs), length(labels[[i]])==nrow(Xs[[i]]), and labels[[i]][t] %in% 1:nstates; ',
             'or (b) list of matrices with length(labels)==length(Xs), nrow(labels[[i]])==nstates, ',
             'ncol(labels[[i]])==nrow(Xs[[i]]), and all(colSums(labels[[i]]) == 1)')
      }
    } else {
      stop('labels for supervised training must be list of numeric vectors or matrices')
    }

    ## if (derivatives > 0){
    ##   for (i in 1:N){
    ##     par$resp = par$resp[-(1:derivatives),]
    ##   }
    ## }

  }

  ### standardize covariates - working in-place to conserve memory

  if (!'standardize' %in% ls(control))
    control$standardize = TRUE

  if (control$standardize){

    Xs = standardizeFeatures(Xs, verbose=control$verbose)

    feature_means = attr(Xs[[1]], 'scaled:center')
    feature_sds = attr(Xs[[1]], 'scaled:scale')

    nonmissing = attr(Xs, 'nonmissing')
    missingness_labels = attr(Xs, 'missingness_labels')
    nonmissing_features = attr(Xs, 'nonmissing_features')

  } else {

    if (is.null(attr(Xs[[1]], 'scaled:center'))){
      feature_means = rep(0, M)
    } else {
      feature_means = attr(Xs[[1]], 'scaled:center')
    }

    if (is.null(attr(Xs[[1]], 'scaled:scale'))){
      feature_sds = rep(1, M)
    } else {
      feature_sds = attr(Xs[[1]], 'scaled:scale')
    }

    if (is.null(attr(Xs, 'nonmissing'))){
      nonmissing = lapply(Ts, function(x) 1:x) # assume nothing is missing
    } else {
      nonmissing = attr(Xs, 'nonmissing')
    }

    if (is.null(attr(Xs, 'missingness_labels'))){
      missingness_labels = lapply(Ts, function(x) rep(1,x)) # assume all obs have same pattern of missingness (none)
    } else {
      missingness_labels = attr(Xs, 'missingness_labels')
    }

    if (is.null(attr(Xs, 'nonmissing_features'))){
      nonmissing_features = list(1:M) # assume nothing is missing
    } else {
      nonmissing_features = attr(Xs, 'nonmissing_features')
    }

  }

  ### initialize state distributions by either
  #   (1) providing (near-uniform) obs-level responsibilities (recommended for small datasets < 1e6 obs), or
  #   (2) providing (dispersed and high-variance) starting state distributions (faster to initialize)

  if ('resp' %in% ls(par)){
    if (!is.null(par$method))
      warning('responsibilities supplied; ignoring "par$method"')
    par$method = 'user-responsibilities'
  } else if (all(c('mus', 'Sigmas') %in% ls(par))){
    par$method = 'user-states'
  } else if (!is.null(par$method)) {
    if (par$method != 'random-spherical')
      warning('"par$method" unknown or improper inputs supplied; ignoring')
  } else {
    par$method = 'random-dirichlet'
  }

  if (par$method == 'random-spherical'){
    Ts_running = cumsum(Ts)
    seq.draws = table(sample.int(N, K, replace=TRUE, prob=sapply(nonmissing, length)))
    par$mus = do.call(cbind, lapply(names(seq.draws), function(seq){
      mus.ind = sample(nonmissing[[as.numeric(seq)]], seq.draws[seq])
      t(Xs[[as.numeric(seq)]][mus.ind, , drop=FALSE])
    }))
#     mus.ind = sample.int(Ts_running[N], K) # randomly draw obs to serve as state means
#     mus.seq = sapply(mus.ind, function(ind) which(ind <= Ts_running)[1]) # seq to which mus.ind[k] belongs
#     mus.obs = mus.ind - c(0, Ts_running)[mus.seq] # obs number in mus.seq[k] corresponding to mus.ind[k]
#     par$mus = sapply(1:K, function(k) Xs[[mus.seq[k]]][mus.obs[k],])

    if (is.null(par$method_arg))
      par$method_arg = 1 # entries on diagonal of initial state covariance matrix

    par$Sigmas = replicate(K, par$method_arg*diag(M), simplify=FALSE)

  }

  # if no initialization whatsoever, assign responsibilities
  if (par$method == 'random-dirichlet'){
    if (is.null(par$method_arg))
      par$method_arg = 1 # dirichlet concentration for initial responsibilities

    par$resp = list()
    for (i in 1:N){
      par$resp[[i]] = gtools::rdirichlet(Ts[[i]], rep(par$method_arg, K))
    }

  }

  # if responsibilities provided
  if (par$method %in% c('user-responsibilities', 'random-dirichlet')){

    if (any(sapply(par$resp, ncol) != K)){
      stop('all responsibility matrices must have nstates columns')
    }

    if (!all(sapply(par$resp, nrow) == Ts)){
      stop('each responsibility matrix must have same nrow as corresponding element of Xs')
    }

    if (any(unlist(sapply(par$resp, rowSums)) - 1 > 4 * .Machine$double.eps)){
      stop('rows of all responsibility matrices must sum to 1')
    }

    if (any(c('mus', 'Sigmas') %in% ls(par))){
      warning('state responsibilities provided; state distribution parameters (mus/Sigmas) ignored')
    }

    state_sizes = rowSums(sapply(1:N, function(i) colSums(par$resp[[i]][nonmissing[[i]],])))

    # for each state, get mean of obs (weighted by responsibility)
    par$mus = matrix(0, K, M)
    for (i in 1:N){
      par$mus = par$mus + crossprod(par$resp[[i]][nonmissing[[i]],], Xs[[i]][nonmissing[[i]],])
    }
    par$mus = t(par$mus)
    par$mus = sweep(par$mus, 2, state_sizes, '/')

    # for each state, get weighted cov matrix (weighted by responsibility)
    par$Sigmas = replicate(K, matrix(0, M, M), simplify=FALSE)
    if (control$verbose >= 1)
      cat('\n  initializing cluster ', rep(' ', K_digits +  N_digits + 9), sep='')
    for (k in 1:K){
      if (control$verbose >= 1){
        cat(rep('\b', K_digits + N_digits + 9), sprintf(paste('%', K_digits, 'd', sep=''), k), sep='')
        cat(' obs seq ', rep(' ', N_digits), sep='')
      }
      for (i in 1:N){
        if (control$verbose >= 1)
          cat(rep('\b', N_digits), sprintf(paste('%', N_digits, 'd', sep=''), i), sep='')
        par$Sigmas[[k]] = par$Sigmas[[k]] +
          t(Xs[[i]][nonmissing[[i]],]) %*% diag(par$resp[[i]][nonmissing[[i]], k]) %*% Xs[[i]][nonmissing[[i]],]
      }
      par$Sigmas[[k]] = par$Sigmas[[k]] / state_sizes[k]
    }

    # if starting state distributions provided, shift and scale to account for standardization
  }
  if (control$verbose >= 1){
        cat('\n')
  }


  if (par$method == 'user-states') {

    if (all(c('mus', 'Sigmas') %in% ls(par)) & is.null(par$method)){

      par$mus = lapply(par$mus, function(mu){
        mu = mu - feature_means
        mu = mu / feature_sds
      })
      par$mus = do.call(cbind, par$mus)

      for (k in 1:K){ # standardize and check Sigmas are PD
        par$Sigmas[[k]] = sweep(par$Sigmas[[k]], 1, feature_sds, '/')
        par$Sigmas[[k]] = sweep(par$Sigmas[[k]], 2, feature_sds, '/')
        chol(par$Sigmas[[k]] + control$lambda*diag(M))
      }

    }
  }

  # initialize transition matrix
  if ('Gamma' %in% ls(par)){

    if (class(par$Gamma) != 'matrix' | !all.equal(dim(par$Gamma), c(K, K)))
      stop('Gamma must be nstates x nstates numeric matrix')

    if (any(rowSums(par$Gamma) - 1 > 4 * .Machine$double.eps)){
      stop('rows of Gamma must sum to 1')
    }

  } else {

    par$Gamma = gtools::rdirichlet(K, rep(10,K))

  }

  # initialize starting distribution
  if ('delta' %in% ls(par)){

    if (class(par$delta) != 'numeric' | length(par$delta) != K)
      stop('delta must be numeric vector of length nstates')

    if (sum(par$delta) > 4 * .Machine$double.eps)
      stop('delta must sum to 1')

    par$delta = t(par$delta)

  } else {
    par$delta = solve(t(diag(K) - par$Gamma + 1), rep(1, K))
  }

  # c++ indexing
  nonmissing = lapply(nonmissing, function(x) x - 1)
  missingness_labels = lapply(missingness_labels, function(x) x - 1)
  nonmissing_features = lapply(nonmissing_features, function(x) x - 1)

  out = hmm_cpp(Xs,
                weights,
                par$delta,
                par$mus,
                par$Sigmas,
                par$Gamma,
                if (supervised) lapply(par$resp, t),
                nonmissing,
                missingness_labels,
                nonmissing_features,
                control$lambda,
                control$tol,
                control$maxiter,
                control$uncollapse,
                control$verbose>=1,
                supervised
  )

  out$weights = weights

  # rescale mus, Sigmas from output
  for (k in 1:K){
    out$mus[[k]] = out$mus[[k]] * feature_sds
    out$mus[[k]] = out$mus[[k]] + feature_means
    out$Sigmas[[k]] = sweep(out$Sigmas[[k]], 1, feature_sds, '*')
    out$Sigmas[[k]] = sweep(out$Sigmas[[k]], 2, feature_sds, '*')
  }

  # stationary distribution
  out$delta = solve(t(diag(K) - out$Gamma + 1), rep(1, K))

  out$nstates = K

  # append feature means/sds to output so new data can be scaled accordingly
  out$scaling = list(feature_means = feature_means,
                     feature_sds = feature_sds)

  # append feature names
  out$dimnames = colnames(Xs[[1]])

  # append initialization and control parameters
  out$par = par
  out$control = control

  # llh of individual obs seq
  out$llhs = as.numeric(out$llhs)

  # trim leading -Inf (used internally so llh increase always defined)
  out$llh_seq = out$llh_seq[-1]
  iters = length(out$llh_seq)
  converged = TRUE
  if (iters >= control$maxiter){
    if (diff(out$llh_seq[-1:0 + length(out$llh_seq)]) >= control$tol){
      converged = FALSE
      warning('failed to converge by maxiter = ', control$maxiter, ' iterations (tol = ', control$tol, ')')
    }
  }

  ## # add NAs back in for leading obs for which derivatives cannot be calculated
  ## if (derivatives > 0){
  ##   for (i in 1:N){
  ##     out$lstateprobs[[i]] = rbind(matrix(NA, derivatives, K),
  ##                                  out$lstateprobs[[i]])
  ##     out$zetas[[i]] = cbind(matrix(NA, K, derivatives),
  ##                            out$zetas[[i]])
  ##   }
  ## }

  out$convergence = list(converged=converged,
                         iters=iters,
                         resets=out$resets)
  out$resets = NULL # move 'resets' from cpp output to convergence diagnostics

  class(out) = 'hmm'

  return(out)

}
