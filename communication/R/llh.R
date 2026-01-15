#########################
# R wrapper for llh_cpp #
#########################

#' Title
#'
#' @param Xs List of nsequences matrices; each matrix represents one observation
#'   sequence and is of dimension nobs x nfeatures. For a single observation
#'   sequence, a single matrix can be provided
#' @param mod Model object of class 'feelr.hmm', as output by \code{hmm}
#' @param control List of control parameters 
#'
#' @return List with two components. \code{llhs} is a numeric vector of
#'   log-likelihoods of each observation sequence in \code{Xs}. \code{llh_total}
#'   is the log-likelihood of all observation sequences together, i.e.
#'   \code{sum(llhs)}. If \code{Xs} is the same data that generated \code{mod},
#'   the values calculated here will be slightly lower than those output in
#'   \code{mod$llhs}. This is because \code{hmm} estimates the starting state of
#'   each sequence, whereas here it is assumed that the starting state is drawn
#'   from the stationary distribution \code{mod$delta}.
#' @export
#'
llh <- function(
  Xs,   # data
  mod,  # fitted feelr.hmm model
  control=list()
) {

  if (class(mod) != 'feelr.hmm')
    stop('"mod" must be fitted "feelr.hmm" model')

  if (class(Xs) == 'matrix')
    Xs = list(Xs)

  if (any(sapply(Xs, function(X) class(X)!='matrix')))
    stop('Xs must be matrix or list of matrices')

  if (any(sapply(Xs, ncol) != length(mod$mus[[1]])))
    stop('all observation sequences must have same number of observed features')

  if (!'standardize' %in% ls(control))
    control$standardize = TRUE

  if (!'verbose' %in% ls(control))
    control$verbose = TRUE


  N = length(Xs)

  # scale new data in the same way as training data

  feature_means = mod$scaling$feature_means
  feature_sds = mod$scaling$feature_sds

  if (control$standardize){
    for (i in 1:N){
      Xs[[i]] = sweep(Xs[[i]], 2, feature_means)
      Xs[[i]] = scale(Xs[[i]], center=F, scale=feature_sds)
    }
  }

  ## if (all(c('nonmissing', 'missingness_labels', 'nonmissing_features') %in% names(attributes(Xs)))){

  ##     nonmissing <- attr(Xs, 'nonmissing')
  ##     missingness_labels <- attr(Xs, 'missingness_labels')
  ##     nonmissing_features <-  attr(Xs, 'nonmissing_features')

  ## } else {
      # identify patterns of missingness

      M <- length(mod$mus[[1]])
      Ts <- sapply(Xs, nrow)

      missingness = lapply(Xs, is.na)
      missingness_binary = lapply(missingness, function(x){
                                      for (m in 1:M){
                                          x[,m] = 2^(M-m) * x[,m] # missingness -> binary sequence -> decimal
                                      }
                                      return(rowSums(x))
                                  })
      nonmissing = lapply(missingness_binary, function(x){
                              which(x==0)
                          })

      missingness_modes = unique(do.call(rbind, missingness))
      missingness_modes_binary = unique(do.call(c, missingness_binary))

      nonmissing_features = lapply(1:nrow(missingness_modes), function(i){
                                       which(!missingness_modes[i,])
                                   })
      missingness_labels = lapply(missingness_binary, function(x){
                                      match(x, missingness_modes_binary)
                                  })
  ## }

    mus = lapply(mod$mus, function(mu){
        mu = mu - feature_means
        mu = mu / feature_sds
    })
    mus = do.call(cbind, mus)

    Sigmas = lapply(mod$Sigmas, function(Sigma){
        Sigma = sweep(Sigma, 1, feature_sds, '/')
        Sigma = sweep(Sigma, 2, feature_sds, '/')
        return(Sigma)
    })

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

  # set aside leading obs where derivatives cannot be calculated

  ## if (derivatives > 0){
  ##   for (i in 1:N){
  ##     Xs[[i]] = Xs[[i]][-(1:derivatives),]
  ##     nonmissing = lapply(nonmissing, function(x) x - derivatives)
  ##   }
  ## }

  ## c++ indexing
  nonmissing = lapply(nonmissing, function(x) x - 1)
  missingness_labels = lapply(missingness_labels, function(x) x - 1)
  nonmissing_features = lapply(nonmissing_features, function(x) x - 1)

  out = llh_cpp(
      Xs,
      t(mod$delta),
      mus,
      Sigmas,
      mod$Gamma,
      nonmissing,
      missingness_labels,
      nonmissing_features,
      mod$control$lambda,
      control$verbose)

  out$llhs = as.numeric(out$llhs)

  return(out)

}
