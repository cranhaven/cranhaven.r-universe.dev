#' Title
#'
#' @param Xs Data
#' @param feature_means Numeric vector corresponding to columns of elements in Xs
#' @param feature_sds Numeric vector corresponding to columns of elements in Xs. If not supplied, will be computed from Xs.
#' @param verbose Verbose printing
#'
#' @return Standardizes `data` of objects of class
#'     `preppedAudio`. Maintains structure of original object
#'     otherwise. Is used to standardize data where the recording
#'     environment systematically shifts audio features.
#' 
#' @details \code{feature_means} and \code{feature_sds} are provided to allow
#'   alignment of new datasets. For example, after a model is trained, new data
#'   for prediction must be transformed in the same way as the training data to
#'   ensure predictions are valid. If either is \code{NULL}, both will be
#'   computed from \code{Xs} and the output will be internally standardized
#'   (i.e., columns of \code{do.call(rbind, standardizeFeatures(Xs))} will be
#'   have a mean of 0 and a standard deviation of 1).
#'
#' @examples
#' data('audio')
#' audio$data <- standardizeFeatures(
#'     lapply(audio$data, function(x) na.omit(x))
#' )
#' 
#' @export
#'
standardizeFeatures = function(Xs, feature_means=NULL, feature_sds=NULL, verbose=1){

  if (class(Xs) == 'matrix')
    Xs = list(Xs)

  if (any(unique(diff(sapply(Xs, ncol))) != 0))
    stop('all observation sequences must have same number of observed features')

  if (length(Xs) > 1){
    for (i in 1:length(Xs)){
      if (!all.equal(colnames(Xs[[i]]), colnames(Xs[[1]])))
        stop('colnames differ across observation sequences')
    }
  }

  if (!is.null(feature_means)){
    if (!is.numeric(feature_means) | !length(feature_means) == ncol(Xs[[1]]))
      stop('"feature_means" must be numeric vector of length ncol(Xs[[i]]) for all i')
  }

  if (!is.null(feature_sds)){
    if (!is.numeric(feature_sds) | !length(feature_sds) == ncol(Xs[[1]]))
      stop('"feature_sds" must be numeric vector of length ncol(Xs[[i]]) for all i')
  }

  internal = TRUE
  if (xor(is.null(feature_means), is.null(feature_sds))){
    warning('both "feature_means" and "feature_sds" must be supplied for external alignment; ignoring')
    feature_means = NULL
    feature_sds = NULL
  }
  if (!(is.null(feature_means) | is.null(feature_sds))) {
    internal = FALSE
  }

  if (verbose == 1)
    cat('standardizing features\n')

  # indices

  N = length(Xs)
  N_digits = ceiling(log(N+1, 10))
  M = ncol(Xs[[1]])
  Ts = sapply(Xs, nrow)  # number of obs in each sequence

  ind_d0 = grep('_d\\d$', colnames(Xs[[1]]), invert=TRUE)  # base features
  ind_d1 = grep('_d1$', colnames(Xs[[1]]))                 # 1st deriv
  ind_d2 = grep('_d2$', colnames(Xs[[1]]))                 # 2nd deriv

  M_d0 = length(ind_d0)
  M_d1 = length(ind_d1)
  M_d2 = length(ind_d2)

  derivatives = 0
  if (M_d1 > 0)
    derivatives = 1
  if (M_d2 > 0)
    derivatives = 2

  if (derivatives >= 1){
    if (!all.equal(colnames(Xs[[1]])[ind_d0], gsub('_d1$', '', colnames(Xs[[1]])[ind_d1]))){
      stop('failed to align base features with first derivatives')
    }
  }

  if (derivatives == 2){
    if (!all.equal(colnames(Xs[[1]])[ind_d0], gsub('_d2$', '', colnames(Xs[[1]])[ind_d2]))){
      stop('failed to align base features with second derivatives')
    }
  }

  # identify patterns of missingness

  for (i in 1:N){
      Xs[[i]][!is.finite(Xs[[i]])] = NA
  }

  missingness = lapply(Xs, is.na)
  missingness_binary = lapply(missingness, function(x){
    for (m in 1:M){
      x[,m] = 2^(M-m) * x[,m] # missingness -> binary sequence -> decimal
    }
    return(rowSums(x))
  })
  nonmissing = lapply(missingness_binary, function(x){
    x == 0
  })
  complete_obs = sum(sapply(nonmissing, sum))
  if (complete_obs <= M && internal){
    stop('only ', complete_obs, ' complete observations (no missing features) found; ',
         'provide at least nfeatures complete observations to estimate model')
  } else if (complete_obs < 1000 & complete_obs < sum(Ts)){
    warning('only ', complete_obs, ' complete observations (no missing features) found; ',
            'partially censored observations are used for likelihood calculations but ',
            'dropped for feature standardization and estimation of state distributions.')
  }

  missingness_modes = unique(do.call(rbind, missingness))
  missingness_modes_binary = unique(do.call(c, missingness_binary))

  nonmissing_features = lapply(1:nrow(missingness_modes), function(i){
    which(!missingness_modes[i,])
  })
  missingness_labels = lapply(missingness_binary, function(x){
    match(x, missingness_modes_binary)
  })

  attr(Xs, 'nonmissing') = lapply(nonmissing, which)
  attr(Xs, 'missingness_labels') = missingness_labels
  attr(Xs, 'nonmissing_features') = nonmissing_features

  # locate zeroes and contaminated adjacent derivatives
  zeroes = lapply(Xs, function(X) which(X[,ind_d0]==0, arr.ind=T))

  if (derivatives >= 1){ # indices of contaminated 1st derivatives
    zeroes_back1 = zeroes_fwd1 = zeroes
    for (i in 1:N){
      zeroes_back1[[i]][,'row'] = zeroes_back1[[i]][,'row'] - 1
      zeroes_back1[[i]] = zeroes_back1[[i]][zeroes_back1[[i]][,'row'] > 0,]
      zeroes_fwd1[[i]][,'row'] = zeroes_fwd1[[i]][,'row'] + 1
      zeroes_fwd1[[i]] = zeroes_fwd1[[i]][zeroes_fwd1[[i]][,'row'] <= Ts[i],]
    }
  }

  if (derivatives == 2){ # indices of contaminated 2nd derivatives
    zeroes_back2 = zeroes_fwd2 = zeroes
    for (i in 1:N){
      zeroes_back2[[i]][,'row'] = zeroes_back2[[i]][,'row'] - 2
      zeroes_back2[[i]] = zeroes_back2[[i]][zeroes_back2[[i]][,'row'] > 0,]
      zeroes_fwd2[[i]][,'row'] = zeroes_fwd2[[i]][,'row'] + 2
      zeroes_fwd2[[i]] = zeroes_fwd2[[i]][zeroes_fwd2[[i]][,'row'] <= Ts[i],]
    }
  }

  # standardize

  if (verbose == 1)
    cat('  centering obs seq ', rep(' ', N_digits), sep='')

  if (internal){ # calculate internal means of features (as opposed to using means from external source)
    running_sum = rep(0, M)
    running_count = rep(0, M)
    for (i in 1:N){
      Xs.zerotoNA <- Xs[[i]]
      Xs.zerotoNA[zeroes[[i]]] <- NA
      running_sum <- running_sum + colSums(Xs.zerotoNA, na.rm = TRUE)
      running_count <- running_count + colSums(!is.na(Xs.zerotoNA))
    }
    feature_means <- running_sum / running_count
    ## rowSums(sapply(1:N, function(i){
    ##         colSums(Xs[[i]][nonmissing[[i]],]), na.rm = TRUE)
    ##     }) /
    ##     sum(sapply(nonmissing, sum))
  }

  for (i in 1:N){
    if (verbose == 1){
      cat(rep('\b', N_digits), sprintf(paste('%', N_digits, 'd', sep=''), i), sep='')
    }
    Xs[[i]] = sweep(Xs[[i]], 2, feature_means)
  }
  if (verbose == 1){
      cat('\n')
  }
  if (verbose == 1){
    cat('  scaling obs seq ', rep(' ', N_digits), sep='')
  }

  if (internal){ # calculate internal sds of features (as opposed to using sds from external source)
    running_sum_sq = rep(0, M)
    for (i in 1:N){
      Xs.zerotoNA <- Xs[[i]]
      Xs.zerotoNA[zeroes[[i]]] <- NA
      running_sum_sq <- running_sum_sq + colSums(Xs.zerotoNA^2, na.rm = TRUE)
    }
    feature_sds = sqrt(running_sum_sq / (running_count - 1))
    ## feature_sds = sqrt(
    ##     rowSums(sapply(1:N, function(i){
    ##         colSums(Xs[[i]][nonmissing[[i]],]^2))
    ##     }) /
    ##     (sum(sapply(nonmissing, sum)) - 1)
    ## )
  }
  if (any(feature_sds == 0 | is.na(feature_sds))){
    stop('zero variance in the following features ',
         'after dropping incomplete obs (missing features):\n  ',
         paste(colnames(Xs[[1]])[feature_sds == 0], collapse=', '),
         '\npartially censored obs are used in likelihood calculations ',
         'but not feature standardization or estimation of state distributions'
         )
  }
  for (i in 1:N){
    if (verbose == 1){
      cat(rep('\b', N_digits), sprintf(paste('%', N_digits, 'd', sep=''), i), sep='')
    }
    Xs[[i]] = scale(Xs[[i]], center=F, scale=feature_sds)
    attr(Xs[[i]], 'scaled:center') = feature_means
  }
  if (verbose == 1){
      cat('\n')
  }

  #




  # handle zeroed-out feature-observations
  seqs_with_zeroes = which(sapply(zeroes, length) > 0)
  if (length(seqs_with_zeroes) > 0){

    if (verbose == 1){
      cat('  handling zeroed-out obs in seq ', rep(' ', N_digits), sep='')
    }

    for (i in seqs_with_zeroes){

      if (verbose == 1)
        cat(rep('\b', N_digits), sprintf(paste('%', N_digits, 'd', sep=''), i), sep='')

      # replace zeroed-out feature-obs with -2 sd + small noise
      Xs[[i]][zeroes[[i]]] = stats::rnorm(nrow(zeroes[[i]]),
                                   mean = -3,
                                   sd = .1)

  }
    if (verbose == 1){
        cat('\n')
    }

    # handle 1st derivative of zeroed-out feature-observations
    if (derivatives >= 1){

      if (verbose == 1)
        cat('  handling 1st derivatives at zeroed-out obs in seq ', rep(' ', N_digits), sep='')

      for (i in seqs_with_zeroes){

        if (verbose == 1)
          cat(rep('\b', N_digits), sprintf(paste('%', N_digits, 'd', sep=''), i), sep='')

        zeroes_back1[[i]][,'col'] = zeroes_back1[[i]][,'col'] + M_d0
        zeroes[[i]][,'col'] = zeroes[[i]][,'col'] + M_d0
        zeroes_fwd1[[i]][,'col'] = zeroes_fwd1[[i]][,'col'] + M_d0

        # pull contaminated derivatives back to zero and add small noise
        Xs[[i]][zeroes_back1[[i]]] =stats::rnorm(nrow(zeroes_back1[[i]]), 0, sd=.01)
        Xs[[i]][zeroes[[i]]] =stats::rnorm(nrow(zeroes[[i]]), 0, sd=.01)
        Xs[[i]][zeroes_fwd1[[i]]] =stats::rnorm(nrow(zeroes_fwd1[[i]]), 0, sd=.01)

    }
      if (verbose == 1){
          cat('\n')
      }

    }

    # handle 2nd derivative of zeroed-out feature-observations
    if (derivatives == 2){

      if (verbose == 1)
        cat('  handling 2nd derivatives at zeroed-out obs in seq ', rep(' ', N_digits), sep='')

      for (i in seqs_with_zeroes){

        if (verbose == 1)
          cat(rep('\b', N_digits), sprintf(paste('%', N_digits, 'd', sep=''), i), sep='')

        zeroes_back2[[i]][,'col'] = zeroes_back2[[i]][,'col'] + M_d0 + M_d1
        zeroes_back1[[i]][,'col'] = zeroes_back1[[i]][,'col'] + M_d0
        zeroes[[i]][,'col'] = zeroes[[i]][,'col'] + M_d0
        zeroes_fwd1[[i]][,'col'] = zeroes_fwd1[[i]][,'col'] + M_d0
        zeroes_fwd2[[i]][,'col'] = zeroes_fwd2[[i]][,'col'] + M_d0 + M_d1

        # pull contaminated derivatives back to zero and add small noise
        Xs[[i]][zeroes_back2[[i]]] =stats::rnorm(nrow(zeroes_back2[[i]]), 0, sd=.01)
        Xs[[i]][zeroes_back1[[i]]] =stats::rnorm(nrow(zeroes_back1[[i]]), 0, sd=.01)
        Xs[[i]][zeroes[[i]]] =stats::rnorm(nrow(zeroes[[i]]), 0, sd=.01)
        Xs[[i]][zeroes_fwd1[[i]]] =stats::rnorm(nrow(zeroes_fwd1[[i]]), 0, sd=.01)
        Xs[[i]][zeroes_fwd2[[i]]] =stats::rnorm(nrow(zeroes_fwd2[[i]]), 0, sd=.01)

        attr(Xs[[i]], 'derivatives') = derivatives

    }
            if (verbose == 1){
          cat('\n')
      }

    }

  }

  if (derivatives > 0){
      for (i in 1:N){
          ## drop leading frames if they have missing derivatives
          if (sum(is.na(Xs[[i]][derivatives,])) >= M / (derivatives + 1)){
              Xs[[i]][1:derivatives,] = NA
          }
    }
  }

  return(Xs)

}
