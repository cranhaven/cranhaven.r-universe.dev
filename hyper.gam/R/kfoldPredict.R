






# `kfoldPredict` https://www.mathworks.com/help/stats/classreg.learning.partition.regressionpartitionedmodel.kfoldpredict.html


#' @title \eqn{k}-Fold Prediction of [hyper_gam] Model
#' 
#' @description
#' \eqn{k}-fold prediction of [hyper_gam] model.
#' 
#' @param object a [hyper_gam] object
#' 
#' @param k \link[base]{integer} scalar
#' 
#' @param mc.cores \link[base]{integer} scalar, see function \link[parallel]{mclapply}
#' 
#' @param ... additional parameters, currently not in use
#' 
#' @returns 
#' The (pseudo) `S3` method [kfoldPredict.hyper_gam()] returns a \link[base]{numeric} \link[base]{vector}, 
#' with \link[base]{attributes} for savvy developers
#' \describe{
#' \item{`attr(.,'fold')`}{\link[base]{integer} \link[base]{vector}, indices of the \eqn{i}-th fold}
#' \item{`attr(.,'sgn')`}{\link[base]{numeric} \link[base]{vector} of length-\eqn{k}, sign-adjustment for each fold}
#' \item{`attr(.,'no_sadj')`}{\link[base]{numeric} \link[base]{vector}, predictions without sign adjustment}
#' \item{`attr(.,'global_sadj')`}{\link[base]{numeric} \link[base]{vector}, predictions based on sign-adjustment by the complete data (instead of each fold)}
#' }
#' 
#' @keywords internal
#' @importFrom caret createFolds
#' @importFrom parallel mclapply
#' @export
kfoldPredict.hyper_gam <- function(
    object, 
    k, 
    mc.cores = getOption('mc.cores'), 
    ...
) { 
  
  if (!inherits(object, what = 'hyper_gam')) stop('input must be `hyper_gam`')
  
  data <- getData.gam(object)
  
  # ?caret::createFolds depends on ?base::set.seed
  nr <- nrow(data)
  fld <- createFolds(y = seq_len(nr), k = k, list = TRUE, returnTrain = FALSE)
  
  sgn <- rep(NA_integer_, times = k)
  fold <- rep(NA_integer_, times = nr)
  ret <- # k-fold predictor (per-fold sign-adjusted)
    no_sadj <- # k-fold predictor (no sign-adjusted)
    rep(NA_real_, times = nr)
  
  tmp <- fld |> 
    mclapply(mc.cores = mc.cores, FUN = \(id) { #(id = fld[[1L]])
      # lapply(FUN = \(id) {
      d0 <- data[-id, , drop = FALSE] # training set
      d1 <- data[id, , drop = FALSE] # test set
      m <- tryCatch(update.hyper_gam(object, data = d0), error = identity) # training model
      if (inherits(m, what = 'error')) return(invisible())
      i_sgn <- m |> cor_xy.hyper_gam() |> sign()
      # predicted value on test set
      i_ret <- m |> predict.hyper_gam(newdata = d1, sgn = i_sgn) # with sign-adjustment
      attr(i_ret, which = 'no_sadj') <- m |> predict.hyper_gam(newdata = d1, sgn = 1) # without sign-adjustment
      attr(i_ret, which = 'sgn') <- i_sgn
      return(i_ret)
    })
  
  for (i in seq_along(fld)) {
    id <- fld[[i]]
    fold[id] <- i
    if (!length(tmp[[i]])) next # exception handing
    sgn[i] <- tmp[[i]] |> attr(which = 'sgn', exact = TRUE)
    no_sadj[id] <- tmp[[i]] |> attr(which = 'no_sadj', exact = TRUE)
    ret[id] <- tmp[[i]]
  }
  
  attr(ret, which = 'fold') <- fold
  attr(ret, which = 'sgn') <- sgn
  attr(ret, which = 'no_sadj') <- no_sadj
  
  # k-fold predictor (global sign-adjusted)
  sgn_global <- object |> cor_xy.hyper_gam() |> sign()
  attr(ret, which = 'global_sadj') <- no_sadj * sgn_global
  
  return(ret)
  
}

# https://www.geeksforgeeks.org/k-fold-cross-validation-in-r-programming/






