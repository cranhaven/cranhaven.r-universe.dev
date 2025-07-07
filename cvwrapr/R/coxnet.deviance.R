#' Compute deviance for Cox model
#'
#' Compute the deviance (-2 log partial likelihood) for Cox model. This is a
#' pared down version of `glmnet`'s `coxnet.deviance` with one big difference:
#' here, `pred` is on the scale of `y` (`mu`) while in `glmnet`, `pred` is the
#' linear predictor (`eta`).
#'
#' Computes the deviance for a single set of predictions, or for a matrix
#' of predictions. Uses the Breslow approach to ties.
#'
#' \code{coxnet.deviance()} is a wrapper: it calls the appropriate internal
#' routine based on whether the response is right-censored data or
#' (start, stop] survival data.
#'
#' @param pred Fit vector or matrix. If `NULL`, it is set to all ones.
#' @param y Survival response variable, must be a \code{Surv} or
#' \code{stratifySurv} object.
#' @param weights Observation weights (default is all equal to 1).
#' @param std.weights If TRUE (default), observation weights are standardized
#' to sum to 1.
#'
#' @return A vector of deviances, one for each column of predictions.
#'
#' @examples
#' set.seed(1)
#' eta <- rnorm(10)
#' time <- runif(10, min = 1, max = 10)
#' d <- ifelse(rnorm(10) > 0, 1, 0)
#' y <- survival::Surv(time, d)
#' coxnet.deviance(pred = exp(eta), y = y)
#'
#' # if pred not provided, it is set to ones vector
#' coxnet.deviance(y = y)
#'
#' # example with (start, stop] data
#' y2 <- survival::Surv(time, time + runif(10), d)
#' coxnet.deviance(pred = exp(eta), y = y2)
#'
#' @export
coxnet.deviance <- function(pred = NULL, y, weights = NULL,
                            std.weights = TRUE) {
  # if y has 2 columns, it is right-censored data
  # if y has 3 columns, it is (start, stop] data
  # otherwise, throw error
  if (ncol(y) == 2) {
    return(coxnet.deviance2(pred = pred, y = y, weights = weights,
                            std.weights = std.weights))
  } else if (ncol(y) == 3) {
    return(coxnet.deviance3(pred = pred, y = y, weights = weights,
                            std.weights = std.weights))
  } else {
    stop("Response y should have 2 or 3 columns")
  }
}

# coxnet.deviance routine for right-censored data
#' @importFrom survival is.Surv
coxnet.deviance2 <- function(pred = NULL, y, weights = NULL,
                             std.weights = TRUE) {
  if (!is.Surv(y)) stop("y must be a Surv object")
  nobs <- nrow(y)

  # if pred is NULL, set pred to all zeros
  if (is.null(pred)) pred <- rep(1, times = nobs)

  # if more than one column of predictions is passed, run coxnet.deviance2()
  # for each column
  if (!is.null(ncol(pred)) && ncol(pred) > 1) {
    return(sapply(seq(ncol(pred)),
                  function(j) coxnet.deviance2(
                    pred = pred[, j], y = y,
                    weights = weights, std.weights = std.weights)))
  } else {
    # check that pred is of the right length
    if(length(pred) != nobs) stop("pred and y must have the same length")

    # normalize weights to sum to nobs
    if (is.null(weights))
      w <- rep(1, nobs)
    else {
      if (length(weights) != nobs) stop("weights and y must have the same length")
      if (std.weights) {
        w <- nobs * weights / sum(weights)
      } else {
        w <- weights
      }
    }

    # extract strata (if any)
    if ("strata" %in% names(attributes(y))) {
      strata <- attr(y, "strata")
    } else {
      strata <- rep(1, nobs)
    }
    if (length(strata) != nobs) stop("length of strata != nobs")

    # if all in same strata, do the deviance computation
    # if not, take the sum of the strata-level deviances
    if (length(unique(strata)) == 1) {
      time <- y[, "time"]
      d <- y[, "status"]

      ### Compute saturated loglikelihood
      wd <- w[d == 1]
      tyd <- time[d == 1]
      if (any(duplicated(tyd))) {
        wd <- tapply(wd, tyd, sum)
      }
      wd <- wd[wd > 0]
      lsat <- -sum(wd * log(wd))
      ####

      # order time, d, pred and w in ascending time order
      # for tied times, all deaths come before censored observations
      if ("stop_time" %in% names(attributes(y))) {
        o <- attr(y, "stop_time")
      } else {
        o <- order(time, d, decreasing = c(FALSE, TRUE))
      }
      time <- time[o]
      d <- d[o]
      pred <- pred[o]
      w <- w[o]

      ### See if there are dups in death times
      dups <- fid(time[d==1],seq(length(d))[d==1])
      dd <- d
      ww <- w

      ### next code replaces each sequence of tied death indicators by a new
      ### sequence where only the first is a 1 and the rest are zero. This
      ### makes the accounting in the following step work properly we also
      ### sums the weights in each of the tied death sets, and assign that
      ### weight to the first
      if(!is.null(ties<-dups$index_ties)){
        dd[unlist(ties)]=0
        dd[dups$index_first]=1
        wsum=sapply(ties,function(i,w)sum(w[i]),ww)
        tie1=sapply(ties,function(i)i[1])
        ww[tie1]=wsum
      }

      # compute the sum inside the log term of the partial likelihood
      w_pred <- w * pred
      rsk <- rev(cumsum(rev(w_pred)))

      # take just the terms related to actual death times
      log_terms <- (ww * log(rsk))[dd > 0]
      loglik <- sum((w * log(pred))[d > 0]) - sum(log_terms)

      return(2 * (lsat - loglik))
    } else {
      # more than one strata provided: return the sum of strata-level
      # deviances
      tot_dev <- 0
      for (i in unique(strata)) {
        ii <- which(strata == i)
        tot_dev <- tot_dev +
          coxnet.deviance2(pred = pred[ii], y = y[ii, , drop = FALSE],
                           weights = w[ii], std.weights = FALSE)
      }
      return(tot_dev)
    }
  }
}

# coxnet.deviance routine for (start, stop] data
#' @importFrom survival is.Surv
coxnet.deviance3 <- function(pred = NULL, y, weights = NULL,
                             std.weights = TRUE) {
  if (!is.Surv(y)) stop("y must be a Surv object")
  nobs <- nrow(y)

  # if pred is NULL, set pred to all zeros
  if (is.null(pred)) pred <- rep(1, times = nobs)

  # if more than one column of predictions is passed, run coxnet.deviance3()
  # for each column
  if (!is.null(ncol(pred)) && ncol(pred) > 1) {
    return(sapply(seq(ncol(pred)),
                  function(j) coxnet.deviance3(
                    pred = pred[, j], y = y,
                    weights = weights, std.weights = std.weights)))
  } else {
    # check that pred is of the right length
    if(length(pred) != nobs) stop("pred and y must have the same length")

    # normalize weights to sum to nobs
    if (is.null(weights))
      w <- rep(1, nobs)
    else {
      if (length(weights) != nobs) stop("weights and y must have the same length")
      if (std.weights) {
        w <- nobs * weights / sum(weights)
      } else {
        w <- weights
      }
    }

    # extract strata (if any)
    if ("strata" %in% names(attributes(y))) {
      strata <- attr(y, "strata")
    } else {
      strata <- rep(1, nobs)
    }
    if (length(strata) != nobs) stop("length of strata != nobs")

    # if all in same strata, do the deviance computation
    # if not, take the sum of the strata-level deviances
    if (length(unique(strata)) == 1) {
      start_time <- y[, "start"]
      stop_time <- y[, "stop"]
      d <- y[, "status"]

      ### Compute saturated loglikelihood
      wd <- w[d == 1]
      tyd <- stop_time[d == 1]
      if (any(duplicated(tyd))) {
        wd <- tapply(wd, tyd, sum)
      }
      wd <- wd[wd > 0]
      lsat <- -sum(wd * log(wd))
      ####

      # get ordering for stop time (ascending, deaths before censored), and
      # start time (ascending)
      if ("stop_time" %in% names(attributes(y))) {
        stop_o <- attr(y, "stop_time")
      } else {
        stop_o <- order(stop_time, d, decreasing = c(FALSE, TRUE))
      }
      if ("start_time" %in% names(attributes(y))) {
        start_o <- attr(y, "start_time")
      } else {
        start_o <- order(start_time, decreasing = c(FALSE))
      }

      # keep a set of values which are ordered by start time
      w_pred_start <- (w * pred)[start_o]
      start_time_start <- start_time[start_o]

      # reorder everything by stop time
      start_time <- start_time[stop_o]
      stop_time  <- stop_time[stop_o]
      d          <- d[stop_o]
      pred       <- pred[stop_o]
      w          <- w[stop_o]

      ### See if there are dups in death times
      dups <- fid(stop_time[d == 1], seq(length(d))[d == 1])
      dd <- d
      ww <- w

      ### next code replaces each sequence of tied death indicators by a new
      ### sequence where only the first is a 1 and the rest are zero. This
      ### makes the accounting in the following step work properly we also
      ### sums the weights in each of the tied death sets, and assign that
      ### weight to the first
      if(!is.null(ties<-dups$index_ties)){
        dd[unlist(ties)]=0
        dd[dups$index_first]=1
        wsum=sapply(ties,function(i,w)sum(w[i]),ww)
        tie1=sapply(ties,function(i)i[1])
        ww[tie1]=wsum
      }

      # compute risk set sums rsk[i] = \sum_{j in R_i} w_j exp(eta_j)
      # where i indexes the observations. (In the end, we will only care
      # about the indices i which have actual death times.)
      rsk <- rev(cumsum(rev(w * pred)))
      current_sum <- 0
      stop_idx <- nobs; start_idx <- nobs
      while (stop_idx > 0 && start_idx > 0) {
        if (start_time_start[start_idx] < stop_time[stop_idx]) {
          # current start time belongs in risk set ending in stop time,
          # so we should remove the current cumulative sum and consider
          # the next risk set
          rsk[stop_idx] <- rsk[stop_idx] - current_sum
          stop_idx <- stop_idx - 1
        } else {
          # current start time does not belong in risk set ending in stop
          # time, so we should add it to current_sum and check if the
          # start time before it should also be added
          current_sum <- current_sum + w_pred_start[start_idx]
          start_idx <- start_idx - 1
        }
      }

      log_terms <- ww[dups$index_first] * (log(rsk[dd == 1]))
      loglik <- sum((w * log(pred))[d > 0]) - sum(log_terms)
      return(2 * (lsat -loglik))
    } else {
      # more than one strata provided: return the sum of strata-level
      # deviances
      tot_dev <- 0
      for (i in unique(strata)) {
        ii <- which(strata == i)
        tot_dev <- tot_dev +
          coxnet.deviance3(pred = pred[ii], y = y[ii, , drop = FALSE],
                           weights = w[ii], std.weights = FALSE)
      }
      return(tot_dev)
    }
  }
}

#' Helper function for Cox deviance and gradient
#'
#' Helps to find ties in death times of data.
#'
#' @param x Sorted vector of death times.
#' @param index Vector of indices for the death times.
#'
#' @return A list with two arguments.
#' \item{index_first}{A vector of indices for the first observation at each
#' death time as they appear in the sorted list.}
#' \item{index_ties}{If there are no ties at all, this is NULL. If not, this is
#' a list with length equal to the number of unique times with ties. For each
#' time with ties, index_ties gives the indices of the observations with a
#' death at that time.}
#'
#' @examples \dontrun{
#' # Example with no ties
#' fid(c(1, 4, 5, 6), 1:5)
#'
#' # Example with ties
#' fid(c(1, 1, 1, 2, 3, 3, 4, 4, 4), 1:9)
#' }
#'
#' @keywords internal
fid <- function(x, index) {
  idup <- duplicated(x)
  if (!any(idup))
    return(list(index_first = index, index_ties = NULL))
  else {
    ndup <- !idup
    xu <- x[ndup]  # first death times
    index_first <- index[ndup]
    ities <- match(x, xu)
    index_ties <- split(index, ities)
    nties <- sapply(index_ties, length)
    return(list(index_first = index_first,
                index_ties = index_ties[nties > 1]))
  }
}
