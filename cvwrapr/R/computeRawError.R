#' Compute the nobs by nlambda matrix of errors
#'
#' Computes the nobs by nlambda matrix of errors corresponding to the error
#' measure provided. Only works for "gaussian" and "poisson" families right
#' now.
#'
#' @param predmat Array of predictions. If `y` is univariate, this has
#' dimensions `c(nobs, nlambda)`. If `y` is multivariate with `nc`
#' levels/columns (e.g. for `family = "multionmial"` or
#' `family = "mgaussian"`), this has dimensions `c(nobs, nc, nlambda)`.
#' Note that these should be on the same scale as `y` (unlike in the
#' glmnet package where it is the linear predictor).
#' @param y Response variable.
#' @param type.measure Loss function to use for cross-validation. See
#' `availableTypeMeasures()` for possible values for `type.measure`. Note that
#' the package does not check if the user-specified measure is appropriate
#' for the family.
#' @param family Model family; used to determine the correct loss function.
#' @param weights Observation weights.
#' @param foldid Vector of values identifying which fold each observation is
#' in.
#' @param grouped Experimental argument; see `kfoldcv()` documentation for
#' details.
#'
#' @return A list with the following elements:
#' \item{cvraw}{An nobs by nlambda matrix of raw error values.}
#' \item{weights}{Observation weights.}
#' \item{N}{A vector of length nlambda representing the number of non-NA
#' predictions associated with each lambda value.}
#' \item{type.measure}{Loss function used for CV.}
computeRawError <- function(predmat, y, type.measure, family, weights, foldid,
                            grouped) {
  checkValidTypeMeasure(type.measure, family)

  if ("family" %in% class(family)) family <- "GLM"

  return(do.call(paste0("computeRawError.", family),
                 list(predmat = predmat, y = y, type.measure = type.measure,
                      weights = weights, foldid = foldid, grouped = grouped)))
}

computeRawError.binomial <- function(predmat, y, type.measure,
                                     weights, foldid, grouped) {
  nc <- dim(y)
  if (is.null(nc)) {
    y <- as.factor(y)
    ntab <- table(y)
    nc <- as.integer(length(ntab))
    y <- diag(nc)[as.numeric(y), , drop=FALSE]
  }
  N <- nrow(y)

  foldid_vals <- sort(unique(foldid))
  nfolds <- length(foldid_vals)

  if ((N / nfolds < 10) && type.measure == "auc") {
    warning(paste("Too few (< 10) observations per fold for type.measure='auc'",
                  "in binomial family; changed to type.measure='deviance'.",
                  "Alternatively, use smaller value for nfolds"),
            call. = FALSE)
    type.measure <- "deviance"
  }

  nlambda <- ncol(predmat)
  if (type.measure == "auc") {
    cvraw <- matrix(NA, nfolds, nlambda)
    good <- matrix(0, nfolds, nlambda)
    for (i in seq_along(foldid_vals)) {
      good[i, seq(nlambda)] <- 1
      out_idx <- (foldid == foldid_vals[i])
      for (j in seq(nlambda)) {
        cvraw[i, j] <- getAUC(y[out_idx, ], predmat[out_idx, j], weights[out_idx])
      }
    }
    N <- apply(good, 2, sum)
    weights <- tapply(weights, foldid, sum)
    grouped <- FALSE
  }
  else {
    ywt <- apply(y, 1, sum)
    y <- y / ywt
    weights <- weights * ywt
    N <- nrow(y) - apply(is.na(predmat), 2, sum)
    prob_min <- 1e-05
    prob_max <- 1 - prob_min
    cvraw <- switch(type.measure,
                    mse = (y[, 1] - (1 - predmat))^2 + (y[, 2] - predmat)^2,
                    mae = abs(y[, 1] - (1 - predmat)) + abs(y[, 2] - predmat),
                    deviance = {
                      predmat <- pmin(pmax(predmat, prob_min), prob_max)
                      lp <- y[, 1] * log(1 - predmat) + y[, 2] * log(predmat)
                      ly <- log(y)
                      ly[y == 0] <- 0
                      ly <- drop((y * ly) %*% c(1, 1))
                      2 * (ly - lp)
                    },
                    class = y[, 1] * (predmat > 0.5) + y[, 2] * (predmat <= 0.5)
    )
  }

  return(list(cvraw = cvraw, weights = weights, N = N,
              type.measure = type.measure, grouped = grouped))
}

computeRawError.gaussian <- function(predmat, y, type.measure,
                                     weights, foldid, grouped) {
  N <- length(y) - apply(is.na(predmat), 2, sum)

  cvraw <- switch(type.measure,
                  mse = (y - predmat)^2,
                  deviance = (y - predmat)^2,
                  mae = abs(y - predmat)
  )

  return(list(cvraw = cvraw, weights = weights, N = N,
              type.measure = type.measure, grouped = grouped))
}

computeRawError.poisson <- function(predmat, y, type.measure,
                                     weights, foldid, grouped) {
  N <- length(y) - apply(is.na(predmat), 2, sum)

  cvraw <- switch(type.measure,
                  mse = (y - predmat)^2,
                  mae = abs(y - predmat),
                  deviance = {
                    deveta <- y * log(predmat) - predmat
                    devy <- y * log(y) - y
                    2 * (devy - deveta)
                  }
  )

  return(list(cvraw = cvraw, weights = weights, N = N,
              type.measure = type.measure, grouped = grouped))
}

computeRawError.cox <- function(predmat, y, type.measure,
                                     weights, foldid, grouped) {
  foldid_vals <- sort(unique(foldid))
  nfolds <- length(foldid_vals)

  if (type.measure == "deviance") {
    if (!("cvraw" %in% names(attributes(predmat)))) {
      if (grouped)
        stop(paste("predmat needs 'cvraw' attribute for family='cox',",
                   "type.measure='deviance' and grouped=TRUE;",
                   "can be obtained via buildPredMat()"))
      else {
        cvraw <- matrix(NA, nfolds, ncol(predmat))
        for (i in seq_along(foldid_vals)) {
          out_idx <- (foldid == foldid_vals[i])
          cvraw[i, ] <- coxnet.deviance(pred = predmat[out_idx, ],
                                        y = y[out_idx, ],
                                        weights = weights[out_idx])
        }
      }
    } else {
      cvraw <- attr(predmat, "cvraw")  # hard work done in buildPredMat()
    }
    N <- nfolds - apply(is.na(cvraw), 2, sum)
    weights <- as.vector(tapply(weights * y[, "status"], foldid, sum))
    cvraw <- cvraw / weights
  } else {
    # type.measure == "C"
    nlambda <- ncol(predmat)
    nlams <- rep(nlambda, nfolds)
    cvraw <- matrix(NA, nfolds, nlambda)
    good <- matrix(0, nfolds, nlambda)
    for (i in seq_along(foldid_vals)) {
      good[i, seq(nlams[i])] <- 1
      out_idx <- (foldid == foldid_vals[i])
      for (j in seq(nlams[i])) {
        cvraw[i, j] = getCindex(predmat[out_idx, j], y[out_idx, ],
                                weights[out_idx])
      }
    }
    N <- apply(good, 2, sum)
    weights <- tapply(weights, foldid, sum)
  }

  # grouped is always FALSE because cvraw is always an nfolds by nlambda matrix
  # (not nobs by lambda); aggregation within folds has already been done here
  return(list(cvraw = cvraw, weights = weights, N = N,
              type.measure = type.measure, grouped = FALSE))
}

computeRawError.multinomial <- function(predmat, y, type.measure,
                                        weights, foldid, grouped) {
  # get no. of levels in y, and get y into the form we want
  nc = dim(y)
  if (is.null(nc)) {
    y <- as.factor(y)
    ntab <- table(y)
    nc <- as.integer(length(ntab))
    y <- diag(nc)[as.numeric(y), ,drop=FALSE]
  }
  else nc <- nc[2]

  ywt <- apply(y, 1, sum)
  y <- y/ywt
  weights <- weights * ywt
  N <- nrow(y) - apply(is.na(predmat[, 1, , drop = FALSE]), 2, sum) ## dimensions could be lost if third dim=1
  bigY <- array(y, dim(predmat))
  prob_min <- 1e-05
  prob_max <- 1 - prob_min
  cvraw = switch(type.measure,
                 mse = apply((bigY - predmat)^2, c(1, 3), sum),
                 mae = apply(abs(bigY - predmat), c(1, 3), sum),
                 deviance = {
                   predmat <- pmin(pmax(predmat, prob_min), prob_max)
                   lp <- bigY * log(predmat)
                   ly <- bigY * log(bigY)
                   ly[bigY == 0] <- 0
                   apply(2 * (ly - lp), c(1, 3), sum)
                 },
                 class = {
                   classid <- as.numeric(apply(predmat, 3, getSoftmax,
                                               ignore_labels = TRUE))
                   yperm <- matrix(aperm(bigY, c(1, 3, 2)), ncol = nc)
                   matrix(1 - yperm[cbind(seq(classid), classid)],
                          ncol = dim(predmat)[3])
                 }
  )
  return(list(cvraw = cvraw, weights = weights, N = N,
              type.measure = type.measure, grouped = grouped))
}

computeRawError.mgaussian <- function(predmat, y, type.measure,
                                      weights, foldid, grouped) {
  ndim <- dim(y)
  nc <- ndim[2]
  nobs <- ndim[1]
  N <- nobs - apply(is.na(predmat[, 1, , drop=FALSE ]), 2, sum) # dimensions could be lost if third dim=1
  bigY <- array(y, dim(predmat))
  cvraw <- switch(type.measure,
                 mse = apply((bigY - predmat)^2, c(1, 3), sum),
                 deviance = apply((bigY - predmat)^2, c(1, 3), sum),
                 mae = apply(abs(bigY - predmat), c(1, 3), sum)
  )
  return(list(cvraw = cvraw, weights = weights, N = N,
              type.measure = type.measure, grouped = grouped))
}

computeRawError.GLM <- function(predmat, y, type.measure,
                                weights, foldid, grouped) {
  family <- attr(predmat, "family")
  nobs <- nrow(predmat)
  N <- nobs - apply(is.na(predmat), 2, sum)
  cvraw <- switch(type.measure,
                  mse = (y - predmat)^2,
                  mae = abs(y - predmat),
                  deviance = family$dev.resids(array(y,dim(predmat)),
                                               predmat, 1)
  )

  return(list(cvraw = cvraw, weights = weights, N = N,
              type.measure = type.measure, grouped = grouped))
}
