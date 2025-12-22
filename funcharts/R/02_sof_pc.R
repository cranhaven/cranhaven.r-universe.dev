#' Scalar-on-function linear regression based on principal components
#'
#' Scalar-on-function linear regression based on
#' principal components.
#' This function performs multivariate functional principal component analysis
#' (MFPCA)
#' to extract multivariate functional principal components
#' from the multivariate functional covariates,
#' then it builds a linear regression model of a
#' scalar response variable on the
#' covariate scores.
#' Functional covariates are standardized before the regression.
#' See Capezza et al. (2020) for additional details.
#'
#' @param y
#' A numeric vector containing the observations of the
#' scalar response variable.
#' @param mfdobj_x
#' A multivariate functional data object of class mfd
#' denoting the functional covariates.
#' @param single_min_variance_explained
#' The minimum fraction of variance
#' that has to be explained
#' by each multivariate functional principal component into the
#' MFPCA model fitted
#' on the functional covariates such that it is retained into the MFPCA model.
#' Default is 0.
#' @param tot_variance_explained
#' The minimum fraction of variance
#' that has to be explained
#' by the set of multivariate functional principal components
#' retained into the MFPCA model
#' fitted on the functional covariates.
#' Default is 0.9.
#' @param selection
#' A character value with one of three possible values:
#'
#' if "variance", the first M multivariate functional principal components
#' are retained into the MFPCA model such
#' that together they explain a fraction of variance greater
#' than \code{tot_variance_explained},
#'
#' if "PRESS", each j-th functional principal component is retained
#' into the MFPCA model if,
#' by adding it to the
#' set of the first j-1 functional principal components,
#' then the predicted residual error sum of squares (PRESS) statistic decreases,
#' and at the same time the fraction of variance explained
#' by that single component
#' is greater than \code{single_min_variance_explained}.
#' This criterion is used in Capezza et al. (2020).
#'
#' if "gcv", the criterion is equal as in the previous "PRESS" case,
#' but the "PRESS" statistic is substituted by the
#' generalized cross-validation (GCV) score.
#'
#' Default value is "variance".
#' @param components
#' A vector of integers with the components over which
#' to project the functional covariates.
#' If this is not NULL, the criteria to select components are ignored.
#' If NULL, components are selected according to
#' the criterion defined by \code{selection}.
#' Default is NULL.
#'
#' @return
#' a list containing the following arguments:
#'
#' * \code{mod}: an object of class \code{lm} that is a linear regression
#' model where
#' the scalar response variable is \code{y} and
#' the covariates are the MFPCA scores of the functional covariates,
#' * \code{mod$coefficients} contains the matrix of coefficients of the
#' functional regression basis functions,
#'
#' * \code{pca}: an object of class \code{pca_mfd} obtained by doing MFPCA
#' on the functional covariates,
#'
#' * \code{beta_fd}: an object of class \code{mfd} object containing
#' the functional regression coefficient
#' \eqn{\beta(t)} estimated with the
#' scalar-on-function linear regression model,
#'
#' * \code{components}: a vector of integers with the components
#' selected in the \code{pca} model,
#'
#' * \code{selection}: the same as the provided argument
#'
#' * \code{single_min_variance_explained}: the same as the provided argument
#'
#' * \code{tot_variance_explained}: the same as the provided argument
#'
#' * \code{gcv}: a vector whose j-th element is the GCV score obtained
#' when retaining the first j components
#' in the MFPCA model.
#'
#' * \code{PRESS}: a vector whose j-th element is the PRESS statistic
#' obtained when retaining the first j components
#' in the MFPCA model.
#'
#'
#' @export
#'
#' @references
#' Capezza C, Lepore A, Menafoglio A, Palumbo B, Vantini S. (2020)
#' Control charts for
#' monitoring ship operating conditions and CO2 emissions based
#' on scalar-on-function regression.
#' \emph{Applied Stochastic Models in Business and Industry},
#' 36(3):477--500.
#' <doi:10.1002/asmb.2507>
#'
#' @examples
#' library(funcharts)
#' data("air")
#' air <- lapply(air, function(x) x[1:10, , drop = FALSE])
#' fun_covariates <- c("CO", "temperature")
#' mfdobj_x <- get_mfd_list(air[fun_covariates], lambda = 1e-2)
#' y <- rowMeans(air$NO2)
#' mod <- sof_pc(y, mfdobj_x)
#'
sof_pc <- function(y,
                   mfdobj_x,
                   tot_variance_explained = 0.9,
                   selection = "variance",
                   single_min_variance_explained = 0,
                   components = NULL) {

  if (!is.numeric(y)) {
    stop("y must be numeric.")
  }
  if (!is.mfd(mfdobj_x)) {
    stop("mfdobj_x must be an mfd object.")
  }

  if (!(selection %in% c("variance", "PRESS", "gcv"))) {
    stop("selection must be one of 'variance', 'PRESS', 'gcv'.")
  }

  nobsx <- dim(mfdobj_x$coefs)[2]
  nobsy <- length(y)
  if (nobsx != nobsy) {
    stop(paste0("y and mfdobj_x must have ",
                "the same number of observations."))
  }
  nobs <- nobsx

  if (!is.null(components)) {
    if (!is.numeric(components)) {
      stop("components must be a vector of positive integers.")
      }
    if (!is.integer(components)) {
      components <- as.integer(components)
    }
    if (sum(components <= 0) > 0) {
      stop("components must be a vector of positive integers.")
    }
  }

  nbasis <- mfdobj_x$basis$nbasis
  if (nbasis < 13) {
    stop("mfdobj_x must have at least 13 basis functions.")
  }
  nvar <- dim(mfdobj_x$coefs)[3]
  nharm <- min(nobs - 2, nbasis * nvar)
  pca <- pca_mfd(mfdobj_x, nharm = nharm)
  scores <- pca$pcscores

  XtX_diag <- colSums(scores^2)
  X2 <- scores^2
  beta_scores <- c(mean(y), crossprod(scores, y) / XtX_diag)
  yhat <- t(Rfast::colCumSums(t(cbind(1, scores)) * beta_scores))
  res2 <- (y - yhat)^2
  H <- t(Rfast::colCumSums(rbind(1 / length(y), t(scores^2) / XtX_diag)))
  PRESS <- colSums(res2 / (1 - H)^2)
  gcv <- colMeans(t(t(res2 / (1 - colMeans(H)^2))))

  if (is.null(components)) {

    if (selection == "PRESS") {
      components <-
        which(diff(PRESS) < 0 & pca$varprop > single_min_variance_explained)
    }

    if (selection == "gcv") {
      components <-
        which(diff(gcv) < 0 & pca$varprop > single_min_variance_explained)
    }

    if (selection == "variance") {
      components_enough_var <- cumsum(pca$varprop) > tot_variance_explained
      if (sum(components_enough_var) == 0)
        ncomponents <- length(pca$varprop) else
          ncomponents <- which(cumsum(pca$varprop) > tot_variance_explained)[1]
      components <- seq_len(ncomponents)
      components <-
        which(pca$varprop[components] > single_min_variance_explained)
    }
  }

  mod <- stats::lm(y ~ .,
                   data = data.frame(scores[, components, drop = FALSE], y = y))

  beta_fd <- 0
  for (jj in seq_along(components)) {
    beta_fd <-
      beta_fd + mod$coefficients[1 + jj] * pca$harmonics[components[jj]]
  }

  variables <- mfdobj_x$fdnames[[3]]
  n_var <- length(variables)
  bs <- pca$harmonics$basis
  n_basis <- bs$nbasis


  coefs <- apply(pca$harmonics[components]$coefs, 3,
                 function(coefs) coefs %*% mod$coefficients[- 1])
  coefs <- array(coefs, dim = c(n_basis, 1, n_var))
  dimnames(coefs) <- list(bs$names, "beta", variables)
  fdnames <- list(pca$harmonics$fdnames[[1]], "beta", variables)
  beta_fd <- mfd(coefs, bs, fdnames, B = bs$B)

  list(mod = mod,
       pca = pca,
       beta_fd = beta_fd,
       residuals = mod$residuals,
       components = components,
       selection = selection,
       single_min_variance_explained = single_min_variance_explained,
       tot_variance_explained = tot_variance_explained,
       gcv = gcv,
       PRESS = PRESS)

}


#' Use a scalar-on-function linear regression model for prediction
#'
#' Predict new observations of the scalar response variable
#' and calculate the corresponding prediction error,
#' with prediction interval limits,
#' given new observations of functional covariates and
#' a fitted scalar-on-function linear regression model
#'
#' @param object
#' A list obtained as output from \code{sof_pc},
#' i.e. a fitted scalar-on-function linear regression model.
#' @param y_new
#' A numeric vector containing the new observations of
#' the scalar response variable
#' to be predicted.
#' @param mfdobj_x_new
#' An object of class \code{mfd} containing
#' new observations of the functional covariates.
#' If NULL, it is set as the functional covariates data used for model fitting.
#' @param alpha
#' A numeric value indicating the Type I error
#' for the regression control chart
#' and such that this function returns the \code{1-alpha}
#' prediction interval on the response.
#' Default is 0.05.
#' @param newdata
#' Deprecated, use \code{mfdobj_x_new} argument.
#'
#' @return
#' A \code{data.frame} with as many rows as the
#' number of functional replications in \code{newdata},
#' with the following columns:
#'
#' * \code{fit}: the predictions of the response variable
#' corresponding to \code{new_data},
#'
#' * \code{lwr}:
#' lower limit of the \code{1-alpha} prediction interval
#' on the response, based on the assumption that it is normally distributed.
#'
#' * \code{upr}:
#' upper limit of the \code{1-alpha} prediction interval
#' on the response, based on the assumption that it is normally distributed.
#'
#' * \code{res}:
#' the residuals obtained as the values of \code{y_new} minus their
#' fitted values. If the scalar-on-function model has been fitted with
#' \code{type_residual == "studentized"}, then the studentized residuals
#' are calculated.
#'
#' @export
#' @examples
#' library(funcharts)
#' data("air")
#' air <- lapply(air, function(x) x[1:10, , drop = FALSE])
#' fun_covariates <- c("CO", "temperature")
#' mfdobj_x <- get_mfd_list(air[fun_covariates], lambda = 1e-2)
#' y <- rowMeans(air$NO2)
#' mod <- sof_pc(y, mfdobj_x)
#' predict_sof_pc(mod)
#'
predict_sof_pc <- function(object,
                           y_new = NULL,
                           mfdobj_x_new = NULL,
                           alpha = 0.05,
                           newdata) {

  if (!missing(newdata)) {
    warning(paste0("argument newdata is deprecated; ",
                   "please use mfdobj_x_new instead."),
            call. = FALSE)
    mfdobj_x_new <- newdata
  }

  if (!is.list(object)) {
    stop("object must be a list produced by sof_pc.")
  }

  if (!identical(names(object), c(
    "mod",
    "pca",
    "beta_fd",
    "residuals",
    "components",
    "selection",
    "single_min_variance_explained",
    "tot_variance_explained",
    "gcv",
    "PRESS"
  ))) {
    stop("object must be a list produced by sof_pc.")
  }

  if (!is.null(y_new)) {
    if (!is.numeric(y_new)) {
      stop("y_new must be numeric.")
    }
  }

  if (!is.null(mfdobj_x_new)) {
    if (!is.mfd(mfdobj_x_new)) {
      stop("mfdobj_x_new must be an object from mfd class.")
    }
    if (dim(mfdobj_x_new$coefs)[3] != dim(object$pca$data$coefs)[3]) {
      stop(paste0("mfdobj_x_new must have the same number of variables ",
                  "as training data."))
    }
  }

  if (alpha <= 0 | alpha >= 1) {
    stop("alpha must be strictly between 0 and 1.")
  }

  mod <- object$mod
  pca <- object$pca
  components <- object$components

  if (is.null(mfdobj_x_new) | is.null(y_new)) {
    mfdobj_x_new <- pca$data
    mfdobj_x_new_scaled <- pca$data_scaled
    fml <- stats::formula(object$mod)
    response_name <- all.vars(fml)[1]
    y_new <- object$mod$model[, response_name]
  } else {
    mfdobj_x_new_scaled <- scale_mfd(mfdobj_x_new,
                                center = pca$center_fd,
                                scale = if (pca$scale) pca$scale_fd else FALSE)
  }

  nobsx <- dim(mfdobj_x_new$coefs)[2]
  nobsy <- length(y_new)
  if (nobsx != nobsy) {
    stop(paste0("y_new and mfdobj_x_new must have ",
                "the same number of observations."))
  }

  scores <- as.data.frame(get_scores(pca,
                                     components,
                                     newdata_scaled = mfdobj_x_new_scaled))

  y_hat_int <- stats::predict(mod,
                              newdata = scores,
                              interval = "prediction",
                              level = 1 - alpha)

  ret <- data.frame(y_hat_int)

  res <- y_new - ret$fit
  # hatvalues_new <-
  #   colSums(t(cbind(1, scores)^2) / colSums(stats::model.matrix(object$mod)^2))
  # if (object$type_residual == "studentized") {
  #   res <- res / (summary(mod)$sigma * sqrt(1 - hatvalues_new))
  # }
  ret$pred_err <- res
  ret$y <- y_new

  return(ret)

}


#' Plot bootstrapped estimates of the scalar-on-function regression coefficient
#'
#' Plot bootstrapped estimates of the
#' scalar-on-function regression coefficient
#' for empirical uncertainty quantification. For each iteration,
#' a data set is sampled with replacement
#' from the training data use to fit the model,
#' and the regression coefficient is estimated.
#'
#' @param mod
#' A list obtained as output from \code{\link{sof_pc}},
#' i.e. a fitted scalar-on-function linear regression model.
#' @param nboot
#' Number of bootstrap replicates
#' @param ncores
#' If you want estimate the bootstrap replicates in parallel,
#' give the number of cores/threads.
#'
#' @return
#' A ggplot showing several bootstrap replicates
#' of the multivariate functional coefficients estimated
#' fitting the scalar-on-function linear model.
#' Gray lines indicate the different bootstrap estimates,
#' the black line indicate the estimate on the entire dataset.
#' @export
#' @examples
#' library(funcharts)
#' data("air")
#' air <- lapply(air, function(x) x[1:10, , drop = FALSE])
#' fun_covariates <- c("CO", "temperature")
#' mfdobj_x <- get_mfd_list(air[fun_covariates], lambda = 1e-2)
#' y <- rowMeans(air$NO2)
#' mod <- sof_pc(y, mfdobj_x)
#' plot_bootstrap_sof_pc(mod, nboot = 5)
#'
plot_bootstrap_sof_pc <- function(mod, nboot = 25, ncores = 1) {

  variables <- mod$beta_fd$fdnames[[3]]
  nbasis <- mod$beta_fd$basis$nbasis
  nn <- nrow(mod$mod$model)
  components <- mod$components

  single_boot <- function(ii) {
    rows_B <- sample(seq_len(nn), nn, TRUE)
    mod <- sof_pc(mfdobj_x = mod$pca$data[rows_B],
                  y = mod$mod$model$y[rows_B],
                  components = mod$components)
    mod$beta_fd$coefs[, 1, ]
  }
  if (ncores == 1) {
    B <- lapply(seq_len(nboot), single_boot)
  } else {
    if (.Platform$OS.type == "unix") {
      B <- parallel::mclapply(seq_len(nboot), single_boot, mc.cores = ncores)
    } else {
      cl <- parallel::makeCluster(ncores)
      parallel::clusterExport(cl, c("nn", "mod"), envir = environment())
      B <- parallel::parLapply(cl, seq_len(nboot), single_boot)
      parallel::stopCluster(cl)
    }
  }
  B <- simplify2array(B)
  if (length(variables) == 1) {
    B <- array(B, dim = c(nrow(B), ncol(B), 1))
  } else B <- aperm(B, c(1, 3, 2))

  dimnames(B)[[2]] <- seq_len(nboot)

  B_mfd <- mfd(
    B,
    mod$beta_fd$basis,
    list(mod$beta_fd$fdnames[[1]],
         seq_len(nboot),
         variables),
    B = mod$beta_fd$basis$B)
  p <- plot_mfd(mfdobj = B_mfd,
                alpha = .3,
                lwd = .3,
                col = "darkgray",
                y_lim_equal = TRUE) &
    ggplot2::geom_hline(yintercept = 0, lty = 2)
  lines_mfd(p, mfdobj_new = mod$beta_fd, linewidth = 0.5, y_lim_equal = TRUE)

}


