#' Function-on-function linear regression based on principal components
#'
#' Function-on-function linear regression based on
#' principal components.
#' This function performs multivariate functional principal component analysis
#' (MFPCA)
#' to extract multivariate functional principal components
#' from the multivariate functional covariates as well as from the
#' functional response, then it builds a linear regression model
#' of the response scores on the
#' covariate scores.
#' Both functional covariates and response are standardized before
#' the regression.
#' See Centofanti et al. (2021) for additional details.
#'
#' @param mfdobj_y
#' A multivariate functional data object of class
#' mfd denoting the functional response variable.
#' Although it is a multivariate functional data object, it must
#' have only one functional variable.
#' @param mfdobj_x
#' A multivariate functional data object of class
#' mfd denoting the functional covariates.
#' @param tot_variance_explained_x
#' The minimum fraction of variance
#' that has to be explained
#' by the multivariate functional principal components retained into
#' the MFPCA model fitted on the functional covariates.
#' Default is 0.95.
#' @param tot_variance_explained_y
#' The minimum fraction of variance that
#' has to be explained
#' by the multivariate functional principal components retained into
#' the MFPCA model fitted on the functional response.
#' Default is 0.95.
#' @param tot_variance_explained_res
#' The minimum fraction of variance
#' that has to be explained
#' by the multivariate functional principal components retained into
#' the MFPCA model fitted on the functional residuals
#' of the functional regression model.
#' Default is 0.95.
#' @param components_x
#' A vector of integers with the components over which
#' to project the functional covariates.
#' If NULL, the first components that explain a minimum fraction of variance
#' equal to \code{tot_variance_explained_x}
#' is selected.
#' #' If this is not NULL, the criteria to select components are ignored.
#' Default is NULL.
#' @param components_y
#' A vector of integers with the components over which
#' to project the functional response.
#' If NULL, the first components that explain a minimum fraction of variance
#' equal to \code{tot_variance_explained_y}
#' is selected.
#' #' If this is not NULL, the criteria to select components are ignored.
#' Default is NULL.
#' @param type_residuals
#' A character value that can be
#' "standard" or "studentized".
#' If "standard", the MFPCA on functional residuals is calculated on
#' the standardized covariates and response.
#' If "studentized", the MFPCA on studentized version of
#' the functional residuals is calculated on the
#' non-standardized covariates and response.
#' See Centofanti et al. (2021) for additional details.
#'
#' @return
#' A list containing the following arguments:
#'
#' * \code{mod}: an object of class \code{lm} that is a linear regression model
#' where
#' the response variables are the MFPCA scores of the response variable and
#' the covariates are the MFPCA scores of the functional covariates.
#' \code{mod$coefficients} contains the matrix of coefficients
#' of the functional regression basis functions,
#'
#' * \code{beta_fd}: a \code{bifd} object containing the
#' bivariate functional regression coefficients \eqn{\beta(s,t)}
#' estimated with the function-on-function linear regression model,
#'
#' * \code{fitted.values}: a multivariate functional data object of
#' class mfd with the fitted values of the
#' functional response observations based on the
#' function-on-function linear regression model,
#'
#' * \code{residuals_original_scale}: a multivariate functional data object
#' of class mfd
#' with the functional residuals of the
#' function-on-function linear regression model on the original scale,
#' i.e. they are the difference between
#' \code{mfdobj_y} and \code{fitted.values},
#'
#' * \code{residuals}: a multivariate functional data object of class mfd
#' with the functional residuals of the
#' function-on-function linear regression model,
#' standardized or studentized depending on
#' the argument \code{type_residuals},
#'
#' * \code{type_residuals}: the same as the provided argument,
#'
#' * \code{pca_x}: an object of class \code{pca_mfd}
#' obtained by doing MFPCA on the functional covariates,
#'
#' * \code{pca_y}: an object of class \code{pca_mfd}
#' obtained by doing MFPCA on the functional response,
#'
#' * \code{pca_res}: an object of class \code{pca_mfd}
#' obtained by doing MFPCA on the functional residuals,
#'
#' * \code{components_x}: a vector of integers
#' with the components selected in the \code{pca_x} model,
#'
#' * \code{components_y}: a vector of integers
#' with the components selected in the \code{pca_y} model,
#'
#' * \code{components_res}: a vector of integers
#' with the components selected in the \code{pca_res} model,
#'
#' * \code{y_standardized}: the standardized functional response
#' obtained doing \code{scale_mfd(mfdobj_y)},
#'
#' * \code{tot_variance_explained_x}: the same as the provided argument
#'
#' * \code{tot_variance_explained_y}: the same as the provided argument
#'
#' * \code{tot_variance_explained_res}: the same as the provided argument
#'
#' * \code{get_studentized_residuals}: a function that allows
#' to calculate studentized residuals on new data,
#' given the estimated function-on-function linear regression model.
#'
#' @export
#'
#' @references
#' Centofanti F, Lepore A, Menafoglio A, Palumbo B, Vantini S. (2021)
#' Functional Regression Control Chart.
#' \emph{Technometrics}, 63(3), 281--294. <doi:10.1080/00401706.2020.1753581>
#'
#' @examples
#' library(funcharts)
#' data("air")
#' air <- lapply(air, function(x) x[1:10, , drop = FALSE])
#' fun_covariates <- c("CO", "temperature")
#' mfdobj <- get_mfd_list(air, lambda = 1e-2)
#' mfdobj_y <- mfdobj[, "NO2"]
#' mfdobj_x <- mfdobj[, fun_covariates]
#' mod <- fof_pc(mfdobj_y, mfdobj_x)
#'
fof_pc <- function(mfdobj_y,
                   mfdobj_x,
                   tot_variance_explained_x = 0.95,
                   tot_variance_explained_y = 0.95,
                   tot_variance_explained_res = 0.95,
                   components_x = NULL,
                   components_y = NULL,
                   type_residuals = "standard") {

  if (!is.mfd(mfdobj_y)) {
    stop("mfdobj_y must be an mfd object.")
  }
  if (!is.mfd(mfdobj_x)) {
    stop("mfdobj_x must be an mfd object.")
  }

  n_obsx <- dim(mfdobj_x$coefs)[2]
  n_obsy <- dim(mfdobj_y$coefs)[2]
  if (n_obsx != n_obsy) {
    stop(paste0("mfdobj_y and mfdobj_x must have ",
                "the same number of observations."))
  }
  n_obs <- n_obsx

  n_var_x <- length(mfdobj_x$fdnames[[3]])
  n_var_y <- length(mfdobj_y$fdnames[[3]])

  if (!(type_residuals %in% c("standard", "studentized"))) {
    stop("type_residuals can be only standard or studentized")
  }

  if (!is.null(components_x)) {
    if (!is.integer(components_x)) {
      components_x <- as.integer(components_x)
    }
    if (sum(components_x <= 0) > 0) {
      stop("components_x must be a vector of positive integers.")
    }
  }

  if (!is.null(components_y)) {
    if (!is.integer(components_y)) {
      components_y <- as.integer(components_y)
    }
    if (sum(components_y <= 0) > 0) {
      stop("components_y must be a vector of positive integers.")
    }
  }

  basis_x <- mfdobj_x$basis
  basis_y <- mfdobj_y$basis
  nbasis_x <- basis_x$nbasis
  nbasis_y <- basis_y$nbasis

  if (nbasis_x < 13) {
    stop("mfdobj_x must have at least 13 basis functions.")
  }
  if (nbasis_y < 13) {
    stop("mfdobj_y must have at least 13 basis functions.")
  }

  argvar_x <- mfdobj_x$fdnames[[1]]
  argvar_y <- mfdobj_y$fdnames[[1]]

  y_z <- scale_mfd(mfdobj_y)

  nharm_x <- min(n_obs - 1, nbasis_x * n_var_x)
  nharm_y <- min(n_obs - 1, nbasis_y * n_var_y)
  nharm_res <- min(n_obs - 1, nbasis_y, nbasis_y * n_var_y)

  x_pca <- pca_mfd(mfdobj_x, nharm = nharm_x)
  y_pca <- pca_mfd(mfdobj_y, nharm = nharm_y)

  if (is.null(components_x)) {
    components_enough_var_x <-
      cumsum(x_pca$varprop) > tot_variance_explained_x
    if (sum(components_enough_var_x) == 0) {
      ncomponents_x <-
        length(x_pca$varprop)
    } else {
      ncomponents_x <-
        which(cumsum(x_pca$varprop) > tot_variance_explained_x)[1]
    }
    components_x <- seq_len(ncomponents_x)
  }

  if (is.null(components_y)) {
    components_enough_var_y <-
      cumsum(y_pca$varprop) > tot_variance_explained_y
      if (sum(components_enough_var_y) == 0) {
        ncomponents_y <-
          length(y_pca$varprop)
      } else {
        ncomponents_y <-
          which(cumsum(y_pca$varprop) > tot_variance_explained_y)[1]
      }
    components_y <- seq_len(ncomponents_y)
  }

  df_y <- data.frame(y_pca$pcscores[, components_y, drop = FALSE])
  names(df_y) <- paste0("y.", names(df_y))
  df_x <- data.frame(x_pca$pcscores[, components_x, drop = FALSE])
  names(df_x) <- paste0("x.", names(df_x))

  y_fml <- paste(names(df_y), collapse = ", ")
  y_fml <- paste0("cbind(", y_fml, ")")
  x_fml <- paste(names(df_x), collapse = " + ")
  fml <- stats::as.formula(paste(y_fml, "~ -1 +", x_fml))

  mod <- stats::lm(fml, data = data.frame(df_x, df_y))
  B <- mod$coefficients
  B <- as.matrix(B)

  Gx <- x_pca$harmonics$coefs[, components_x, , drop = FALSE]
  Gx <- do.call(rbind, lapply(seq_len(n_var_x), function(jj) {
    matrix(Gx[, , jj], nrow = dim(Gx)[1])
    }))
  Gy <- y_pca$harmonics$coefs[, components_y, , drop = FALSE]
  Gy <- do.call(rbind, lapply(seq_len(n_var_y), function(jj) {
    matrix(Gy[, , jj], nrow = dim(Gy)[1])
    }))
  beta_coefs <- Gx %*% B %*% t(Gy)
  kx <- x_pca$harmonics$basis$nbasis
  ky <- y_pca$harmonics$basis$nbasis
  beta_coefs <- array(as.numeric(beta_coefs),
                      dim = c(kx, n_var_x, ky, n_var_y))
  beta_coefs <- aperm(beta_coefs, perm = c(1, 3, 2, 4))
  beta_coefs <- array(as.numeric(beta_coefs),
                      dim = c(kx, ky, 1, n_var_x * n_var_y))

  fdnames <- as.matrix(expand.grid(y_pca$harmonics$fdnames[[3]],
                                   x_pca$harmonics$fdnames[[3]]))
  fdnames <- apply(fdnames, 1, function(x) paste(x, collapse = " vs "))

  bifdnames <- list(x_pca$harmonics$basis$names,
                    y_pca$harmonics$basis$names,
                    "",
                    fdnames)
  beta_fd <- fda::bifd(beta_coefs,
                       x_pca$harmonics$basis,
                       y_pca$harmonics$basis,
                       bifdnames)

  y_scores_hat <- stats::predict(mod)

  if (!is.matrix(y_scores_hat)) {
    y_scores_hat <- matrix(y_scores_hat)
    rownames(y_scores_hat) <- mfdobj_x$fdnames[[2]]
    colnames(y_scores_hat) <- names(df_y)
  }

  y_hat_z <- get_fit_pca_given_scores(
    y_scores_hat,
    y_pca$harmonics[components_y])

  y_hat <- descale_mfd(
    y_hat_z,
    center = attr(y_z, "scaled:center"),
    scale = attr(y_z, "scaled:scale"))

  if (type_residuals == "standard") {
    res <- mfd(y_z$coefs - y_hat_z$coefs,
               mfdobj_y$basis,
               mfdobj_y$fdnames,
               B = mfdobj_y$basis$B)
    res_pca <- pca_mfd(res, scale = FALSE, nharm = nharm_res)
  }

  get_studentized_residuals <- NULL

  res_original <- mfd(mfdobj_y$coefs - y_hat$coefs,
                      basis_y,
                      mfdobj_y$fdnames,
                      B = mfdobj_y$basis$B)

  if (type_residuals == "studentized") {

    domain <- res_original$basis$rangeval
    bs <- fda::create.bspline.basis(domain, 100)
    bs$B <- fda::inprod.bspline(fda::fd(diag(1, bs$nbasis), bs))

    sd_res <- fda::sd.fd(res_original)
    var_res <- fda::times.fd(sd_res, sd_res, bs)

    sigma_M <- crossprod(mod$residuals) / n_obs

    # `gain` is \xi'(\xi'\xi)^(-1) \xi in Eq. (36),
    # one value for each of the n_obs.
    gain <- stats::hatvalues(mod)

    # `psp_coef` is the vector of basis coefficients of the
    # function psi_M(t)' sigma_M psi_M(t) in Eq. (36).
    xseq <- seq(bs$rangeval[1], bs$rangeval[2], length.out = 1000)
    y_pca_eval <- fda::eval.fd(xseq, y_pca$harmonics)
    y_pca_coef <- fda::project.basis(y_pca_eval, xseq, bs)
    G <- as.numeric(y_pca_coef[, seq_len(ncomponents_y), 1])
    G <- matrix(G, ncol = ncomponents_y)
    psp_coef <- G %*% sigma_M %*% t(G)
    bifd_obj <- fda::bifd(psp_coef, sbasisobj = bs, tbasisobj = bs)
    psp_eval <- fda::evaldiag.bifd(xseq, bifd_obj)
    psp_coef <- as.numeric(fda::project.basis(psp_eval, xseq, bs))

    get_studentized_residuals <- function(gain, pred_error) {

      # `omega_coef` is the matrix of basis coefficients of the
      # function \omega(t,t) in Eq. (36).
      omega_coef <- outer(psp_coef, gain)

      # `cov_hat_coef` is the matrix of basis coefficients
      ## of the covariance function estimate at the denominator of Eq. (35).
      cov_hat_coef <- omega_coef + var_res$coefs[, 1]

      # `cov_hat_sqrt` is the inverse 1 over the sqrt of the cov in the
      # denominator of Eq. (35) (one function per each of the n_obs).
      cov_hat_sqrt_inv <- fda::fd(cov_hat_coef, bs)^(-.5)

      studentized <- fda::times.fd(fda::fd(pred_error$coefs[, , 1], basis_y),
                                   fda::fd(cov_hat_sqrt_inv$coefs, bs),
                                   basisobj = bs)

      mfd(array(studentized$coefs, dim = c(dim(studentized$coefs), 1)),
          bs,
          pred_error$fdnames,
          B = bs$B)

    }

    res <- get_studentized_residuals(gain, res_original)
    res_pca <- pca_mfd(res, scale = FALSE, nharm = nharm_res)

  }

  components_enough_var_res <-
    cumsum(res_pca$varprop) > tot_variance_explained_res
  if (sum(components_enough_var_res) == 0) {
    ncomponents_res <- length(res_pca$varprop)
  } else {
    ncomponents_res <-
      which(cumsum(res_pca$varprop) > tot_variance_explained_res)[1]
  }
  components_res <- seq_len(ncomponents_res)

  list(mod = mod,
       beta_fd = beta_fd,
       fitted.values = y_hat,
       residuals_original_scale = res_original,
       residuals = res,
       type_residuals = type_residuals,
       pca_x = x_pca,
       pca_y = y_pca,
       pca_res = res_pca,
       components_x = components_x,
       components_y = components_y,
       components_res = components_res,
       y_standardized = y_z,
       tot_variance_explained_x = tot_variance_explained_x,
       tot_variance_explained_y = tot_variance_explained_y,
       tot_variance_explained_res = tot_variance_explained_res,
       get_studentized_residuals = get_studentized_residuals)

}


#' Use a function-on-function linear regression model for prediction
#'
#' Predict new observations of the functional response variable
#' and calculate the corresponding prediction error
#' (and their standardized or studentized version)
#' given new observations of functional covariates and
#' a fitted function-on-function linear regression model.
#'
#' @param object
#' A list obtained as output from \code{fof_pc},
#' i.e. a fitted function-on-function linear regression model.
#' @param mfdobj_y_new
#' An object of class \code{mfd} containing
#' new observations of the functional response.
#' @param mfdobj_x_new
#' An object of class \code{mfd} containing
#' new observations of the functional covariates.
#'
#' @return
#' A list of mfd objects. It contains:
#'
#' * \code{pred_error}: the prediction error of the
#' standardized functional response variable,
#'
#' * \code{pred_error_original_scale}:
#' the prediction error of the functional
#' response variable on the original scale,
#'
#' * \code{y_hat_new}: the prediction of the
#' functional response observations on the original scale,
#'
#' * \code{y_z_new}: the standardized version of the
#' functional response observations provided in \code{mfdobj_y_new},
#'
#' * \code{y_hat_z_new}: the prediction of the
#' functional response observations on the standardized/studentized scale.
#'
#'
#' @export
#'
#' @references
#' Centofanti F, Lepore A, Menafoglio A, Palumbo B, Vantini S. (2021)
#' Functional Regression Control Chart.
#' \emph{Technometrics}, 63(3):281--294. <doi:10.1080/00401706.2020.1753581>
#'
#' @examples
#' library(funcharts)
#' data("air")
#' air <- lapply(air, function(x) x[1:10, , drop = FALSE])
#' fun_covariates <- c("CO", "temperature")
#' mfdobj_x <- get_mfd_list(air[fun_covariates], lambda = 1e-2)
#' mfdobj_y <- get_mfd_list(air["NO2"], lambda = 1e-2)
#' mod <- fof_pc(mfdobj_y, mfdobj_x)
#' predict_fof_pc(mod,
#'                mfdobj_y_new = mfdobj_y,
#'                mfdobj_x_new = mfdobj_x)
#'
predict_fof_pc <- function(object,
                           mfdobj_y_new,
                           mfdobj_x_new) {

  if (!is.list(object)) {
    stop("object must be a list produced by fof_pc.")
  }

  if (!identical(names(object), c(
    "mod",
    "beta_fd",
    "fitted.values",
    "residuals_original_scale",
    "residuals",
    "type_residuals",
    "pca_x",
    "pca_y",
    "pca_res",
    "components_x",
    "components_y",
    "components_res",
    "y_standardized",
    "tot_variance_explained_x",
    "tot_variance_explained_y",
    "tot_variance_explained_res",
    "get_studentized_residuals"
  ))) {
    stop("object must be a list produced by fof_pc.")
  }

  if (!is.mfd(mfdobj_y_new)) {
    stop("mfdobj_y_new must be an mfd object.")
  }
  if (!is.mfd(mfdobj_x_new)) {
    stop("mfdobj_x_new must be an mfd object.")
  }

  n_obsx <- dim(mfdobj_x_new$coefs)[2]
  n_obsy <- dim(mfdobj_y_new$coefs)[2]
  if (n_obsx != n_obsy) {
    stop(paste0("mfdobj_y_new and mfdobj_x_new must have ",
                "the same number of observations."))
  }

  n_var_x <- length(mfdobj_x_new$fdnames[[3]])
  n_var_y <- length(mfdobj_y_new$fdnames[[3]])

  mod <- object$mod
  pca_x <- object$pca_x
  pca_y <- object$pca_y
  components_x <- object$components_x
  components_y <- object$components_y
  obs_train <- pca_x$data$fdnames[[2]]
  obs_new <- mfdobj_x_new$fdnames[[2]]
  nobs_train <- length(obs_train)
  nobs_new <- length(obs_new)

  if (is.null(mfdobj_x_new)) {
    mfdobj_x_new_scaled <- pca_x$data_scaled
  } else {
    mfdobj_x_new_scaled <-
      scale_mfd(mfdobj_x_new,
                center = pca_x$center_fd,
                scale = if (pca_x$scale) pca_x$scale_fd else FALSE)
  }

  x_new <- get_scores(pca = pca_x,
                      components = components_x,
                      newdata_scaled = mfdobj_x_new_scaled)
  df_x_new <- data.frame(x_new)
  names(df_x_new) <- paste0("x.", names(df_x_new))

  y_scores_hat <- stats::predict(mod, newdata = df_x_new)
  if (!is.matrix(y_scores_hat)) {
    y_scores_hat <- matrix(y_scores_hat,
                           nrow = nobs_new,
                           ncol = length(components_y))
    rownames(y_scores_hat) <- obs_new
    colnames(y_scores_hat) <-
      paste0("y.", pca_y$harmonics$fdnames[[2]][components_y])
  }
  y_hat_z_new <- get_fit_pca_given_scores(
    scores = y_scores_hat,
    harmonics = pca_y$harmonics[components_y])

  center <- attr(object$y_standardized, "scaled:center")
  scale <- attr(object$y_standardized, "scaled:scale")
  y_hat_new <- descale_mfd(y_hat_z_new, center = center, scale = scale)
  y_z_new <- scale_mfd(mfdobj_y_new, center = center, scale = scale)

  pred_error_original_scale <- mfd(
    mfdobj_y_new$coefs - y_hat_new$coefs,
    mfdobj_y_new$basis,
    mfdobj_y_new$fdnames,
    B = mfdobj_y_new$basis$B)

  if (object$type_residuals == "standard") {
    pred_error <- mfd(
      y_z_new$coefs - y_hat_z_new$coefs,
      mfdobj_y_new$basis,
      mfdobj_y_new$fdnames,
      B = mfdobj_y_new$basis$B)
  }

  if (object$type_residuals == "studentized") {

    X <- pca_x$pcscores[, object$components_x, drop = FALSE]
    # XX <- crossprod(X)
    # XXinv <- solve(XX)
    # gain_new <- diag(x_new %*% XXinv %*% t(x_new))
    gain_new <- rowSums(t(t(x_new)^2 / colSums(X^2)))
    pred_error <- object$get_studentized_residuals(
      gain = gain_new,
      pred_error = pred_error_original_scale)

  }

  list(
    pred_error = pred_error,
    pred_error_original_scale = pred_error_original_scale,
    y_hat_new = y_hat_new,
    y_z_new = y_z_new,
    y_hat_z_new = y_hat_z_new
  )

}

