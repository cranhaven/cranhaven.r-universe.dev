#' Multivariate functional principal components analysis
#'
#' Multivariate functional principal components analysis (MFPCA)
#' performed on an object of class \code{mfd}.
#' It is a wrapper to \code{fda::\link[fda]{pca.fd}},
#' providing some additional arguments.
#'
#' @param mfdobj
#' A multivariate functional data object of class mfd.
#' @param scale
#' If TRUE, it scales data before doing MFPCA
#' using \code{scale_mfd}. Default is TRUE.
#' @param nharm
#' Number of multivariate functional principal components
#' to be calculated. Default is 20.
#'
#' @return
#' Modified \code{pca.fd} object, with
#' multivariate functional principal component scores summed over variables
#' (\code{fda::\link[fda]{pca.fd}} returns an array of scores
#' when providing a multivariate functional data object).
#' Moreover, the multivariate functional principal components
#' given in \code{harmonics}
#' are converted to the \code{mfd} class.
#' @export
#' @seealso \code{\link{scale_mfd}}
#'
#' @examples
#' library(funcharts)
#' mfdobj <- data_sim_mfd()
#' pca_obj <- pca_mfd(mfdobj)
#' plot_pca_mfd(pca_obj)
#'
pca_mfd <- function(mfdobj, scale = TRUE, nharm = 20) {

  obs_names <- mfdobj$fdnames[[2]]
  variables <- mfdobj$fdnames[[3]]

  data_scaled <- scale_mfd(mfdobj, scale = scale)

  data_pca <- if (length(variables) == 1)
    fda::fd(data_scaled$coefs[, , 1],
            data_scaled$basis,
            data_scaled$fdnames) else data_scaled

  nobs <- length(obs_names)
  nvar <- length(variables)
  nbasis <- mfdobj$basis$nbasis
  nharm <- min(nobs - 1, nvar * nbasis, nharm)

  pca <- pca.fd_inprods_faster(data_pca, nharm = nharm, centerfns = FALSE)
  pca$harmonics$fdnames[c(1, 3)] <- mfdobj$fdnames[c(1, 3)]

  pca$pcscores <- if(length(dim(pca$scores)) == 3) {
    apply(pca$scores, 1:2, sum)
  } else pca$scores

  pc_names <- pca$harmonics$fdnames[[2]]
  rownames(pca$scores) <- rownames(pca$pcscores) <- obs_names
  colnames(pca$scores) <- colnames(pca$pcscores) <- pc_names
  if (length(variables) > 1) dimnames(pca$scores)[[3]] <- variables
  pca$data <- mfdobj
  pca$data_scaled <- data_scaled
  pca$scale <- scale
  pca$center_fd <- attr(data_scaled, "scaled:center")
  pca$scale_fd <- if (scale) attr(data_scaled, "scaled:scale") else NULL

  if (length(variables) > 1) {
    coefs <- pca$harmonics$coefs
  } else {
    coefs <- array(pca$harmonics$coefs,
                   dim = c(nrow(pca$harmonics$coefs),
                           ncol(pca$harmonics$coefs),
                           1))
    dimnames(coefs) <- list(
      dimnames(pca$harmonics$coefs)[[1]],
      dimnames(pca$harmonics$coefs)[[2]],
      variables
    )
  }


  if (length(variables) == 1) {
    pca$scores <- array(pca$scores,
                        dim = c(nrow(pca$scores),ncol(pca$scores), 1))
    dimnames(pca$scores) <-
      list(obs_names, pc_names, variables)

  }

  pca$harmonics <- mfd(coefs,
                       pca$harmonics$basis,
                       pca$harmonics$fdnames,
                       B = mfdobj$basis$B)

  class(pca) <- c("pca_mfd", "pca.fd")
  pca

}

#' Predict from a multivariate functional PCA
#'
#' Computes either the scores of new observations on selected principal
#' components, or their reconstruction from the selected components,
#' given a PCA fitted by \code{\link{pca_mfd}}.
#'
#' This function is an S3 method for objects of class \code{"pca_mfd"}.
#' It is usually called via the generic \code{\link{predict}} function.
#'
#' @name predict.pca_mfd
#' @method predict pca_mfd
#'
#' @param object An object of class \code{"pca_mfd"}, typically the output of
#'   \code{\link{pca_mfd}}.
#' @param newdata An object of class \code{"mfd"} containing the new
#'   multivariate functional data to be projected. If \code{NULL}, the training
#'   data used to fit \code{object} are used.
#' @param components Integer vector specifying the indices of the principal
#'   components to use. Defaults to all available components.
#' @param type Character string: either \code{"scores"} (default) to return the
#'   scores of \code{newdata}, or \code{"reconstruction"} to return the data
#'   reconstructed from the selected components.
#' @param ... Further arguments passed to or from other methods (not used).
#'
#' @details
#' The new data are first centered and (optionally) scaled using the functional
#' center and scale stored in the PCA object.
#' * If \code{type = "scores"}, inner products with the selected eigenfunctions
#'   are computed and summed across basis functions.
#' * If \code{type = "reconstruction"}, the predicted functional data are
#'   reconstructed from the scores and harmonics.
#'
#' @return
#' * If \code{type = "scores"}, a numeric matrix of dimension
#'   \eqn{nobs \times length(components)}.
#' * If \code{type = "reconstruction"}, an object of class \code{"mfd"}.
#'
#' @seealso \code{\link{pca_mfd}}, \code{\link{scale_mfd}}
#'
#'
#' @export
predict.pca_mfd <- function(object, newdata = NULL,
                            components = seq_len(ncol(object$pcscores)),
                            type = c("scores", "reconstruction"), ...) {
  type <- match.arg(type)

  # Use training data if newdata is missing or NULL
  if (is.null(newdata)) {
    if (is.null(object$data)) {
      stop("'newdata' is NULL and no training data were stored in the 'pca' object.")
    }
    newdata <- object$data
  }

  # checks
  if (!is.mfd(newdata)) {
    stop("'newdata' must be an object of class 'mfd'.")
  }
  if (!identical(newdata$basis, object$data$basis)) {
    stop("The basis of 'newdata' must be identical to that used in 'object'.")
  }
  if (nvar(newdata) != dim(object$center_fd$coefs)[3]) {
    stop("The number of variables in 'newdata' does not match 'object'.")
  }
  if (any(components < 1 | components > length(object$values))) {
    stop("Invalid 'components' index: must be between 1 and the number of components in 'object'.")
  }

  # center and scale
  newdata_scaled <- scale_mfd(
    newdata,
    center = object$center_fd,
    scale  = if (object$scale) object$scale_fd else FALSE
  )

  # scores
  inprods <- get_pre_scores(object, components, newdata_scaled)
  scores <- apply(inprods, 1:2, sum)

  if (type == "scores") {
    return(scores)
  } else {
    # reconstruction
    yhat <- get_fit_pca_given_scores(scores, object$harmonics[components])
    if (object$scale) {
      yhat <- descale_mfd(yhat, center = object$center_fd, scale = object$scale_fd)
    }
    return(yhat)
  }
}




#' Plot the harmonics of a \code{pca_mfd} object
#'
#' @param pca
#' A fitted multivariate functional principal component analysis
#' (MFPCA) object of class \code{pca_mfd}.
#' @param harm
#' A vector of integers with the harmonics to plot.
#' If 0, all harmonics are plotted. Default is 0.
#' @param scaled
#' If TRUE, eigenfunctions are multiplied by the square root of the
#' corresponding eigenvalues, if FALSE the are not scaled and the
#' all have unit norm.
#' Default is FALSE
#'
#' @return
#' A ggplot of the harmonics/multivariate functional
#' principal components contained in the object \code{pca}.
#' @export
#' @examples
#' library(funcharts)
#' mfdobj <- data_sim_mfd()
#' pca_obj <- pca_mfd(mfdobj)
#' plot_pca_mfd(pca_obj)
#'
plot_pca_mfd <- function(pca, harm = 0, scaled = FALSE) {

  if (harm[1] == 0) harm <- seq_along(pca$harmonics$fdnames[[2]])
  if (scaled) {
    scaled_coefs <- apply(
      pca$harmonics$coefs[, harm, , drop = FALSE],
      3,
      function(x) t(t(x) * sqrt(pca$values[harm])))
    nbasis <- pca$harmonics$basis$nbasis
    nharm <- length(harm)
    nvar <- length(pca$harmonics$fdnames[[3]])
    scaled_coefs <- array(scaled_coefs, dim = c(nbasis, nharm, nvar))
    dimnames(scaled_coefs) <-
      dimnames(pca$harmonics$coefs[, harm, , drop = FALSE])
    pca$harmonics$coefs <- scaled_coefs
  }

  p_functions <- plot_mfd(ggplot2::aes(col = !!dplyr::sym("id")),
                          mfdobj = pca$harmonics[harm])

  components <- which(cumsum(pca$varprop) < .99)
  # p_values <- data.frame(eigenvalues = pca$values[components]) %>%
  #   dplyr::mutate(n_comp = seq_len(dplyr::n())) %>%
  #   ggplot2::ggplot() +
  #   ggplot2::geom_col(ggplot2::aes(n_comp, eigenvalues)) +
  #   ggplot2::theme_bw() +
  #   ggplot2::xlab("Number of components")

  p_functions

}

#' Calculate the scores given a fitted \code{pca_mfd} object
#'
#' This function calculates the
#' multivariate functional principal component scores
#' given a multivariate functional principal component analysis (MFPCA) object.
#' Note that scores are already provided with a fitted \code{pca_mfd} object,
#' but they correspond to the
#' multivariate functional observations used to perform MFPCA,
#' With this function scores corresponding to new
#' multivariate functional data can be calculated.
#'
#' @param pca
#' A fitted multivariate functional principal
#' component analysis (MFPCA) object of class \code{pca_mfd}.
#' @param components
#' A vector of integers with the multivariate
#' functional principal components
#' corresponding to the scores to be calculated.
#' @param newdata
#' An object of class \code{mfd} containing new
#' multivariate functional data for which
#' the scores must be calculated.
#' If NULL, it is set to the data used to get \code{pca}, i.e. \code{pca$data}.
#' Default is NULL.
#'
#' @return
#' a NxM matrix, where
#'  N is the number of replications in newdata and
#'  M is the number of components given in the \code{components} argument.
#' @noRd
#' @seealso \code{\link{get_pre_scores}}
#'
get_scores <- function(pca, components, newdata_scaled = NULL) {

  inprods <- get_pre_scores(pca, components, newdata_scaled)
  apply(inprods, 1:2, sum)

}

#' Calculate the inner products to be summed to get scores given a
#' fitted \code{pca_mfd} object
#'
#' This function calculates inner products needed to calculate
#' the multivariate functional principal component scores
#' given a multivariate functional principal component analysis (MFPCA) object.
#' See details.
#' This function is called by \code{get_scores}.
#'
#' @param pca
#' A fitted MFPCA object of class \code{pca_mfd}.
#' @param components
#' A vector of integers with the components
#' for which to calculate the inner products.
#' @param newdata
#' An object of class \code{mfd} containing
#' new multivariate functional data for which
#' the inner products needed to calculate the scores must be calculated.
#' If NULL, it is set to the data used to get \code{pca}, i.e. \code{pca$data}.
#' Default is NULL.
#'
#' @return
#' A three-dimensional array:
#'
#' * the first dimension is the number of replications in \code{newdata}.
#' * the second dimension is the number of components
#' given in the \code{components} argument.
#' * the third dimension is the number of functional variables.
#'
#' @details
#' The MFPCA scores for a multivariate functional data observation
#' X(t)=(X_1(t),\dots,X_p(t)) in \code{newdata}
#' are calculated as \eqn{<X_1,\psi_1>+\dots+<X_p,\psi_p>}, where
#' <.,.> denotes the inner product in L^2 and
#' \eqn{\psi_m(t)=(\psi_m1(t),\dots,\psi_pm(t))}
#' is the vector of functions of the m-th
#' harmonics/multivariate functional principal component in \code{pca} object.
#' This function calculates the
#' individual inner products \eqn{<X_j,\psi_j>},
#' for all replications in \code{newdata}
#' and all components.
#'
#' @noRd
#' @seealso \code{\link{get_scores}}
#'
get_pre_scores <- function(pca, components, newdata_scaled = NULL) {

  if (missing(components)) {
    stop("components argument must be provided")
  }
  if (is.null(newdata_scaled)) {
    inprods <- pca$scores[, components, , drop = FALSE]
  } else {
    inprods <- inprod_mfd(newdata_scaled, pca$harmonics[components])
  }

  inprods

}

#' Get the projected data onto a
#' multivariate functional principal component subspace.
#'
#' Get the projection of multivariate functional data
#' onto the multivariate functional principal component subspace
#' defined by a subset of functional principal components.
#' The obtained data can be considered a prediction or
#' a finite-dimensional approximation of the original data,
#' with the dimension being equal to the number of selected components.
#'
#' @param pca
#' A fitted MFPCA object of class \code{pca_mfd}.
#' @param components
#' A vector of integers with the components over which
#' to project the data.
#' @param newdata
#' An object of class \code{mfd} containing
#' new multivariate functional data to be projected onto the desired subspace.
#' If NULL, it is set to the data used to get \code{pca}, i.e. \code{pca$data}.
#' Default is NULL.
#'
#' @return
#' An object of class \code{mfd} with the same variables
#' and observations as in \code{newdata},
#' containing the projection of \code{newdata}
#' onto the the multivariate functional principal component subspace
#' defined by the \code{components} argument.
#'
#' @details
#' In case you want to calculate the projection of the data
#' directly from scores already calculated,
#' see \code{\link{get_fit_pca_given_scores}}.
#'
#' @noRd
#'
#' @seealso \code{\link{get_fit_pca_given_scores}}
#'
get_fit_pca <- function(pca, components, newdata_scaled = NULL) {

  scores <- get_scores(pca, components, newdata_scaled)
  get_fit_pca_given_scores(scores, pca$harmonics[components])

}

#' Get the projected data onto the
#' multivariate functional principal component subspace,
#' given scores and harmonics.
#'
#' Get the projection of multivariate functional data
#' onto the multivariate functional principal component subspace
#' defined by a subset of functional principal components.
#' If you want to calculate projected data directly from original data,
#' see \code{\link{get_fit_pca}}.
#' Here you must provide the matrix of scores and the harmonics.
#' This happens for example when the scores are be predicted directly
#' with a regression model.
#' The obtained data can be considered a prediction
#' or a finite-dimensional approximation of the original data,
#' with the dimension being equal to the number of selected components.
#'
#' @param scores
#' A NxM matrix containing the values of the scores, where
#' N is the number of observations for which
#' we want to calculate the projection,
#' M is the number of selected multivariate functional principal components.
#' @param harmonics
#' An object of class \code{mfd}
#' containing the multivariate functional principal components.
#' Its M functional replications must correspond each to the
#' corresponding column of \code{scores}.
#'
#' @return
#' An object of class \code{mfd} with the same functional variables
#' as in \code{harmonics}
#' and the same observations as the rows in \code{scores},
#' containing the projection of \code{newdata} onto the
#' multivariate functional principal component subspace
#' defined by the components given as the argument \code{harmonics}.
#' @noRd
#'
#' @seealso \code{\link{get_fit_pca}}
#'
get_fit_pca_given_scores <- function(scores, harmonics) {

  basis <- harmonics$basis
  nbasis <- basis$nbasis
  basisnames <- basis$names
  variables <- harmonics$fdnames[[3]]
  nvar <- length(variables)
  obs <- rownames(scores)
  nobs <- nrow(scores)

  fit_coefs <- array(NA, dim = c(nbasis, nobs, nvar))
  dimnames(fit_coefs) <- list(basisnames, obs, variables)
  fit_coefs[] <- apply(harmonics$coefs, 3, function(x) x %*% t(scores))

  fdnames <- list(harmonics$fdnames[[1]],
                  obs, variables)

  mfd(fit_coefs, basis, fdnames, B = basis$B)
}

#' Calculate the Hotelling's T2 statistics of multivariate functional data
#'
#' Calculate the Hotelling's T2 statistics of
#' multivariate functional data projected
#' onto a multivariate functional principal component subspace.
#'
#' @param pca
#' A fitted MFPCA object of class \code{pca_mfd}.
#' @param components
#' A vector of integers with the components
#' over which to project the data.
#' @param newdata
#' An object of class \code{mfd} containing
#' new multivariate functional data to be projected onto the desired subspace.
#' If NULL, it is set to the data used to get \code{pca},
#' i.e. \code{pca$data}.
#' Default is NULL.
#'
#' @return
#' A \code{data.frame} with as many rows as the number of
#' functional replications in \code{newdata}.
#' It has one \code{T2} column containing the Hotelling T2
#' statistic calculated for all observations, as well as
#' one column per each functional variable, containing its
#' contribution to the T2 statistic.
#' See Capezza et al. (2020) for definition of contributions.
#' @noRd
#'
#' @references
#' Capezza C, Lepore A, Menafoglio A, Palumbo B, Vantini S. (2020)
#' Control charts for monitoring ship operating conditions and CO2
#' emissions based on scalar-on-function regression.
#' \emph{Applied Stochastic Models in Business and Industry},
#' 36(3):477--500. <doi:10.1002/asmb.2507>
#'
get_T2_spe <- function(pca,
                       components,
                       newdata_scaled = NULL,
                       absolute_error = FALSE) {
  inprods <- get_pre_scores(pca, components, newdata_scaled)
  scores <- apply(inprods, 1:2, sum)
  values <- pca$values[components]
  T2 <- colSums(t(scores^2) / values)
  variables <- dimnames(inprods)[[3]]
  obs <- if (is.null(newdata_scaled)) {
    pca$data$fdnames[[2]]
    } else {
      newdata_scaled$fdnames[[2]]
    }
  contribution <- vapply(variables, function(variable) {
    rowSums(t(t(inprods[, , variable] * scores) / values))
  }, numeric(length(obs)))
  contribution <- matrix(contribution,
                         nrow = length(obs),
                         ncol = length(variables))
  rownames(contribution) <- obs
  contribution <- data.frame(contribution)
  names_contribution <- paste0("contribution_T2_", variables)

  out_T2 <- data.frame(T2 = T2, contribution)
  names(out_T2) <- c("T2", names_contribution)


  fit <- get_fit_pca_given_scores(scores, pca$harmonics[components])

  res_fd <- if (is.null(newdata_scaled)) {
    fda::minus.fd(pca$data_scaled, fit)
  } else {
    fda::minus.fd(newdata_scaled, fit)
  }

  res_fd <- mfd(res_fd$coefs, res_fd$basis, res_fd$fdnames, B = res_fd$basis$B)

  if (!absolute_error) {
    cont_spe <- inprod_mfd_diag(res_fd)
  } else {
    rg <- pca$data$basis$rangeval
    PP <- 200
    xseq <- seq(rg[1], rg[2], l = PP)
    yy <- fda::eval.fd(xseq, res_fd)
    cont_spe <- apply(abs(yy), 2:3, sum) / PP
  }

  rownames(cont_spe) <- obs
  colnames_cont_spe <- paste0("contribution_spe_", variables)
  spe <- rowSums(cont_spe)

  out_spe <- data.frame(spe = spe, cont_spe)
  colnames(out_spe) <- c("spe", colnames_cont_spe)

  cbind(out_T2, out_spe)

}


#' @noRd
#'
pca.fd_inprods_faster <- function (fdobj,
                                   nharm = 2,
                                   harmfdPar = fda::fdPar(fdobj),
                                   centerfns = TRUE) {
  if (!(fda::is.fd(fdobj) || fda::is.fdPar(fdobj)))
    stop("First argument is neither a functional data
         or a functional parameter object.")
  if (fda::is.fdPar(fdobj))
    fdobj <- fdobj$fd

  if (length(dim(fdobj$coefs)) == 3 & dim(fdobj$coefs)[3] == 1) {
    fdobj$coefs <- matrix(fdobj$coefs,
                          nrow = dim(fdobj$coefs)[1],
                          ncol = dim(fdobj$coefs)[2])
  }

  meanfd <- fda::mean.fd(fdobj)
  if (centerfns) {
    fdobj <- fda::center.fd(fdobj)
  }
  coef <- fdobj$coefs
  coefd <- dim(coef)
  ndim <- length(coefd)
  nrep <- coefd[2]
  coefnames <- dimnames(coef)
  if (nrep < 2)
    stop("PCA not possible without replications.")
  basisobj <- fdobj$basis
  nbasis <- basisobj$nbasis
  type <- basisobj$type
  harmbasis <- harmfdPar$fd$basis
  nhbasis <- harmbasis$nbasis
  Lfdobj <- harmfdPar$Lfd
  lambda <- harmfdPar$lambda

  if (ndim == 3) {
    nvar <- coefd[3]
    ctemp <- matrix(0, nvar * nbasis, nrep)
    for (j in seq_len(nvar)) {
      index <- seq_len(nbasis) + (j - 1) * nbasis
      ctemp[index, ] <- coef[, , j]
    }
  }
  else {
    nvar <- 1
    ctemp <- coef
  }
  Lmat <- fda::eval.penalty(harmbasis, 0)
  if (lambda > 0) {
    Rmat <- fda::eval.penalty(harmbasis, Lfdobj)
    Lmat <- Lmat + lambda * Rmat
  }
  Lmat <- (Lmat + t(Lmat))/2
  Mmat <- chol(Lmat)
  Mmatinv <- solve(Mmat)
  Wmat <- crossprod(t(ctemp))/nrep
  if (identical(harmbasis, basisobj)) {
    if (!is.null(basisobj$B)) {
      Jmat <- basisobj$B
    } else {
      if (basisobj$type == "bspline") {
        Jmat <- fda::inprod.bspline(fda::fd(diag(basisobj$nbasis), basisobj))
      }
      if (basisobj$type == "fourier") {
        Jmat <- diag(basisobj$nbasis)
      }
      if (basisobj$type == "const") {
        Jmat <- matrix(diff(harmbasis$rangeval))
      }
      if (basisobj$type == "expon") {
        out_mat <- outer(basisobj$params, basisobj$params, "+")
        exp_out_mat <- exp(out_mat)
        Jmat <- (exp_out_mat ^ basisobj$rangeval[2] -
                   exp_out_mat ^ basisobj$rangeval[1]) / out_mat
        Jmat[out_mat == 0] <- diff(basisobj$rangeval)
      }
      if (basisobj$type == "monom") {
        out_mat <- outer(basisobj$params, basisobj$params, "+") + 1
        Jmat <- (basisobj$rangeval[2] ^ out_mat -
                   basisobj$rangeval[1] ^ out_mat) / out_mat
        Jmat[out_mat == 0] <- diff(basisobj$rangeval)
      }
      if (basisobj$type == "polygonal") {
        Jmat <- inprod_fd(fda::fd(diag(basisobj$nbasis), basisobj),
                          fda::fd(diag(basisobj$nbasis), basisobj))
      }
      if (basisobj$type == "power") {
        out_mat <- outer(basisobj$params, basisobj$params, "+") + 1
        Jmat <- (basisobj$rangeval[2] ^ out_mat -
                   basisobj$rangeval[1] ^ out_mat) / out_mat
        Jmat[out_mat == 0] <-
          log(basisobj$rangeval[2]) - log(basisobj$rangeval[1])
      }
    }
  } else {
    Jmat <- inprod_fd(fda::fd(diag(harmbasis$nbasis), harmbasis),
                      fda::fd(diag(basisobj$nbasis), basisobj))
  }

  MIJW <- crossprod(Mmatinv, Jmat)
  if (nvar == 1) {
    Cmat <- MIJW %*% Wmat %*% t(MIJW)
  }
  else {
    Cmat <- matrix(0, nvar * nhbasis, nvar * nhbasis)
    for (i in seq_len(nvar)) {
      indexi <- seq_len(nbasis) + (i - 1) * nbasis
      for (j in seq_len(nvar)) {
        indexj <- seq_len(nbasis) + (j - 1) * nbasis
        Cmat[indexi, indexj] <- MIJW %*% Wmat[indexi,
                                              indexj] %*% t(MIJW)
      }
    }
  }
  Cmat <- (Cmat + t(Cmat))/2
  result <- eigen(Cmat)
  eigvalc <- result$values
  eigvecc <- as.matrix(result$vectors[, seq_len(nharm)])
  sumvecc <- apply(eigvecc, 2, sum)
  eigvecc[, sumvecc < 0] <- -eigvecc[, sumvecc < 0]
  varprop <- eigvalc[seq_len(nharm)]/sum(eigvalc)
  if (nvar == 1) {
    harmcoef <- Mmatinv %*% eigvecc
  }
  else {
    harmcoef <- array(0, c(nbasis, nharm, nvar))
    for (j in seq_len(nvar)) {
      index <- seq_len(nbasis) + (j - 1) * nbasis
      temp <- eigvecc[index, ]
      harmcoef[, , j] <- Mmatinv %*% temp
    }
  }
  harmnames <- rep("", nharm)
  for (i in seq_len(nharm)) harmnames[i] <- paste("PC", i, sep = "")
  if (length(coefd) == 2)
    harmnames <- list(coefnames[[1]], harmnames, "values")
  if (length(coefd) == 3)
    harmnames <- list(coefnames[[1]], harmnames, coefnames[[3]])
  harmfd <- fda::fd(harmcoef, harmbasis, harmnames)
  if (is.null(fdobj$basis$B)) {
    if (fdobj$basis$type == "bspline") {
      B <- fda::inprod.bspline(fda::fd(diag(fdobj$basis$nbasis), fdobj$basis))
    }
    if (fdobj$basis$type == "fourier") {
      B <- diag(fdobj$basis$nbasis)
    }

  } else {
    B <- fdobj$basis$B
  }

  if (nvar == 1) {
    if (identical(fdobj$basis, harmfd$basis)) {
      harmscr <- t(fdobj$coefs) %*% B %*% harmfd$coefs
    } else harmscr <- inprod_fd(fdobj, harmfd)
  }
  else {
    harmscr <- array(0, c(nrep, nharm, nvar))
    coefarray <- fdobj$coefs
    harmcoefarray <- harmfd$coefs
    for (j in seq_len(nvar)) {
      fdobjj <- fda::fd(as.matrix(coefarray[, , j]), basisobj)
      harmfdj <- fda::fd(as.matrix(harmcoefarray[, , j]), basisobj)
      if (identical(fdobjj$basis, harmfdj$basis)) {
        harmscr[, , j] <- t(fdobjj$coefs) %*% B %*% harmfdj$coefs
      } else harmscr[, , j] <- inprod_fd(fdobjj, harmfdj)
    }
  }
  pcafd <- list(harmfd, eigvalc, harmscr, varprop, meanfd)
  class(pcafd) <- "pca.fd"
  names(pcafd) <- c("harmonics", "values", "scores", "varprop",
                    "meanfd")
  return(pcafd)
}

