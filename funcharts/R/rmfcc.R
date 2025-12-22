# RoMFPCA functions -------------------------------------------------------
#' Robust multivariate functional principal components analysis
#'
#' It performs robust MFPCA as described in Capezza et al. (2024).
#'
#' @param mfdobj A multivariate functional data object of class mfd.
#' @param center If TRUE, it centers the data before doing MFPCA with respect
#' to the functional mean of the input data.
#' If \code{"fusem"}, it uses the functional M-estimator of location proposed
#' by Centofanti et al. (2023) to center the data.
#' Default is \code{"fusem"}.
#' @param scale If \code{"funmad"}, it scales the data before doing MFPCA using
#' the functional normalized median absolute deviation estimator
#' proposed by Centofanti et al. (2023).
#' If TRUE, it scales data using \code{scale_mfd}.
#' Default is \code{"funmad"}.
#' @param nharm Number of multivariate functional principal components
#' to be calculated. Default is 20.
#' @param method
#' If \code{"ROBPCA"}, MFPCA uses ROBPCA of Hubert et al. (2005),
#' as described in Capezza et al. (2024).
#' If \code{"Locantore"}, MFPCA uses the Spherical Principal Components
#' procedure proposed by Locantore et al. (1999).
#' If \code{"Proj"}, MFPCA uses the Robust Principal Components based on
#' Projection Pursuit algorithm of Croux and Ruiz-Gazen (2005).
#' method If \code{"normal"}, it uses \code{pca_mfd} on \code{mfdobj}.
#' Default is \code{"ROBPCA"}.
#' @param alpha This parameter measures the fraction of outliers the algorithm
#' should resist and is used only if \code{method} is \code{"ROBPCA"}.
#' Default is 0.8.
#'
#' @return An object of \code{pca_mfd} class, as returned by the \code{pca_mfd}
#' function when performing non robust multivariate
#' functional principal component analysis.
#' @export
#'
#'
#' @references
#'
#' Capezza, C., Centofanti, F., Lepore, A., Palumbo, B. (2024)
#' Robust Multivariate Functional Control Chart.
#' \emph{Technometrics}, 66(4):531--547, <doi:10.1080/00401706.2024.2327346>.
#'
#' Centofanti, F., Colosimo, B.M., Grasso, M.L., Menafoglio, A., Palumbo, B.,
#' Vantini, S. (2023)
#' Robust functional ANOVA with application to additive manufacturing.
#' \emph{Journal of the Royal Statistical Society Series C: Applied Statistics}
#' 72(5), 1210–1234 <doi:10.1093/jrsssc/qlad074>
#'
#' Croux, C., Ruiz-Gazen, A. (2005).
#' High breakdown estimators for principal components: The projection-pursuit
#' approach revisited.
#' \emph{Journal of Multivariate Analysis}, 95, 206–226,
#' <doi:10.1016/j.jmva.2004.08.002>.
#'
#' Hubert, M., Rousseeuw, P.J., Branden, K.V. (2005)
#' ROBPCA: A New Approach to Robust Principal Component Analysis,
#' \emph{Technometrics} 47(1), 64--79, <doi:10.1198/004017004000000563>
#'
#' Locantore, N., Marron, J., Simpson, D., Tripoli, N., Zhang, J., Cohen K.,
#' K. (1999),
#' Robust principal components for functional data.
#' \emph{Test}, 8, 1-28. <doi:10.1007/BF02595862>
#'
#' @examples
#' library(funcharts)
#' dat <- simulate_mfd(nobs = 20, p = 1, correlation_type_x = "Bessel")
#' mfdobj <- get_mfd_list(dat$X_list, n_basis = 5)
#'
#' # contaminate first observation
#' mfdobj$coefs[, 1, ] <- mfdobj$coefs[, 1, ] + 0.05
#'
#' # plot_mfd(mfdobj) # plot functions to see the outlier
#' # pca <- pca_mfd(mfdobj) # non robust MFPCA
#' rpca <- rpca_mfd(mfdobj) # robust MFPCA
#' # plot_pca_mfd(pca, harm = 1) # plot first eigenfunction, affected by outlier
#' # plot_pca_mfd(rpca, harm = 1) # plot first eigenfunction in robust case
#'
rpca_mfd <- function(mfdobj,
                     center = "fusem", # TRUE, or fusem
                     scale = "funmad", # TRUE, FALSE, or funmad
                     nharm = 20,
                     method = "ROBPCA",
                     alpha = 0.8) {

  if (!method %in% c("normal", "ROBPCA", "Locantore", "Proj")) {
    stop("method must be \"normal\", \"ROBPCA\", \"Locantore\", or \"Proj\"")
  }

  obs_names <- mfdobj$fdnames[[2]]
  variables <- mfdobj$fdnames[[3]]
  nobs <- length(obs_names)
  nvar <- length(variables)
  basis <- mfdobj$basis
  nbasis <- basis$nbasis
  if (center == "fusem") {
    mu_fd_coef <- array(0, c(nbasis, 1, nvar))
    for (j in 1:nvar) {
      X_fd <- fda::fd(mfdobj$coefs[, , j], basis)
      X_fdata <- fda.usc::fdata(X_fd)
      mu_fdata <- rofanova::fusem(X_fdata)$mu
      mu_fd <- fda.usc::fdata2fd(mu_fdata, nbasis = nbasis)
      mu_fd_coef[, , j] <- mu_fd$coefs
    }
    center <- fda::fd(mu_fd_coef, basis)
  }
  if (scale == "funmad") {
    sd_fd_coef <- matrix(0, nbasis, nvar)
    for (j in 1:nvar) {
      X_fd <- fda::fd(mfdobj$coefs[, , j], basis)
      X_fdata <- fda.usc::fdata(X_fd)
      sd_fdata <- rofanova::funmad(X_fdata)
      sd_fd <- fda.usc::fdata2fd(sd_fdata, nbasis = nbasis)
      sd_fd_eval <- fda::eval.fd(seq(basis$rangeval[1],
                                     basis$rangeval[2],
                                     l = 200),
                                 sd_fd)
      lambda <- -10
      while (min(sd_fd_eval) < 1e-5) {
        lambda <- lambda + 1
        sd_fd <- fda.usc::fdata2fd(sd_fdata,
                                   nbasis = nbasis,
                                   lambda = 10^lambda)
        sd_fd_eval <- fda::eval.fd(seq(basis$rangeval[1],
                                       basis$rangeval[2], l = 200),
                                   sd_fd)
      }
      sd_fd_coef[, j] <- sd_fd$coefs
    }
    scale <- fda::fd(sd_fd_coef, basis)
  }

  data_scaled <- scale_mfd(mfdobj, center = center, scale = scale)
  center <- attributes(data_scaled)$"scaled:center"
  scale <- attributes(data_scaled)$"scaled:scale"

  # if (method == "normal") {
  #   pca <- pca_mfd(mfdobj, scale = scale, nharm = nharm)
  #   return(pca)
  # }


  data_pca <- if (length(variables) == 1) {
    fda::fd(data_scaled$coefs[, , 1], data_scaled$basis, data_scaled$fdnames)
  } else{
    data_scaled
  }

  nharm <- min(nobs - 1, nvar * nbasis, nharm)
  pca <- rpca.fd(data_pca,
                 nharm = nharm,
                 method = method,
                 alpha = alpha)

  if (is.null(scale)) {
    scale <- FALSE
    if (is.null(center)) {
      pca$meanfd <- pca$meanfd
    } else {
      pca$meanfd <- pca$meanfd + center
    }
  } else {
    if (is.null(center)) {
      pca$meanfd <- descale_mfd(pca$meanfd, center = FALSE, scale)
    } else {
      pca$meanfd <- descale_mfd(pca$meanfd, center = center, scale)
    }
  }
  data_scaled <- scale_mfd(mfdobj, center = pca$meanfd, scale = scale)

  pca$harmonics$fdnames[c(1, 3)] <- mfdobj$fdnames[c(1, 3)]
  pca$pcscores <- if (length(dim(pca$scores)) == 3) {
    apply(pca$scores, 1:2, sum)
  } else {
    pca$scores
  }
  pc_names <- pca$harmonics$fdnames[[2]]
  rownames(pca$scores) <- rownames(pca$pcscores) <- obs_names
  colnames(pca$scores) <- colnames(pca$pcscores) <- pc_names
  if (length(variables) > 1) {
    dimnames(pca$scores)[[3]] <- variables
  }
  pca$data <- mfdobj
  pca$data_scaled <- data_scaled
  pca$scale <- scale
  pca$center_fd <- attr(data_scaled, "scaled:center")
  if ((is.logical(scale))) {
    if (scale)
      pca$scale_fd <- attr(data_scaled, "scaled:scale")
  }
  else if (fda::is.fd(scale)) {
    pca$scale_fd <- attr(data_scaled, "scaled:scale")
  }
  else
    NULL
  if (length(variables) > 1) {
    coefs <- pca$harmonics$coefs
  }
  else {
    coefs <- array(pca$harmonics$coefs, dim = c(nrow(pca$harmonics$coefs),
                                                ncol(pca$harmonics$coefs), 1))
    dimnames(coefs) <- list(dimnames(pca$harmonics$coefs)[[1]],
                            dimnames(pca$harmonics$coefs)[[2]],
                            variables)
  }
  if (length(variables) == 1) {
    pca$scores <- array(pca$scores, dim = c(nrow(pca$scores),
                                            ncol(pca$scores), 1))
    dimnames(pca$scores) <- list(obs_names, pc_names, variables)
  }
  pca$harmonics <- mfd(coefs,
                       pca$harmonics$basis,
                       pca$harmonics$fdnames,
                       B = pca$harmonics$basis$B)

  pca$sd_fd <- if (is.logical(scale)) fda::sd.fd(mfdobj)
  class(pca) <- c("pca_mfd", "pca.fd")
  pca
}

rpca.fd <- function(fdobj,
                    nharm = 2,
                    harmfdPar = fda::fdPar(fdobj),
                    method = "normal",
                    alpha = 0.8) {

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
  coef <- fdobj$coefs
  coefd <- dim(coef)
  ndim <- length(coefd)
  nrep <- coefd[2]
  coefnames <- dimnames(coef)
  if (nrep < 2) stop("PCA not possible without replications.")
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
    for (j in 1:nvar) {
      index <- 1:nbasis + (j - 1) * nbasis
      ctemp[index, ] <- coef[, , j]
    }
  } else {
    nvar <- 1
    ctemp <- coef
  }
  Lmat <- fda::eval.penalty(harmbasis, 0)
  if(is.null(harmbasis$B)) harmbasis$B <- Lmat
  if(is.null(basisobj$B)) basisobj$B <- fdobj$basis$B <- Lmat
  if (lambda > 0) {
    Rmat <- fda::eval.penalty(harmbasis, Lfdobj)
    Lmat <- Lmat + lambda * Rmat
  }
  Lmat <- (Lmat + t(Lmat))/2
  Mmat <- chol(Lmat)
  Mmatinv <- solve(Mmat)
  Wmat <- crossprod(t(ctemp))/nrep
  if (identical(harmbasis, basisobj)) {
    bs_fd <- fda::fd(coef = diag(1, harmbasis$nbasis), basisobj = harmbasis)
    Jmat <- t(bs_fd$coef) %*% as.matrix(harmbasis$B) %*% bs_fd$coef
  }
  else Jmat <- inprod_fd(harmbasis, basisobj)
  MIJW <- crossprod(Mmatinv, Jmat)

  if (nvar == 1) {
    data_new <- MIJW %*% ctemp
  } else {
    data_new <- matrix(0, nvar * nhbasis, nrep)
    for (i in 1:nvar) {
      indexi <- 1:nbasis + (i - 1) * nbasis
      data_new[indexi, ] <- MIJW %*% ctemp[indexi,]
    }
  }

  nharm <- min(nharm, nrep - 1, nbasis * nvar)
  values <- svd(data_new)$d^2
  nharm <- min(nharm, sum(values > sqrt(.Machine$double.eps)))

  if (method == "normal") {
    results <- list()
    X_cen <- scale(t(as.matrix(data_new)), scale = FALSE)
    mean_sc <- apply(t(as.matrix(data_new)), 2, mean)
    mod_pca <- rrcov::PcaClassic(X_cen, k = nharm, kmax = nharm)
    results$vectors <- mod_pca$loadings
    results$values <- mod_pca$eigenvalues
    results$values <- results$values
    coeff_mean <- array(0,c(nbasis,1,nvar))
    for (i in 1:nvar) {
      indexi <- 1:nbasis + (i - 1) * nbasis
      coeff_mean[, ,i] <- solve(MIJW) %*% mean_sc[indexi]
    }

    mean_fd <- mfd(coeff_mean,basisobj, B = basisobj$B)
  }
  else if (method == "ROBPCA") {
    results <- list()
    mod_pca <- rrcov::PcaHubert(t(as.matrix(data_new)),k=nharm,kmax=nharm,
                                alpha = alpha,
                                mcd = FALSE)
    results$vectors <- mod_pca$loadings
    results$values <- mod_pca$eigenvalues
    coeff_mean <- array(0,c(nbasis,1,nvar))
    for (i in 1:nvar) {
      indexi <- 1:nbasis + (i - 1) * nbasis
      coeff_mean[, ,i] <- solve(MIJW) %*% mod_pca$center[indexi]
    }

    mean_fd <- mfd(coeff_mean,basisobj, B = basisobj$B)

  }
  else if(method == "Locantore") {
    results <- list()
    if (ncol(data_new) <= nrow(data_new)) {
      suppressWarnings(mod_pca <- rrcov::PcaLocantore(t(as.matrix(data_new)),
                                                      k = nharm,
                                                      kmax = min(nharm, 200)))
    } else {
      mod_pca <- rrcov::PcaLocantore(t(as.matrix(data_new)),
                                     k = nharm,
                                     kmax = min(nharm, 200))
    }
    results$vectors <- mod_pca$loadings
    results$values <- Rfast::colMads(mod_pca$scores)^2
    coeff_mean <- array(0,c(nbasis,1,nvar))
    for (i in 1:nvar) {
      indexi <- 1:nbasis + (i - 1) * nbasis
      coeff_mean[, ,i] <- solve(MIJW) %*% mod_pca$center[indexi]
    }

    mean_fd <- mfd(coeff_mean,basisobj, B = basisobj$B)

  }
  else if(method == "Proj") {
    results <- list()
    mod_pca <- rrcov::PcaProj(t(as.matrix(data_new)),k=nharm,kmax=200)
    results$vectors <- mod_pca$loadings
    results$values <- mod_pca$eigenvalues
    coeff_mean <- array(0,c(nbasis,1,nvar))
    for (i in 1:nvar) {
      indexi <- 1:nbasis + (i - 1) * nbasis
      coeff_mean[, ,i] <- solve(MIJW) %*% mod_pca$center[indexi]
    }

    mean_fd <- mfd(coeff_mean,basisobj, B = basisobj$B)
  }
  if (!is.mfd(fdobj)) {
    mfdobj_temp <- funcharts::get_mfd_fd(fdobj)
  } else {
    mfdobj_temp <- fdobj
  }
  fdobj_cen <- scale_mfd(mfdobj_temp, center = mean_fd, scale = FALSE)
  nharm <- dim(mod_pca$loadings)[2]
  eigvalc <- results$values
  eigvecc <- as.matrix(results$vectors[, 1:nharm])
  sumvecc <- apply(eigvecc, 2, sum)
  eigvecc[, sumvecc < 0] <- -eigvecc[, sumvecc < 0]
  varprop <- eigvalc[1:nharm] / sum(eigvalc)
  if (nvar == 1) {
    harmcoef <- Mmatinv %*% eigvecc
  }
  else {
    harmcoef <- array(0, c(nbasis, nharm, nvar))
    for (j in 1:nvar) {
      index <- 1:nbasis + (j - 1) * nbasis
      temp <- eigvecc[index, ]
      harmcoef[, , j] <- Mmatinv %*% temp
    }
  }
  harmnames <- rep("", nharm)
  for (i in 1:nharm) harmnames[i] <- paste("PC", i, sep = "")
  if (length(coefd) == 2)
    harmnames <- list(coefnames[[1]], harmnames, "values")
  if (length(coefd) == 3)
    harmnames <- list(coefnames[[1]], harmnames, coefnames[[3]])
  harmfd <- fda::fd(harmcoef, basisobj, harmnames)
  if (nvar == 1) {
    if (identical(fdobj$basis, harmfd$basis)) {
      bs <- fdobj$basis
      harmscr <- t(fdobj_cen$coefs[,,1]) %*% as.matrix(bs$B) %*% harmfd$coefs
    } else harmscr <- inprod_fd(fdobj_cen, harmfd)
  } else {
    harmscr <- array(0, c(nrep, nharm, nvar))
    coefarray <- fdobj_cen$coefs
    harmcoefarray <- harmfd$coefs
    for (j in 1:nvar) {
      fdobjj <- fda::fd(as.matrix(coefarray[, , j]), basisobj)
      harmfdj <- fda::fd(as.matrix(harmcoefarray[, , j]), basisobj)
      if (identical(fdobjj$basis, harmfdj$basis)) {
        bs <- fdobjj$basis
        harmscr[, , j] <- t(fdobjj$coefs) %*% as.matrix(bs$B) %*% harmfdj$coefs
      }
      else harmscr[, , j] <- inprod_fd(fdobjj, harmfdj)
    }
  }
  pcafd <- list(harmfd, eigvalc, harmscr, varprop, mean_fd)
  class(pcafd) <- "pca.fd"
  names(pcafd) <- c("harmonics",
                    "values",
                    "scores",
                    "varprop",
                    "meanfd")
  return(pcafd)
}



# Functional filter ---------------------------------------------
#' Finds functional componentwise outliers
#'
#' It finds functional componentwise outliers
#' as described in Capezza et al. (2024).
#'
#' @param mfdobj A multivariate functional data object of class mfd.
#' @param method_pca The method used in \code{rpca_mfd} to perform
#' robust multivariate functional principal component analysis (RoMFPCA).
#' See \code{\link{rpca_mfd}}.
#' @param alpha
#' Probability value such that only values of functional distances greater than
#' the \code{alpha}-quantile of the Chi-squared
#' distribution, with a number of degrees of freedom equal to the number
#' of principal components selected by \code{fev}, are considered
#' to determine the proportion of flagged componentwise outliers.
#' Default value is 0.95, as recommended by Agostinelli et al. (2015).
#' See Capezza et al. (2024) for more details.
#' @param fev Number between 0 and 1 denoting the fraction
#' of variability that must be explained by the
#' principal components to be selected to calculate functional distances after
#' applying RoMFPCA on \code{mfdobj}. Default is 0.999.
#' @param delta Number between 0 and 1 denoting the parameter of the
#' Binomial distribution whose \code{alpha_binom}-quantile
#' determines the threshold
#' used in the bivariate filter.
#' Given the i-th observation and the j-th functional variable,
#' the number of pairs flagged as functional componentwise outliers in
#' the i-th observation where the component (i, j) is involved
#' is compared against this threshold to identify additional functional
#' componentwise outliers to the ones found by the univariate filter.
#' Default is 0.1, recommended as conservative choice by Leung et al. (2017).
#' See Capezza et al. (2024) for more details.
#' @param alpha_binom Probability value such that the \code{alpha}-quantile
#' of the Binomial distribution is considered as threshold
#' in the bivariate filter. See \code{delta} and Capezza et al. (2024)
#' for more details. Default value is 0.99.
#' @param bivariate If TRUE, both univariate and bivariate filters
#' are applied. If FALSE, only the univariate filter is used.
#' Default is TRUE.
#' @param max_proportion_componentwise
#' If the functional filter identifies a proportion of functional
#' componentwise outliers larger than \code{max_proportion_componentwise},
#' for a given observation, then it is considered as a functional casewise
#' outlier. Default value is 0.5.
#'
#' @return A list with two elements.
#' The first element is an \code{mfd} object containing
#' the original observation in the \code{mfdobj} input, but where
#' the basis coefficients of the components identified as functional
#' componentwise outliers are replaced by NA.
#' The second element of the list is a list of numbers, with length equal
#' to the number of functional variables in \code{mfdobj}.
#' Each element of this list contains the observations of the flagged
#' functional componentwise outliers for the corresponding functional variable.
#' @export
#'
#' @references
#'
#' Agostinelli, C., Leung, A., Yohai, V. J., and Zamar, R. H. (2015).
#' Robust estimation of
#' multivariate location and scatter in the presence of componentwise and
#' casewise contamination.
#' \emph{Test}, 24(3):441–461.
#'
#' Capezza, C., Centofanti, F., Lepore, A., Palumbo, B. (2024)
#' Robust Multivariate Functional Control Chart.
#' \emph{Technometrics}, 66(4):531--547, <doi:10.1080/00401706.2024.2327346>.
#'
#' Leung, A., Yohai, V., and Zamar, R. (2017).
#' Multivariate location and scatter matrix
#' estimation under componentwise and casewise contamination.
#' \emph{Computational Statistics & Data Analysis}, 111:59–76.
#'
#' @examples
#' \donttest{
#' library(funcharts)
#' mfdobj <- get_mfd_list(air, grid = 1:24, n_basis = 13, lambda = 1e-2)
#' plot_mfd(mfdobj)
#' out <- functional_filter(mfdobj, bivariate = FALSE)
#' }
#'
functional_filter <- function(mfdobj,
                              method_pca = "ROBPCA",
                              alpha = 0.95,
                              fev = 0.999,
                              delta = 0.10,
                              alpha_binom = 0.99,
                              bivariate = TRUE,
                              max_proportion_componentwise = 0.5) {

  n <- ind <- NULL

  nvar <- dim(mfdobj$coefs)[3]

  ind_out_fil_univariate <- filter_univariate(mfdobj = mfdobj,
                                              method_pca = method_pca,
                                              alpha = alpha,
                                              fev = fev)
  if (bivariate) {
    ind_out_fil_bivariate <- filter_bivariate(
      mfdobj = mfdobj,
      ind_out_fil = ind_out_fil_univariate,
      method_pca = method_pca,
      alpha = alpha,
      fev = fev,
      delta = delta,
      alpha_binom = alpha_binom
    )

    ind_out_fil <- list()
    for (jj in 1:nvar) {
      ind_out_fil[[jj]] <- sort(unique(c(ind_out_fil_univariate[[jj]],
                                         ind_out_fil_bivariate[[jj]])))
    }
  } else {
    ind_out_fil <- ind_out_fil_univariate
  }
  out_remove <- ind_out_fil %>%
    unlist() %>%
    as.data.frame() %>%
    stats::setNames("ind") %>%
    dplyr::count(ind) %>%
    dplyr::filter(n > nvar * max_proportion_componentwise) %>%
    dplyr::pull(ind)
  X_mfdm <- mfdobj
  for (ii in 1:nvar) {
    X_mfdm$coefs[, ind_out_fil[[ii]], ii] <- NA
  }
  if (length(out_remove) > 0) {
    X_mfdm <- X_mfdm[-out_remove]
  }

  out <- list(mfdobj = X_mfdm,
              flagged_outliers = ind_out_fil)

  return(out)

  # variables <- X_mfdm$fdnames[[3]]
  # nvar <- dim(X_mfdm$coefs)[3]
  # nobs <- dim(X_mfdm$coefs)[2]
  # df_outliers <- lapply(1:nvar, function(jj) {
  #   data.frame(is_out = 1:nobs %in% out$flagged_outliers[[jj]],
  #              var = variables[jj],
  #              id = X_mfdm$fdnames[[2]])
  # }) %>%
  #   dplyr::bind_rows() %>%
  #   dplyr::mutate(is_out = factor(is_out))
  #
  # plot_mfd(X_mfdm,
  #          data = df_outliers,
  #          mapping = ggplot2::aes(col = is_out)) &
  # ggplot2::scale_colour_manual(values = c("TRUE" = "tomato1",
  #                                         "FALSE" = "black"),
  #                              drop = FALSE) &
  # ggplot2::scale_size_manual(values = c("TRUE" = 2, "FALSE" = 0.25),
  #                            drop = FALSE)

}


filter_univariate <- function(mfdobj,
                              method_pca = "ROBPCA",
                              alpha = 0.95,
                              fev = 0.999) {

  nvar <- dim(mfdobj$coefs)[3]
  obs_out_list <- list()
  nobs <- dim(mfdobj$coefs)[2]
  nb <- mfdobj$basis$nbasis
  nvars <- dim(mfdobj$coefs)[3]
  nharm <- min(nobs - 1, nb)

  obs_out_list <- lapply(1:nvar, function(jj) {

    xj <- mfdobj[,jj]
    mod_pca <- rpca_mfd(mfdobj = xj,
                        method = method_pca,
                        center = "fusem",
                        scale = "funmad",
                        nharm = nharm)
    cum_var <- cumsum(mod_pca$values / sum(mod_pca$values))
    K <- sum(cum_var < fev) + 1
    K <- min(K, length(mod_pca$values))

    T2 <- colSums(t(mod_pca$scores[,1:K,1]^2) / mod_pca$values[1:K])
    filtered <- univ_fil_gse(T2, alpha, K)
    which(is.na(filtered))

  })
  obs_out_list
}


filter_bivariate <- function(mfdobj,
                             ind_out_fil,
                             method_pca = "ROBPCA",
                             alpha = 0.85,
                             fev = 0.999,
                             delta = 0.10,
                             alpha_binom = 0.99) {

  jj1 <- jj2 <- NULL

  nvar <- dim(mfdobj$coefs)[3]
  if (nvar == 1) {
    warning("bivariate_filter does not apply to a single functional variable.")
    return(list(integer()))
  }
  obs_out_pairs <- list()
  nobs <- dim(mfdobj$coefs)[2]
  nb <- mfdobj$basis$nbasis
  nharm <- min(nobs - 1, nb)
  count <- 0
  for (jj in 1:(nvar-1)) {
    for (jj2 in (jj+1):nvar) {
      count <- count + 1
      xj <- mfdobj[, c(jj,jj2)]
      mod_pca <- rpca_mfd(mfdobj = xj,
                          method = method_pca,
                          center = "fusem",
                          scale = "funmad",
                          nharm = 20)
      cum_var <- cumsum(mod_pca$values / sum(mod_pca$values))
      K <- sum(cum_var < fev) + 1

      T2 <- colSums(t(mod_pca$pcscores[, 1:K]^2) / mod_pca$values[1:K])
      filtered <- univ_fil_gse(T2, alpha, K)

      which_fil <- which(is.na(filtered))
      obs_out_pairs[[count]] <- which_fil
      if (length(which_fil) > 0) {
        df_fil <- data.frame(filtered = which_fil)
        df_fil$jj1 <- jj
        df_fil$jj2 <- jj2
        obs_out_pairs[[count]] <- df_fil
      } else {
        obs_out_pairs[[count]] <- NULL
      }

    }
  }
  obs_out_bivariate_df <- do.call(rbind, obs_out_pairs)
  obs_out_univariate_df <- dplyr::bind_rows(lapply(1:nvar, function(ii) {
    if (length(ind_out_fil[[ii]]) > 0) {
      out <- data.frame(filtered = ind_out_fil[[ii]], jj = ii)
    } else {
      out <- data.frame()
    }
  }))

  mm <- list()
  flagged <- list()
  for (ii in 1:nobs) {
    m_ii <- rep(NA, nvar)
    flagged_ii <- rep(FALSE, nvar)

    which_jj_univariate <- obs_out_univariate_df %>%
      dplyr::filter(filtered == ii) %>%
      dplyr::pull(jj)
    which_jj_unflagged <- setdiff(1:nvar, which_jj_univariate)

    obs_ii <- obs_out_bivariate_df %>%
      dplyr::filter(filtered == ii) %>%
      dplyr::filter((jj1 %in% which_jj_unflagged) &
                      (jj2 %in% which_jj_unflagged))

    for (kk in which_jj_unflagged) {
      m_ii[kk] <- obs_ii %>%
        dplyr::filter(jj1 == kk | jj2 == kk) %>%
        nrow()
      c_ii_jj <- stats::qbinom(alpha_binom,
                               size = length(which_jj_unflagged) - 1,
                               prob = delta)
      if (m_ii[kk] > c_ii_jj) {
        flagged_ii[kk] <- TRUE
      }
    }
    mm[[ii]] <- m_ii
    flagged[[ii]] <- flagged_ii
  }
  mm <- do.call(rbind, mm)
  flagged <- do.call(rbind, flagged)
  obs_out_list <- list()
  for (jj in 1:nvar) {
    obs_out_list[[jj]] <- which(flagged[, jj])
  }
  obs_out_list
}


univ_fil_gse <- function(v, alpha, df) {
  n <- length(v)
  id <- (1:n)[!is.na(v)]
  v.out <- rep(NA, n)
  v <- stats::na.omit(v)
  n <- length(v)
  v.order <- order(v)
  v <- sort(v)
  i0 <- which(v < stats::qchisq(alpha, df))
  n0 <- 0
  if (length(i0) > 0) {
    i0 <- rev(i0)[1]
    dn <- max(pmax(stats::pchisq(v[i0:n], df) - (i0:n - 1)/n, 0))
    n0 <- round(dn * n)
  }
  v <- v[order(v.order)]
  v.na <- v
  if (n0 > 0)
    v.na[v.order[(n - n0 + 1):n]] <- NA
  v.out[id] <- v.na
  return(v.out)
}





# RoMFDI functions --------------------------------------------------------
#' Robust Multivariate Functional Data Imputation (RoMFDI)
#'
#' It performs Robust Multivariate Functional Data Imputation (RoMFDI)
#' as in Capezza et al. (2024).
#'
#' @param mfdobj
#' A multivariate functional data object of class mfd.
#' @param update
#' The RoMFDI performs sequential imputation of missing functional
#' components.
#' If TRUE, Robust Multivariate Functional
#' Principal Component Analysis (RoMFPCA) \code{niter_update} is updated times
#' during the algorithm.
#' If FALSE, the RoMFPCA used for imputation is always the same, i.e.,
#' the one performed on the original data sets containing only
#' the observations with no missing functional components.
#' Default is TRUE.
#' @param method_pca
#' The method used in \code{rpca_mfd} to perform
#' robust multivariate functional principal component analysis (RoMFPCA).
#' See \code{\link{rpca_mfd}}.
#' Default is \code{"ROBPCA"}.
#' @param n_dataset
#' To take into account the increased noise due to single imputation,
#' the proposed RoMFDI allows multiple imputation.
#' Due to the presence of the stochastic component in the imputation,
#' it is worth explicitly noting that the imputed data set
#' is not deterministically assigned.
#' Therefore, by performing several times the RoMFDI in the
#' imputation step of the
#' RoMFCC implementation, the corresponding multiple estimated
#' RoMFPCA models could
#' be combined by averaging the robustly estimated covariance functions,
#' thus performing a
#' multiple imputation strategy as suggested by Van Ginkel et al. (2007).
#' Default is 3.
#' @param niter_update
#' The number of times the RoMFPCA is updated during the algorithm.
#' It applies only if update is TRUE. Default value is 10.
#' @param fev
#' Number between 0 and 1 denoting the proportion
#' of variability that must be explained by the
#' principal components to be selected for dimension reduction after
#' applying RoMFPCA on the observed components to impute the missing ones.
#' Default is 0.999.
#' @param alpha This parameter measures the fraction of outliers the
#' RoMFPCA algorithm should resist and is used only
#' if \code{method_pca} is \code{"ROBPCA"}.
#' Default is 0.8.
#'
#' @return
#' A list with \code{n_dataset} elements.
#' Each element is an \code{mfd} object containing \code{mfdobj} with
#' stochastic imputation of the missing components.
#' @export
#' @references
#'
#' Capezza, C., Centofanti, F., Lepore, A., Palumbo, B. (2024)
#' Robust Multivariate Functional Control Chart.
#' \emph{Technometrics}, 66(4):531--547, <doi:10.1080/00401706.2024.2327346>.
#'
#' Van Ginkel, J. R., Van der Ark, L. A., Sijtsma, K., and Vermunt, J. K.
#' (2007). Two-way
#' imputation: a Bayesian method for estimating missing scores in tests
#' and questionnaires, and an accurate approximation.
#' \emph{Computational Statistics & Data Analysis}, 51(8):4013–-4027.
#'
#'
#' @examples
#' \donttest{
#' library(funcharts)
#' mfdobj <- get_mfd_list(air[1:3], grid = 1:24, n_basis = 13, lambda = 1e-2)
#' out <- functional_filter(mfdobj, bivariate = FALSE)
#' mfdobj_imp <- RoMFDI(out$mfdobj, n_dataset = 1, update = FALSE)
#' }
#'
RoMFDI <- function(mfdobj,
                   method_pca = "ROBPCA",
                   fev = 0.999,
                   n_dataset = 3,
                   update = TRUE,
                   niter_update = 10,
                   alpha = 0.8) {

  if (!anyNA(mfdobj$coefs)) {
    return(list(mfdobj))
  }

  basis <- mfdobj$basis
  nvar <- dim(mfdobj$coefs)[3]
  n <- dim(mfdobj$coefs)[2]
  nbasis <- basis$nbasis
  nharm <- min(nbasis*nvar, n-1)
  coefdd <- mfdobj$coefs
  ctempdd <- matrix(0, nvar * nbasis, n)
  for (j in 1:nvar) {
    index <- 1:nbasis + (j - 1) * nbasis
    ctempdd[index, ] <- coefdd[, , j]
  }
  x <- t(ctempdd)

  n <- nrow(x)
  p <- ncol(x)
  isnanx <- is.na(x) + 0
  risnanx <- apply(isnanx, 1, sum)

  sortx <- sort.int(risnanx, index.return = TRUE)
  sorth <- sort.int(sortx$ix, index.return = TRUE)
  mfdobj <- mfdobj[sortx$ix, ]
  x <- x[sortx$ix, ]
  isnanx <- is.na(x) + 0
  risnanx <- sortx$x
  complobs <- which(risnanx == 0)
  ncomplobs <- length(complobs)
  misobs <- which(risnanx != 0)
  nmisobs <- length(misobs)

  X_mfd_comp <- mfdobj[complobs]

  mod_pca_com <- rpca_mfd(X_mfd_comp,
                          center = "fusem",
                          scale = "funmad",
                          nharm = nharm,
                          method = method_pca,
                          alpha = alpha)

  cum_var <- cumsum(mod_pca_com$values / sum(mod_pca_com$values))
  K <- min(sum(cum_var < fev) + 1, nharm, length(mod_pca_com$values))
  loadings <- mod_pca_com$harmonics[1:K]
  values <- mod_pca_com$values[1:K]

  W_new <- lapply(1:nvar, function(ii) {
    basis$B %*% loadings$coefs[, , ii]
  })
  W_new <- do.call(rbind, W_new)
  icovx <- diag(1/values)
  icovxmul <- W_new %*% icovx %*% t(W_new)

  X_mfd_comp_sc <- scale_mfd(X_mfd_comp, mod_pca_com$meanfd, mod_pca_com$scale)
  x_coef <- lapply(1:nvar, function(ii) X_mfd_comp_sc$coefs[, , ii])
  x_coef_big <- do.call(rbind, x_coef)


  mvar_mat <- sapply(1:nvar, function(ii) {
    index <- 1:nbasis + (ii - 1) * nbasis
    rowSums(isnanx[misobs, index, drop = FALSE])
  })
  mvar_mat <- matrix(mvar_mat, ncol = nvar)
  miss_var <- apply(mvar_mat, 1, function(x) which(x > 0))
  miss_var_new <- unique(lapply(miss_var, sort))

  res_list <- cov_res <- list()

  for (jj in seq_along(miss_var_new)) {
    mvar_jj <- miss_var_new[[jj]]

    index <- as.numeric(sapply(mvar_jj,function(x) 1:nbasis + (x - 1) * nbasis))

    xo <- x_coef_big[-index, ]
    x_m <- x_coef_big[index, ]
    icovxmul <- W_new %*% icovx %*% t(W_new)
    x_new <- -(MASS::ginv(icovxmul[index, index, drop = FALSE]) %*%
                 icovxmul[index, -index, drop = FALSE]) %*% as.matrix(xo)

    x_new_com <- x_coef_big
    x_new_com[index, ] <- x_new

    if (K < nharm - 5) {
      for (ii in 1:dim(x_new_com)[2]) {
        coef_mat <- matrix(x_new_com[, ii], nrow = nbasis, ncol=nvar)
        scores_new <- rowSums(sapply(1:nvar, function(kk) {
          t(coef_mat[, kk]) %*% basis$B %*% loadings$coefs[, , kk]
          }))
        fd_new <- get_fit_pca_given_scores(t(scores_new), loadings[1:K])
        x_new_com[index, ii] <- fd_new$coefs[, 1, mvar_jj]
      }
    }

    res_list[[jj]]<-x_m-x_new_com[index,]
  }

  # cov_res <- parallel::mclapply(seq_along(miss_var_new), function(jj) {
  #   mod_cov <- rrcov::CovRobust(t(res_list[[jj]]))
  #   mod_cov$cov
  #
  # }, mc.cores = ncores)
  cov_res <- lapply(seq_along(miss_var_new), function(jj) {
    mod_cov <- rrcov::CovRobust(t(res_list[[jj]]))
    mod_cov$cov
  })

  X_mfdimp <- list()
  if (length(misobs) < niter_update) {
    niter_update <- length(misobs)
  }
  iter_update <- as.integer(length(misobs) / niter_update)

  X_mfd_scaled0 <- scale_mfd(mfdobj[misobs],
                             mod_pca_com$meanfd,
                             mod_pca_com$scale)

  for (D in 1:n_dataset) {
    complobs_i <- complobs
    ncomplobs_i <- length(complobs_i)
    X_mfd_comp <- mfdobj[complobs_i]
    X_mfd_scaled <- X_mfd_scaled0

    coef_list <- list()
    imputed_obs <- numeric()
    for (inn in seq_along(misobs)) {
      mvar <- as.logical(isnanx[misobs[inn], ])
      x_mfd_i <- X_mfd_scaled[inn]
      x_i <- as.numeric(sapply(1:nvar, function(ii) x_mfd_i$coefs[, , ii]))
      xo <- x_i[!mvar]
      mvar_mat <- colSums(matrix(mvar, nbasis, nvar))
      miss_var <- which(mvar_mat > 0)
      ind_cov <- which(sapply(miss_var_new,
                              function(iii) identical(iii, miss_var)))
      new_cov_err <- cov_res[[ind_cov]]


      x_new <- -as.numeric((MASS::ginv(icovxmul[mvar, mvar]) %*%
                              icovxmul[mvar, !mvar]) %*% as.matrix(xo))
      x_new_com <- numeric(length(mvar))
      x_new_com[mvar] <- x_new
      x_new_com[!mvar] <- xo

      coef_mat <- matrix(x_new_com,nrow = nbasis,ncol=nvar)
      scores_new <- rowSums(sapply(1:nvar, function(kkk) {
        t(coef_mat[,kkk]) %*% basis$B %*% loadings$coefs[, , kkk]
        }))
      fd_new <- get_fit_pca_given_scores(t(scores_new), loadings[1:K])
      x_new_com <- as.numeric(fd_new$coefs)

      x_new_com[mvar] <- x_new_com[mvar] +
        MASS::mvrnorm(1,  rep(0, length(x_new)), new_cov_err)

      coef_list[[inn]] <- matrix(x_new_com, nrow = nbasis, ncol = nvar)

      complobs_i <- c(complobs_i, misobs[inn])
      ncomplobs_i <- length(complobs_i)
      if (update & ((inn %% iter_update == 0) | (inn == length(misobs)))) {

        obs_to_impute <- setdiff(seq_along(coef_list), imputed_obs)
        coeff_i <- simplify2array(coef_list[obs_to_impute])
        coeff_i <- aperm(coeff_i, c(1, 3, 2))
        X_mfd_i <- mfd(coeff_i, basis, B = basis$B)
        X_mfd_unc <- descale_mfd(X_mfd_i,
                                 center = mod_pca_com$meanfd,
                                 scale = mod_pca_com$scale_fd)
        coef_new <- array(0,
                          c(nbasis,
                            dim(X_mfd_comp$coefs)[2] + dim(X_mfd_unc$coefs)[2],
                            nvar))
        for (j in 1:nvar) {
          coef_new[, , j] <- cbind(X_mfd_comp$coefs[, , j],
                                   X_mfd_unc$coefs[, , j])
        }
        coef_temp <- mfdobj$coefs[, 1:dim(coef_new)[2], ]
        coef_new[!is.na(coef_temp)] <- coef_temp[!is.na(coef_temp)]
        X_mfd_comp <- mfd(coef_new, basis, B = basis$B)
        imputed_obs <- c(imputed_obs, obs_to_impute)

        if (inn < length(misobs)) {
          mod_pca_com <- rpca_mfd(X_mfd_comp,
                                  center = "fusem",
                                  scale = "funmad",
                                  nharm = nharm,
                                  method = method_pca)
          cum_var <- cumsum(mod_pca_com$values / sum(mod_pca_com$values))
          K <- min(sum(cum_var < fev) + 1,
                   nharm,
                   length(mod_pca_com$values))
          loadings <- mod_pca_com$harmonics[1:K]
          values <- mod_pca_com$values[1:K]
          X_mfd_scaled <- scale_mfd(mfdobj[misobs],
                                    mod_pca_com$meanfd,
                                    mod_pca_com$scale)
          W_new <- lapply(1:nvar, function(ii) {
            as.matrix(basis$B) %*% loadings$coefs[, , ii]
          })
          W_new <- do.call(rbind, W_new)
          icovx <- diag(1/values)
          icovxmul <- W_new %*% icovx %*% t(W_new)
        }
      }
    }

    if (!update) {
      coeff_i <- simplify2array(coef_list)
      coeff_i <- aperm(coeff_i, c(1, 3, 2))
      X_mfd_i <- mfd(coeff_i, basis, B = basis$B)
      X_mfd_unc <- descale_mfd(X_mfd_i,
                               center = mod_pca_com$meanfd,
                               scale = mod_pca_com$scale_fd)
      coef_new <- array(0, c(nbasis,
                             dim(X_mfd_comp$coefs)[2] + dim(X_mfd_unc$coefs)[2],
                             nvar))
      for (j in 1:nvar) {
        coef_new[,,j] <- cbind(X_mfd_comp$coefs[, , j], X_mfd_unc$coefs[, , j])
      }
      coef_new[!is.na(mfdobj$coefs)] <- mfdobj$coefs[!is.na(mfdobj$coefs)]
      X_mfd_comp <- mfd(coef_new, basis, B = basis$B)
    }

    X_mfdimp[[D]] <- X_mfd_comp[sorth$ix, ]
  }
  return(X_mfdimp)
}


# RoMFCC functions ----------------------------------------------------------
#' Robust Multivariate Functional Control Charts - Phase I
#'
#' It performs Phase I of the Robust Multivariate Functional Control Chart
#' (RoMFCC) as proposed by Capezza et al. (2024).
#' @param mfdobj
#' A multivariate functional data object of class mfd.
#' A functional filter is applied to this data set, then
#' flagged functional componentwise outliers are imputed in the
#' robust imputation step.
#' Finally robust multivariate functional principal component analysis
#' is applied to the imputed data set for dimension reduction.
#' @param mfdobj_tuning
#' An additional functional data object of class mfd.
#' After applying the filter and imputation steps on this data set,
#' it is used to robustly estimate the distribution of the Hotelling's T2 and
#' SPE statistics in order to calculate control limits
#' to prevent overfitting issues that could reduce the
#' monitoring performance of the RoMFCC.
#' Default is NULL, but it is strongly recommended to use a tuning data set.
#' @param functional_filter_par
#' A list with an argument \code{filter} that can be TRUE or FALSE depending
#' on if the functional filter step must be performed or not.
#' All the other arguments of this list are passed as arguments to the function
#' \code{functional_filter} in the filtering step.
#' All the arguments that are not passed take their default values.
#' See \code{\link{functional_filter}} for all the arguments and their default
#' values.
#' Default is \code{list(filter = TRUE)}.
#' @param imputation_par
#' A list with an argument \code{method_imputation}
#' that can be \code{"RoMFDI"} or \code{"mean"} depending
#' on if the imputation step must be done by means of \code{\link{RoMFDI}} or
#' by just using the mean of each functional variable.
#' If \code{method_imputation = "RoMFDI"},
#' all the other arguments of this list are passed as arguments to the function
#' \code{RoMFDI} in the imputation step.
#' All the arguments that are not passed take their default values.
#' See \code{\link{RoMFDI}} for all the arguments and their default
#' values.
#' Default value is \code{list(method_imputation = "RoMFDI")}.
#' @param pca_par
#' A list with an argument \code{fev}, indicating a number between 0 and 1
#' denoting the fraction of variability that must be explained by the
#' principal components to be selected in the RoMFPCA step.
#' All the other arguments of this list are passed as arguments to the function
#' \code{rpca_mfd} in the RoMFPCA step.
#' All the arguments that are not passed take their default values.
#' See \code{\link{rpca_mfd}} for all the arguments and their default
#' values.
#' Default value is \code{list(fev = 0.7)}.
#' @param alpha
#' The overall nominal type-I error probability used to set
#' control chart limits.
#' Default value is 0.05.
#' @param verbose
#' If TRUE, it prints messages about the steps of the algorithm.
#' Default is FALSE.
#'
#' @return
#' A list of the following elements that are needed in Phase II:
#'
#' * \code{T2} the Hotelling's T2 statistic values for the Phase I data set,
#'
#' * \code{SPE} the SPE statistic values for the Phase I data set,
#'
#' * \code{T2_tun} the Hotelling's T2 statistic values for the tuning data set,
#'
#' * \code{SPE_tun} the SPE statistic values for the tuning data set,
#'
#' * \code{T2_lim} the Phase II control limit of
#' the Hotelling's T2 control chart,
#'
#' * \code{spe_lim} the Phase II control limit of
#' the SPE control chart,
#'
#' * \code{tuning} TRUE if the tuning data set is provided, FALSE otherwise,
#'
#' * \code{mod_pca} the final RoMFPCA model fitted on the Phase I data set,
#'
#' * \code{K} = K the number of selected principal components,
#'
#' * \code{T_T2_inv} if a tuning data set is provided,
#' it returns the inverse of the covariance matrix
#' of the first \code{K} scores, needed to calculate the Hotelling's T2
#' statistic for the Phase II observations.
#'
#' * \code{mean_scores_tuning_rob_mean} if a tuning data set is provided,
#' it returns the robust location estimate of the scores, needed to calculate
#' the Hotelling's T2 and SPE
#' statistics for the Phase II observations.
#'
#' @export
#'
#' @references
#'
#' Capezza, C., Centofanti, F., Lepore, A., Palumbo, B. (2024)
#' Robust Multivariate Functional Control Chart.
#' \emph{Technometrics}, 66(4):531--547, <doi:10.1080/00401706.2024.2327346>.
#'
#' @examples
#' \donttest{
#' library(funcharts)
#' mfdobj <- get_mfd_list(air, n_basis = 5)
#' nobs <- dim(mfdobj$coefs)[2]
#' set.seed(0)
#' ids <- sample(1:nobs)
#' mfdobj1 <- mfdobj[ids[1:100]]
#' mfdobj_tuning <- mfdobj[ids[101:300]]
#' mfdobj2 <- mfdobj[ids[-(1:300)]]
#' mod_phase1 <- RoMFCC_PhaseI(mfdobj = mfdobj1,
#'                             mfdobj_tuning = mfdobj_tuning,
#'                             functional_filter_par = list(filter = FALSE))
#' phase2 <- RoMFCC_PhaseII(mfdobj_new = mfdobj2,
#'                          mod_phase1 = mod_phase1)
#' plot_control_charts(phase2)
#' }
#'
RoMFCC_PhaseI <- function(mfdobj,
                          mfdobj_tuning = NULL,
                          functional_filter_par = list(filter = TRUE),
                          imputation_par = list(method_imputation = "RoMFDI"),
                          pca_par = list(fev = 0.7),
                          alpha = 0.05,
                          verbose = FALSE) {

  # filter default arguments
  if (!is.list(functional_filter_par)) {
    stop("functional_filter_par must be a list.")
  }
  if (is.null(functional_filter_par$filter)) {
    functional_filter_par$filter <- TRUE
  }

  # imputation default arguments
  if (!is.list(imputation_par)) {
    stop("imputation_par must be a list.")
  }
  if (is.null(imputation_par$method_imputation)) {
    imputation_par$method_imputation <- "RoMFDI"
  }

  # RoMFPCA default arguments
  if (!is.list(pca_par)) {
    stop("pca_par must be a list.")
  }
  if (is.null(pca_par$fev)) {
    pca_par$fev <- 0.7
  }

  nvar <- dim(mfdobj$coefs)[3]
  nobs <- dim(mfdobj$coefs)[2]

  # Functional filter
  if (functional_filter_par$filter) {
    if (verbose) {
      print("Training set: functional filtering...")
    }

    functional_filter_default <- formals(functional_filter)
    functional_filter_args <- functional_filter_par
    functional_filter_args$filter <- NULL
    functional_filter_args <- c(
      functional_filter_args,
      functional_filter_default[!(names(functional_filter_default) %in%
                                    names(functional_filter_par))])
    functional_filter_args$mfdobj <- mfdobj

    filter_out <- do.call(functional_filter, functional_filter_args)

    ind_out_fil <- filter_out$flagged_outliers
    X_mfdm <- filter_out$mfdobj

  } else {
    ind_out_fil <- NULL
    X_mfdm <- mfdobj
  }

  # Imputation
  if (anyNA(X_mfdm$coefs)) {
    if (verbose) {
      print("Training set: imputation step...")
    }

    if (!(imputation_par$method_imputation %in% c("RoMFDI", "mean"))) {
      stop("method_imputation must be either \"RoMFDI\" or \"mean\"")
    }

    if (imputation_par$method_imputation == "RoMFDI") {
      imputation_default <- formals(RoMFDI)
      imputation_args <- imputation_par
      imputation_args$method_imputation <- NULL
      imputation_args <- c(
        imputation_args,
        imputation_default[!(names(imputation_default) %in%
                               names(imputation_par))])
      imputation_args$mfdobj <- X_mfdm

      X_imp <- do.call(RoMFDI, imputation_args)
    }
    if (imputation_par$method_imputation == "mean") {
      X_imp <- X_mfdm
      for (j in 1:nvar) {
        which_na <- sort(unique(which(is.na(X_imp$coefs[, , j]),
                                      arr.ind = TRUE)[, 2]))
        which_ok <- setdiff(1:dim(X_imp$coefs)[2], which_na)
        X_fd <- fda::fd(X_imp$coefs[, which_ok, j], X_imp$basis)
        X_fdata <- fda.usc::fdata(X_fd)
        mu_fdata <- rofanova::fusem(X_fdata)$mu
        mu_fd <- fda.usc::fdata2fd(mu_fdata, nbasis = X_mfdm$basis$nbasis)
        X_imp$coefs[, which_na, j] <- mu_fd$coefs
      }
      X_imp <- list(X_imp)
    }
  } else {
    X_imp <- list(X_mfdm)
  }

  n_dataset <- length(X_imp)

  # Robust PCA
  if (verbose) {
    print("Training set: dimension reduction step...")
  }
  pca_default <- formals(rpca_mfd)
  pca_args <- pca_par
  pca_args$fev <- NULL
  pca_args <- c(
    pca_args,
    pca_default[!(names(pca_default) %in%
                           names(pca_par))])

  T2_lim_d <- spe_lim_d <- numeric()
  mod_pca_list <- corr_coef_list <- list()
  for (D in 1:n_dataset) {
    X_imp_d <- X_imp[[D]]
    nobs <- dim(X_imp_d$coefs)[2]
    nb <- X_imp_d$basis$nbasis
    nvars <- dim(X_imp_d$coefs)[3]
    nharm <- min(nobs - 1, nb * nvars)

    pca_args$mfdobj <- X_imp_d
    pca_args$nharm <- nharm
    mod_pca <- do.call(rpca_mfd, pca_args)

    cum_var <- cumsum(mod_pca$values / sum(mod_pca$values))
    mod_pca_list[[D]] <- mod_pca

    Gx <- mod_pca$harmonics$coefs
    Gx <- do.call(rbind, lapply(1:nvars, function(jj) Gx[, , jj]))
    corr_coef <- Gx %*% diag(mod_pca$values) %*% t(Gx)
    corr_coef_list[[D]] <- corr_coef

  }

  corr_coef_mean <- apply(simplify2array(corr_coef_list), 1:2, mean)
  # W <- as.matrix(Matrix::bdiag(rep(list(mod_pca$harmonics$basis$B), nvars)))
  W <- kronecker(diag(nvars), mod_pca$harmonics$basis$B)
  W_sqrt <- chol(W)
  W_sqrt_inv <- solve(W_sqrt)
  VCOV <- W_sqrt %*% corr_coef_mean %*% t(W_sqrt)
  e <- eigen(VCOV)
  loadings_coef <- W_sqrt_inv %*% e$vectors
  loadings_list <- list()
  for (jj in 1:nvars) {
    index <- 1:nb + (jj - 1) * nb
    loadings_list[[jj]] <- loadings_coef[index, 1:nharm]
  }
  loadings_coef_array <- simplify2array(loadings_list)
  loadings <- mfd(loadings_coef_array,
                  mod_pca$harmonics$basis,
                  B = mod_pca$harmonics$basis$B)
  values <- e$values[1:nharm]
  meanfd <- mfd(Reduce("+", lapply(mod_pca_list, function(x) {
    x$meanfd$coefs
  })) / n_dataset,
  mod_pca$harmonics$basis)
  scale_fd <- fda::fd(Reduce("+",
                             lapply(mod_pca_list, function(x) {
                               x$scale_fd$coefs
                               })) / n_dataset,
                      mod_pca$harmonics$basis)
  varprop <- values / sum(values)
  mod_pca_final <- list(harmonics = loadings,
                        values = values,
                        meanfd = meanfd,
                        scale_fd = scale_fd,
                        varprop = varprop)

  if (verbose) {
    print("Training set: calculating monitoring statistics...")
  }
  # T2 SPE statistics
  K <- which(cumsum(mod_pca_final$varprop) >= pca_par$fev)[1]

  alpha_sid<-1-(1-alpha)^(1/2)
  T2_lim <- stats::qf(1 - alpha_sid, K, nobs - K) *
    (K * (nobs- 1 ) * (nobs + 1)) /
    ((nobs - K) * nobs)
  values_spe <- values[(K+1):length(values)]
  teta_1 <- sum(values_spe)
  teta_2 <- sum(values_spe^2)
  teta_3 <- sum(values_spe^3)
  h0 <- 1 - (2 * teta_1 * teta_3) / (3 * teta_2^2)

  c_alpha <- sign(h0) * abs(stats::qnorm(alpha_sid))
  spe_lim <- teta_1 * (((c_alpha * sqrt(2 * teta_2 * h0^2)) / teta_1) +
                        1 +
                        (teta_2 * h0 * (h0 - 1)) / teta_1^2)^(1 / h0)



  mod_T2_spe_all <- get_T2_spe(
    pca = mod_pca_final,
    components = 1:K,
    newdata_scaled = scale_mfd(mfdobj,
                               center = mod_pca_final$meanfd,
                               scale = mod_pca_final$scale_fd)
  )
  T2_all <- mod_T2_spe_all$T2
  SPE_all <- mod_T2_spe_all$spe

  T2_tun <- NULL
  SPE_tun <- NULL
  T_T2_inv <- NULL
  mean_scores_tuning_rob_mean <- NULL

  tuning <- FALSE
  if (!is.null(mfdobj_tuning)) {

    tuning <- TRUE

    nobs_tuning <- dim(mfdobj_tuning$coefs)[2]

    # Functional filter
    if (verbose) {
      print("Tuning set: functional filtering...")
    }
    if (functional_filter_par$filter) {

      functional_filter_args$mfdobj <- mfdobj_tuning
      filter_out_tuning <- do.call(functional_filter, functional_filter_args)

      ind_out_fil_tuning <- filter_out_tuning$flagged_outliers
      X_mfd_tuning_m <- filter_out_tuning$mfdobj

    } else {
      ind_out_fil_tuning <- NULL
      X_mfd_tuning_m <- mfdobj_tuning
    }


    # Imputation
    if (anyNA(X_mfd_tuning_m$coefs)) {
      if (verbose) {
        print("Tuning set: imputation step...")
      }

      if (imputation_par$method_imputation == "RoMFDI") {
        imputation_args$mfdobj <- rbind_mfd(X_imp[[1]], X_mfd_tuning_m)
        X_imp_tuning <- do.call(RoMFDI, imputation_args)
        for (D in 1:length(X_imp_tuning)) {
          X_imp_tuning[[D]] <- X_imp_tuning[[D]][-(1:nobs)]
        }
      }
      if (imputation_par$method_imputation == "mean") {
        X_imp_tuning <- X_mfd_tuning_m
        for (j in 1:nvar) {
          which_na <- sort(unique(which(is.na(X_imp_tuning$coefs[, , j]),
                                        arr.ind = TRUE)[, 2]))
          which_ok <- setdiff(1:dim(X_imp_tuning$coefs)[2], which_na)
          X_fd <- fda::fd(X_imp_tuning$coefs[, which_ok, j], X_imp_tuning$basis)
          X_fdata <- fda.usc::fdata(X_fd)
          mu_fdata <- rofanova::fusem(X_fdata)$mu
          mu_fd <- fda.usc::fdata2fd(mu_fdata, nbasis = X_mfd_tuning_m$basis$nbasis)
          X_imp_tuning$coefs[, which_na, j] <- mu_fd$coefs
        }
        X_imp_tuning <- list(X_imp_tuning)
        n_dataset <- 1
      }
    } else {
      X_imp_tuning <- list(X_mfd_tuning_m)
    }

    n_dataset <- length(X_imp_tuning)

    mean_scores_tuning_rob <- list()
    S_scores_tuning_rob <- list()
    scores_tuning <- list()
    S_scores_tuning_rob2 <- list()
    for (D in seq_along(X_imp_tuning)) {
      X_imp_tuning_scaled <- scale_mfd(X_imp_tuning[[D]],
                                       center = mod_pca_final$meanfd,
                                       scale = mod_pca_final$scale_fd)
      scores_tuning[[D]] <- get_scores(
        mod_pca_final,
        components = 1:dim(mod_pca_final$harmonics$coefs)[2],
        newdata_scaled = X_imp_tuning_scaled
      )

      mod <- rrcov::CovMcd(scores_tuning[[D]][,1:K], alpha = 0.8)
      mod2 <- rrcov::CovMcd(scores_tuning[[D]][,-(1:K)], alpha = 0.8)

      mean_scores_tuning_rob[[D]] <- c(mod$center, mod2$center)
      S_scores_tuning_rob[[D]] <- mod$cov
      S_scores_tuning_rob2[[D]] <- mod2$cov

    }

    if (verbose) {
      print("Tuning set: calculating monitoring statistics and control chart limits...")
    }

    mean_scores_tuning_rob <- do.call(cbind, mean_scores_tuning_rob)
    mean_scores_tuning_rob_mean <- rowMeans(mean_scores_tuning_rob)
    S_scores_tuning_rob <- simplify2array(S_scores_tuning_rob)
    T_mean <- apply(S_scores_tuning_rob, 1:2, mean)

    T_T2 <- T_mean
    T_T2_inv <- solve(T_T2)
    S_scores_tuning_rob2 <- simplify2array(S_scores_tuning_rob2)
    T_mean <- apply(S_scores_tuning_rob2, 1:2, mean)
    T_SPE <- T_mean

    scores_tuning_cen <- t(t(scores_tuning[[D]][, 1:K]) -
                             mean_scores_tuning_rob_mean[1:K])
    T2_tun <- rowSums((scores_tuning_cen %*% T_T2_inv) * scores_tuning_cen)

    alpha_sid<-1-(1-alpha)^(1/2)
    ntun <- length(T2_tun)
    T2_lim <- stats::qf(1 - alpha_sid, K, ntun - K) *
      (K * (ntun- 1 ) * (ntun + 1)) /
      ((ntun - K) * ntun)


    scores_tun_SPE <- t(t(scores_tuning[[D]][, -(1:K)]) -
                          mean_scores_tuning_rob_mean[-(1:K)])
    SPE_tun <- rowSums(scores_tun_SPE^2)

    e <- eigen(T_SPE)
    values_spe <- e$values

    teta_1 <- sum(values_spe)
    teta_2 <- sum(values_spe^2)
    teta_3 <- sum(values_spe^3)
    h0 <- 1 - (2 * teta_1 * teta_3) / (3 * teta_2^2)

    c_alpha <- sign(h0) * abs(stats::qnorm(alpha_sid))
    spe_lim <- teta_1 * (((c_alpha * sqrt(2 * teta_2 * h0^2)) / teta_1) +
                          1 +
                          (teta_2 * h0 * (h0 - 1)) / teta_1^2)^(1 / h0)

  }

  out <- list(T2 = T2_all,
              SPE = SPE_all,
              T2_tun = T2_tun,
              SPE_tun = SPE_tun,
              T2_lim = T2_lim,
              spe_lim = spe_lim,
              tuning = tuning,
              mod_pca = mod_pca_final,
              K = K,
              T_T2_inv = T_T2_inv,
              mean_scores_tuning_rob_mean = mean_scores_tuning_rob_mean)

  return(out)
}





#' Robust Multivariate Functional Control Charts - Phase II
#'
#' It calculates the Hotelling's and SPE monitoring statistics
#' needed to plot the Robust Multivariate Functional Control Chart in Phase II.
#'
#' @param mfdobj_new
#' A multivariate functional data object of class mfd, containing the
#' Phase II observations to be monitored.
#' @param mod_phase1
#' Output obtained by applying the function \code{RoMFCC_PhaseI}
#' to perform Phase I. See \code{\link{RoMFCC_PhaseI}}.
#' @return
#' A \code{data.frame} with as many rows as the number of
#' multivariate functional observations in the phase II data set and
#' the following columns:
#'
#' * one \code{id} column identifying the multivariate functional observation
#' in the phase II data set,
#'
#' * one \code{T2} column containing the Hotelling T2 statistic
#' calculated for all observations,
#'
#' * one column per each functional variable,
#' containing its contribution to the T2 statistic,
#'
#' * one \code{spe} column containing the SPE statistic calculated
#' for all observations,
#'
#' * \code{T2_lim} gives the upper control limit of
#' the Hotelling's T2 control chart,
#'
#' * \code{spe_lim} gives the upper control limit of the SPE control chart
#'
#' @export
#'
#' @references
#' Capezza, C., Centofanti, F., Lepore, A., Palumbo, B. (2024)
#' Robust Multivariate Functional Control Chart.
#' \emph{Technometrics}, 66(4):531--547, <doi:10.1080/00401706.2024.2327346>.
#'
#' @examples
#' \donttest{
#' library(funcharts)
#' mfdobj <- get_mfd_list(air, n_basis = 5)
#' nobs <- dim(mfdobj$coefs)[2]
#' set.seed(0)
#' ids <- sample(1:nobs)
#' mfdobj1 <- mfdobj[ids[1:100]]
#' mfdobj_tuning <- mfdobj[ids[101:300]]
#' mfdobj2 <- mfdobj[ids[-(1:300)]]
#' mod_phase1 <- RoMFCC_PhaseI(mfdobj = mfdobj1,
#'                             mfdobj_tuning = mfdobj_tuning,
#'                             functional_filter_par = list(filter = FALSE))
#' phase2 <- RoMFCC_PhaseII(mfdobj_new = mfdobj2,
#'                          mod_phase1 = mod_phase1)
#' plot_control_charts(phase2)
#' }
#'
RoMFCC_PhaseII <- function(mfdobj_new,
                           mod_phase1) {

  mod_pca <- mod_phase1$mod_pca
  K <- mod_phase1$K
  mfdobj_new_std <- scale_mfd(mfdobj_new,
                              center = mod_pca$meanfd,
                              scale = mod_pca$scale_fd)

  mod_T2_spe <- get_T2_spe(mod_pca, 1:K, mfdobj_new_std)
  T2 <- mod_T2_spe$T2
  SPE <- mod_T2_spe$spe

  scores_new <- get_scores(mod_pca,
                           components = 1:dim(mod_pca$harmonics$coefs)[2],
                           newdata_scaled = mfdobj_new_std)

  if (mod_phase1$tuning) {
    scores_new_cen <- t(t(scores_new[, 1:K]) -
                          mod_phase1$mean_scores_tuning_rob_mean[1:K])
    T2 <- rowSums((scores_new_cen %*% mod_phase1$T_T2_inv) * scores_new_cen)
    scores_new_cen_SPE <- t(t(scores_new[, -(1:K)]) -
                              mod_phase1$mean_scores_tuning_rob_mean[-(1:K)])
    SPE <- rowSums(scores_new_cen_SPE^2)
  }
  frac_out <- mean(T2 > mod_phase1$T2_lim | SPE > mod_phase1$spe_lim)
  ARL <- 1 / frac_out

  out <- data.frame(id = mfdobj_new$fdnames[[2]],
                    T2 = T2,
                    spe = SPE,
                    T2_lim = mod_phase1$T2_lim,
                    spe_lim = mod_phase1$spe_lim)

  return(out)
}



