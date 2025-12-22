#' Calculate limits of T2 and SPE control charts on
#' multivariate functional data
#'
#' Calculate limits of Hotelling T2 and
#' squared prediction error (SPE) control charts
#' on multivariate functional data.
#' A training data set has already been used to fit a \code{pca_mfd} object.
#' A tuning data set can be provided that is used to estimate the
#' control chart limits.
#'
#' @param pca
#' An object of class \code{pca_mfd} obtained by doing
#' multivariate functional principal component analysis (MFPCA)
#' on the training data set of functional covariates.
#' @param tuning_data
#' An object of class \code{mfd} containing
#' the tuning data set of the functional covariates observations,
#' used to estimate the control chart limits.
#' If NULL, the training data, i.e. the data used to fit the MFPCA model,
#' are also used as the tuning data set, i.e. \code{tuning_data=pca$data}.
#' Default is NULL.
#' @param components
#' Set of multivariate functional principal components
#' retained into the MFPCA model.
#' These components are used to calculate the projected observations and
#' the Hotelling's T2 statistic,
#' while the difference between the original functional data and the
#' projected ones (based on the
#' selected components) is used to calculate the SPE statistic.
#' @param alpha
#' A named list with two elements, names \code{T2} and \code{spe},
#' respectively, each containing
#' the desired Type I error probability of the corresponding control chart.
#' Note that at the moment you have to take into account manually
#' the family-wise error rate and adjust
#' the two values accordingly. See Capezza et al. (2020) and
#' Centofanti et al. (2021)
#' for additional details. Default value is
#' \code{list(T2 = 0.025, spe = 0.025)}.
#'
#' @return
#' A \code{data.frame} with one single row,
#' returning the limits of the monitoring statistics:
#'
#' * \code{T2_lim} gives the upper control limit of the
#' Hotelling's T2 control chart,
#'
#' * one \code{contribution_T2_*_lim} column per each
#' functional variable giving the
#' limits of the contribution of that variable to
#' the Hotelling's T2 statistic,
#'
#' * \code{spe_lim} gives the upper control limit of the SPE control chart
#'
#' * one \code{contribution_spe*_lim} column per each
#' functional variable giving the
#' limits of the contribution of that variable to the SPE statistic.
#'
#' @noRd
#'
#' @references
#' Capezza C, Lepore A, Menafoglio A, Palumbo B, Vantini S. (2020)
#' Control charts for
#' monitoring ship operating conditions and CO2
#' emissions based on scalar-on-function regression.
#' \emph{Applied Stochastic Models in Business and Industry},
#' 36(3):477--500.
#' <doi:10.1002/asmb.2507>
#'
#' Centofanti F, Lepore A, Menafoglio A, Palumbo B, Vantini S. (2021)
#' Functional Regression Control Chart.
#' \emph{Technometrics}, 63(3):281--294. <doi:10.1080/00401706.2020.1753581>
calculate_limits <- function(pca,
                             tuning_data = NULL,
                             components,
                             alpha = list(T2 = .025, spe = .025),
                             absolute_error = FALSE) {

  if (!is.list(pca)) {
    stop("pca must be a list produced by pca_mfd.")
  }

  if (!is.null(tuning_data)) {
    if (!is.mfd(tuning_data)) {
      stop("tuning_data must be an object from mfd class.")
    }
    if (dim(tuning_data$coefs)[3] != dim(pca$data$coefs)[3]) {
      stop(paste0("tuning_data must have the same number of variables ",
                  "as training data."))
    }
  }

  if (!is.null(tuning_data)) {
    tuning_data <-
      scale_mfd(tuning_data,
                center = pca$center_fd,
                scale = if (pca$scale) pca$scale_fd else FALSE)
  }

  T2_spe <- get_T2_spe(pca,
                       components,
                       newdata_scaled = tuning_data,
                       absolute_error = absolute_error)
  T2 <- dplyr::select(T2_spe, "T2", dplyr::contains("contribution_T2",
                                                    ignore.case = FALSE))
  spe <- dplyr::select(T2_spe, "spe", dplyr::contains("contribution_spe",
                                                      ignore.case = FALSE))

  obs <- if (!is.null(tuning_data)) {
    tuning_data$fdnames[[2]]
  } else {
    pca$data$fdnames[[2]]
  }

  id <- data.frame(id = obs)
  T2_lim <- apply(T2, 2, function(x) stats::quantile(x, 1 - alpha$T2))
  spe_lim <- apply(spe, 2, function(x) stats::quantile(x, 1 - alpha$spe))

  # kde_quantile <- function(x, prob = 0.95, n = 512) {
  #   d <- density(x, n = n)
  #   cdf <- cumsum(d$y) / sum(d$y)  # Approximate CDF
  #   idx <- which.min(abs(cdf - prob))
  #   return(d$x[idx])
  # }
  # T2_lim <- apply(T2, 2, function(x) kde_quantile(x, prob = 1 - alpha$T2))
  # spe_lim <- apply(spe, 2, function(x) kde_quantile(x, prob = 1 - alpha$spe))


  T2_lim <- as.data.frame(t(T2_lim))
  spe_lim <- as.data.frame(t(spe_lim))

  names(T2_lim) <- paste0(names(T2_lim), "_lim")
  names(spe_lim) <- paste0(names(spe_lim), "_lim")

  cbind(T2_lim, spe_lim)

}

#' Calculate limits of T2 and SPE control charts on multivariate
#' functional data using
#' cross-validation
#'
#' Calculate limits of Hotelling T2 and
#' squared prediction error (SPE) control charts
#' on multivariate functional data using cross-validation.
#' In the case few data are available to use a separate tuning data set in the
#' function \code{\link{calculate_limits}}, one can use k-fold cross-validation.
#' k groups of observations at a time are removed from the
#' training data set and used as tuning data set,
#' while the remaining observations are used as training data set
#' to estimate the multivariate functional principal component analysis model.
#' Then the T2 and SPE monitoring statistics can be
#' calculated on the tuning data
#' that have been removed from the model and control limits can be
#' calculated on the basis
#' of these values.
#'
#' @param pca
#' An object of class \code{pca_mfd} obtained by doing
#' multivariate functional principal component analysis (MFPCA)
#' on the training data set of functional covariates.
#' @param components
#' Set of multivariate functional principal components
#' retained into the MFPCA model.
#' These components are used to calculate the projected observations
#' and the Hotelling's T2 statistic,
#' while the difference between the original functional data
#' and the projected ones (based on the
#' selected components) is used to calculate the SPE statistic.
#' @param seed Deprecated: use \code{set.seed()} before calling
#' the function for reproducibility.
#' Since the split in the k groups is random,
#' you can fix a seed to ensure reproducibility.
#' @param nfold
#' The number of groups k used for k-fold cross-validation.
#' If it is equal to the number of observations in the training data set,
#' then we have leave-one-out cross-validation.
#' @param alpha
#' A named list with two elements, names \code{T2} and \code{spe},
#' respectively, each containing
#' the desired Type I error probability
#' of the corresponding control chart.
#' Note that at the moment you have to take into account manually
#' the family-wise error rate and adjust
#' the two values accordingly. See Capezza et al. (2020) and
#' Centofanti et al. (2021)
#' for additional details. Default value is
#' \code{list(T2 = 0.025, spe = 0.025)}.
#' @param ncores
#' If you want perform the analysis in the k groups in parallel,
#' give the number of cores/threads.
#'
#' @return
#' A \code{data.frame} with one single row, returning the
#' limits of the monitoring statistics:
#'
#' * \code{T2_lim} gives the upper control limit of the
#' Hotelling's T2 control chart,
#'
#' * one \code{contribution_T2_*_lim} column per each
#' functional variable giving the
#' limits of the contribution of that variable to the
#' Hotelling's T2 statistic,
#'
#' * \code{spe_lim} gives the upper control limit of the SPE control chart
#'
#' * one \code{contribution_spe*_lim} column per each
#' functional variable giving the
#' limits of the contribution of that variable to the SPE statistic,
#'
#' @noRd
#' @seealso \code{\link{calculate_limits}}
#'
calculate_cv_limits <- function(pca,
                                components,
                                seed,
                                nfold = 5,
                                alpha = list(T2 = .025, spe = .025),
                                ncores = 1,
                                absolute_error = FALSE) {

  if (!missing(seed)) {
    warning(paste0("argument seed is deprecated; ",
                   "please use set.seed()
                   before calling the function instead."),
            call. = FALSE)
  }

  if (!is.list(pca)) {
    stop("pca must be a list produced by pca_mfd.")
  }

  mfdobj <- pca$data
  nobs <- dim(mfdobj$coefs)[2]
  nvar <- dim(mfdobj$coefs)[3]
  folds <- split(seq_len(nobs), sample(cut(seq_len(nobs),
                                           nfold,
                                           labels = FALSE)))

  single_cv <- function(ii) {
    fd_train <- mfdobj[- folds[[ii]]]
    fd_test <- mfdobj[folds[[ii]]]

    pca_cv <- pca_mfd(fd_train, scale = pca$scale, nharm = max(components))
    control_charts_pca(pca = pca_cv,
                       components = components,
                       newdata = fd_test,
                       absolute_error = absolute_error)

    # T2 <- get_T2(pca_cv, components, newdata = fd_test)
    # spe <- get_spe(pca_cv, components, newdata = fd_test)
    #
    # list(id = fd_test$fdnames[[2]], T2 = T2, spe = spe)
  }
  if (ncores == 1) {
    statistics_cv <- lapply(seq_len(nfold), single_cv)
  } else {
    if (.Platform$OS.type == "unix") {
      statistics_cv <- parallel::mclapply(seq_len(nfold), single_cv, mc.cores = ncores)
    } else {
      cl <- parallel::makeCluster(ncores)
      parallel::clusterExport(cl,
                              c("mfdobj",
                                "folds",
                                "pca",
                                "components"),
                              envir = environment())
      statistics_cv <- parallel::parLapply(cl, seq_len(nfold), single_cv)
      parallel::stopCluster(cl)
    }
  }

  statistics_cv <- statistics_cv %>%
    dplyr::bind_rows() %>%
    dplyr::arrange("id")

  cont_T2 <- statistics_cv %>%
    dplyr::select(!dplyr::contains("_lim")) %>%
    dplyr::select(dplyr::contains("contribution_T2", ignore.case = FALSE))

  cont_spe <- statistics_cv %>%
    dplyr::select(!dplyr::contains("_lim")) %>%
    dplyr::select(dplyr::contains("contribution_spe", ignore.case = FALSE))

  T2_lim <- data.frame(T2 = stats::quantile(statistics_cv$T2, 1 - alpha$T2))
  spe_lim <- data.frame(spe = stats::quantile(statistics_cv$spe, 1 - alpha$T2))

  cont_T2_lim  <- apply(cont_T2, 2, function(x) {
    stats::quantile(x, 1 - alpha$T2 / nvar)
    })
  cont_spe_lim <-  apply(cont_spe, 2, function(x) {
    stats::quantile(x, 1 - alpha$spe / nvar)
    })

  cont_T2_lim <- as.data.frame(t(cont_T2_lim))
  cont_spe_lim <- as.data.frame(t(cont_spe_lim))

  df_limits <- dplyr::bind_cols(T2_lim, cont_T2_lim, spe_lim, cont_spe_lim)

  names(df_limits) <- paste0(names(df_limits), "_lim")

  df_limits

}


#' Get possible outliers of a training data set of a
#' scalar-on-function regression model.
#'
#' Get possible outliers of a training data set of a
#' scalar-on-function regression model.
#' It sets the training data set also as tuning data set for the
#' calculation of control chart limits,
#' and as phase II data set to compare monitoring statistics
#' against the limits and identify
#' possible outliers.
#' This is only an empirical approach. It is advised to use methods
#' appropriately designed for phase I monitoring to identify outliers.
#'
#' @param mfdobj
#' A multivariate functional data object of class mfd
#' denoting the functional covariates.
#' @param y
#' A numeric vector containing the observations of the
#' scalar response variable.
#'
#' @return
#' A character vector with the ids of functional observations
#' signaled as possibly anomalous.
#' @export
#' @examples
#' \donttest{
#' library(funcharts)
#' data("air")
#' air <- lapply(air, function(x) x[1:10, , drop = FALSE])
#' fun_covariates <- c("CO", "temperature")
#' mfdobj_x <- get_mfd_list(air[fun_covariates], lambda = 1e-2)
#' y <- rowMeans(air$NO2)
#' get_sof_pc_outliers(y, mfdobj_x)
#' }
#'
get_sof_pc_outliers <- function(y, mfdobj) {

  obs <- mfdobj$fdnames[[2]]

  names(y) <- obs
  mod <- sof_pc(y = y, mfdobj_x = mfdobj, selection = "variance")
  cclist <- control_charts_sof_pc(mod,
                                  y_test = y,
                                  mfdobj_x_test = mfdobj,
                                  alpha = list(T2 = .01, spe = .01, y = .01))
  ooc <- which_ooc(cclist)
  c(sort(unique(unlist(lapply(ooc, function(x) x$id)))))

}


#' Get outliers from multivariate functional data
#'
#' Get outliers from multivariate functional data
#' using the functional boxplot with the
#' modified band depth of Sun et al. (2011, 2012).
#' This function relies on the \code{fbplot} function
#' of the \code{roahd} package.
#'
#' @param mfdobj A multivariate functional data object of class mfd
#'
#' @return
#' A numeric vector with the indexes of the functional observations
#' signaled as outliers.
#' @export
#'
#' @examples
#' library(funcharts)
#' data("air")
#' air <- lapply(air, function(x) x[1:20, , drop = FALSE])
#' fun_covariates <- c("CO", "temperature")
#' mfdobj_x <- get_mfd_list(air[fun_covariates], lambda = 1e-2)
#' get_outliers_mfd(mfdobj_x)
#'
#' @references
#' * Sun, Y., & Genton, M. G. (2011). Functional boxplots.
#' \emph{Journal of Computational and Graphical Statistics}, 20(2), 316-334.
#' * Sun, Y., & Genton, M. G. (2012).
#' Adjusted functional boxplots for spatio-temporal data visualization
#' and outlier detection. \emph{Environmetrics}, 23(1), 54-64.
#'
get_outliers_mfd <- function(mfdobj) {

  xseq <- seq(0, 1, l = 200)
  nvar <- dim(mfdobj$coefs)[3]
  nobs <- dim(mfdobj$coefs)[2]
  if (nvar == 1) {
    fd_obj <- fda::fd(mfdobj$coefs[, , 1], mfdobj$basis)
    fd_eval <- fda::eval.fd(xseq, fd_obj)
    fData_obj <- roahd::fData(xseq, t(fd_eval))
  } else {
    fd_obj <- fda::fd(mfdobj$coefs, mfdobj$basis)
    fd_eval <- fda::eval.fd(xseq, fd_obj)
    fd_eval_list <- lapply(seq_len(nvar), function(ii) t(fd_eval[, , ii]))
    fData_obj <- roahd::mfData(xseq, fd_eval_list)
  }
  is_outlier <- rep(FALSE, nobs)
  new_outliers <- 100
  while (length(new_outliers) > 0) {
    fbplot_obj <- roahd::fbplot(fData_obj[!is_outlier], display = FALSE)
    new_outliers <- fbplot_obj$ID_outliers
    is_outlier[!is_outlier][new_outliers] <- TRUE
  }
  # fbplot_obj <- roahd::fbplot(fData_obj[!is_outlier], display = TRUE)
  outliers <- which(is_outlier)
  names(outliers) <- mfdobj$fdnames[[2]][outliers]
  return(outliers)

}
