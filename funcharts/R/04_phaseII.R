#' T2 and SPE control charts for multivariate functional data
#'
#' This function builds a data frame needed to plot
#' the Hotelling's T2 and squared prediction error (SPE)
#' control charts
#' based on multivariate functional principal component analysis
#' (MFPCA) performed
#' on multivariate functional data, as Capezza et al. (2020)
#' for the multivariate functional covariates.
#' The training data have already been used to fit the model.
#' An optional tuning data set can be provided to estimate
#' the control chart limits.
#' A phase II data set contains the observations
#' to be monitored with the control charts.
#'
#' @param pca
#' An object of class \code{pca_mfd}
#' obtained by doing MFPCA on the
#' training set of multivariate functional data.
#' @param components
#' A vector of integers with the components over which
#' to project the multivariate functional data.
#' If this is not NULL, the arguments `single_min_variance_explained`
#' and `tot_variance_explained` are ignored.
#' If NULL, components are selected such that
#' the total fraction of variance explained by them
#' is at least equal to the argument `tot_variance_explained`,
#' where only components explaining individually a fraction of variance
#' at least equal to the argument `single_min_variance_explained`
#' are considered to be retained.
#' Default is NULL.
#' @param tuning_data
#' An object of class \code{mfd} containing
#' the tuning set of the multivariate functional data, used to estimate the
#' T2 and SPE control chart limits.
#' If NULL, the training data, i.e. the data used to fit the MFPCA model,
#' are also used as the tuning data set, i.e. \code{tuning_data=pca$data}.
#' Default is NULL.
#' @param newdata
#' An object of class \code{mfd} containing
#' the phase II set of the multivariate functional data to be monitored.
#' @param alpha
#' If it is a number between 0 and 1,
#' it defines the overall type-I error probability and the Bonferroni
#' correction is applied by setting the type-I error probability
#' in the two control charts equal to \code{alpha/2}.
#' If you want to set manually the Type-I error probabilities in the
#' two control charts, then the argument \code{alpha} must be
#' a named list
#' with two elements, named \code{T2} and \code{spe},
#' respectively, each containing
#' the desired Type I error probability of
#' the corresponding control chart.
#' Default value is 0.05.
#' @param limits
#' A character value.
#' If "standard", it estimates the control limits on the tuning
#' data set. If "cv", the function calculates the control limits only on the
#' training data using cross-validation
#' using \code{calculate_cv_limits}. Default is "standard".
#' @param seed
#' If \code{limits=="cv"},
#' since the split in the k groups is random,
#' you can fix a seed to ensure reproducibility.
#' Deprecated: use \code{set.seed()} before calling
#' the function for reproducibility.
#' @param nfold
#' If \code{limits=="cv"}, this gives the number of groups k
#' used for k-fold cross-validation.
#' If it is equal to the number of observations in the training data set,
#' then we have
#' leave-one-out cross-validation.
#' Otherwise, this argument is ignored.
#' @param ncores
#' If \code{limits=="cv"}, if you want perform the analysis
#' in the k groups in parallel,
#' give the number of cores/threads.
#' Otherwise, this argument is ignored.
#' @param single_min_variance_explained
#' The minimum fraction of variance
#' that has to be explained
#' by each multivariate functional principal component
#' such that it is retained into the MFPCA model.
#' Default is 0.
#' @param tot_variance_explained
#' The minimum fraction of variance
#' that has to be explained
#' by the set of multivariate functional principal components
#' retained into the MFPCA model
#' fitted on the functional covariates.
#' Default is 0.9.
#' @param absolute_error
#' If FALSE, the SPE statistic, which monitors the principal components
#' not retained in the MFPCA model, is calculated as the sum
#' of the integrals of the squared prediction error functions, obtained
#' as the difference between the actual functions and their approximation
#' after projection over the selected principal components.
#' If TRUE, the SPE statistic is calculated by replacing the square of
#' the prediction errors with the absolute value, as proposed by
#' Capizzi and Masarotto (2018).
#' Default value is FALSE.
#'
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
#' * one column per each functional variable,
#' containing its contribution to the SPE statistic,
#'
#' * \code{T2_lim} gives the upper control limit of
#' the Hotelling's T2 control chart,
#'
#' * one \code{contribution_T2_*_lim} column per each
#' functional variable giving the
#' limits of the contribution of that variable
#' to the Hotelling's T2 statistic,
#'
#' * \code{spe_lim} gives the upper control limit of the SPE control chart
#'
#' * one \code{contribution_spe*_lim} column per each
#' functional variable giving the
#' limits of the contribution of that variable to the SPE statistic.
#'
#' @export
#'
#' @seealso \code{\link{regr_cc_fof}}
#'
#' @references
#'
#' Capezza C, Lepore A, Menafoglio A, Palumbo B, Vantini S. (2020)
#' Control charts for
#' monitoring ship operating conditions and CO2 emissions
#' based on scalar-on-function regression.
#' \emph{Applied Stochastic Models in Business and Industry},
#' 36(3):477--500.
#' <doi:10.1002/asmb.2507>
#'
#' Capizzi, G., & Masarotto, G. (2018).
#' Phase I distribution-free analysis with the R package dfphase1.
#' In \emph{Frontiers in Statistical Quality Control 12 (pp. 3-19)}.
#' Springer International Publishing.
#'
#'
#' @examples
#' library(funcharts)
#' data("air")
#' air <- lapply(air, function(x) x[1:220, , drop = FALSE])
#' fun_covariates <- c("CO", "temperature")
#' mfdobj_x <- get_mfd_list(air[fun_covariates],
#'                          n_basis = 15,
#'                          lambda = 1e-2)
#' y <- rowMeans(air$NO2)
#' y1 <- y[1:100]
#' y_tuning <- y[101:200]
#' y2 <- y[201:220]
#' mfdobj_x1 <- mfdobj_x[1:100]
#' mfdobj_x_tuning <- mfdobj_x[101:200]
#' mfdobj_x2 <- mfdobj_x[201:220]
#' pca <- pca_mfd(mfdobj_x1)
#' cclist <- control_charts_pca(pca = pca,
#'                              tuning_data = mfdobj_x_tuning,
#'                              newdata = mfdobj_x2)
#' plot_control_charts(cclist)
#'
#'
control_charts_pca <- function(pca,
                               components = NULL,
                               tuning_data = NULL,
                               newdata,
                               alpha = 0.05,
                               limits = "standard",
                               seed,
                               nfold = 5,
                               ncores = 1,
                               tot_variance_explained = 0.9,
                               single_min_variance_explained = 0,
                               absolute_error = FALSE) {

  if (!missing(seed)) {
    warning(paste0("argument seed is deprecated; ",
                   "please use set.seed()
                   before calling the function instead."),
            call. = FALSE)
  }

  if (!(is.numeric(alpha) | is.list(alpha))) {
    stop("alpha must be either a number or a list")
  }

  if (is.numeric(alpha) & length(alpha) > 1) {
    stop("alpha must be a single number, between 0 and 1, or a list.")
  }

  if (is.numeric(alpha) & !all(alpha > 0 & alpha < 1)) {
    stop("alpha must be a single number, between 0 and 1, or a list.")
  }

  if (is.list(alpha) & !all((names(alpha) %in% c("T2", "spe")))) {
    stop("if alpha is a list, it must be named with names T2 and spe.")
  }

  if (!is.list(pca)) {
    stop("pca must be a list produced by pca_mfd.")
  }

  if (length(limits) != 1) {
    stop("Only one type of 'limits' allowed.")
  }
  if (!(limits %in% c("standard", "cv"))) {
    stop("'limits' argument can be only 'standard' or 'cv'.")
  }

  if (is.null(components)) {
    components_enough_var <- cumsum(pca$varprop) > tot_variance_explained
    if (sum(components_enough_var) == 0)
      ncomponents <- length(pca$varprop) else
        ncomponents <- which(cumsum(pca$varprop) > tot_variance_explained)[1]
      components <- seq_len(ncomponents)
      components <-
        which(pca$varprop[components] > single_min_variance_explained)
  }

  if (is.numeric(alpha)) alpha <- list(T2 = alpha / 2, spe = alpha / 2)

  if (limits == "standard") lim <- calculate_limits(
    pca = pca,
    tuning_data = tuning_data,
    components = components,
    alpha = alpha,
    absolute_error = absolute_error)
  if (limits == "cv") lim <- calculate_cv_limits(
    pca = pca,
    components = components,
    alpha = alpha,
    nfold = nfold,
    ncores = ncores,
    absolute_error = absolute_error)

  newdata_scaled <- scale_mfd(newdata,
                              center = pca$center_fd,
                              scale = if (pca$scale) pca$scale_fd else FALSE)

  T2_spe <- get_T2_spe(pca,
                       components,
                       newdata_scaled = newdata_scaled,
                       absolute_error = absolute_error)
  id <- data.frame(id = newdata$fdnames[[2]])

  cbind(id, T2_spe, lim)
}



#' Control charts for monitoring a scalar quality characteristic adjusted for
#' by the effect of multivariate functional covariates
#'
#' @description
#' This function builds a data frame needed to
#' plot control charts
#' for monitoring a monitoring a scalar quality characteristic adjusted for
#' the effect of multivariate functional covariates based on
#' scalar-on-function regression,
#' as proposed in Capezza et al. (2020).
#'
#' In particular, this function provides:
#'
#' * the Hotelling's T2 control chart,
#'
#' * the squared prediction error (SPE) control chart,
#'
#' * the scalar regression control chart.
#'
#' This function calls \code{control_charts_pca} for the control charts on
#' the multivariate functional covariates and \code{\link{regr_cc_sof}}
#' for the scalar regression control chart.
#'
#' The training data have already been used to fit the model.
#' An optional tuning data set can be provided that is used to estimate
#' the control chart limits.
#' A phase II data set contains the observations to be monitored
#' with the control charts.
#'
#'
#' @param mod
#' A list obtained as output from \code{sof_pc},
#' i.e. a fitted scalar-on-function linear regression model.
#' @param y_test
#' A numeric vector containing the observations
#' of the scalar response variable
#' in the phase II data set.
#' @param mfdobj_x_test
#' An object of class \code{mfd} containing
#' the phase II data set of the functional covariates observations.
#' @param mfdobj_x_tuning
#' An object of class \code{mfd} containing
#' the tuning set of the multivariate functional data, used to estimate the
#' T2 and SPE control chart limits.
#' If NULL, the training data, i.e. the data used to fit the MFPCA model,
#' are also used as the tuning data set, i.e. \code{tuning_data=pca$data}.
#' Default is NULL.
#' @param alpha
#' A named list with three elements, named \code{T2}, \code{spe},
#' and \code{y},
#' respectively, each containing
#' the desired Type I error probability of the corresponding control chart
#' (\code{T2} corresponds to the T2 control chart,
#' \code{spe}  corresponds to the SPE control chart,
#' \code{y} corresponds to the scalar regression control chart).
#' Note that at the moment you have to take into account manually
#' the family-wise error rate and adjust
#' the two values accordingly. See Capezza et al. (2020)
#' for additional details. Default value is
#' \code{list(T2 = 0.0125, spe = 0.0125, y = 0.025)}.
#' @param limits
#' A character value.
#' If "standard", it estimates the control limits on the tuning
#' data set. If "cv", the function calculates the control limits only on the
#' training data using cross-validation
#' using \code{calculate_cv_limits}. Default is "standard".
#' @param seed
#' If \code{limits=="cv"},
#' since the split in the k groups is random,
#' you can fix a seed to ensure reproducibility.
#' Deprecated: use \code{set.seed()} before calling
#' the function for reproducibility.
#' @param nfold
#' If \code{limits=="cv"}, this gives the number of groups k
#' used for k-fold cross-validation.
#' If it is equal to the number of observations in the training data set,
#' then we have
#' leave-one-out cross-validation.
#' Otherwise, this argument is ignored.
#' @param ncores
#' If \code{limits=="cv"}, if you want perform the analysis
#' in the k groups in parallel,
#' give the number of cores/threads.
#' Otherwise, this argument is ignored.
#'
#' @return
#' A \code{data.frame} with as many rows as the number of
#' multivariate functional observations in the phase II data set and
#' the following columns:
#'
#' * one \code{id} column identifying the multivariate functional observation
#' in the phase II data set,
#'
#' * one \code{T2} column containing the Hotelling T2 statistic calculated
#' for all observations,
#'
#' * one column per each functional variable, containing its contribution
#' to the T2 statistic,
#'
#' * one \code{spe} column containing the SPE statistic calculated
#' for all observations,
#'
#' * one column per each functional variable, containing its contribution
#' to the SPE statistic,
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
#' * one \code{contribution_spe*_lim} column per
#' each functional variable giving the
#' limits of the contribution of that variable to the SPE statistic.
#'
#' * \code{y_hat}: the predictions of the response variable
#' corresponding to \code{mfdobj_x_new},
#'
#' * \code{y}: the same as the argument \code{y_new}
#' given as input to this function,
#'
#' * \code{lwr}: lower limit of the \code{1-alpha}
#' prediction interval on the response,
#'
#' * \code{pred_err}: prediction error calculated as \code{y-y_hat},
#'
#' * \code{pred_err_sup}: upper limit of the \code{1-alpha}
#' prediction interval on the prediction error,
#'
#' * \code{pred_err_inf}: lower limit of the \code{1-alpha}
#'  prediction interval on the prediction error.
#'
#' @export
#'
#' @seealso \code{\link{control_charts_pca}}, \code{\link{regr_cc_sof}}
#'
#' @examples
#' \donttest{
#' #' library(funcharts)
#' data("air")
#' air <- lapply(air, function(x) x[201:300, , drop = FALSE])
#' fun_covariates <- c("CO", "temperature")
#' mfdobj_x <- get_mfd_list(air[fun_covariates],
#'                          n_basis = 15,
#'                          lambda = 1e-2)
#' y <- rowMeans(air$NO2)
#' y1 <- y[1:60]
#' y2 <- y[91:100]
#' mfdobj_x1 <- mfdobj_x[1:60]
#' mfdobj_x_tuning <- mfdobj_x[61:90]
#' mfdobj_x2 <- mfdobj_x[91:100]
#' mod <- sof_pc(y1, mfdobj_x1)
#' cclist <- control_charts_sof_pc(mod = mod,
#'                                 y_test = y2,
#'                                 mfdobj_x_test = mfdobj_x2,
#'                                 mfdobj_x_tuning = mfdobj_x_tuning)
#' plot_control_charts(cclist)
#' }
#'
control_charts_sof_pc <- function(mod,
                                  y_test,
                                  mfdobj_x_test,
                                  mfdobj_x_tuning = NULL,
                                  alpha = list(
                                    T2  = .0125,
                                    spe = .0125,
                                    y   = .025),
                                  limits = "standard",
                                  seed,
                                  nfold = NULL,
                                  ncores = 1) {

  .Deprecated("regr_cc_sof")

  regr_cc_sof(object = mod,
              y_new = y_test,
              mfdobj_x_new = mfdobj_x_test,
              mfdobj_x_tuning = mfdobj_x_tuning,
              y_tuning = NULL,
              alpha = alpha,
              parametric_limits = TRUE,
              include_covariates = TRUE)

}




#' Plot control charts
#'
#' This function takes as input a data frame produced
#' with functions such as
#' \code{\link{control_charts_pca}} and \code{\link{control_charts_sof_pc}} and
#' produces a ggplot with the desired control charts, i.e.
#' it plots a point for each
#' observation in the phase II data set against
#' the corresponding control limits.
#'
#' @param cclist
#' A \code{data.frame} produced by
#' \code{\link{control_charts_pca}}, \code{\link{control_charts_sof_pc}}
#' \code{\link{regr_cc_fof}}, or \code{\link{regr_cc_sof}}.
#' @param nobsI
#' An integer indicating the first observations that are plotted in gray.
#' It is useful when one wants to plot the phase I data set together
#' with the phase II data. In that case, one needs to indicate the number
#' of phase I observations included in \code{cclist}.
#' Default is zero.
#'
#' @return A ggplot with the functional control charts.
#'
#' @details Out-of-control points are signaled by colouring them in red.
#'
#' @export
#' @examples
#' library(funcharts)
#' data("air")
#' air <- lapply(air, function(x) x[1:100, , drop = FALSE])
#' fun_covariates <- c("CO", "temperature")
#' mfdobj_x <- get_mfd_list(air[fun_covariates],
#'                          n_basis = 15,
#'                          lambda = 1e-2)
#' mfdobj_y <- get_mfd_list(air["NO2"],
#'                          n_basis = 15,
#'                          lambda = 1e-2)
#' mfdobj_y1 <- mfdobj_y[1:60]
#' mfdobj_y_tuning <- mfdobj_y[61:90]
#' mfdobj_y2 <- mfdobj_y[91:100]
#' mfdobj_x1 <- mfdobj_x[1:60]
#' mfdobj_x_tuning <- mfdobj_x[61:90]
#' mfdobj_x2 <- mfdobj_x[91:100]
#' mod_fof <- fof_pc(mfdobj_y1, mfdobj_x1)
#' cclist <- regr_cc_fof(mod_fof,
#'                       mfdobj_y_new = mfdobj_y2,
#'                       mfdobj_x_new = mfdobj_x2,
#'                       mfdobj_y_tuning = NULL,
#'                       mfdobj_x_tuning = NULL)
#' plot_control_charts(cclist)
#'
plot_control_charts <- function(cclist, nobsI = 0) {

  statistic <- ucl <- lcl <- id <- ooc <- ytext <- NULL

  if (!is.data.frame(cclist)) {
    stop(paste0("cclist must be a data frame ",
                "containing data to produce control charts."))
  }

  df_hot <- NULL
  if (!is.null(cclist$T2)) {
    df_hot <- data.frame(statistic = cclist$T2,
                         lcl = 0,
                         ucl = cclist$T2_lim) %>%
      dplyr::mutate(id = seq_len(dplyr::n()),
                    ooc = statistic > ucl,
                    type = "HOTELLING~T2~CONTROL~CHART")

    hot_range <- df_hot %>%
      dplyr::select(statistic, ucl, lcl) %>%
      range() %>%
      diff()

    df_hot <- df_hot %>%
      dplyr::mutate(ytext = dplyr::case_when(
        statistic < lcl ~ statistic - hot_range * 0.2,
        statistic > ucl ~ statistic + hot_range * 0.2,
        TRUE ~ statistic,
      ))
  }

  df_spe <- NULL
  if (!is.null(cclist$spe)) {
    df_spe <- data.frame(statistic = cclist$spe,
                         lcl = 0,
                         ucl = cclist$spe_lim) %>%
      dplyr::mutate(id = seq_len(dplyr::n()),
                    ooc = statistic > ucl,
                    type = "SPE~CONTROL~CHART")

    spe_range <- df_spe %>%
      dplyr::select(statistic, ucl, lcl) %>%
      range() %>%
      diff()

    df_spe <- df_spe %>%
      dplyr::mutate(ytext = dplyr::case_when(
        statistic < lcl ~ statistic - spe_range * 0.2,
        statistic > ucl ~ statistic + spe_range * 0.2,
        TRUE ~ statistic,
      ))
  }

  df_y <- NULL
  if (!is.null(cclist$pred_err)) {
    df_y <- data.frame(statistic = cclist$pred_err,
                       lcl = cclist$pred_err_inf,
                       ucl = cclist$pred_err_sup)

    y_range <- df_y %>%
      dplyr::select(c(statistic, ucl, lcl)) %>%
      range() %>%
      diff()

    df_y <- df_y %>%
      dplyr::mutate(ytext = dplyr::case_when(
        statistic < lcl ~ statistic - y_range * 0.2,
        statistic > ucl ~ statistic + y_range * 0.2,
        TRUE ~ statistic,
      ))

    df_y <- df_y %>%
      dplyr::mutate(id = seq_len(dplyr::n()),
                    ooc = statistic > ucl | statistic < lcl,
                    type = "REGRESSION~CONTROL~CHART")

  }

  df_hot_x <- NULL
  if (!is.null(cclist$T2_x)) {
    df_hot_x <- data.frame(statistic = cclist$T2_x,
                           lcl = 0,
                           ucl = cclist$T2_lim_x) %>%
      dplyr::mutate(id = seq_len(dplyr::n()),
                    ooc = statistic > ucl,
                    type = "HOTELLING~T2~CONTROL~CHART~(COVARIATES)")

    hot_range <- df_hot_x %>%
      dplyr::select(statistic, ucl, lcl) %>%
      range() %>%
      diff()

    df_hot_x <- df_hot_x %>%
      dplyr::mutate(ytext = dplyr::case_when(
        statistic < lcl ~ statistic - hot_range * 0.2,
        statistic > ucl ~ statistic + hot_range * 0.2,
        TRUE ~ statistic,
      ))
  }

  df_spe_x <- NULL
  if (!is.null(cclist$spe_x)) {
    df_spe_x <- data.frame(statistic = cclist$spe_x,
                           lcl = 0,
                           ucl = cclist$spe_lim_x) %>%
      dplyr::mutate(id = seq_len(dplyr::n()),
                    ooc = statistic > ucl,
                    type = "SPE~CONTROL~CHART~(COVARIATES)")

    spe_range <- df_spe_x %>%
      dplyr::select(statistic, ucl, lcl) %>%
      range() %>%
      diff()

    df_spe_x <- df_spe_x %>%
      dplyr::mutate(ytext = dplyr::case_when(
        statistic < lcl ~ statistic - spe_range * 0.2,
        statistic > ucl ~ statistic + spe_range * 0.2,
        TRUE ~ statistic,
      ))
  }

  df_amfewma <- NULL
  if (!is.null(cclist$amfewma_monitoring_statistic)) {
    df_amfewma <- data.frame(statistic = cclist$amfewma_monitoring_statistic,
                         lcl = 0,
                         ucl = cclist$amfewma_monitoring_statistic_lim) %>%
      dplyr::mutate(id = seq_len(dplyr::n()),
                    ooc = statistic > ucl,
                    type = "AMFEWMA~CONTROL~CHART")

    amfewma__range <- df_amfewma %>%
      dplyr::select(statistic, ucl, lcl) %>%
      range() %>%
      diff()

    df_amfewma <- df_amfewma %>%
      dplyr::mutate(ytext = dplyr::case_when(
        statistic < lcl ~ statistic - amfewma__range * 0.2,
        statistic > ucl ~ statistic + amfewma__range * 0.2,
        TRUE ~ statistic,
      ))
  }

  plot_list <- list()

  if (!is.null(cclist$T2)) {
    plot_list$p_hot <- ggplot2::ggplot(df_hot,
                                       ggplot2::aes(x = id,
                                                    y = statistic)) +
      ggplot2::geom_line() +
      ggplot2::geom_point(ggplot2::aes(colour = ooc)) +
      ggplot2::geom_blank(ggplot2::aes(y = 0)) +
      ggplot2::geom_point(ggplot2::aes(y = ucl),
                          pch = "-", size = 5) +
      ggplot2::theme_bw() +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90,
                                                         vjust = 0.5)) +
      ggplot2::theme(legend.position = "none",
                     plot.title = ggplot2::element_text(hjust = 0.5)) +
      ggplot2::geom_text(ggplot2::aes(y = ytext, label = id),
                         data = dplyr::filter(df_hot, ooc),
                         size = 3) +
      ggplot2::xlab("Observation") +
      ggplot2::ylab(expression(T2~statistic)) +
      ggplot2::ggtitle(expression(HOTELLING~T2~CONTROL~CHART))
  }

  if (!is.null(cclist$spe)) {
    plot_list$p_spe <- ggplot2::ggplot(df_spe,
                                       ggplot2::aes(x = id, y = statistic)) +
      ggplot2::geom_line() +
      ggplot2::geom_point(ggplot2::aes(colour = ooc)) +
      ggplot2::geom_blank(ggplot2::aes(y = 0)) +
      ggplot2::geom_point(ggplot2::aes(y = ucl),
                          pch = "-", size = 5) +
      ggplot2::theme_bw() +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90,
                                                         vjust = 0.5)) +
      ggplot2::theme(legend.position = "none",
                     plot.title = ggplot2::element_text(hjust = 0.5)) +
      ggplot2::geom_text(ggplot2::aes(y = ytext, label = id),
                         data = dplyr::filter(df_spe, ooc),
                         size = 3) +
      ggplot2::xlab("Observation") +
      ggplot2::ylab(expression(SPE~statistic)) +
      ggplot2::ggtitle(expression(SPE~CONTROL~CHART))
  }

  if (!is.null(cclist$pred_err)) {
    plot_list$p_y <- ggplot2::ggplot(df_y,
                                     ggplot2::aes(x = id, y = statistic)) +
      ggplot2::geom_line() +
      ggplot2::geom_point(ggplot2::aes(colour = ooc)) +
      ggplot2::geom_blank(ggplot2::aes(y = 0)) +
      ggplot2::geom_line(ggplot2::aes(y = lcl), lty = 2) +
      ggplot2::geom_line(ggplot2::aes(y = ucl), lty = 2) +
      ggplot2::theme_bw() +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90,
                                                         vjust = 0.5)) +
      ggplot2::theme(legend.position = "none",
            plot.title = ggplot2::element_text(hjust = 0.5)) +
      ggplot2::geom_text(ggplot2::aes(y = ytext, label = id),
                         data = dplyr::filter(df_y, ooc),
                         size = 3) +
      ggplot2::xlab("Observation") +
      ggplot2::ylab(expression("Regression residuals")) +
      ggplot2::ggtitle(expression(REGRESSION~CONTROL~CHART))
  }

  if (!is.null(cclist$T2_x)) {
    plot_list$p_hot_x <- ggplot2::ggplot(df_hot_x,
                                         ggplot2::aes(x = id, y = statistic)) +
      ggplot2::geom_line() +
      ggplot2::geom_point(ggplot2::aes(colour = ooc)) +
      ggplot2::geom_blank(ggplot2::aes(y = 0)) +
      ggplot2::geom_point(ggplot2::aes(y = ucl),
                          pch = "-", size = 5) +
      ggplot2::theme_bw() +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90,
                                                         vjust = 0.5)) +
      ggplot2::theme(legend.position = "none",
                     plot.title = ggplot2::element_text(hjust = 0.5)) +
      ggplot2::geom_text(ggplot2::aes(y = ytext, label = id),
                         data = dplyr::filter(df_hot_x, ooc),
                         size = 3) +
      ggplot2::xlab("Observation") +
      ggplot2::ylab(expression(T2~statistic)) +
      ggplot2::ggtitle(expression(HOTELLING~T2~CONTROL~CHART~(COVARIATES)))

    plot_list$p_hot <- plot_list$p_hot +
      ggplot2::ggtitle(expression(HOTELLING~T2~CONTROL~CHART~(RESPONSE)))
  }

  if (!is.null(cclist$spe_x)) {
    plot_list$p_spe_x <- ggplot2::ggplot(df_spe_x,
                                         ggplot2::aes(x = id, y = statistic)) +
      ggplot2::geom_line() +
      ggplot2::geom_point(ggplot2::aes(colour = ooc)) +
      ggplot2::geom_blank(ggplot2::aes(y = 0)) +
      ggplot2::geom_point(ggplot2::aes(y = ucl),
                          pch = "-", size = 5) +
      ggplot2::theme_bw() +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90,
                                                         vjust = 0.5)) +
      ggplot2::theme(legend.position = "none",
                     plot.title = ggplot2::element_text(hjust = 0.5)) +
      ggplot2::geom_text(ggplot2::aes(y = ytext, label = id),
                         data = dplyr::filter(df_spe_x, ooc),
                         size = 3) +
      ggplot2::xlab("Observation") +
      ggplot2::ylab(expression(SPE~statistic)) +
      ggplot2::ggtitle(expression(SPE~CONTROL~CHART~(COVARIATES)))

    plot_list$p_spe <- plot_list$p_spe +
      ggplot2::ggtitle(expression(SPE~CONTROL~CHART~(RESPONSE)))
  }

  if (!is.null(cclist$amfewma_monitoring_statistic)) {
    plot_list$p_amfewma <- ggplot2::ggplot(df_amfewma,
                                           ggplot2::aes(x = id,
                                                        y = statistic)) +
      ggplot2::geom_line() +
      ggplot2::geom_point(ggplot2::aes(colour = ooc)) +
      ggplot2::geom_blank(ggplot2::aes(y = 0)) +
      ggplot2::geom_point(ggplot2::aes(y = ucl),
                          pch = "-", size = 5) +
      ggplot2::theme_bw() +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90,
                                                         vjust = 0.5)) +
      ggplot2::theme(legend.position = "none",
                     plot.title = ggplot2::element_text(hjust = 0.5)) +
      ggplot2::geom_text(ggplot2::aes(y = ytext, label = id),
                         data = dplyr::filter(df_amfewma, ooc),
                         size = 3) +
      ggplot2::xlab("Observation") +
      ggplot2::ylab(expression(AMFEWMA~monitorng~statistic)) +
      ggplot2::ggtitle(expression(AMFEWMA~CONTROL~CHART))
  }

  p <- patchwork::wrap_plots(plot_list, ncol = 1) &
    ggplot2::scale_color_manual(values = c("TRUE" = "red",
                                           "FALSE" = "black",
                                           "phase1" = "grey")) &
    ggplot2::scale_x_continuous(
      limits = c(0, nrow(cclist) + 1),
      breaks = seq(1, nrow(cclist), by = round(nrow(cclist) / 50) + 1),
      expand = c(0.015, 0.015))

  if (nobsI > 0) {

    nplots <- length(p$patches$plots) + 1
    for (jj in seq_len(nplots)) {
      p[[jj]]$data$ooc[seq_len(nobsI)] <- "phase1"
      p[[jj]]$layers[[5]]$data <- p[[jj]]$layers[[5]]$data %>%
        dplyr::filter(id > nobsI) %>%
        as.data.frame()
      p[[jj]] <- p[[jj]] +
        ggplot2::geom_point(ggplot2::aes(y = !!dplyr::sym("ucl")),
                            pch = "-",
                            size = 5,
                            col = "grey",
                            data = dplyr::filter(p[[jj]]$data, id <= nobsI)) +
        ggplot2::geom_line(
          ggplot2::aes(!!dplyr::sym("id"), !!dplyr::sym("statistic")),
          col = "grey",
          data = dplyr::filter(p[[jj]]$data, id < nobsI))
    }
    p <- p &
      ggplot2::geom_vline(ggplot2::aes(xintercept = nobsI + 1), lty = 2)
  }

  return(p)

}






#' Scalar-on-Function Regression Control Chart
#'
#' This function is deprecated. Use \code{\link{regr_cc_sof}}.
#' This function builds a data frame needed
#' to plot the scalar-on-function regression control chart,
#' based on a fitted function-on-function linear regression model and
#' proposed in Capezza et al. (2020).
#' If \code{include_covariates} is \code{TRUE},
#' it also plots the Hotelling's T2 and
#' squared prediction error control charts built on the
#' multivariate functional covariates.
#'
#' The training data have already been used to fit the model.
#' An additional tuning data set can be provided that is used to estimate
#' the control chart limits.
#' A phase II data set contains the observations to be monitored
#' with the built control charts.
#'
#' @param object
#' A list obtained as output from \code{sof_pc},
#' i.e. a fitted scalar-on-function linear regression model.
#' @param mfdobj_x_new
#' An object of class \code{mfd} containing
#' the phase II data set of the functional covariates observations.
#' @param y_new
#' A numeric vector containing the observations of
#' the scalar response variable
#' in the phase II data set.
#' @param y_tuning
#' A numeric vector containing the observations of the scalar response
#' variable in the tuning data set.
#' If NULL, the training data, i.e. the data used to
#' fit the scalar-on-function regression model,
#' are also used as the tuning data set.
#' Default is NULL.
#' @param mfdobj_x_tuning
#' An object of class \code{mfd} containing
#' the tuning set of the multivariate functional data, used to estimate the
#' control chart limits.
#' If NULL, the training data, i.e. the data used to
#' fit the scalar-on-function regression model,
#' are also used as the tuning data set.
#' Default is NULL.
#' @param alpha
#' If it is a number between 0 and 1,
#' it defines the overall type-I error probability.
#' If \code{include_covariates} is \code{TRUE}, i.e.,
#' also the Hotelling's T2 and SPE control charts are built
#' on the functional covariates, then the Bonferroni
#' correction is applied by setting the type-I error probability
#' in the three control charts equal to \code{alpha/3}.
#' In this last case,
#' if you want to set manually the Type-I error probabilities,
#' then the argument \code{alpha} must be a named list
#' with three elements, named \code{T2}, \code{spe} and \code{y},
#' respectively, each containing
#' the desired Type I error probability of
#' the corresponding control chart, where \code{y} refers to the
#' regression control chart.
#' Default value is 0.05.
#' @param parametric_limits
#' If \code{TRUE}, the limits are calculated based on the normal distribution
#' assumption on the response variable, as in Capezza et al. (2020).
#' If \code{FALSE}, the limits are calculated nonparametrically as
#' empirical quantiles of the distribution of the residuals calculated
#' on the tuning data set.
#' The default value is \code{FALSE}.
#' @param include_covariates
#' If TRUE, also functional covariates are monitored through
#'  \code{control_charts_pca},.
#' If FALSE, only the scalar response, conditionally on the covariates,
#' is monitored.
#' @param absolute_error
#' A logical value that, if \code{include_covariates} is TRUE, is passed
#' to \code{\link{control_charts_pca}}.
#'
#' @return
#' A \code{data.frame} with as many rows as the
#' number of functional replications in \code{mfdobj_x_new},
#' with the following columns:
#'
#' * \code{y_hat}: the predictions of the response variable
#' corresponding to \code{mfdobj_x_new},
#'
#' * \code{y}: the same as the argument \code{y_new} given as input
#' to this function,
#'
#' * \code{lwr}: lower limit of the \code{1-alpha} prediction interval
#' on the response,
#'
#' * \code{pred_err}: prediction error calculated as \code{y-y_hat},
#'
#' * \code{pred_err_sup}: upper limit of the \code{1-alpha} prediction interval
#' on the prediction error,
#'
#' * \code{pred_err_inf}: lower limit of the \code{1-alpha} prediction interval
#' on the prediction error.
#'
#' @export
#'
#' @references
#' Capezza C, Lepore A, Menafoglio A, Palumbo B, Vantini S. (2020)
#' Control charts for
#' monitoring ship operating conditions and CO2 emissions
#' based on scalar-on-function regression.
#' \emph{Applied Stochastic Models in Business and Industry},
#' 36(3):477--500.
#' <doi:10.1002/asmb.2507>
#'
#' @examples
#' library(funcharts)
#' air <- lapply(air, function(x) x[1:100, , drop = FALSE])
#' fun_covariates <- c("CO", "temperature")
#' mfdobj_x <- get_mfd_list(air[fun_covariates],
#'                          n_basis = 15,
#'                          lambda = 1e-2)
#' y <- rowMeans(air$NO2)
#' y1 <- y[1:80]
#' y2 <- y[81:100]
#' mfdobj_x1 <- mfdobj_x[1:80]
#' mfdobj_x2 <- mfdobj_x[81:100]
#' mod <- sof_pc(y1, mfdobj_x1)
#' cclist <- regr_cc_sof(object = mod,
#'                       y_new = y2,
#'                       mfdobj_x_new = mfdobj_x2)
#' plot_control_charts(cclist)
#'
regr_cc_sof <- function(object,
                        y_new,
                        mfdobj_x_new,
                        y_tuning = NULL,
                        mfdobj_x_tuning = NULL,
                        alpha = 0.05,
                        parametric_limits = FALSE,
                        include_covariates = FALSE,
                        absolute_error = FALSE) {

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


  if (!is.numeric(y_new)) {
    stop("y_new must be numeric.")
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
  nobsx <- dim(mfdobj_x_new$coefs)[2]
  nobsy <- length(y_new)
  if (nobsx != nobsy) {
    stop(paste0("y_new and mfdobj_x_new must have ",
                "the same number of observations."))
  }

  if (!(is.numeric(alpha) | is.list(alpha))) {
    stop("alpha must be either a number or a list")
  }

  if (is.numeric(alpha) & length(alpha) > 1) {
    stop("alpha must be a single number, between 0 and 1, or a list.")
  }

  if (is.numeric(alpha) & !all(alpha > 0 & alpha < 1)) {
    stop("alpha must be a single number, between 0 and 1, or a list.")
  }

  if (is.numeric(alpha)) {
    if (include_covariates) {
      alpha <- list(T2 = alpha / 3, spe = alpha / 3, y = alpha / 3)
    } else {
      alpha <- list(y = alpha)
    }
  }

  if (is.list(alpha)) {
    if (include_covariates) {
      if (!all(names(alpha) %in% c("T2", "spe", "y")) |
          !all(c("T2", "spe", "y") %in% names(alpha))) {
        stop("if alpha is a list and include_covariates is TRUE,
             it must be named with names T2, spe, y.")
      }
    }
    if (!include_covariates) {
      if (!all(names(alpha) %in% "y") |
          !all("y" %in% names(alpha))) {
        stop("if alpha is a list and include_covariates is FALSE,
             it must be named with name y.")
      }
    }
  }

  y_hat <- predict_sof_pc(object = object,
                          y_new = y_new,
                          mfdobj_x_new = mfdobj_x_new,
                          alpha = alpha$y)

  ret <- data.frame(
    y_hat,
    pred_err_sup = y_hat$upr - y_hat$fit,
    pred_err_inf = y_hat$lwr - y_hat$fit)

  if (!parametric_limits) {

    if (is.null(y_tuning) | is.null(mfdobj_x_tuning)) {

      fml <- stats::formula(object$mod)
      response_name <- all.vars(fml)[1]
      y_tuning <- object$mod$model[, response_name]
      mfdobj_x_tuning <- object$pca$data

    }

    y_hat_tuning <- predict_sof_pc(object = object,
                                   y_new = y_tuning,
                                   mfdobj_x_new = mfdobj_x_tuning,
                                   alpha = alpha$y)
    upr_lim <- as.numeric(stats::quantile(y_hat_tuning$pred_err, 1 - alpha$y/2))
    lwr_lim <- as.numeric(stats::quantile(y_hat_tuning$pred_err, alpha$y/2))

    ret$pred_err_sup <- upr_lim
    ret$pred_err_inf <- lwr_lim

  }


  if (include_covariates) {
    alpha_x <- alpha[c("T2", "spe")]
    ccpca <- control_charts_pca(pca = object$pca,
                                components = object$components,
                                tuning_data = mfdobj_x_tuning,
                                newdata = mfdobj_x_new,
                                alpha = alpha_x,
                                limits = "standard",
                                absolute_error = absolute_error)

    ret <- cbind(ccpca, ret)

  } else {
    ret <- cbind(data.frame(id = mfdobj_x_new$fdnames[[2]]), ret)
  }

  return(ret)

}



#' Functional Regression Control Chart
#'
#' It builds a data frame needed to plot the
#' Functional Regression Control Chart
#' introduced in Centofanti et al. (2021),
#' for monitoring a functional quality characteristic adjusted for
#' by the effect of multivariate functional covariates,
#' based on a fitted
#' function-on-function linear regression model.
#' The training data have already been used to fit the model.
#' An optional tuning data set can be provided that is used to estimate
#' the control chart limits.
#' A phase II data set contains the observations to be monitored
#' with the control charts.
#' It also allows to jointly monitor the multivariate functional covariates.
#'
#' @param object
#' A list obtained as output from \code{fof_pc},
#' i.e. a fitted function-on-function linear regression model.
#' @param mfdobj_y_new
#' An object of class \code{mfd} containing
#' the phase II data set of the functional response
#' observations to be monitored.
#' @param mfdobj_x_new
#' An object of class \code{mfd} containing
#' the phase II data set of the functional covariates
#' observations to be monitored.
#' @param mfdobj_y_tuning
#' An object of class \code{mfd} containing
#' the tuning data set of the functional response observations,
#' used to estimate the control chart limits.
#' If NULL, the training data, i.e. the data used to fit the
#' function-on-function linear regression model,
#' are also used as the tuning data set, i.e.
#' \code{mfdobj_y_tuning=object$pca_y$data}.
#' Default is NULL.
#' @param mfdobj_x_tuning
#' An object of class \code{mfd} containing
#' the tuning data set of the functional covariates observations,
#' used to estimate the control chart limits.
#' If NULL, the training data, i.e. the data used to fit the
#' function-on-function linear regression model,
#' are also used as the tuning data set, i.e.
#' \code{mfdobj_x_tuning=object$pca_x$data}.
#' Default is NULL.
#' @param alpha
#' If it is a number between 0 and 1,
#' it defines the overall type-I error probability.
#' By default, it is equal to 0.05 and the Bonferroni correction
#' is applied by setting the type-I error probabilities equal to
#' \code{alpha/2} in the Hotelling's T2 and SPE control charts.
#' If \code{include_covariates} is \code{TRUE}, i.e.,
#' the Hotelling's T2 and SPE control charts are built
#' also on the multivariate functional covariates, then the Bonferroni
#' correction is applied by setting the type-I error probability
#' in the four control charts equal to \code{alpha/4}.
#' If you want to set manually the Type-I error probabilities,
#' then the argument \code{alpha} must be a named list
#' with elements named as \code{T2}, \code{spe},
#' \code{T2_x} and, \code{spe_x}, respectively, containing
#' the desired Type I error probability of
#' the T2 and SPE control charts for the functional response and
#' the multivariate functional covariates, respectively.
#' @param include_covariates
#' If TRUE, also functional covariates are monitored through
#'  \code{control_charts_pca},.
#' If FALSE, only the functional response, conditionally on the covariates,
#' is monitored.
#' @param absolute_error
#' A logical value that, if \code{include_covariates} is TRUE, is passed
#' to \code{\link{control_charts_pca}}.
#'
#' @return
#' A \code{data.frame} containing the output of the
#' function \code{control_charts_pca} applied to
#' the prediction errors.
#' @export
#'
#' @seealso \code{\link{control_charts_pca}}
#'
#' @references
#' Centofanti F, Lepore A, Menafoglio A, Palumbo B, Vantini S. (2021)
#' Functional Regression Control Chart.
#' \emph{Technometrics}, 63(3):281--294. <doi:10.1080/00401706.2020.1753581>
#'
#' @examples
#' library(funcharts)
#' data("air")
#' air <- lapply(air, function(x) x[1:100, , drop = FALSE])
#' fun_covariates <- c("CO", "temperature")
#' mfdobj_x <- get_mfd_list(air[fun_covariates],
#'                          n_basis = 15,
#'                          lambda = 1e-2)
#' mfdobj_y <- get_mfd_list(air["NO2"],
#'                          n_basis = 15,
#'                          lambda = 1e-2)
#' mfdobj_y1 <- mfdobj_y[1:60]
#' mfdobj_y_tuning <- mfdobj_y[61:90]
#' mfdobj_y2 <- mfdobj_y[91:100]
#' mfdobj_x1 <- mfdobj_x[1:60]
#' mfdobj_x_tuning <- mfdobj_x[61:90]
#' mfdobj_x2 <- mfdobj_x[91:100]
#' mod_fof <- fof_pc(mfdobj_y1, mfdobj_x1)
#' cclist <- regr_cc_fof(mod_fof,
#'                       mfdobj_y_new = mfdobj_y2,
#'                       mfdobj_x_new = mfdobj_x2,
#'                       mfdobj_y_tuning = NULL,
#'                       mfdobj_x_tuning = NULL)
#' plot_control_charts(cclist)
#'
regr_cc_fof <- function(object,
                        mfdobj_y_new,
                        mfdobj_x_new,
                        mfdobj_y_tuning = NULL,
                        mfdobj_x_tuning = NULL,
                        alpha = 0.05,
                        include_covariates = FALSE,
                        absolute_error = FALSE) {

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

  if (!(is.numeric(alpha) | is.list(alpha))) {
    stop("alpha must be either a number or a list")
  }

  if (is.numeric(alpha) & length(alpha) > 1) {
    stop("alpha must be a single number, between 0 and 1, or a list.")
  }

  if (is.numeric(alpha) & !all(alpha > 0 & alpha < 1)) {
    stop("alpha must be a single number, between 0 and 1, or a list.")
  }

  if (is.list(alpha)) {
    if (include_covariates) {
      if (!all(names(alpha) %in% c("T2_x", "spe_x", "T2", "spe")) |
          !all(c("T2_x", "spe_x", "T2", "spe") %in% names(alpha))) {
        stop("if alpha is a list and include_covariates is TRUE,
             it must be named with names T2_x spe_x, T2, spe.")
      }
    }
    if (!include_covariates) {
      if (!all(names(alpha) %in% c("T2", "spe")) |
          !all(c("T2", "spe") %in% names(alpha))) {
        stop("if alpha is a list and include_covariates is FALSE,
             it must be named with names T2 and spe.")
      }
    }

    alpha_x <- alpha[c("T2_x", "spe_x")]
    alpha_y <- alpha[c("T2", "spe")]

  }

  if (is.numeric(alpha)) {
      alpha_y <- list(T2 = alpha / 2, spe = alpha / 2)
    if (include_covariates) {
      alpha_y <- list(T2 = alpha / 4, spe = alpha / 4)
      alpha_x <- list(T2_x = alpha / 4, spe_x = alpha / 4)
    }
  }

  if (is.null(mfdobj_y_tuning) | is.null(mfdobj_x_tuning)) {
    mfdobj_y_tuning <- object$pca_y$data
    mfdobj_x_tuning <- object$pca_x$data
  }
  tuning <- predict_fof_pc(
    object = object,
    mfdobj_y_new = mfdobj_y_tuning,
    mfdobj_x_new = mfdobj_x_tuning)

  phase_II <- predict_fof_pc(
    object = object,
    mfdobj_y_new = mfdobj_y_new,
    mfdobj_x_new = mfdobj_x_new)

  out <- control_charts_pca(
    pca = object$pca_res,
    components = object$components_res,
    tuning_data = tuning$pred_error,
    newdata = phase_II$pred_error,
    alpha = alpha_y,
    limits = "standard"
  )
  ret <- out %>%
    dplyr::select(-dplyr::contains("contribution_"))

  if (include_covariates) {
    alpha_x <- list(T2 = alpha_x$T2, spe = alpha_x$spe)
    ret_covariates <- control_charts_pca(
      pca = object$pca_x,
      components = object$components_x,
      tuning_data = mfdobj_x_tuning,
      newdata = mfdobj_x_new,
      alpha = alpha_x,
      limits = "standard",
      absolute_error = absolute_error
    ) %>%
      dplyr::rename("T2_x" = "T2",
                    "spe_x" = "spe",
                    "T2_lim_x" = "T2_lim",
                    "spe_lim_x" = "spe_lim")
    ret <- cbind(ret, dplyr::select(ret_covariates, -"id"))
  }

  return(ret)

}


