#' @import stats
#' @import ggplot2
#' @import compiler

get_CDF_full_BNPdensity <- function(fit, xs = seq(-5, 5, length.out = 100)) {
  pmix_vec_loop(qs = xs, locations_list = fit$means, scales_list = fit$sigmas, weights_list = fit$weights, distr.k = fit$distr.k)
}
get_PDF_full_BNPdensity <- function(fit, xs = seq(-5, 5, length.out = 100)) {
  dmix_vec_loop(xs = xs, locations_list = fit$means, scales_list = fit$sigmas, weights_list = fit$weights, distr.k = fit$distr.k)
}
get_quantiles_full_BNPdensity <- function(fit, ps = seq(-5, 5, length.out = 100), thinning_to = 500) {
  it_retained <- compute_thinning_grid(length(fit$means), thinning_to = thinning_to)
  qmix_vec_loop(
    ps = ps,
    locations_list = fit$means[it_retained],
    scales_list = fit$sigmas[it_retained],
    weights_list = fit$weights[it_retained],
    distr.k = fit$distr.k
  )
}

get_CDF_semi_BNPdensity <- function(fit, xs = seq(-5, 5, length.out = 100)) {
  fit$sigmas_filled <- fill_sigmas(fit)
  pmix_vec_loop(qs = xs, locations_list = fit$means, scales_list = fit$sigmas_filled, weights_list = fit$weights, distr.k = fit$distr.k)
}
get_PDF_semi_BNPdensity <- function(fit, xs = seq(-5, 5, length.out = 100)) {
  fit$sigmas_filled <- fill_sigmas(fit)
  dmix_vec_loop(xs = xs, locations_list = fit$means, scales_list = fit$sigmas_filled, weights_list = fit$weights, distr.k = fit$distr.k)
}
get_quantiles_semi_BNPdensity <- function(fit, ps = seq(-5, 5, length.out = 100), thinning_to = 500) {
  fit$sigmas_filled <- fill_sigmas(fit)
  it_retained <- compute_thinning_grid(length(fit$means), thinning_to = thinning_to)
  qmix_vec_loop(
    ps = ps,
    locations_list = fit$means[it_retained],
    scales_list = fit$sigmas[it_retained],
    weights_list = fit$weights[it_retained],
    distr.k = fit$distr.k
  )
}



#' Plot the empirical and fitted CDF for non censored data.
#'
#'
#' @param fit The result of the fit, obtained through the function MixNRMI1 or
#' MixNRMI2.
#' @return Plot of the empirical and fitted CDF for non censored data.
#' @examples
#'
#' set.seed(150520)
#' data(acidity)
#' out <- MixNRMI1(acidity, extras = TRUE, Nit = 10)
#' BNPdensity:::plotCDF_noncensored(out)
plotCDF_noncensored <- function(fit) {
  data <- fit$data

  grid <- grid_from_data(data)

  if (is_semiparametric(fit)) {
    cdf <- get_CDF_semi_BNPdensity(fit = fit, xs = grid)
  } else {
    cdf <- get_CDF_full_BNPdensity(fit = fit, xs = grid)
  }
  ggplot2::ggplot(data = data.frame(data = grid, CDF = cdf), aes_string(x = "data", y = "CDF")) +
    geom_line(color = "red") +
    theme_classic() +
    stat_ecdf(data = data.frame(data), aes(y = NULL), geom = "step") +
    xlab("Data")
}



#' Plot the Turnbull CDF and fitted CDF for censored data.
#'
#'
#' @param fit The result of the fit, obtained through the function MixNRMI1cens
#' or MixNRMI2cens.
#' @return Plot of the empirical and fitted CDF for non censored data.
#' @examples
#'
#' set.seed(150520)
#' data(salinity)
#' out <- MixNRMI1cens(salinity$left, salinity$right, extras = TRUE, Nit = 100)
#' BNPdensity:::plotCDF_censored(out)
plotCDF_censored <- function(fit) {
  data <- fit$data

  grid <- grid_from_data(data)

  Survival_object <- survival::survfit(formula = survival::Surv(data$left, data$right, type = "interval2") ~ 1)

  if (is_semiparametric(fit)) {
    cdf <- get_CDF_semi_BNPdensity(fit = fit, xs = grid)
  } else {
    cdf <- get_CDF_full_BNPdensity(fit = fit, xs = grid)
  }
  ggplot2::ggplot(data = data.frame(data = grid, CDF = cdf), aes_string(x = "data", y = "CDF")) +
    geom_line(color = "red") +
    theme_classic() +
    geom_step(data = data.frame(x = c(Survival_object$time, max(grid)), y = c(1 - Survival_object$surv, 1)), aes_string(x = "x", y = "y")) +
    xlab("Data")
}



#' Plot the density and a histogram for non censored data.
#'
#' @param fit The result of the fit, obtained through the function MixNRMI1 or
#' MixNRMI2.
#' @return Plot of the density and a histogram for non censored data.
#' @examples
#'
#' set.seed(150520)
#' data(acidity)
#' out <- MixNRMI1(acidity, extras = TRUE, Nit = 100)
#' BNPdensity:::plotPDF_noncensored(out)
plotPDF_noncensored <- function(fit) {
  p <- plotPDF_censored(fit)
  p$layers <- c(geom_histogram(data = data.frame(data = fit$data), aes_string(y = "..density..")), p$layers)
  return(p)
}



#' Plot the density for censored data.
#'
#'
#' @param fit The result of the fit, obtained through the function MixNRMI1cens
#' or MixNRMI2cens.
#' @return Plot of the density and a histogram for non censored data.
#' @examples
#'
#' set.seed(150520)
#' data(salinity)
#' out <- MixNRMI1cens(xleft = salinity$left, xright = salinity$right, extras = TRUE, Nit = 100)
#' BNPdensity:::plotPDF_censored(out)
plotPDF_censored <- function(fit) {
  grid <- grid_from_data(fit$data)

  if (is_semiparametric(fit)) {
    pdf <- get_PDF_semi_BNPdensity(fit = fit, xs = grid)
  } else {
    pdf <- get_PDF_full_BNPdensity(fit = fit, xs = grid)
  }
  ggplot2::ggplot(data = data.frame(data = grid, PDF = pdf), aes_string(x = "data", y = "PDF")) +
    geom_line(color = "red") +
    theme_classic() +
    xlab("Data")
}




#' Plot the percentile-percentile graph for non censored data.
#'
#'
#' @param fit The result of the fit, obtained through the function MixNRMI1 or
#' MixNRMI2.
#' @return Percentile-percentile plot for non censored data.
#' @examples
#'
#' set.seed(150520)
#' data(acidity)
#' out <- MixNRMI1(acidity, extras = TRUE, Nit = 100)
#' BNPdensity:::pp_plot_noncensored(out)
pp_plot_noncensored <- function(fit) {
  data <- fit$data

  if (is_semiparametric(fit)) {
    cdf <- get_CDF_semi_BNPdensity(fit = fit, xs = data)
  } else {
    cdf <- get_CDF_full_BNPdensity(fit = fit, xs = data)
  }
  ggplot2::ggplot(data = data.frame(x = cdf, y = ecdf(data)(data)), aes_string(x = "x", y = "y")) +
    geom_point() +
    geom_abline(slope = 1, intercept = 0, color = "red") +
    theme_classic() +
    xlab("Theoretical percentiles") +
    ylab("Empirical percentiles")
}



#' Plot the quantile-quantile graph for non censored data.
#'
#' This function may be rather slow for many iterations/many data because it
#' relies on numerical inversion of the mixture Cumulative Distribution
#' Function.
#'
#' @param fit The result of the fit, obtained through the function MixNRMI1 or
#' MixNRMI2, MixMRMI1cens or MixMRMI2cens
#' @param thinning_to How many iterations to compute the mean posterior
#' quantiles
#' @return quantile-quantile plot for non censored data.
#' @examples
#'
#'
#' ### Not run
#' # set.seed(150520)
#' # data(acidity)
#' # out <- MixNRMI1(acidity, extras = TRUE, Nit = 100)
#' # BNPdensity:::qq_plot_noncensored(out)
qq_plot_noncensored <- function(fit, thinning_to = 500) {
  data <- sort(fit$data)
  ndat <- length(data)
  percentiles_to_compute <- 1:ndat / (ndat + 1)

  if (is_semiparametric(fit)) {
    theoretical_quantiles <- get_quantiles_semi_BNPdensity(fit = fit, ps = percentiles_to_compute, thinning_to = thinning_to)
  } else {
    theoretical_quantiles <- get_quantiles_full_BNPdensity(fit = fit, ps = percentiles_to_compute, thinning_to = thinning_to)
  }
  ggplot2::ggplot(data = data.frame(x = theoretical_quantiles, y = data), aes_string(x = "x", y = "y")) +
    geom_point() +
    geom_abline(slope = 1, intercept = 0, color = "red") +
    theme_classic() +
    xlab("Theoretical quantiles") +
    ylab("Empirical quantiles")
}



#' Plot the percentile-percentile graph for non censored data, using the
#' Turnbull estimator the position of the percentiles.
#'
#'
#' @param fit The result of the fit, obtained through the function MixNRMI1cens
#' or MixNRMI2cens.
#' @return Percentile-percentile graph using the Turnbull estimator
#' @examples
#'
#' set.seed(150520)
#' data(salinity)
#' out <- MixNRMI1cens(xleft = salinity$left, xright = salinity$right, extras = TRUE, Nit = 100)
#' BNPdensity:::pp_plot_censored(out)
pp_plot_censored <- function(fit) {
  Survival_object <- survival::survfit(formula = survival::Surv(fit$data$left, fit$data$right, type = "interval2") ~ 1)
  estimated_data <- Survival_object$time

  if (is_semiparametric(fit)) {
    cdf <- get_CDF_semi_BNPdensity(fit = fit, xs = estimated_data)
  } else {
    cdf <- get_CDF_full_BNPdensity(fit = fit, xs = estimated_data)
  }
  ggplot2::ggplot(data = data.frame(x = cdf, y = 1 - Survival_object$surv), aes_string(x = "x", y = "y")) +
    geom_point() +
    geom_abline(slope = 1, intercept = 0, color = "red") +
    theme_classic() +
    xlab("Theoretical percentiles") +
    ylab("Empirical percentiles (Turnbull)")
}

# min_greater_than_0 = function(x) min(x[x>0])
which_min_greater_than_0 <- function(x) which.min(ifelse(test = x < 0, yes = Inf, no = x))

compute_quantiles_from_Turnbull_estimate <- function(Survival_object) {
  cdf <- 1 - Survival_object$surv
  grid <- Survival_object$time
  ndat <- length(grid)
  percentiles_to_compute <- 1:ndat / (ndat + 1)
  return(sapply(percentiles_to_compute, function(p) grid[which_min_greater_than_0(cdf - p)]))
}



#' Plot the quantile-quantile graph for censored data.
#'
#' This function may be rather slow for many iterations/many data because it
#' relies on numerical inversion of the mixture Cumulative Distribution
#' Function. set.seed(150520) data(salinity) out <- MixNRMI1cens(xleft =
#' salinity$left, xright = salinity$right, extras = TRUE, Nit = 100)
#' BNPdensity:::qq_plot_censored(out)
#'
#' @param fit The result of the fit, obtained through the function MixNRMI1 or
#' MixNRMI2, MixMRMI1cens or MixMRMI2cens
#' @param thinning_to How many iterations to compute the mean posterior
#' quantiles
#' @return quantile-quantile plot for non censored data.
qq_plot_censored <- function(fit, thinning_to = 500) {
  # Survival_object <- survival::survfit(formula = survival::Surv(fit$data$left, fit$data$right, type = "interval2") ~ 1)
  # estimated_data <- sort(Survival_object$time)
  Turnbull_quantiles <- compute_quantiles_from_Turnbull_estimate(survival::survfit(formula = survival::Surv(fit$data$left, fit$data$right, type = "interval2") ~ 1))

  ndat <- length(Turnbull_quantiles)
  percentiles_to_compute <- 1:ndat / (ndat + 1)

  if (is_semiparametric(fit)) {
    theoretical_quantiles <- get_quantiles_semi_BNPdensity(fit = fit, ps = percentiles_to_compute, thinning_to = thinning_to)
  } else {
    theoretical_quantiles <- get_quantiles_full_BNPdensity(fit = fit, ps = percentiles_to_compute, thinning_to = thinning_to)
  }
  ggplot2::ggplot(data = data.frame(x = theoretical_quantiles, y = Turnbull_quantiles), aes_string(x = "x", y = "y")) +
    geom_point() +
    geom_abline(slope = 1, intercept = 0, color = "red") +
    theme_classic() +
    xlab("Theoretical quantiles") +
    ylab("Empirical quantiles (Turnbull)")
}



#' Plot Goodness of fits graphical checks for non censored data
#'
#'
#' @param fit The result of the fit, obtained through the function MixNRMI1 or
#' MixNRMI2, MixMRMI1cens or MixMRMI2cens
#' @param qq_plot Whether to compute the QQ-plot
#' @param thinning_to How many iterations to compute the mean posterior
#' quantiles
#' @return A density plot with histogram, a cumulative density plot with the
#' empirical cumulative distribution, and a percentile-percentile plot.
#' @examples
#'
#' set.seed(150520)
#' data(acidity)
#' out <- MixNRMI1(acidity, extras = TRUE, Nit = 100)
#' BNPdensity:::GOFplots_noncensored(out)
GOFplots_noncensored <- function(fit, qq_plot = FALSE, thinning_to = 500) {
  CDFplot <- plotCDF_noncensored(fit)
  PDFplot <- plotPDF_noncensored(fit)
  pplot <- pp_plot_noncensored(fit)
  if (qq_plot) {
    qqplot <- qq_plot_noncensored(fit, thinning_to = thinning_to)
    gridExtra::grid.arrange(PDFplot, CDFplot, pplot, qqplot)
  } else {
    gridExtra::grid.arrange(PDFplot, CDFplot, pplot)
  }
}



#' Plot Goodness of fits graphical checks for censored data
#'
#'
#' @param fit The result of the fit, obtained through the function MixNRMI1 or
#' MixNRMI2, MixMRMI1cens or MixMRMI2cens
#' @param qq_plot Whether to compute the QQ-plot
#' @param thinning_to How many iterations to compute the mean posterior
#' quantiles
#' @return A density plot, a cumulative density plot with the Turnbull
#' cumulative distribution, and a percentile-percentile plot.
#' @examples
#'
#' set.seed(150520)
#' data(salinty)
#' out <- MixNRMI1cens(salinity$left, salinity$right, extras = TRUE, Nit = 100)
#' BNPdensity:::GOFplots_censored(out)
GOFplots_censored <- function(fit, qq_plot = FALSE, thinning_to = 500) {
  CDFplot <- plotCDF_censored(fit)
  PDFplot <- plotPDF_censored(fit)
  pplot <- pp_plot_censored(fit)
  if (qq_plot) {
    qqplot <- qq_plot_censored(fit, thinning_to = thinning_to)
    gridExtra::grid.arrange(PDFplot, CDFplot, pplot, qqplot)
  } else {
    gridExtra::grid.arrange(PDFplot, CDFplot, pplot)
  }
}



#' Plot Goodness of fits graphical checks for censored data
#'
#'
#' @param fit The result of the fit, obtained through the function MixNRMI1 or
#' MixNRMI2, MixMRMI1cens or MixMRMI2cens
#' @param qq_plot Whether to compute the QQ-plot
#' @param thinning_to How many iterations to compute the mean posterior
#' quantiles
#' @return A density plot, a cumulative density plot with the Turnbull
#' cumulative distribution, a percentile-percentile plot, and potentially a
#' quantile-quantile plot.
#' @examples
#'
#' set.seed(150520)
#' data(salinity)
#' out <- MixNRMI1cens(salinity$left, salinity$right, extras = TRUE, Nit = 100)
#' GOFplots(out)
#' @export GOFplots
GOFplots <- function(fit, qq_plot = FALSE, thinning_to = 500) {
  if (is_censored(fit$data)) {
    GOFplots_censored(fit, qq_plot = qq_plot, thinning_to = 500)
  } else {
    GOFplots_noncensored(fit, qq_plot = qq_plot, thinning_to = 500)
  }
}
