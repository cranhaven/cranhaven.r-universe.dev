#' Diagnostic, Density and Contour Plots
#'
#' @param x an object of class \code{dsspMod}
#' @param robust_residuals whether to use robust residuals (median of predicted).
#'   Default to be \code{TRUE}.
#' @param contour_plots whether or not to return a second panel with contour plots.
#'   Defaults to \code{TRUE}
#' @param nx dimension of output grid in x direction.
#'   Used for interpolation (\code{akime::interp()}).
#' @param ny dimension of output grid in y direction.
#'   Used for interpolation (\code{akime::interp()}).
#' @param nlevels number of levels used in contour plot.
#' @param ... additional arguments that are passed to \code{ggplot2::scale_fill_distiller()}.
#'
#' @return a list containing the plots printed (individually and together in grid)
#' @export
#'
#' @examples
#' library(sp)
#' library(gstat)
#' data(meuse.all)
#' coordinates(meuse.all) <- ~ x + y
#'
#' f <- function(x) -x ## log-prior for exponential distribution for the smoothing parameter
#'
#' ## Draw 100 samples from the posterior of eta given the data y.
#' OUTPUT <- DSSP(
#'   formula = log(zinc) ~ 1, data = meuse.all, N = 100,
#'   pars = c(0.001, 0.001), log_prior = f
#' )
#' plot(OUTPUT, contour_plots = FALSE)
plot.dsspMod <- function(x,
                         robust_residuals = TRUE,
                         contour_plots = TRUE,
                         nx = 100, ny = 100, nlevels = 5,
                         ...) {
  if (!inherits(x, "dsspMod")) stop("use only with \"dsspMod\" objects")

  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("'ggplot2' is required for generating plots")
  }

  if (!requireNamespace("cowplot", quietly = TRUE)) {
    message("'cowplot' is required for arranging plots into a grid.\nPlots will be returned individually.\n\n")
    make_grid <- FALSE
  } else {
    make_grid <- TRUE
  }

  if (!requireNamespace("interp", quietly = TRUE) & contour_plots) {
    message("'interp' is required for making contour plots but isn't installed.\nSkipping contour plots.\n\n")
    contour_plots <- FALSE
  }

  r <- residuals.dsspMod(x, robust = robust_residuals)
  ylim <- range(r)
  ypred <- predict.dsspMod(x)
  metric <- ifelse(robust_residuals, stats::median, mean)
  yh <- apply(ypred, 1, metric)
  y <- reverse_scaling(x$Y, x$y_scaling)
  n <- eta <- delta <- z <- values <- NULL
  df_params <- data.frame(eta = x$eta, delta = x$delta)
  df_params$n <- seq.int(nrow(df_params))

  resid_vs_fitted <-
    ggplot2::ggplot() +
    ggplot2::geom_point(ggplot2::aes(x = yh, y = r)) +
    ggplot2::geom_smooth(ggplot2::aes(x = yh, y = r), se = FALSE, col = "black", method = "loess") +
    ggplot2::labs(
      title = "Residuals vs fitted values",
      x = paste("fitted", x$dep_var),
      y = "residuals"
    )

  predicted_vs_actual <-
    ggplot2::ggplot() +
    ggplot2::geom_point(ggplot2::aes(y, yh)) +
    ggplot2::geom_abline(slope = 1, intercept = 0) +
    ggplot2::labs(
      title = "Observed vs fitted values",
      x = paste("observed", x$dep_var),
      y = paste("fitted", x$dep_var)
    )

  eta_density <-
    ggplot2::ggplot() +
    ggplot2::geom_density(ggplot2::aes(x$eta)) +
    ggplot2::labs(title = expression("Posterior Density of " * eta), x = expression(eta))

  eta_trace <-
    ggplot2::ggplot(data = df_params) +
    ggplot2::geom_line(ggplot2::aes(x = n, y = eta)) +
    ggplot2::labs(title = expression("Traceplot of " * eta), y = expression(eta))

  delta_density <-
    ggplot2::ggplot() +
    ggplot2::geom_density(ggplot2::aes(x$delta)) +
    ggplot2::labs(title = expression("Posterior Density of " * delta), x = expression(delta))

  delta_trace <-
    ggplot2::ggplot(data = df_params) +
    ggplot2::geom_line(ggplot2::aes(x = n, y = delta)) +
    ggplot2::labs(title = expression("Traceplot of " * delta), y = expression(delta))

  diagnostic_plots <- list(
    resid_vs_fitted = resid_vs_fitted,
    predicted_vs_actual = predicted_vs_actual,
    eta_density = eta_density,
    eta_trace = eta_trace,
    delta_density = delta_density,
    delta_trace = delta_trace
  )

  if (contour_plots) {
    interp_y <- interp::interp(x$coords[, 1], x$coords[, 2], y, nx = nx, ny = ny)
    interp_df <- stats::na.omit(as.data.frame(interp::interp2xyz(interp_y)))

    contour <-
      ggplot2::ggplot(data = interp_df, ggplot2::aes(x = x, y = y, z = z)) +
      ggplot2::geom_contour(col = "black", bins = nlevels) +
      ggplot2::labs(
        x = colnames(x$coords)[1],
        y = colnames(x$coords)[2],
        title = "Contour plot"
      )

    filled_contour <-
      ggplot2::ggplot(data = interp_df, ggplot2::aes(x = x, y = y, fill = z)) +
      ggplot2::geom_tile() +
      ggplot2::scale_fill_distiller(...) +
      ggplot2::labs(
        x = colnames(x$coords)[1],
        y = colnames(x$coords)[2],
        title = "Filled contour plot",
        fill = x$dep_var
      ) +
      ggplot2::theme(legend.position = "bottom")


    diagnostic_plots_return <- c(
      diagnostic_plots, list(contour = contour, filled_contour = filled_contour)
    )

    filled_contour_no_legend <- filled_contour + ggplot2::theme(legend.position = "none")

    diagnostic_plots_grid <- c(
      diagnostic_plots,
      list(contour = contour, filled_contour = filled_contour_no_legend)
    )

    if (make_grid) {
      plots <- cowplot::plot_grid(plotlist = diagnostic_plots_grid, ncol = 2)
      legend <- cowplot::get_legend(filled_contour)
      diagnostic_grid <- cowplot::plot_grid(plots, legend, rel_heights = c(1, 0.12), ncol = 1)
      print(diagnostic_grid)
    }
  } else if (make_grid) {
    diagnostic_plots_grid <- diagnostic_plots_return <- diagnostic_plots
    diagnostic_grid <- cowplot::plot_grid(plotlist = diagnostic_plots_grid, ncol = 2)
    print(diagnostic_grid)
  }

  covariates_plots <- list()
  for (i in 1:nrow(x$covariates_posterior)) {
    cov_name <- rownames(x$covariates_posterior)[i]
    df_i <- data.frame(values = x$covariates_posterior[i, ])
    df_i$n <- seq.int(nrow(df_i))

    trace_plot <-
      ggplot2::ggplot(data = df_i, ggplot2::aes(x = n, y = values)) +
      ggplot2::geom_line() +
      ggplot2::labs(y = cov_name)

    density_plot <-
      ggplot2::ggplot(data = df_i) +
      ggplot2::geom_density(ggplot2::aes(x = values)) +
      ggplot2::labs(x = cov_name)

    new_plots <- list(density_plot = density_plot, trace_plot = trace_plot)
    names(new_plots) <- paste(cov_name, names(new_plots), sep = "_")

    covariates_plots <- c(covariates_plots, new_plots)
  }

  if (make_grid) {
    title <- cowplot::ggdraw() + 
      cowplot::draw_label(
        "Posterior density and trace plots for model covariates",
        fontface = 'bold',
        x = 0,
        hjust = 0
      ) +
      ggplot2::theme(
        plot.margin = ggplot2::margin(0, 0, 0, 7)
      )
    covariates_grid <- cowplot::plot_grid(plotlist = covariates_plots, ncol = 2)
    covariates_grid <- cowplot::plot_grid(title, covariates_grid, ncol = 1, rel_heights = c(0.08, 1))
    print(covariates_grid)
    res <- list(
      diagnostic_plots = diagnostic_plots_return,
      covariates_plots = covariates_plots,
      diagnostic_grid = diagnostic_grid,
      covariates_grid = covariates_grid
    )
  } else {
    for (gg in c(diagnostic_plots_return, covariates_plots)) {
      print(gg)
    }
    res <- list(
      diagnostic_plots = diagnostic_plots_return,
      covariates_plots = covariates_plots
    )
  }

  invisible(res)
}
