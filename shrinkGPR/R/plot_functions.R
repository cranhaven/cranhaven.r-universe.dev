#' Graphical summary of posterior of theta
#'
#' \code{plot.shrinkGPR} generates a boxplot visualizing the posterior distribution of
#' \code{theta} obtained from a fitted \code{shrinkGPR} object.
#'
#' @param x a \code{shrinkGPR} object.
#' @param nsamp a positive integer specifying the number of posterior samples to draw for plotting.
#' The default is \code{1000}.
#' @param ... further arguments passed to the internal \code{\link[graphics]{boxplot}} function,
#' such as axis labeling or plotting options. By default, \code{las = 2} is used unless explicitly
#' overridden by the user.
#' @return Called for its side effects. Returns \code{invisible(NULL)}.
#' @examples
#' \donttest{
#' # Simulate and fit a shrinkGPR model, then plot:
#' sim <- simGPR()
#' mod <- shrinkGPR(y ~ ., data = sim$data)
#' plot(mod)
#'
#' ## Change axis label orientation
#' plot(mod, las = 1)
#' }
#'
#' @author Peter Knaus \email{peter.knaus@@wu.ac.at}
#' @family plotting functions
#' @export
plot.shrinkGPR <- function(x, nsamp = 1000, ...) {
  if (int_input_bad(nsamp)) {
    stop("nsamp must be a positive integer")
  }

  args <- list(...)
  if (!"las" %in% names(args)) {
    args$las <- 2
  }
  post_samps <- gen_posterior_samples(x, nsamp = nsamp)

  do.call(boxplot, c(list(post_samps$thetas), args))

  invisible(NULL)
}


#' Graphical summary of posterior of theta
#'
#' \code{plot.shrinkTPR} generates a boxplot visualizing the posterior distribution of
#' \code{theta} obtained from a fitted \code{shrinkTPR} object.
#'
#' @param x a \code{shrinkTPR} object.
#' @param nsamp a positive integer specifying the number of posterior samples to draw for plotting.
#' The default is \code{1000}.
#' @param ... further arguments passed to the internal \code{\link[graphics]{boxplot}} function,
#' such as axis labeling or plotting options. By default, \code{las = 2} is used unless explicitly
#' overridden by the user.
#' @return Called for its side effects. Returns \code{invisible(NULL)}.
#' @examples
#' \donttest{
#' # Simulate and fit a shrinkTPR model, then plot:
#' sim <- simGPR()
#' mod <- shrinkTPR(y ~ ., data = sim$data)
#' plot(mod)
#'
#' ## Change axis label orientation
#' plot(mod, las = 1)
#' }
#'
#' @author Peter Knaus \email{peter.knaus@@wu.ac.at}
#' @family plotting functions
#' @export
plot.shrinkTPR <- function(x, nsamp = 1000, ...) {
  plot.shrinkGPR(x, nsamp = nsamp, ...)
}

#' Plot method for 1D marginal predictions
#'
#' @description Generates a plot of 1D conditional predictive samples produced by \code{\link{gen_marginal_samples}}
#' with a single covariate.
#'
#' @param x An object of class \code{"shrinkGPR_marg_samples_1D"}, typically returned by
#' \code{\link{gen_marginal_samples}} when providing a single covariate to sweep over.
#' @param ... Additional arguments passed to \link[shrinkTVP]{plot.mcmc.tvp} for customizing the plot,
#' such as axis labels or plotting options.
#'
#' @details
#' By default, the function visualizes the posterior predictive median and 95% and 50% credible intervals
#' for the selected covariate across a grid of evaluation points.
#' Axis labels are automatically inferred if not explicitly provided.
#'
#' Note: The \pkg{shrinkTVP} package must be installed to use this function.
#'
#' @return Called for its side effects. Returns \code{invisible(NULL)}.
#' @seealso \code{\link{gen_marginal_samples}}
#'
#' @examples
#' \donttest{
#' # Simulate data
#' set.seed(123)
#' torch::torch_manual_seed(123)
#' n <- 100
#' x <- matrix(runif(n * 2), n, 2)
#' y <- sin(2 * pi * x[, 1]) + rnorm(n, sd = 0.1)
#' data <- data.frame(y = y, x1 = x[, 1], x2 = x[, 2])
#'
#' # Fit GPR model
#' res <- shrinkGPR(y ~ x1 + x2, data = data)
#'
#' # Generate marginal samples
#' marginal_samps_x1 <- gen_marginal_samples(res, to_eval = "x1", nsamp = 100)
#' marginal_samps_x2 <- gen_marginal_samples(res, to_eval = "x2", nsamp = 100)
#'
#' # Plot marginal predictions
#' plot(marginal_samps_x1)
#' plot(marginal_samps_x2)
#'
#' # Customize plot appearance (see plot.mcmc.tvp from shrinkTVP package for more options)
#' plot(marginal_samps_x2, shaded = FALSE, quantlines = TRUE, quantcol = "red")
#'}
#'
#' @author Peter Knaus \email{peter.knaus@@wu.ac.at}
#' @family plotting functions
#' @export
plot.shrinkGPR_marg_samples_1D <- function(x, ...) {

  if (!requireNamespace("shrinkTVP", quietly = TRUE)) {
    stop("The 'shrinkTVP' package is required for this function. Please install it with install.packages('shrinkTVP').")
  }

  plot_tmp <- getFromNamespace("plot.mcmc.tvp", "shrinkTVP")


  if (!inherits(x, "shrinkGPR_marg_samples_1D")) {
    stop("x must be a shrinkGPR_marg_samples_1D object")
  }

  args <- list(...)

  if (!"xlab" %in% names(args)) {
    args$xlab <- attr(x, "to_eval")
  }

  if (!"ylab" %in% names(args)) {
    args$ylab <- attr(x, "response")
  }


  args$x <- x$mean_pred
  attr(args$x, "index") <-  x$grid
  do.call(plot_tmp, args)

  invisible(NULL)
}



#' Plot method for 2D marginal predictions from \code{shrinkGPR}
#'
#' @description
#' Generates a 3D surface plot of 2D conditional predictive samples produced by \code{\link{gen_marginal_samples}}.
#'
#' @param x An object of class \code{"shrinkGPR_marg_samples_2D"}, typically returned by
#' \code{\link{gen_marginal_samples}} when evaluating two covariates.
#' @param ... Additional arguments passed to \code{\link[plotly]{add_surface}} for customizing the appearance
#' of the surface plot (e.g., \code{opacity}, \code{colorscale}, \code{showscale}).
#'
#' @details
#' The function visualizes the posterior predictive mean across a 2D grid of evaluation points for two covariates.
#' Interactive plotting is handled via the \pkg{plotly} package. Axis labels are automatically inferred from the object.
#'
#' Note: The \pkg{plotly} package must be installed to use this function.
#'
#' @return A \code{plotly} plot object (interactive 3D surface). Can be further customized via pipes and \code{plotly} functions.
#'
#' @seealso \code{\link{gen_marginal_samples}}, \code{\link[plotly]{plot_ly}}, \code{\link[plotly]{add_surface}}
#'
#' @examples
#' \donttest{
#' # Simulate data
#' set.seed(123)
#' torch::torch_manual_seed(123)
#' n <- 100
#' x <- matrix(runif(n * 2), n, 2)
#' y <- sin(2 * pi * x[, 1]) * cos(2 * pi * x[, 2]) + rnorm(n, sd = 0.1)
#' data <- data.frame(y = y, x1 = x[, 1], x2 = x[, 2])
#'
#' # Fit GPR model
#' res <- shrinkGPR(y ~ x1 + x2, data = data)
#'
#' # Generate marginal predictions for x1 and x2 jointly
#' marginal_samps_both <- gen_marginal_samples(res, to_eval = c("x1", "x2"), nsamp = 100)
#'
#' # Plot interactive surface
#' plot(marginal_samps_both)
#'
#' # Customize surface appearance
#' plot(marginal_samps_both, opacity = 0.8, colorscale = "Magma")
#'
#' # Customize further via plotly
#' p <- plot(marginal_samps_both, opacity = 0.8, colorscale = "Magma")
#'
#' p |> plotly::layout(
#'   scene = list(
#'     xaxis = list(title = "Covariate 1"),
#'     yaxis = list(title = "Covariate 2"),
#'     zaxis = list(title = "Expected value")
#'   )
#' )
#'}
#'
#' @author Peter Knaus \email{peter.knaus@@wu.ac.at}
#' @family plotting functions
#' @export
plot.shrinkGPR_marg_samples_2D <- function(x, ...) {

  if (!requireNamespace("plotly", quietly = TRUE)) {
    stop("The 'plotly' package is required for this function. Please install it with install.packages('plotly').")
  }

  if (!inherits(x, "shrinkGPR_marg_samples_2D")) {
    stop("x must be a shrinkGPR_marg_samples_2D object.")
  }

  mean_surface <- t(apply(x$mean_pred, c(2, 3), median))

  to_eval <- attr(x, "to_eval")
  resp_var <- attr(x, "response")

  plotly::plot_ly(
    x = x$grid$grid1,
    y = x$grid$grid2,
    z = mean_surface
  ) |>
    plotly::add_surface(...) |>
    plotly::layout(
      scene = list(
        xaxis = list(title = to_eval[1]),
        yaxis = list(title = to_eval[2]),
        zaxis = list(title = resp_var)
      )
    )
}
