#' Plot Log-Likelihood vs. Values of One Parameter
#'
#' Generates plot of log-likelihood vs. one parameter of interest while other
#' parameters are held fixed at certain values (e.g. MLEs). This is not a 
#' profile likelihood, and is mainly intended for use with a Shiny app.
#' 
#' Note that \code{objective} should be the negative log-likelihood function, 
#' since internal optimization uses (\code{\link[stats]{nlminb}}), which does 
#' minimization.
#'
#' @param start See \code{\link[stats]{nlminb}}.
#' @param objective See \code{\link[stats]{nlminb}}.
#' @param lower See \code{\link[stats]{nlminb}}.
#' @param upper See \code{\link[stats]{nlminb}}.
#' @param xaxis_param Integer value specifying which parameter should be plotted
#' on the x-axis.
#' @param xaxis_range Numeric vector specifying x-axis range over which to vary 
#' the parameter of interest. Only values with likelihood ratio > 0.01 are 
#' ultimately plotted.
#' @param param_values Numeric vector of values to use for other parameters in
#' model, in case you want an additional curve for log-likelihood function vs.
#' parameter of interest at certain non-MLE values for other parameters. For
#' example, if there are 3 parameters and \code{xaxis_param = 2}, you could set
#' \code{param_values = c(0, NA, 0)}.
#' @param mles Numeric vector of previously obtained maximum likelihood
#' estimates.
#' @param return_info Logical value for whether to return the estimated MLEs and 
#' 99.99\% confidence intervals for parameters rather than create the plot.
#'
#'
#' @return Plot of log-likelihood vs. value of parameter of interest, generated
#' by \code{\link[ggplot2]{ggplot}}.
#'
#'
#' @examples
#' # Generate normal data, define log-likelihood function, and plot likelihood
#' set.seed(123)
#' x <- rnorm(100, mean = 0.5, sd = sqrt(0.25))
#' ll.f <- function(theta) {
#'   return(-sum(dnorm(x, log = TRUE, mean = theta[1], sd = sqrt(theta[2]))))
#' }
#' plot_ll(start = c(0, 1), objective = ll.f, lower = c(-Inf, 1e-6))
#'
#'
#' @export
plot_ll <- function(start,
                    objective,
                    lower = -Inf,
                    upper = Inf,
                    xaxis_param = 1,
                    xaxis_range = NULL, 
                    param_values = NULL,
                    mles = NULL,
                    return_info = FALSE) {

  # Maximize log-likelihood if necessary
  mles.unspecified <- is.null(mles)
  if (mles.unspecified) {
    llmax <- nlminb(objective = objective, start = start, 
                    lower = lower, upper = upper, control = list(trace = 1))
    llval <- -llmax$objective
    mles <- llmax$par
  } else {
    llval <- -objective(mles)
  }
  p <- length(mles)

  if (is.null(xaxis_range) | return_info) {

    # Estimate variance-covariance matrix
    hessian.mat <- pracma::hessian(f = objective, x0 = mles)
    varcov <- solve(hessian.mat)

    # Calculate 99.99% confidence interval, to determine range to plot over
    ses <- sqrt(diag(varcov))
    x1 <- (mles - qnorm(p = 0.9999) * ses)
    x2 <- (mles + qnorm(p = 0.9999) * ses)
    xaxis_range <- c(x1[xaxis_param], x2[xaxis_param])

  }

  # Return mles and xaxis_range if requested
  if (return_info) {
    return(list(mles = mles, param_range = data.frame(lower = x1, upper = x2)))
  }

  # Prepare (X, Y) data for ggplot
  xvals <- seq(xaxis_range[1], xaxis_range[2], (diff(xaxis_range)) / 1000)
  thetas <- matrix(mles, byrow = TRUE, ncol = p, nrow = 1001)
  thetas[, xaxis_param] <- xvals
  yvals <- -apply(thetas, 1, objective)
  x <- y <- NULL
  df <- data.frame(curve = rep(1, 1001), x = xvals, y = yvals)

  # # Figure out xvals corresponding to 95% CI
  # twoll <- 2 * (llval - yvals)
  # xintercepts <-
  #   c(xvals[which.min(abs(twoll - qchisq(p = 0.95, df = 1)) *
  #                       ifelse(xvals < mles[xaxis_param], 1, NA))],
  #     xvals[which.min(abs(twoll - qchisq(p = 0.95, df = 1)) *
  #                       ifelse(xvals > mles[xaxis_param], 1, NA))])
  
  # Add (X, Y) for additional curve if requested
  if (! is.null(param_values)) {
    thetas <- matrix(param_values, byrow = TRUE, ncol = p, nrow = 1001)
    thetas[, xaxis_param] <- xvals
    yvals <- -apply(thetas, 1, objective)
    df <- df %>% 
      dplyr::bind_rows(data.frame(curve = rep(2, 1001), x = xvals, y = yvals))
  }
  df$curve <- factor(df$curve,
                     levels = c(1, 2),
                     labels = c("Other parameters at MLEs", "Other parameters varied"))
  
  # Exclude points with likelihood ratio < 0.01
  df <- df %>% dplyr::filter(y >= (log(0.01) + llval))
  
  # Create plot
  if (is.null(param_values)) {
    
    # Curve at MLEs for other parameters
    q <- ggplot(df, aes(x = x, y = y)) +
      geom_line() +
      labs(title = "Log-likelihood function",
           x = "Parameter values", 
           y = "Log-likelihood") +
      geom_hline(yintercept = llval - qchisq(p = 0.95, df = 1) / 2, 
                 linetype = 2)
    
  } else {
    
    # Two curves
    q <- ggplot(df, aes(x = x, y = y, color = curve)) +
      geom_line() +
      theme(legend.justification = c(1, 0), 
            legend.position = c(1, 0),
            legend.background = element_rect(color = "black", 
                                             linetype = "solid")) +
      guides(color = guide_legend(title = NULL)) +
      labs(title = "Log-likelihood function",
           x = "Parameter value", y = "Log-likelihood") +
      geom_hline(yintercept = llval - qchisq(p = 0.95, df = 1) / 2, 
                 linetype = 2)
  }

  # Plot
  q

}
