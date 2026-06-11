#' Histogram with Added Options
#'
#' Similar to base R function \code{\link[graphics]{hist}}, but with two added
#' features: (1) Can overlay one or more fitted probability density/mass
#' functions (PDFs/PMFs) for any univariate distribution supported in R (see
#' \code{\link[stats]{Distributions}}); and (2) Can generate more of a barplot
#' type histogram, where each possible value gets its own bin centered over its
#' value (useful for discrete variables with not too many possible values).
#'
#' When \code{x} takes on whole numbers, you typically want to set
#' \code{dis_shift = -0.5} if \code{right = TRUE}
#' (\code{\link[graphics]{hist}}'s default) and \code{dis_shift = 0.5} if
#' \code{right = FALSE}. The function will do this internally by default.
#'
#' To illustrate, suppose a particular bin represents \code{(7, 10]}. Its
#' midpoint will be at \code{x = 8.5} on the graph. But if input values are
#' whole numbers, this bin really only includes values of 8, 9, and 10, which
#' have a mean of 9. So you really want \code{f(9)} to appear at \code{x = 8.5}.
#' This requires shifting the curve to the left 0.5 units, i.e. setting
#' \code{dis_shift = -0.5}.
#'
#' When \code{x} takes on whole numbers with not too many unique values, you may
#' want the histogram to show one bin for each integer. You can do this by
#' setting \code{integer_breaks = TRUE}. By default, the function sets
#' \code{integer_breaks = TRUE} if \code{x} contains whole numbers with 10 or
#' fewer unique values.
#'
#' @param x Numeric vector of values.
#'
#' @param dis Character vector indicating which distributions should be used to
#' add fitted PDF/PMF to the histogram. If not \code{"none"}, choices for each
#' element are:
#'
#' \code{"beta"}
#'
#' \code{"binom"} (must specify \code{size})
#'
#' \code{"cauchy"}
#'
#' \code{"chisq"}
#'
#' \code{"exp"}
#'
#' \code{"f"}
#'
#' \code{"gamma"}
#'
#' \code{"geom"}
#'
#' \code{"hyper"} (must specify total number of balls in urn, \code{N}, and
#' number of balls drawn each time, \code{k})
#'
#' \code{"lnorm"}
#'
#' \code{"nbinom"} (must specify \code{size})
#'
#' \code{"norm"}
#'
#' \code{"pois"},
#'
#' \code{"t"}
#'
#' \code{"unif"}
#'
#' \code{"weibull"}
#'
#' @param dis_shift Numeric value for shifting the fitted PDF/PMF along the
#' x-axis of the histogram.
#'
#' @param integer_breaks If \code{TRUE}, integers covering the range of \code{x}
#' are used for breaks, so there is one bin for each integer. Useful for
#' discrete distributions that don't take on too many unique values.
#'
#' @param colors Character vector of colors for each PDF/PMF.
#'
#' @param lty Integer vector specifying line types for each curve.
#'
#' @param legend_form Integer value controlling what type of legend to include.
#' Choices are 0 for no legend, 1 for legend naming each distribution, and 2 for
#' legend naming each distribution and the corresponding AIC.
#'
#' @param aic_decimals Integer value for number of decimals for AIC.
#'
#' @param points_list Optional list of inputs to pass to
#' \code{\link[graphics]{points}} function, which is used to add the fitted
#' PDF/PMF.
#'
#' @param axis_list Optional list of inputs to pass to
#' \code{\link[graphics]{axis}}.
#' 
#' @param legend_list Optional list of inputs to pass to 
#' \code{\link[graphics]{legend}}. 
#'
#' @param ... May include arguments to pass to \code{\link[graphics]{hist}}
#' and/or parameter values needed for certain distributions (\code{size} if
#' \code{dis = "binom"} or \code{dis = "nbinom"}, \code{N} and \code{k} if
#' \code{dis = "hyper"}).
#'
#'
#' @return Histogram with fitted PDFs/PMFs if requested.
#'
#'
#' @examples
#' # Sample 10,000 Poisson(2) values and commpare default hist vs. histo
#' set.seed(123)
#' x <- rpois(n = 10000, lambda = 2)
#' par(mfrow = c(1, 2))
#' hist(x, main = "hist function")
#' histo(x, main = "histo function")
#'
#' # Sample 10,000 lognormal(0, 0.35) values. Create histogram with curves
#' # showing fitted lognormal, normal, and Gamma PDFs.
#' set.seed(123)
#' x <- rlnorm(n = 10000, meanlog = 0, sdlog = 0.35)
#' par(mfrow = c(1, 1))
#' histo(x, c("lnorm", "norm", "gamma"), main = "X ~ Lognormal(0, 0.35)")
#'
#' # Generate 10,000 Binomial(8, 0.25) values. Create histogram, specifying
#' # size = 5, with blue line/points showing fitted PMF.
#' set.seed(123)
#' x <- rbinom(n = 10000, size = 5, prob = 0.25)
#' par(mfrow = c(1, 1))
#' histo(x, dis = "binom", size = 5, colors = "blue", 
#'       points_list = list(type = "b"))
#'
#' @export
histo <- function(x,
                  dis = "none", dis_shift = NULL,
                  integer_breaks = NULL,
                  colors = rep("black", length(dis)),
                  lty = 1: length(dis),
                  legend_form = ifelse(length(dis) == 1, 0, 1),
                  aic_decimals = 1,
                  points_list = NULL,
                  axis_list = NULL,
                  legend_list = NULL,
                  ...) {
  
  # Get x name for labels
  xname <- deparse(substitute(x))
  
  # Drop missing values
  x <- x[! is.na(x)]
  
  # Create list with ... arguments
  extra.args <- list(...)

  # Extract any parameters (i.e. arguments NOT for hist) included in ...
  if (! is.null(extra.args)) {
    loc <- which(names(extra.args) == "size")
    if (length(loc) == 1) {
      size <- extra.args[[loc]]
      extra.args <- extra.args[-loc]
    }
    loc <- which(names(extra.args) == "N")
    if (length(loc) == 1) {
      N <- extra.args[[loc]]
      extra.args <- extra.args[-loc]
    }
    loc <- which(names(extra.args) == "k")
    if (length(loc) == 1) {
      k <- extra.args[[loc]]
      extra.args <- extra.args[-loc]
    }
  }

  # If integer_breaks is NULL, set to TRUE if x takes on whole numbers with 20
  # or fewer distinct values, else set to FALSE
  if (is.null(integer_breaks)) {
    integer_breaks <- all(x %% 1 == 0) & length(unique(x)) <= 10
  }

  # If right is not specified or integer_breaks is TRUE, set to TRUE
  if (! "right" %in% names(extra.args) | integer_breaks) {
    extra.args$right <- TRUE
  }

  # If integer_breaks is TRUE, make breaks a vector of integers
  # covering the range of x
  if (integer_breaks) {
    extra.args$breaks <- seq(floor(min(x)) - 1, ceiling(max(x)), 1)
  }

  # If freq is not specified, set to FALSE
  if (! "freq" %in% names(extra.args)) {
    extra.args$freq <- FALSE
  }

  # If xlab/main not specified, set
  if (! "xlab" %in% names(extra.args)) {
    extra.args$xlab <- xname
  }
  if (! "main" %in% names(extra.args)) {
    extra.args$main <- paste("Histogram of ", xname, sep = "")
  }

  # Create histogram
  if (integer_breaks) {
    hist.fig <- do.call(hist, c(list(x = quote(x), xaxt = "n"), extra.args))
    hist.fig <- do.call(axis, c(list(side = 1, at = hist.fig$mids,
                                     labels = hist.fig$breaks[-1]),
                                axis_list))
  } else {
    hist.fig <- do.call(hist, c(list(x = quote(x)), extra.args))
  }

  # Add fitted pdf/pmf if requested
  if (dis[1] != "none") {

    # If dis_shift is NULL and all values in x are integers, figure out how to
    # shift curve to make it match up with the histogram bars
    if (is.null(dis_shift)) {
      if (all(x %% 1 == 0)) {
        dis_shift <- ifelse(extra.args$right, -0.5, 0.5)
      } else {
        dis_shift <- 0
      }
    }

    # List of inputs for 'points' function call
    points_list <- list_override(list1 = list(type = "l"),
                                 list2 = points_list)

    # Initialize vector for legend text and create spf value for AIC
    legend.text <- c()
    spf.val <- paste("%.", aic_decimals, "f", sep = "")

    for (ii in 1: length(dis)) {

      if (dis[ii] == "beta") {

        fit <- fitdistr(x, "beta", start = list(shape1 = 0.5, shape2 = 0.5))
        theta.hat <- fit$estimate

        x.vals <- seq(min(x), max(x), diff(range(x)) / 1000)
        y.vals <- dbeta(x = x.vals, shape1 = theta.hat[1], shape2 = theta.hat[2])
        do.call(points, c(list(x = x.vals + dis_shift, y = y.vals,
                               col = colors[ii], lty = lty[ii]),
                          points_list))

        if (legend_form == 1) {
          legend.text[ii] <- "Beta"
        } else if (legend_form == 2) {
          aic.ii <- 2 * 2 - 2 * sum(dbeta(x = x, log = TRUE,
                                          shape1 = theta.hat[1],
                                          shape2 = theta.hat[2]))
          legend.text[ii] <-
            paste("Beta (AIC = ", sprintf(spf.val, aic.ii), ")", sep = "")
        }

      } else if (dis[ii] == "binom") {

        # Need user-input value for size
        p.hat <- mean(x) / size

        x.vals <- seq(round(min(x)), round(max(x)), 1)
        y.vals <- dbinom(x = x.vals, size = size, prob = p.hat)
        
        do.call(points, c(list(x = x.vals + dis_shift, y = y.vals,
                               col = colors[ii], lty = lty[ii]),
                          points_list))

        if (legend_form == 1) {
          legend.text[ii] <- "Binomial"
        } else if (legend_form == 2) {
          aic.ii <- 2 * 2 - 2 * sum(dbinom(x = x, log = TRUE,
                                           size = size, prob = p.hat))
          legend.text[ii] <-
            paste("Binomial (AIC = ", sprintf(spf.val, aic.ii), ")", sep = "")
        }

      } else if (dis[ii] == "cauchy") {

        fit <- fitdistr(x, "cauchy")
        theta.hat <- fit$estimate

        x.vals <- seq(min(x), max(x), diff(range(x)) / 1000)
        y.vals <-
          dcauchy(x = x.vals, location = theta.hat[1], scale = theta.hat[2])
        do.call(points, c(list(x = x.vals + dis_shift, y = y.vals,
                               col = colors[ii], lty = lty[ii]),
                          points_list))

        if (legend_form == 1) {
          legend.text[ii] <- "Cauchy"
        } else if (legend_form == 2) {
          aic.ii <- 2 * 2 - 2 * sum(dcauchy(x = x, log = TRUE,
                                            location = theta.hat[1],
                                            scale = theta.hat[2]))
          legend.text[ii] <-
            paste("Cauchy (AIC = ", sprintf(spf.val, aic.ii), ")", sep = "")
        }

      } else if (dis[ii] == "chisq") {

        fit <- fitdistr(x, "chi-squared", start = list(df = 1))
        theta.hat <- fit$estimate

        x.vals <- seq(min(x), max(x), diff(range(x)) / 1000)
        y.vals <- dchisq(x = x.vals, df = theta.hat[1])
        do.call(points, c(list(x = x.vals + dis_shift, y = y.vals,
                               col = colors[ii], lty = lty[ii]),
                          points_list))

        if (legend_form == 1) {
          legend.text[ii] <- "Chi-squared"
        } else if (legend_form == 2) {
          aic.ii <- 2 * 2 - 2 * sum(dchisq(x = x, log = TRUE, df = theta.hat))
          legend.text[ii] <-
            paste("Chi-squared (AIC = ", sprintf(spf.val, aic.ii), ")", sep = "")
        }

      } else if (dis[ii] == "exp") {

        fit <- fitdistr(x, "exponential")
        theta.hat <- fit$estimate

        x.vals <- seq(min(x), max(x), diff(range(x)) / 1000)
        y.vals <- dexp(x = x.vals, rate = theta.hat)
        do.call(points, c(list(x = x.vals + dis_shift, y = y.vals,
                               col = colors[ii], lty = lty[ii]),
                          points_list))

        if (legend_form == 1) {
          legend.text[ii] <- "Exponential"
        } else if (legend_form == 2) {
          aic.ii <- 2 * 2 - 2 * sum(dexp(x = x, log = TRUE, rate = theta.hat))
          legend.text[ii] <-
            paste("Exponential (AIC = ", sprintf(spf.val, aic.ii), ")", sep = "")
        }

      } else if (dis[ii] == "f") {

        fit <- fitdistr(x, "f", start = list(df1 = 1, df2 = 1))
        theta.hat <- fit$estimate

        x.vals <- seq(min(x), max(x), diff(range(x)) / 1000)
        y.vals <- df(x = x.vals, df1 = theta.hat[1], df2 = theta.hat[2])
        do.call(points, c(list(x = x.vals + dis_shift, y = y.vals,
                               col = colors[ii], lty = lty[ii]),
                          points_list))

        if (legend_form == 1) {
          legend.text[ii] <- "F"
        } else if (legend_form == 2) {
          aic.ii <- 2 * 2 - 2 * sum(df(x = x, log = TRUE,
                                       df1 = theta.hat[1],
                                       df2 = theta.hat[2]))
          legend.text[ii] <-
            paste("F (AIC = ", sprintf(spf.val, aic.ii), ")", sep = "")
        }

      } else if (dis[ii] == "gamma") {

        fit <- fitdistr(x, "gamma")
        theta.hat <- fit$estimate
        x.vals <- seq(min(x), max(x), diff(range(x)) / 1000)
        y.vals <- dgamma(x = x.vals, shape = theta.hat[1], rate = theta.hat[2])
        do.call(points, c(list(x = x.vals + dis_shift, y = y.vals,
                               col = colors[ii], lty = lty[ii]),
                          points_list))

        if (legend_form == 1) {
          legend.text[ii] <- "Gamma"
        } else if (legend_form == 2) {
          aic.ii <- 2 * 2 - 2 * sum(dgamma(x = x, log = TRUE,
                                           shape = theta.hat[1],
                                           rate = theta.hat[2]))
          legend.text[ii] <-
            paste("Gamma (AIC = ", sprintf(spf.val, aic.ii), ")", sep = "")
        }

      } else if (dis[ii] == "geom") {

        fit <- fitdistr(x, "geometric")
        theta.hat <- fit$estimate

        x.vals <- seq(round(min(x)), round(max(x)), 1)
        y.vals <- dgeom(x = x.vals, prob = theta.hat)
        do.call(points, c(list(x = x.vals + dis_shift, y = y.vals,
                               col = colors[ii], lty = lty[ii]),
                          points_list))

        if (legend_form == 1) {
          legend.text[ii] <- "Geometric"
        } else if (legend_form == 2) {
          aic.ii <- 2 * 2 - 2 * sum(dgeom(x = x, log = TRUE, prob = theta.hat))
          legend.text[ii] <-
            paste("Geometric (AIC = ", sprintf(spf.val, aic.ii), ")", sep = "")
        }

      } else if (dis[ii] == "hyper") {

        # Need user-input values for N, k
        ll.hyper <- function(m) {
          n <- N - m
          ll <- sum(log(choose(m, x) * choose(n, k - x) / choose(n + m, k)))
          return(-ll)
        }
        m.hat <- round(nlminb(objective = ll.hyper, start = k)$par)

        x.vals <- seq(round(min(x)), round(max(x)), 1)
        y.vals <- dhyper(x = x.vals, m = m.hat, n = N - m.hat, k = k)
        do.call(points, c(list(x = x.vals + dis_shift, y = y.vals,
                               col = colors[ii], lty = lty[ii]),
                          points_list))

        if (legend_form == 1) {
          legend.text[ii] <- "Hypergeo."
        } else if (legend_form == 2) {
          aic.ii <- 2 * 2 - 2 * sum(dhyper(x = x, log = TRUE,
                                           m = m.hat, n = N - m.hat, k = k))
          legend.text[ii] <-
            paste("Hypergeo. (AIC = ", sprintf(spf.val, aic.ii), ")", sep = "")
        }

      } else if (dis[ii] == "lnorm") {

        fit <- fitdistr(x, "lognormal")
        theta.hat <- fit$estimate
        x.vals <- seq(min(x), max(x), diff(range(x)) / 1000)
        y.vals <- dlnorm(x = x.vals, meanlog = theta.hat[1], sdlog = theta.hat[2])
        do.call(points, c(list(x = x.vals + dis_shift, y = y.vals,
                               col = colors[ii], lty = lty[ii]),
                          points_list))

        if (legend_form == 1) {
          legend.text[ii] <- "Lognormal"
        } else if (legend_form == 2) {
          aic.ii <- 2 * 2 - 2 * sum(dlnorm(x = x, log = TRUE,
                                           meanlog = theta.hat[1],
                                           sdlog = theta.hat[2]))
          legend.text[ii] <-
            paste("Lognormal (AIC = ", sprintf(spf.val, aic.ii), ")", sep = "")
        }

      } else if (dis[ii] == "nbinom") {

        ll.nbinom <- function(p) {
          ll <- sum(log(gamma(x + size) / (gamma(size) * factorial(x)) *
                          p^size * (1 - p)^x))
          return(-ll)
        }
        p.hat <- nlminb(objective = ll.nbinom, start = 0.5)$par

        x.vals <- seq(round(min(x)), round(max(x)), 1)
        y.vals <- dnbinom(x = x.vals, size = size, prob = p.hat)
        do.call(points, c(list(x = x.vals + dis_shift, y = y.vals,
                               col = colors[ii], lty = lty[ii]),
                          points_list))

        if (legend_form == 1) {
          legend.text[ii] <- "Neg. binom."
        } else if (legend_form == 2) {
          aic.ii <- 2 * 2 - 2 * sum(dnbinom(x = x, log = TRUE, prob = p.hat))
          legend.text[ii] <-
            paste("Neg. binom. (AIC = ", sprintf(spf.val, aic.ii), ")", sep = "")
        }

      } else if (dis[ii] == "norm") {

        fit <- fitdistr(x, "normal")
        theta.hat <- fit$estimate

        x.vals <- seq(min(x), max(x), diff(range(x)) / 1000)
        y.vals <- dnorm(x = x.vals, mean = theta.hat[1], sd = theta.hat[2])
        do.call(points, c(list(x = x.vals + dis_shift, y = y.vals,
                               col = colors[ii], lty = lty[ii]),
                          points_list))

        if (legend_form == 1) {
          legend.text[ii] <- "Normal"
        } else if (legend_form == 2) {
          aic.ii <- 2 * 2 - 2 * sum(dnorm(x = x, log = TRUE,
                                          mean = theta.hat[1],
                                          sd = theta.hat[2]))

          legend.text[ii] <-
            paste("Normal (AIC = ", sprintf(spf.val, aic.ii), ")", sep = "")
        }

      } else if (dis[ii] == "pois") {

        fit <- fitdistr(x, "poisson")
        theta.hat <- fit$estimate

        x.vals <- seq(round(min(x)), round(max(x)), 1)
        y.vals <- dpois(x = x.vals, lambda = theta.hat)
        do.call(points, c(list(x = x.vals + dis_shift, y = y.vals,
                               col = colors[ii], lty = lty[ii]),
                          points_list))

        if (legend_form == 1) {
          legend.text[ii] <- "Poisson"
        } else if (legend_form == 2) {
          aic.ii <- 2 * 2 - 2 * sum(dpois(x = x, log = TRUE, lambda = theta.hat))
          legend.text[ii] <-
            paste("Poisson (AIC = ", sprintf(spf.val, aic.ii), ")", sep = "")
        }

      } else if (dis[ii] == "t") {

        ll.t <- function(theta) {
          f.df <- theta[1]
          f.ncp <- theta[2]
          ll <- sum(dt(x = x, df = f.df, ncp = f.ncp, log = TRUE))
          return(-ll)
        }
        theta.hat <- nlminb(objective = ll.t, start = c(5, 0),
                            lower = c(1e-4, -Inf))$par

        x.vals <- seq(min(x), max(x), diff(range(x)) / 1000)
        y.vals <- dt(x = x.vals, df = theta.hat[1], ncp = theta.hat[2])
        do.call(points, c(list(x = x.vals + dis_shift, y = y.vals,
                               col = colors[ii], lty = lty[ii]),
                          points_list))

        if (legend_form == 1) {
          legend.text[ii] <- "t"
        } else if (legend_form == 2) {
          aic.ii <- 2 * 2 - 2 * sum(dt(x = x, log = TRUE,
                                       df = theta.hat[1], ncp = theta.hat[2]))
          legend.text[ii] <-
            paste("t (AIC = ", sprintf(spf.val, aic.ii), ")", sep = "")
        }

      } else if (dis[ii] == "unif") {

        min.hat <- min(x)
        max.hat <- max(x)
        x.vals <- seq(min(x), max(x), diff(range(x)) / 1000)
        y.vals <- dunif(x = x.vals, min = min.hat, max = max.hat)
        do.call(points, c(list(x = x.vals + dis_shift, y = y.vals,
                               col = colors[ii], lty = lty[ii]),
                          points_list))

        if (legend_form == 1) {
          legend.text[ii] <- "Uniform"
        } else if (legend_form == 2) {
          aic.ii <- 2 * 2 - 2 * sum(dunif(x = x, log = TRUE,
                                          min = min.hat, max = max.hat))
          legend.text[ii] <-
            paste("Uniform (AIC = ", sprintf(spf.val, aic.ii), ")", sep = "")
        }

      } else if (dis[ii] == "weibull") {

        fit <- fitdistr(x, "weibull")
        theta.hat <- fit$estimate

        x.vals <- seq(min(x), max(x), diff(range(x)) / 1000)
        y.vals <- dweibull(x = x.vals, shape = theta.hat[1], scale = theta.hat[2])
        do.call(points, c(list(x = x.vals + dis_shift, y = y.vals,
                               col = colors[ii], lty = lty[ii]),
                          points_list))

        if (legend_form == 1) {
          legend.text[ii] <- "Weibull"
        } else if (legend_form == 2) {
          aic.ii <- 2 * 2 - 2 * sum(dweibull(x = x, log = TRUE,
                                             shape = theta.hat[1],
                                             scale = theta.hat[2]))
          legend.text[ii] <-
            paste("Uniform (AIC = ", sprintf(spf.val, aic.ii), ")", sep = "")
        }
        
      }
      
    }
    
    # Add legend if requested
    if (legend_form %in% c(1, 2)) {
      legend_list <- list_override(list1 = list(x = "topright"),
                                   list2 = legend_list)
      do.call(legend, c(list(lty = lty, col = colors, legend = legend.text),
                        legend_list))
    }
    
  }
}
