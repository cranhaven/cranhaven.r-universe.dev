#' Plot Log-OR vs. X for Gamma Discriminant Function Approach
#'
#' Archived on 7/23/2018. Please use \code{\link{plot_gdfa}} instead.
#'
#'
#' @inheritParams plot_dfa
#'
#' @param estimates Numeric vector of point estimates for
#' \code{(gamma_0, gamma_y, gamma_c^T, b1, b0)}.
#'
#'
#' @inherit plot_dfa return
#'
#'
#' @examples
#' # Fit Gamma discriminant function model for poolwise Xtilde vs. (Y, C),
#' # without assuming a constant log-OR. Ignoring processing errors for simplicity.
#' data(pdat2)
#' dat <- pdat2$dat
#' c.list <- pdat2$c.list
#' fit <- p_dfa_xerrors2(
#'   g = dat$g,
#'   y = dat$y,
#'   xtilde = dat$xtilde,
#'   c = c.list,
#'   errors = "neither",
#'   constant_or = FALSE
#' )
#'
#' # Plot estimated log-OR vs. X at mean value for C
#' p <- plot_dfa2(
#'   estimates = fit$estimates,
#'   varcov = fit$theta.var,
#'   xrange = range(dat$xtilde / dat$g),
#'   cvals = mean(unlist(c.list))
#' )
#' p
#'
#'
#' @export
plot_dfa2 <- function(estimates,
                      varcov = NULL,
                      xrange,
                      xname = "X",
                      cvals = NULL,
                      set_labels = NULL,
                      set_panels = TRUE) {

  # Extract parameter estimates
  names_estimates <- names(estimates)

  loc.gammas <- which(substr(names_estimates, 1, 6) == "gamma_")
  loc.b1 <- which(names_estimates == "b1")
  loc.b0 <- which(names_estimates == "b0")

  gammas <- estimates[loc.gammas]
  gamma_0 <- gammas[1]
  gamma_y <- gammas[2]
  gamma_c <- gammas[-c(1, 2)]
  b1 <- estimates[loc.b1]
  b0 <- estimates[loc.b0]

  # Subset useful part of variance-covariance matrix
  locs <- c(loc.gammas, loc.b1, loc.b0)
  varcov <- varcov[locs, locs]

  # Create X vector
  x <- seq(xrange[1], xrange[2], (xrange[2] - xrange[1]) / 500)

  if (is.null(cvals)) {

    # No-covariate case - plot curve and confidence bands (if possible)

    # Calculate log-OR's
    logOR <- 1 / b0 - 1 / b1 + log((x + 1) / x) *
      exp(gamma_0) * (exp(gamma_y) - 1)
    df <- data.frame(x = x, logOR = logOR)


    # Calculate confidence bands
    if (! is.null(varcov)) {

      ses <- sapply(x, function(x) {
        fprime <- matrix(c(
          log((x + 1) / x) * exp(gamma_0) * (exp(gamma_y) - 1),
          log((x + 1) / x) * exp(gamma_0 + gamma_y),
          1 / b1^2,
          -1 / b0^2
          ), nrow = 1)
        sqrt(fprime %*% varcov %*% t(fprime))
      })
      df$lower <- logOR - qnorm(0.975) * ses
      df$upper <- logOR + qnorm(0.975) * ses

    }

    # Create plot
    p <- ggplot(df, aes(x, logOR)) +
      geom_line() +
      geom_hline(yintercept = 0, linetype = 2) +
      labs(title = paste("Estimated Log-OR vs.", xname),
           y = "Log-OR",
           x = xname) +
      ylim(min(logOR), max(logOR)) +
      theme_bw()

    # Add confidence bands
    if (! is.null(varcov)) {

      p <- p +
        geom_ribbon(aes_string(ymin = "lower", ymax = "upper"),
                    alpha = 0.2) +
        ylim(min(df$lower), max(df$upper))

    }

  } else if (is.numeric(cvals)) {

    # 1 set of covariate values - plot curve and confidence bands (if possible)

    # Calculate log-OR's
    logOR <- 1 / b0 - 1 / b1 + log((x + 1) / x) *
      exp(gamma_0 + sum(gamma_c * cvals)) * (exp(gamma_y) - 1)
    df <- data.frame(x = x, logOR = logOR)

    # Calculate confidence bands
    if (! is.null(varcov)) {

      ses <- sapply(x, function(x) {
        fprime <- matrix(c(
          log((x + 1) / x) *
            exp(gamma_0 + sum(gamma_c * cvals)) * (exp(gamma_y) - 1),
          log((x + 1) / x) *
            exp(gamma_0 + gamma_y + sum(gamma_c * cvals)),
          log((x + 1) / x) *
            exp(gamma_0 + sum(gamma_c * cvals)) * (exp(gamma_y) - 1) * cvals,
          1 / b1^2,
          -1 / b0^2
          ), nrow = 1)
        sqrt(fprime %*% varcov %*% t(fprime))
      })
      df$lower <- logOR - qnorm(0.975) * ses
      df$upper <- logOR + qnorm(0.975) * ses

    }

    # Create plot
    p <- ggplot(df, aes(x, logOR)) +
      geom_line() +
      geom_hline(yintercept = 0, linetype = 2) +
      labs(title = paste("Log-OR vs.", xname),
           y = "Log-OR",
           x = xname) +
      ylim(min(logOR), max(logOR)) +
      theme_bw()

    # Add confidence bands
    if (! is.null(varcov)) {

      p <- p +
        geom_ribbon(aes_string(ymin = "lower", ymax = "upper"),
                    alpha = 0.2) +
        ylim(min(df$lower), max(df$upper))

    }

  } else if (is.list(cvals)) {

    # Multiple sets of covariate values

    # Create labels for covariate sets
    if (is.null(set_labels)) {
      cnames <- substr(names(gamma_c), start = 7, stop = 100)
      set_labels <- sapply(cvals, function(x) paste(cnames, "=", x, collapse = ", "))
    }

    # Loop through covariate sets and calculate log-OR's for each
    df <- NULL
    for (ii in 1: length(cvals)) {

      # Calculate log-OR's
      cvals.ii <- cvals[[ii]]
      logOR <- 1 / b0 - 1 / b1 + log((x + 1) / x) *
        exp(gamma_0 + sum(gamma_c * cvals.ii)) * (exp(gamma_y) - 1)
      df <- dplyr::bind_rows(df, data.frame(Covariates = ii, x = x, logOR = logOR))

      # Calculate confidence bands
      if (! is.null(varcov) & set_panels) {

        ses <- sapply(x, function(x) {
          fprime <- matrix(c(
            log((x + 1) / x) *
              exp(gamma_0 + sum(gamma_c * cvals.ii)) * (exp(gamma_y) - 1),
            log((x + 1) / x) * exp(gamma_0 + gamma_y + sum(gamma_c * cvals.ii)),
            log((x + 1) / x) *
              exp(gamma_0 + sum(gamma_c * cvals.ii)) * (exp(gamma_y) - 1) * cvals.ii,
            1 / b1^2,
            -1 / b0^2
            ), nrow = 1)
          sqrt(fprime %*% varcov %*% t(fprime))
        })
        df$lower <- logOR - qnorm(0.975) * ses
        df$upper <- logOR + qnorm(0.975) * ses

      }

    }
    df$Covariates <- factor(df$Covariates, levels = 1: length(cvals), labels = set_labels)

    # Create plot
    if (set_panels) {

      p <- ggplot(df, aes(x, logOR)) +
        facet_grid(reformulate("Covariates", ".")) +
        #facet_grid(reformulate("Covariates", "."), labeller = set_labels) +
        #facet_grid(facets = . ~ Covariates, labeller = set_labels) +
        geom_line() +
        geom_hline(yintercept = 0, linetype = 2) +
        labs(title = paste("Log-OR vs.", xname),
             y = "Log-OR",
             x = xname) +
        ylim(min(logOR), max(logOR)) +
        theme_bw()

      if (! is.null(varcov)) {

        p <- p +
          geom_ribbon(aes_string(ymin = "lower", ymax = "upper"),
                      alpha = 0.2) +
          ylim(min(df$lower), max(df$upper))

      }

    } else {

      p <- ggplot(df, aes_string(x = "x",
                                 y = "logOR",
                                 group = "Covariates",
                                 color = "Covariates")) +
        geom_line() +
        geom_hline(yintercept = 0, linetype = 2) +
        labs(title = paste("Log-OR vs.", xname),
             y = "Log-OR",
             x = xname) +
        ylim(min(logOR), max(logOR)) +
        theme_bw()

    }

  }

  # Plot
  p

}
