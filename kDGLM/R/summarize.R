#' Summary for a fitted kDGLM model
#'
#' Prints a report for a fitted_dlm object.
#'
#' @param object A fitted_dlm object.
#' @param t Integer: The time index for the latent states.
#' @param lag Integer: The number of steps ahead used for the evaluating the latent states. Use lag<0 for the smoothed distribution, If lag==0 for the filtered distribution and lag=h for the h-step-ahead prediction.
#' @param metric.lag Integer: The number of steps ahead used for the evaluating the predictions used when calculating metrics. Use metric.lag<0 for the smoothed distribution, If metric.lag==0 for the filtered distribution and metric.lag=h for the h-step-ahead prediction.
#' @param metric.cutoff Integer: The cutoff time index for the metric calculation. Values before that time will be ignored.
#' @param pred.cred numeric: The credibility interval to be used for the interval score.
#' @param ... Extra arguments passed to the coef method.#'
#' @rdname summary.fitted_dlm
#' @export
#' @importFrom stats pnorm
#'
#' @return No return value, called to print a summary of the fitted kDGLM model.
#'
#' @examples
#'
#' data <- c(AirPassengers)
#'
#' level <- polynomial_block(rate = 1, order = 2, D = 0.95)
#' season <- harmonic_block(rate = 1, order = 2, period = 12, D = 0.975)
#'
#' outcome <- Poisson(lambda = "rate", data)
#'
#' fitted.data <- fit_model(level, season,
#'   AirPassengers = outcome
#' )
#' summary(fitted.data)
#'
#' @family auxiliary visualization functions for the fitted_dlm class
summary.fitted_dlm <- function(object, t = object$t, lag = -1, metric.lag = 1, metric.cutoff = floor(object$t / 10), pred.cred = 0.95, ...) {
  k <- object$k
  T_len <- object$t
  predictions <- coef.fitted_dlm(object, t.eval = (metric.cutoff + 1):T_len, lag = metric.lag, pred.cred = pred.cred, eval.pred = TRUE, eval.metric = TRUE)
  metric.log.like <- sum(predictions$metrics$log.like, na.rm = TRUE)

  # metric.vals <- rbind(
  #   colMeans(predictions$metrics$interval.score, na.rm = TRUE),
  #   colMeans(predictions$metrics$mase, na.rm = TRUE),
  #   colMeans(predictions$metrics$mae, na.rm = TRUE),
  #   colMeans(predictions$metrics$mse, na.rm = TRUE)
  # )
  metric.vals <- rbind(
    mean(predictions$metrics$interval.score, na.rm = TRUE),
    mean(predictions$metrics$mase, na.rm = TRUE),
    mean(predictions$metrics$mae, na.rm = TRUE),
    mean(predictions$metrics$mse, na.rm = TRUE)
  )
  metric.names <- c(
    "Log-likelihood",
    "Interval Score",
    "Mean Abs. Scaled Error",
    "Relative abs. Error",
    "Mean Abs. Error",
    "Mean Squared Error"
  )
  metric.len <- 22
  predictions <- coef.fitted_dlm(object, t.eval = T_len, lag = lag, pred.cred = pred.cred, eval.pred = FALSE, eval.metric = FALSE)

  flag.dynamic <- object$dynamic

  distr.names <- lapply(object$outcomes, function(x) {
    x$name
  })
  distr.names.len <- max(sapply(names(distr.names), nchar))

  coef.label <- if (lag < 0) {
    " (smoothed)"
  } else if (lag == 0) {
    paste0(" (filtered) at time ", t)
  } else if (lag == 1) {
    paste0(" (one-step-ahead prediction) at time ", t)
  } else {
    paste0(" (", lag, "-steps-ahead prediction) at time ", t)
  }

  metric.label <- if (metric.lag < 0) {
    "Smoothed predictions"
  } else if (metric.lag == 0) {
    "Filtered predictions"
  } else if (metric.lag == 1) {
    "One-step-ahead prediction"
  } else {
    paste0(metric.lag, "-steps-ahead prediction")
  }
  len.names <- max(sapply(as.character(object$var.labels), function(x) {
    nchar(x)
  }))
  var.labels <- format(object$var.labels, width = len.names, justify = "l")


  mean.coef <- predictions$theta.mean[, 1]
  var.mat <- predictions$theta.cov[, , 1]
  if (length(var.mat) == 1) {
    std.coef <- sqrt(abs(var.mat))
  } else {
    std.coef <- sqrt(abs(diag(var.mat)))
  }
  t.coef <- mean.coef / std.coef
  p.val <- 2 * (1 - pnorm(abs(mean.coef) / std.coef))
  status <- rep(" ", length(var.labels))
  status[p.val <= 0.01] <- "."
  status[p.val <= 0.05] <- "*"
  status[p.val <= 0.01] <- "**"
  status[p.val <= 0.001] <- "***"
  mean.coef <- format(round(mean.coef, 5), width = 8, justify = "l")
  std.coef <- format(round(std.coef, 5), width = 10, justify = "l")
  t.coef <- format(round(t.coef, 5), width = 7, justify = "l")
  p.val.str <- ifelse(p.val < 0.001,
    format(p.val, digits = 3, justify = "l", scientific = TRUE),
    format(round(p.val, 3), width = 8, justify = "l", scientific = FALSE)
  )
  p.val.str <- ifelse(p.val < 1e-12,
    "  <1e-12",
    p.val.str
  )

  metric.log.like <- format(round(metric.log.like, 5), justify = "l", scientific = FALSE)
  metric.vals <- ifelse(abs(metric.vals) < 0.00001 | abs(metric.vals) > 1e5,
    format(metric.vals, digits = 4, justify = "l", scientific = TRUE),
    format(round(metric.vals, 5), justify = "l", scientific = FALSE)
  )
  metric.names <- format(metric.names, width = metric.len, justify = "l")

  cat(paste0(
    "Fitted DGLM with ", length(object$outcomes), " outcomes.\n\n",
    "distributions:\n",
    paste0("    ", names(distr.names), ": ", distr.names, "\n", collapse = ""), "\n",
    if (any(!flag.dynamic)) {
      paste0(
        "Static coeficients", coef.label, ":\n",
        paste(format(" ", width = len.names, justify = "l"), "Estimate", "Std. Error", "  t value", "Pr(>|t|)"), "\n",
        paste(var.labels[!flag.dynamic], mean.coef[!flag.dynamic],
          std.coef[!flag.dynamic], t.coef[!flag.dynamic],
          p.val.str[!flag.dynamic], status[!flag.dynamic], "\n",
          collapse = ""
        ),
        "---\n",
        "Signif. codes:  0 \xe2\x80\x98***\xe2\x80\x99 0.001 \xe2\x80\x98**\xe2\x80\x99 0.01 \xe2\x80\x98*\xe2\x80\x99 0.05 \xe2\x80\x98.\xe2\x80\x99 0.1 \xe2\x80\x98 \xe2\x80\x99 1\n\n"
      )
    } else {
      "---\nNo static coeficients.\n"
    },
    "---\n",
    "See the coef.fitted_dlm for the coeficients with temporal dynamic.\n\n",
    metric.label, "\n",
    paste0(paste0(metric.names[1], ": ", metric.log.like), collapse = "\n"), "\n",
    paste0(paste0(
      metric.names[2:3], ": ",
      sapply(1:2, function(i) {
        paste(metric.vals[i, ], collapse = "    ")
      })
    ), collapse = "\n"), "\n",
    "---"
  ))
}

#' Summary for a kDGLM outcome
#'
#' Prints a report for a dlm_distr object.
#'
#' @param object A dlm_distr object.
#'
#' @rdname summary.dlm_distr
#' @export
#'
#' @return No return value, called to print a summary of a kDGLM outcome.
#'
#' @keywords internal
#' @family auxiliary functions for a creating outcomes
#' @family Reports for dlm_distr objects.
summary.dlm_distr <- function(object, ...) {
  cat(paste0(
    object$name, " distribution.\n\nUnknown parameters: \n",
    paste0("    ", names(object$pred.names), " - ", object$pred.names, collapse = "\n"), "\n",
    if (length(object$parms) > 0) {
      paste0("Known parameters: \n", paste0("    ", names(object$parms), "=", object$parms, collapse = "\n"), "\n")
    } else {
      ""
    },
    "\n",
    paste0("Serie length: ", object$t, "\n"),
    paste0("Number of outcomes: ", object$r, "\n"),
    paste0("Number of parameters: ", object$k), "\n"
  ))
}

#' Summary for a kDGLM structure
#'
#' Prints a report for a dlm_block object.
#'
#' @param object A dlm_block object.
#'
#' @return No return value, called to print a summary of a kDGLM structure.
#'
#' @rdname summary.dlm_block
#' @export
#' @keywords internal
#' @family auxiliary functions for structural blocks
summary.dlm_block <- function(object, ...) {
  block.names <- names(object$var.names)

  for (name in unique(block.names)) {
    count.name <- sum(block.names == name)
    if (count.name > 1) {
      len.char <- floor(log10(count.name)) + 1
      block.names[block.names == name] <- paste0(name, ".", formatC(1:count.name, width = len.char, flag = "0"))
    }
  }

  cat(paste0(
    object$type, " DLM block.",
    "\n",
    paste0("latent states: \n", paste0("    ", block.names, ": ", lapply(object$var.names, function(x) {
      paste0(names(x), collapse = ", ")
    }), " (", lapply(object$var.names, length), " variable(s))", collapse = "\n"), "\n"),
    "\n",
    paste0("Linear predictors: \n", paste0("    ", object$pred.names, collapse = "\n"), "\n"),
    "\n",
    paste0("Status: ", object$status, "\n"),
    paste0("Serie length: ", object$t, "\n"),
    paste0(
      "Interventions at: ",
      paste0(
        lapply(
          object$interventions,
          function(x) {
            paste0(x$times, collapse = ", ")
          }
        ),
        collapse = ", "
      ), "\n"
    ),
    paste0("Number of latent states: ", object$n, "\n"),
    paste0("Number of linear predictors: ", object$k)
  ))
}

#' Summary for a searched_dlm object
#'
#' Prints a report for a searched_dlm object.
#'
#' @param object A searched_dlm object.
#'
#' @return No return value, called to print a summary of a searched_dlm object.
#'
#' @rdname summary.searched_dlm
#' @export
#' @keywords internal
#' @family auxiliary visualization functions for the fitted_dlm class
summary.searched_dlm <- function(object, ...) {
  print(object$search.data[1:5, ])
  summary(object$model)
}
