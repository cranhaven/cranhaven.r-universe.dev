#' Visualizing a fitted kDGLM model
#'
#' Calculate the predictive mean and some quantile for the observed data and show a plot.
#'
#' @param x fitted_dlm object: A fitted DGLM.
#' @param outcomes character: The name of the outcomes to plot.
#' @param latent.states character: The name of the latent states to plot.
#' @param linear.predictors character: The name of the linear predictors to plot.
#' @param pred.cred numeric: The credibility value for the credibility interval.
#' @param lag integer: The number of steps ahead to be used for prediction. If lag<0, the smoothed distribution is used and, if lag==0, the filtered interval.score is used.
#' @param cutoff integer: The number of initial steps that should be skipped in the plot. Usually, the model is still learning in the initial steps, so the predictions are not reliable.
#' @param plot.pkg character: A flag indicating if a plot should be produced. Should be one of 'auto', 'base', 'ggplot2' or 'plotly'.
#' @param ... Extra arguments passed to the plot method.
#'
#' @return ggplot or plotly object: A plot showing the predictive mean and credibility interval with the observed data.
#'
#' @rdname plot.fitted_dlm
#' @export
#' @importFrom grDevices rainbow
#' @importFrom Rfast colAny rowAny
#' @import graphics
#' @import grDevices
#'
#' @examples
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
#'
#' plot(fitted.data, plot.pkg = "base")
#'
#' @seealso \code{\link{fit_model}}
#'
#' @family auxiliary visualization functions for the fitted_dlm class
plot.fitted_dlm <- function(x, outcomes = NULL, latent.states = NULL, linear.predictors = NULL, pred.cred = 0.95, lag = NA, cutoff = floor(x$t / 10), plot.pkg = "auto", ...) {
  if (is.null(outcomes) & is.null(latent.states) & is.null(linear.predictors)) {
    outcomes <- names(x$outcomes)
  }

  if (plot.pkg == "auto") {
    plot.pkg <- if (requireNamespace("plotly", quietly = TRUE) & requireNamespace("ggplot2", quietly = TRUE)) {
      "plotly"
    } else if (requireNamespace("ggplot2", quietly = TRUE)) {
      "ggplot2"
    } else {
      "base"
    }
  }
  t_last <- x$t
  outcome.names <- sapply(names(x$outcomes), function(name) {
    paste0(name, x$outcomes[[name]]$sufix)
  })
  theta.names <- x$var.labels
  lambda.names <- x$pred.names

  if (!is.null(outcomes)) {
    flags.outcomes <- (sapply(outcomes, function(x) {
      grepl(tolower(x), tolower(outcome.names), fixed = TRUE)
    }) |> matrix(length(outcome.names), length(outcomes)) |> rowSums()) > 0
  } else {
    flags.outcomes <- rep(FALSE, length(outcome.names))
  }

  if (!is.null(latent.states)) {
    flags.theta <- (sapply(latent.states, function(x) {
      grepl(tolower(x), tolower(theta.names), fixed = TRUE)
    }) |> matrix(length(theta.names), length(latent.states)) |> rowSums()) > 0
  } else {
    flags.theta <- rep(FALSE, length(theta.names))
  }

  if (!is.null(linear.predictors)) {
    flags.lambda <- (sapply(linear.predictors, function(x) {
      grepl(tolower(x), tolower(lambda.names), fixed = TRUE)
    }) |> matrix(length(lambda.names), length(linear.predictors)) |> rowSums()) > 0
  } else {
    flags.lambda <- rep(FALSE, length(lambda.names))
  }

  if (!any(colAny(flags.outcomes) | colAny(flags.theta) | colAny(flags.lambda))) {
    stop(paste0("Error: Invalid outcome selection. Got '", outcomes, "', expected one of the following:\n", paste0(outcome.names, collapse = "\n")))
  }

  if (is.na(lag)) {
    if (all(!flags.outcomes)) {
      lag <- -1
    } else {
      lag <- 1
    }
  }

  coefs <- coef.fitted_dlm(x, t.eval = (cutoff + 1):t_last, lag = lag, pred.cred = pred.cred, eval.pred = TRUE, eval.metric = FALSE)
  eval <- coefs$data

  outcomes.labels <- outcome.names[flags.outcomes]
  theta.labels <- theta.names[flags.theta]
  lambda.labels <- lambda.names[flags.lambda]

  eval <- eval[eval$Serie %in% outcomes.labels, ]

  var.labels <- c(theta.labels, lambda.labels)
  size <- sum(flags.theta) + sum(flags.lambda)
  seq.time <- (cutoff + 1):t_last

  if (size > 0) {
    m1 <- rbind(
      coefs$theta.mean[flags.theta, , drop = FALSE],
      coefs$lambda.mean[flags.lambda, , drop = FALSE]
    ) |>
      t()
    std.mat.var <- coefs$theta.cov[flags.theta, flags.theta, , drop = FALSE] |>
      apply(3, diag) |>
      sqrt() |>
      matrix(sum(flags.theta), t_last - cutoff) |>
      t()
    std.mat.pred <- coefs$lambda.cov[flags.lambda, flags.lambda, , drop = FALSE] |>
      apply(3, diag) |>
      sqrt() |>
      matrix(sum(flags.lambda), t_last - cutoff) |>
      t()
    std.mat <- cbind(std.mat.var, std.mat.pred)

    lim.i <- m1 + qnorm((1 - pred.cred) / 2) * std.mat
    lim.s <- m1 + qnorm(1 - (1 - pred.cred) / 2) * std.mat

    eval <- rbind(
      eval,
      data.frame(
        Time = sort(rep(seq.time, size)),
        Serie = rep(var.labels, length(seq.time)),
        Observation = 2 * NA,
        Prediction = m1 |> t() |> c(),
        Variance = std.mat**2 |> t() |> c(),
        C.I.lower = lim.i |> t() |> c(),
        C.I.upper = lim.s |> t() |> c()
      )
    )
  }

  obs.na.rm <- c(eval$Observation[!is.na(eval$Observation)], eval$Prediction)
  max.value <- evaluate_max(obs.na.rm - min(obs.na.rm))[[3]] + min(obs.na.rm)
  min.value <- -evaluate_max(-(obs.na.rm - max(obs.na.rm)))[[3]] + max(obs.na.rm)

  n.series <- length(unique(eval$Serie))
  colors <- rainbow(n.series, s = 0.8, v = 0.9)
  fills <- rainbow(n.series, s = 0.4, v = 0.9)
  series.names <- unique(eval$Serie)
  names(colors) <- series.names
  names(fills) <- series.names
  colors[["Detected changes"]] <- "black"
  linetypes <- c(
    "Detected changes" = "dashed",
    "Observation" = NA,
    "Fitted values" = "solid"
  )
  shapes <- c(
    "Detected changes" = NA,
    "Observation" = 16,
    "Fitted values" = NA
  )

  title <- if (lag < 0) {
    "Smoothed predictions"
  } else if (lag == 0) {
    "Filtered predictions"
  } else if (lag == 1) {
    "One-step-ahead predictions"
  } else {
    paste0(lag, "-steps-ahead predictions")
  }

  if (plot.pkg == "base" || !requireNamespace("ggplot2", quietly = TRUE)) {
    points <- paste0(colors, "55")
    fills <- paste0(fills, "33")
    names(colors) <- names(points) <- names(fills) <- series.names

    if (plot.pkg != "base") {
      warning("The ggplot2 package is required for ggplot2 and plotly plots and was not found. Falling back to R base plot functions.")
    }
    cur.height <- dev.size("cm")[2]
    count.spaces <- ceiling(n.series / 4)
    font.cm <- 0.35

    oldpar <- par(no.readonly = TRUE)
    on.exit(par(oldpar))

    layout(
      mat = matrix(c(1, 2, 3), 3, 1),
      heights = c(
        cur.height - font.cm * count.spaces - 1 - 0.75,
        0.75,
        font.cm * count.spaces + 1
      )
    )


    par(mar = c(4.1, 4.1, 4.1, 2.1), cex = 1)
    plot(0, 0, type = "n", xlim = c(cutoff + 1, t_last), ylim = c(min.value, max.value), ylab = expression(Y[t]), xlab = "Time", main = title)
    for (serie in series.names) {
      plot.serie <- eval[eval$Serie == serie, ]
      points(plot.serie$Time, plot.serie$Observation,
        col = points[[serie]],
        pch = 16
      )
      lines(plot.serie$Time, plot.serie$Prediction, col = colors[[serie]])
      base_ribbon(plot.serie$Time, plot.serie$C.I.lower, plot.serie$C.I.upper,
        col = fills[[serie]], lty = 0
      )
    }
    if (any(x$alt.flags == 1)) {
      for (t in (1:(t_last - cutoff))[x$alt.flags == 1]) {
        lines(x = c(t, t), c(min.value - 1, max.value + 1), lty = 2)
      }
    }

    par(mar = c(0, 0, 0, 0), cex = 1)
    plot(0, type = "n", axes = FALSE, xlab = "", ylab = "", xlim = c(0, 1), ylim = c(0, 1))
    legend(
      legend = c("Fit", "Obs.", "Detected changes"),
      col = c("black", "black", "black"),
      lty = c(1, 0, 2),
      seg.len = 0.8,
      pch = c(0, 16, 0),
      pt.cex = c(0, 1, 0),
      fill = c("#00000033", "#ffffff00", "#ffffff00"),
      border = "#ffffff00",
      cex = 0.75,
      x = "top", bty = "n", ncol = 3
    )
    par(mar = c(0, 0, 0, 0), cex = 1)
    plot(0, type = "n", axes = FALSE, xlab = "", ylab = "", xlim = c(0, 1), ylim = c(0, 1))
    legend(
      legend = series.names,
      col = colors,
      lty = rep(0, n.series),
      pch = rep(22, n.series),
      pt.cex = rep(2, n.series),
      pt.bg = colors,
      x = 0.5, xjust = 0.5, y = 1, inset = 0, bty = "n",
      cex = 0.75,
      ncol = min(4, ceiling(n.series / count.spaces))
    )
    plt <- NULL
  } else {
    plt <- ggplot2::ggplot() +
      ggplot2::geom_ribbon(data = eval, na.rm = TRUE, ggplot2::aes_string(x = "Time", fill = "Serie", ymin = "C.I.lower", ymax = "C.I.upper"), alpha = 0.25) +
      ggplot2::geom_line(data = eval, na.rm = TRUE, ggplot2::aes_string(x = "Time", color = "Serie", y = "Prediction", linetype = "'Fitted values'")) +
      ggplot2::geom_point(data = eval[!is.na(eval$Observation), ], na.rm = TRUE, ggplot2::aes_string(x = "Time", color = "Serie", y = "Observation", shape = '"Observation"'), alpha = 0.5) +
      ggplot2::scale_linetype_manual("", values = linetypes) +
      ggplot2::scale_shape_manual("", values = shapes) +
      ggplot2::scale_fill_manual("", na.value = NA, values = fills) +
      ggplot2::scale_color_manual("", na.value = NA, values = colors) +
      ggplot2::scale_x_continuous("Time") +
      ggplot2::ggtitle(title) +
      ggplot2::theme_bw() +
      ggplot2::coord_cartesian(ylim = c(min.value, max.value))
    if (any(x$alt.flags == 1)) {
      plt <- plt +
        ggplot2::geom_vline(
          data = data.frame(xintercept = (1:t_last)[x$alt.flags == 1], linetype = "Detected changes"),
          ggplot2::aes_string(xintercept = "xintercept", linetype = "linetype")
        )
    }
    if (plot.pkg == "plotly") {
      if (!requireNamespace("plotly", quietly = TRUE)) {
        warning("The plotly package is required for plotly plots.")
      } else {
        plt <- plotly::ggplotly(plt + ggplot2::ylab(plotly::TeX("Y_t"))) |> plotly::config(mathjax = "cdn")

        for (i in (1:n.series) - 1) {
          if (sort(series.names)[i + 1] %in% c(outcome.names, "Detected changes")) {
            plt$x$data[[i + 1]]$legendgroup <-
              plt$x$data[[i + 1 + n.series]]$legendgroup <-
              plt$x$data[[i + 1]]$name <-
              plt$x$data[[i + 1 + n.series]]$name <- paste0(sort(series.names)[i + 1], ": fitted values")

            plt$x$data[[i + 1]]$showlegend <- FALSE

            plt$x$data[[i + 1 + 2 * n.series]]$legendgroup <-
              plt$x$data[[i + 1 + 2 * n.series]]$name <- paste0(sort(series.names)[i + 1], ": observations")
          } else {
            plt$x$data[[i + 1]]$showlegend <- FALSE
            plt$x$data[[i + 1]]$legendgroup <-
              plt$x$data[[i + 1 + n.series]]$legendgroup <-
              plt$x$data[[i + 1]]$name <-
              plt$x$data[[i + 1 + n.series]]$name <- paste0(sort(series.names)[i + 1])
          }
        }
        n <- length(plt$x$data)
        if (n %% 3 == 1) {
          plt$x$data[[n]]$showlegend <- FALSE
          plt$x$data[[n]]$legendgroup <-
            plt$x$data[[n]]$name <- "Detected changes"
        }
      }
    } else {
      plt <- plt + ggplot2::ylab(expression(Y[t]))
    }
    return(plt)
  }
}

#' Visualizing latent states in a fitted kDGLM model
#'
#' @param x dlm_coef object: The coefficients of a fitted DGLM model.
#' @param var character: The name of the variables to plot (same value passed while creating the structure). Any variable whose name partially match this variable will be plotted.
#' @param cutoff integer: The number of initial steps that should be skipped in the plot. Usually, the model is still learning in the initial steps, so the estimated values are not reliable.
#' @param pred.cred numeric: The credibility value for the credibility interval.
#' @param plot.pkg character: A flag indicating if a plot should be produced. Should be one of 'auto', 'base', 'ggplot2' or 'plotly'.
#' @param ... Extra arguments passed to the plot method.
#'
#' @return ggplot or plotly object: A plot showing the predictive mean and credibility interval with the observed data.
#'
#' @rdname plot.dlm_coef
#' @export
#'
#' @importFrom grDevices rainbow
#' @importFrom stats qnorm
#' @import graphics
#' @import grDevices
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
#'
#' model.coef <- coef(fitted.data)
#'
#' plot(model.coef)$plot
#'
#' @seealso \code{\link{fit_model}},\code{\link{coef}}
#' @family auxiliary visualization functions for the fitted_dlm class
plot.dlm_coef <- function(x, var = rownames(x$theta.mean)[x$dynamic], cutoff = floor(t / 10), pred.cred = 0.95, plot.pkg = "auto", ...) {
  if (plot.pkg == "auto") {
    plot.pkg <- if (requireNamespace("plotly", quietly = TRUE) & requireNamespace("ggplot2", quietly = TRUE)) {
      "plotly"
    } else if (requireNamespace("ggplot2", quietly = TRUE)) {
      "ggplot2"
    } else {
      "base"
    }
  }

  t <- dim(x$theta.mean)[2]
  var.labels <- rownames(x$theta.mean)
  pred.names <- rownames(x$lambda.mean)

  flags.var <- (sapply(var, function(x) {
    grepl(tolower(x), tolower(var.labels), fixed = TRUE)
  }) |> matrix(length(var.labels), length(var)) |> rowSums()) > 0
  flags.pred <- (sapply(var, function(x) {
    grepl(tolower(x), tolower(pred.names), fixed = TRUE)
  }) |> matrix(length(pred.names), length(var)) |> rowSums()) > 0

  if (!any(flags.var) & !any(flags.pred)) {
    stop(paste0("Error: Invalid variable selection. Got '", var, "', expected one of the following:\n", paste0(c(var.labels, pred.names), collapse = "\n")))
  }

  var.labels <- var.labels[flags.var]
  pred.names <- pred.names[flags.pred]

  size <- sum(flags.var) + sum(flags.pred)
  seq.time <- (cutoff + 1):t

  m1 <- rbind(
    x$theta.mean[flags.var, seq.time, drop = FALSE],
    x$lambda.mean[flags.pred, seq.time, drop = FALSE]
  ) |>
    t()
  std.mat.var <- x$theta.cov[flags.var, flags.var, seq.time, drop = FALSE] |>
    apply(3, diag) |>
    sqrt() |>
    matrix(sum(flags.var), t - cutoff) |>
    t()
  std.mat.pred <- x$lambda.cov[flags.pred, flags.pred, seq.time, drop = FALSE] |>
    apply(3, diag) |>
    sqrt() |>
    matrix(sum(flags.pred), t - cutoff) |>
    t()
  std.mat <- cbind(std.mat.var, std.mat.pred)

  lim.i <- m1 + qnorm((1 - pred.cred) / 2) * std.mat
  lim.s <- m1 + qnorm(1 - (1 - pred.cred) / 2) * std.mat

  max.value <- evaluate_max(m1 - min(m1))[[3]] + min(m1)
  min.value <- -evaluate_max(-(m1 - max(m1)))[[3]] + max(m1)

  if (max.value - min.value < 1e-2) {
    center <- (max.value + min.value) / 2
    max.value <- center + 0.01
    max.value <- center - 0.01
  }

  plot.data <- data.frame(
    Time = seq.time,
    Label = as.factor(c(sapply(c(var.labels, pred.names), function(x) {
      rep(x, t - cutoff)
    }))),
    Mean = c(m1),
    C.I.lower = c(lim.i),
    C.I.upper = c(lim.s)
  )

  label <- paste0("C.I. (", pred.cred * 100 |> round(), "%)")

  var.names <- levels(plot.data$Label)
  n.var <- length(var.names)
  color.list <- rainbow(n.var, s = 0.8, v = 0.9)
  names(color.list) <- var.names

  fill.list <- rainbow(n.var, s = 0.4, v = 0.9)
  names(fill.list) <- var.names
  plot.data$fill.name <- plot.data$Label
  plot.data$color.name <- plot.data$Label

  title <- if (x$lag < 0) {
    "Smoothed estimation of latent states"
  } else if (x$lag == 0) {
    "Filtered estimation of latent states"
  } else if (x$lag == 1) {
    "One-step-ahead prediction for latent states"
  } else {
    paste0(x$lag, "-steps-ahead prediction for latent states")
  }

  if (plot.pkg == "base" | !requireNamespace("ggplot2", quietly = TRUE)) {
    points <- paste0(color.list, "55")
    fills <- paste0(color.list, "33")
    names(color.list) <- names(points) <- names(fills) <- var.names

    if (plot.pkg != "base") {
      warning("The ggplot2 package is required for ggplot2 and plotly plots and was not found. Falling back to R base plot functions.")
    }

    cur.height <- dev.size("cm")[2]
    count.spaces <- ceiling(n.var / 4)
    font.cm <- 0.35

    oldpar <- par(no.readonly = TRUE)
    on.exit(par(oldpar))

    layout(
      mat = matrix(c(1, 2, 3), 3, 1),
      heights = c(
        cur.height - font.cm * count.spaces - 1 - 0.75,
        0.75,
        font.cm * count.spaces + 1
      )
    )

    par(mar = c(4.1, 4.1, 4.1, 2.1), cex = 1)
    plot(0, 0, type = "n", xlim = c(cutoff, t), ylim = c(min.value, max.value), ylab = "Parameter value", xlab = "Time", main = title)
    for (var.name in var.names) {
      plot.serie <- plot.data[plot.data$Label == var.name, ]
      points(plot.serie$Time, plot.serie$Observation,
        col = points[[var.name]],
        pch = 16
      )
      lines(plot.serie$Time, plot.serie$Mean, col = color.list[[var.name]])
      base_ribbon(plot.serie$Time, plot.serie$C.I.lower, plot.serie$C.I.upper,
        col = fills[[var.name]], lty = 0
      )
    }
    lines(c(-1, t + 2), c(0, 0), lty = 2)

    par(mar = c(0, 0, 0, 0), cex = 1)
    plot(0, type = "n", axes = FALSE, xlab = "", ylab = "", xlim = c(0, 1), ylim = c(0, 1))
    legend(
      legend = c("Mean", label),
      col = c("black", "black"),
      lty = c(1, 0),
      seg.len = 0.6,
      pch = c(0, 22),
      pt.cex = c(0, 2),
      pt.bg = c("#00000000", "#00000033"),
      border = "#ffffff00",
      x = 0.5, xjust = 0.5, y = 1, bty = "n", cex = 0.75, horiz = TRUE
    )
    par(mar = c(0, 0, 0, 0), cex = 1)
    plot(0, type = "n", axes = FALSE, xlab = "", ylab = "", xlim = c(0, 1), ylim = c(0, 1))
    legend(
      legend = var.names,
      col = color.list,
      lty = rep(0, n.var),
      pch = rep(22, n.var),
      pt.cex = rep(2, n.var),
      pt.bg = color.list,
      x = 0.5, xjust = 0.5, y = 1, inset = 0, cex = 0.75, bty = "n",
      ncol = min(4, ceiling(n.var / count.spaces))
    )
    plt <- NULL
  } else {
    # fix GeomRibbon
    # ggplot2::GeomRibbon$handle_na <- function(data, params) {  data }

    plt <- ggplot2::ggplot(plot.data, ggplot2::aes_string(x = "Time", fill = "Label", color = "color.name")) +
      ggplot2::geom_hline(yintercept = 0, linetype = "dashed") +
      ggplot2::scale_x_continuous("Time") +
      ggplot2::scale_color_manual("", values = color.list, na.value = NA) +
      ggplot2::scale_fill_manual("", values = fill.list, na.value = NA) +
      ggplot2::labs(title = title) +
      ggplot2::scale_y_continuous("Parameter value") +
      ggplot2::theme_bw() +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90)) +
      ggplot2::geom_ribbon(ggplot2::aes_string(ymin = "C.I.lower", ymax = "C.I.upper"), alpha = 0.25, color = NA, na.rm = TRUE) +
      ggplot2::geom_line(ggplot2::aes_string(y = "Mean"), na.rm = TRUE) +
      ggplot2::coord_cartesian(ylim = c(min.value, max.value))
    if (plot.pkg == "plotly") {
      if (!requireNamespace("plotly", quietly = TRUE)) {
        warning("The plotly package is required for plotly plots.")
      } else {
        plt <- plotly::ggplotly(plt)
        for (i in (1:size) - 1) {
          plt$x$data[[i + 1 + 1]]$legendgroup <-
            plt$x$data[[i + 1 + size + 1]]$legendgroup <-
            plt$x$data[[i + 1 + 1]]$name <-
            plt$x$data[[i + 1 + size + 1]]$name <- plt$x$data[[i + 1 + size + 1]]$name

          plt$x$data[[i + 1 + 1]]$showlegend <- FALSE
        }
      }
    }
    return(plt)
  }
}
