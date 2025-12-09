#' Fitting kDGLM models
#'
#' Fit a model given its structure and the observed data. This function can be used for any supported family (see vignette).
#'
#' @param ... dlm_block or dlm_distr objects or named values: The structural blocks of the model (dlm_block objects), alongside the model outcomes (dlm_distr object). If at least one block is undefined, the user must also provide its value in this argument (see last example).
#' @param smooth boolean: A flag indicating if the smoothed distribution for the latent states should be calculated.
#' @param p.monit numeric (optional): The prior probability of changes in the latent space variables that are not part of its dynamic. Only used when performing sensitivity analysis.
#' @param condition character: A character defining which combinations of undefined hyper parameter should be tested. See example for details. Only used when performing sensitivity analysis.
#' @param metric character: The name of the metric to use for model selection. One of log-likelihood for the one-step-ahead prediction ("log.like"), Mean Absolute Scaled Error ("mase") \insertCite{mase}{kDGLM} or Interval Score ("interval.score") \insertCite{interval_score}{kDGLM}.  Only used when performing sensitivity analysis.
#' @param lag integer: The number of steps ahead used for the prediction when calculating the metrics. If lag<0, predictions are made using the smoothed distribution of the latent states.  Only used when performing sensitivity analysis.
#' @param pred.cred numeric: A number between 0 and 1 (not included) indicating the credibility interval for predictions. If not within the valid range of values, 0.95 will be used. Only used when performing sensitivity analysis.
#' @param metric.cutoff integer: The number of observations to ignore when calculating the metrics. Default is 1/10 of the number of observations (rounded down). Only used when performing sensitivity analysis.
#' @param save.models boolean: A flag indicating if all evaluated models should be saved. If FALSE, only the best model (according to the chosen metric) will be saved. Only used when performing sensitivity analysis.
#' @param silent boolean: A flag indicating if a progress bar should be printed. Only used when performing sensitivity analysis.
#'
#' @return A fitted_dlm object.
#' @export
#'
#' @examples
#'
#' # Poisson case
#' data <- c(AirPassengers)
#'
#' level <- polynomial_block(rate = 1, order = 2, D = 0.95)
#' season <- harmonic_block(rate = 1, order = 2, period = 12, D = 0.975)
#'
#' outcome <- Poisson(lambda = "rate", data = data)
#'
#' fitted.data <- fit_model(level, season,
#'   AirPassengers = outcome
#' )
#' summary(fitted.data)
#'
#' plot(fitted.data, plot.pkg = "base")
#'
#' ##################################################################
#'
#' # Multinomial case
#' structure <- (
#'   polynomial_block(p = 1, order = 2, D = 0.95) +
#'     harmonic_block(p = 1, period = 12, D = 0.975) +
#'     noise_block(p = 1, R1 = 0.1) +
#'     regression_block(p = chickenPox$date >= as.Date("2013-09-01"))
#'   # Vaccine was introduced in September of 2013
#' ) * 4
#'
#' outcome <- Multinom(p = structure$pred.names, data = chickenPox[, c(2, 3, 4, 6, 5)])
#' fitted.data <- fit_model(structure, chickenPox = outcome)
#' summary(fitted.data)
#' plot(fitted.data, plot.pkg = "base")
#'
#' ##################################################################
#'
#' # Univariate Normal case
#' structure <- polynomial_block(mu = 1, D = 0.95) +
#'   polynomial_block(V = 1, D = 0.95)
#'
#' outcome <- Normal(mu = "mu", V = "V", data = cornWheat$corn.log.return[1:500])
#' fitted.data <- fit_model(structure, corn = outcome)
#' summary(fitted.data)
#' plot(fitted.data, plot.pkg = "base")
#'
#' ##################################################################
#'
#' # Bivariate Normal case
#' structure <- (polynomial_block(mu = 1, D = 0.95) +
#'   polynomial_block(V = 1, D = 0.95)) * 2 +
#'   polynomial_block(rho = 1, D = 0.95)
#'
#' outcome <- Normal(
#'   mu = c("mu.1", "mu.2"),
#'   V = matrix(c("V.1", "rho", "rho", "V.2"), 2, 2),
#'   data = cornWheat[1:500, c(4, 5)]
#' )
#' fitted.data <- fit_model(structure, cornWheat = outcome)
#' summary(fitted.data)
#' plot(fitted.data, plot.pkg = "base")
#'
#' ##################################################################
#'
#' # Gamma case
#' structure <- polynomial_block(mu = 1, D = 0.95)
#'
#' Y <- (cornWheat$corn.log.return[1:500] - mean(cornWheat$corn.log.return[1:500]))**2
#' outcome <- Gamma(phi = 0.5, mu = "mu", data = Y)
#' fitted.data <- fit_model(structure, corn = outcome)
#' summary(fitted.data)
#' plot(fitted.data, plot.pkg = "base")
#'
#' ##################################################################
#' \donttest{
#' # Sensitivity analysis
#' data <- c(AirPassengers)
#'
#' level <- polynomial_block(rate = 1, order = 2, D = "D.level")
#' season <- harmonic_block(rate = "sazo.effect", order = 2, period = 12, D = "D.sazo")
#'
#' outcome <- Poisson(lambda = "rate", data = data)
#'
#' fit_model(level, season, outcome,
#'   sazo.effect = c(0, 1),
#'   D.level = c(seq.int(0.8, 1, l = 11)),
#'   D.sazo = c(seq.int(0.95, 1, l = 11)),
#'   condition = "sazo.effect==1 | D.sazo==1"
#' )
#' }
#'
#' @details
#'
#' This is the main function of the kDGLM package, as it is used to fit all models.
#'
#' For the details about the implementation see  \insertCite{ArtigoPacote;textual}{kDGLM}.
#'
#' For the details about the methodology see  \insertCite{ArtigokParametrico;textual}{kDGLM}.
#'
#' For the details about the Dynamic Linear Models see  \insertCite{WestHarr-DLM;textual}{kDGLM} and \insertCite{Petris-DLM;textual}{kDGLM}.
#'
#' @seealso auxiliary functions for creating outcomes \code{\link{Poisson}}, \code{\link{Multinom}}, \code{\link{Normal}}, \code{\link{Gamma}}
#' @seealso auxiliary functions for creating structural blocks \code{\link{polynomial_block}}, \code{\link{regression_block}}, \code{\link{harmonic_block}}, \code{\link{TF_block}}
#' @seealso auxiliary functions for defining priors \code{\link{zero_sum_prior}}, \code{\link{CAR_prior}}
#'
#' @family auxiliary functions for fitted_dlm objects
fit_model <- function(..., smooth = TRUE, p.monit = NA, condition = "TRUE", metric = "log.like", lag = 1, pred.cred = 0.95, metric.cutoff = NA, save.models = FALSE, silent = FALSE) {
  extra.args <- list(...)
  structure <- list()
  outcomes <- list()
  out.names <- c()
  hyper.args <- list()
  hyper.names <- c()
  search.args <- list()
  search.names <- c()
  models <- list()
  for (i in seq_along(extra.args)) {
    arg <- extra.args[[i]]
    arg.name <- if.null(names(extra.args)[i], "")
    if (inherits(arg, "dlm_distr")) {
      out.names <- c(out.names, arg.name)
      outcomes[[length(outcomes) + 1]] <- arg
    } else if (inherits(arg, "dlm_block")) {
      structure[[length(structure) + 1]] <- arg
    } else if (is.numeric(arg)) {
      if (is.null(arg.name)) {
        stop(paste0("Error: Extra arguments must be named."))
      }
      if (length(arg) == 1) {
        hyper.args[[arg.name]] <- arg
      } else {
        search.args[[arg.name]] <- arg
      }
    } else {
      stop(paste0("Error: Invalid type for ... argument. Expected a dlm_block or dlm_distr object, got ", class(arg), ". Be sure that all arguments are properly named."))
    }
  }

  if (length(outcomes) == 0) {
    stop("Error: No dlm_distr object was passed. Make sure all outcomes are created using the proper functions (see documentation).")
  }
  if (length(structure) == 0) {
    stop("Error: No dlm_block object was passed. Make sure all blocks are created using the proper functions (see documentation).")
  }
  if (any(out.names == "")) {
    out.names[out.names == ""] <- paste0("Series.", 1:length(outcomes))[out.names == ""]
  }
  names(outcomes) <- out.names

  structure <- do.call(block_superpos, structure)
  if (length(hyper.args) > 0) {
    structure <- do.call(function(...) {
      specify.dlm_block(structure, ...)
    }, hyper.args)
  }

  if (length(outcomes) == 0) {
    stop("Error: No dlm_distr object was passed. Make sure all outcomes are created using the proper functions (see documentation).")
  }
  if (length(structure) == 0) {
    stop("Error: No dlm_block object was passed. Make sure all blocks are created using the proper functions (see documentation).")
  }

  if (structure$status == "defined") {
    return(fit_model_single(structure, outcomes, smooth, p.monit))
  } else {
    if (is.null(pred.cred) || is.na(pred.cred)) {
      pred.cred <- 0.95
    } else {
      if (pred.cred <= 0 || pred.cred >= 1) {
        warning("Invalid value for credibility. Using 95% of credibility.")
        pred.cred <- 0.95
      }

      ref.structure <- structure

      if (any(names(search.args) %in% ref.structure$pred.names)) {
        stop(paste0(
          "Error: Ambiguous label for hyper parameter. Cannot have a hyper parameter with the same name of a linear predictor\n
         The user passed the following hyper parameters: ", paste0(names(search.args), collapse = ", "), ".\n",
          "The model has the the following linear predictors: ", paste0(ref.structure$pred.names, collapse = ", "), "."
        ))
      }

      var.length <- length(search.args)
      search.data <- do.call(expand.grid, search.args)
      if (condition != "TRUE") {
        search.data <- search.data[eval(parse(text = condition), envir = search.data), ]
      }
      if (dim(search.data)[1] == 0) {
        stop("Error: No model to test. Verify if the hyper-parameters are properly specified and if a valid value for condition argument is being passed.")
      }

      search.data$log.like <- NA
      search.data$mase <- NA
      search.data$interval.score <- NA

      dimnames.FF <- dimnames(structure$FF)

      vals.nums <- dim(search.data)[1]
      vals.size <- dim(search.data)[2]
      best.structure <- NULL
      best.model <- NULL
      best.metric <- Inf
      init <- Sys.time()
      for (i in seq_len(vals.nums)) {
        time.past <- Sys.time() - init
        raw.perc <- (i - 1) / vals.nums
        perc <- round(100 * raw.perc, 2)
        n.bar <- round(50 * raw.perc)
        if (!silent) {
          cat(paste0(
            "\r[", paste0(rep("=", n.bar), collapse = ""),
            paste0(rep(" ", 50 - n.bar), collapse = ""), "] - ",
            perc,
            "% - ETA - ", if (i == 1) {
              "NA"
            } else {
              paste0(round(as.numeric((1 - raw.perc) * time.past / raw.perc, units = "mins"), 2), " minutes")
            },
            ""
          ))
        }
        structure <- do.call(function(...) {
          specify.dlm_block(ref.structure, ...)
        }, search.data[i, 1:(vals.size - 3), drop = FALSE])

        if (structure$status == "undefined") {
          stop("Error: not all unkown hiper parameter have values. Check the search grid to make sure every unkown hiper parameter has a range of values.")
        }

        if (any(if.na(structure$D, 0) < 0 | if.na(structure$D, 0) > 1)) {
          stop(paste0("Error: invalid value for D. Expected a real number between 0 and 1, got: ", paste(structure$D[if.na(structure$D, 0) < 0 | if.na(structure$D, 0) > 1], collapse = ", "), "."))
        }

        fitted.model <- fit_model_single(structure = structure, outcomes = outcomes, smooth = FALSE, p.monit = p.monit)

        T_len <- fitted.model$t
        if (is.na(metric.cutoff)) {
          metric.cutoff <- floor(T_len / 10)
        }

        r <- length(fitted.model$outcomes)
        metric <- tolower(metric)
        predictions <- coef.fitted_dlm(fitted.model, t.eval = (metric.cutoff + 1):T_len, lag = lag, pred.cred = pred.cred, eval.pred = TRUE, eval.metric = TRUE)

        search.data$log.like[i] <- sum(predictions$metrics$log.like, na.rm = TRUE)
        search.data$mase[i] <- mean(predictions$metrics$mase, na.rm = TRUE)
        search.data$interval.score[i] <- mean(predictions$metrics$interval.score, na.rm = TRUE)
        if (save.models) {
          label <- paste(names(search.data[i, ])[1:(vals.size - 3)], "=", search.data[i, 1:(vals.size - 3)], collapse = "; ")
          models[[label]] <- fitted.model
        }

        cur.metric <- ifelse(metric == "log.like", -search.data$log.like[i], search.data[[metric]][i])
        if (cur.metric < best.metric) {
          best.structure <- structure
          best.model <- fitted.model
          best.metric <- cur.metric
        }
      }

      if (!silent) {
        cat("\n")
      }
      search.data <- search.data[order(-search.data[[metric]], decreasing = (metric != "log.like")), ]

      if (smooth & !save.models) {
        best.model <- smoothing(best.model)
      }

      out.vals <- list(
        search.data = search.data,
        structure = best.structure,
        model = best.model
      )
      if (save.models) {
        out.vals$models <- models
      }
      class(out.vals) <- "searched_dlm"

      return(out.vals)
    }
  }
}

#' Fitting one kDGLM models
#'
#' Fits one model given its structure and the observed data. This function can be used for any supported family (see vignette).
#'
#' @param structure dlm_block: The structural blocks of the model. All block must be completely defined.
#' @param outcomes dlm_distr or list of dlm_distr objects: The model outcomes.
#' @param smooth boolean: A flag indicating if the smoothed distribution for the latent states should be calculated.
#' @param p.monit numeric (optional): The prior probability of changes in the latent space variables that are not part of its dynamic.
#'
#' @return A fitted_dlm object.
fit_model_single <- function(structure, outcomes, smooth = TRUE, p.monit = NA) {
  if (structure$status == "undefined") {
    stop("Error: One or more hiper parameter are undefined. Did you forget to pass the hyper-parameters?")
  }

  t <- sapply(outcomes, function(x) {
    x$t
  })
  if (min(t) != max(t)) {
    stop(paste0("Error: outcomes does not have the same time length."))
  }
  t <- max(t)

  block.names <- names(structure$var.names)

  for (name in unique(block.names)) {
    count.name <- sum(block.names == name)
    if (count.name > 1) {
      len.char <- floor(log10(count.name)) + 1
      block.names[block.names == name] <- paste0(name, ".", formatC(1:count.name, width = len.char, flag = "0"))
    }
  }
  names(structure$var.names) <- block.names

  coef.names <- rep(NA, structure$n)
  for (name in names(structure$var.names)) {
    if (length(names(structure$var.names[[name]])) > 1) {
      coef.names[structure$var.names[[name]]] <- paste0(name, ".", names(structure$var.names[[name]]))
    } else {
      coef.names[structure$var.names[[name]]] <- name
    }
  }
  structure$var.labels <- coef.names

  if (structure$t == 1) {
    structure$t <- t
    structure$G <- array(structure$G, c(structure$n, structure$n, structure$t), dimnames = dimnames(structure$G))
    structure$D <- array(structure$D, c(structure$n, structure$n, structure$t), dimnames = dimnames(structure$D))
    structure$h <- matrix(structure$h, structure$n, structure$t)
    structure$H <- array(structure$H, c(structure$n, structure$n, structure$t), dimnames = dimnames(structure$H))
    structure$FF <- array(structure$FF, c(structure$n, structure$k, structure$t), dimnames = dimnames(structure$FF))
    structure$FF.labs <- matrix(structure$FF.labs, structure$n, structure$k)
  }
  if (t != structure$t) {
    stop(paste0("Error: outcome does not have the same time length as structure: got ", t, " from outcome, expected ", structure$t))
  }

  pred.names.out <- unique(structure$FF.labs)
  pred.names.out <- pred.names.out[pred.names.out != "const" & pred.names.out != "Covariate"]
  for (outcome in outcomes) {
    pred.names.out <- c(pred.names.out, outcome$pred.names)
  }
  pred.names.out <- unique(pred.names.out)
  if (any(!(pred.names.out %in% structure$pred.names))) {
    stop("Error: Some linear predictors are being used in the outcomes, but are not defined in the model structure.")
  }
  if (any(!(structure$pred.names %in% pred.names.out))) {
    warning("Some linear predictors in the model structure were not used in the outcomes.")
  }

  for (intervention in structure$interventions) {
    var.index.inter <- intervention$var.index
    if (!is.null(intervention$FF)) {
      structure$FF[
        var.index.inter,
        structure$pred.names %in% intervention$pred.names,
        intervention$times
      ] <- intervention$FF
    }

    if (!is.null(intervention$D)) {
      structure$D[
        var.index.inter, var.index.inter,
        intervention$times
      ] <- intervention$D
    }

    if (!is.null(intervention$h)) {
      structure$h[
        var.index.inter,
        intervention$times
      ] <- intervention$h
    }

    if (!is.null(intervention$H)) {
      structure$H[
        var.index.inter, var.index.inter,
        intervention$times
      ] <- intervention$H
    }

    if (!is.null(intervention$G)) {
      structure$G[
        var.index.inter, var.index.inter,
        intervention$times
      ] <- intervention$G
    }
  }
  G <- structure$G
  D <- structure$D
  h <- structure$h
  H <- structure$H

  G.1 <- G[, , 1]
  D.1 <- D[, , 1]
  h.1 <- h[, 1]
  H.1 <- H[, , 1]

  G[, , 1] <- diag(structure$n)
  D[, , 1] <- 1
  h[, 1] <- 0
  H[, , 1] <- 0

  model <- analytic_filter(
    outcomes = outcomes,
    a1 = structure$a1,
    R1 = structure$R1,
    FF = structure$FF,
    FF.labs = structure$FF.labs,
    G = G,
    G.labs = structure$G.labs,
    G.idx = structure$G.idx,
    D = D,
    h = h,
    H = H,
    p.monit = p.monit,
    monitoring = structure$monitoring
  )


  model$G[, , 1] <- G.1
  model$D[, , 1] <- D.1
  model$h[, 1] <- h.1
  model$H[, , 1] <- H.1

  flags.dynamic <- rep(FALSE, structure$n)

  ref.G <- ifelse(is.na(model$G), pi, model$G)
  flags.dynamic <- flags.dynamic | sapply(1:structure$n, function(i) {
    any(ref.G[i, -i, ] != 0) | any(ref.G[i, i, ] != 1)
  })
  flags.dynamic <- flags.dynamic | sapply(1:structure$n, function(i) {
    any(model$h[i, ] != 0)
  })
  flags.dynamic <- flags.dynamic | sapply(1:structure$n, function(i) {
    any(model$W[i, i, ] > 0)
  })

  r <- sum(sapply(model$outcomes, function(x) {
    x$r
  }))
  model$a1 <- structure$a1
  model$R1 <- structure$R1
  model$var.names <- structure$var.names
  model$var.labels <- structure$var.labels
  model$t <- t
  model$n <- structure$n
  model$k <- structure$k
  model$l <- structure$l
  model$r <- r
  model$p.monit <- p.monit
  model$monitoring <- structure$monitoring
  model$structure <- structure
  model$period <- structure$period
  model$dynamic <- flags.dynamic

  rownames(model$mt) <- rownames(model$Ct) <- colnames(model$Ct) <-
    rownames(model$at) <- rownames(model$Rt) <- colnames(model$Rt) <-
    model$var.labels

  names(model$a1) <- rownames(model$R1) <- colnames(model$R1) <-
    model$var.labels

  rownames(model$ft) <- rownames(model$Qt) <- colnames(model$Qt) <-
    colnames(model$FF)

  if (smooth) {
    model <- smoothing(model)
  }

  class(model) <- "fitted_dlm"

  return(model)
}

#' Auxiliary function for model smoothing
#'
#' @param model A fitted_dlm object.
#'
#' @family auxiliary functions for fitted_dlm objects
#' @return A fitted_dlm object with smoothed means (mts) and covariance matrix (Cts) for each observation.
#' @export
smoothing <- function(model) {
  if (!model$smooth) {
    smoothed <- generic_smoother(model$mt, model$Ct, model$at, model$Rt, model$G, model$G.labs, model$G.idx)
    model$mts <- smoothed$mts
    model$Cts <- smoothed$Cts

    rownames(model$mts) <- rownames(model$Cts) <- colnames(model$Cts) <- model$var.labels

    model$smooth <- TRUE
  } else {
    warning("Model already smoothed.")
  }
  return(model)
}

#' Auxiliary function for forecasting
#'
#' @param object fitted_dlm object: The fitted model to be use for predictions.
#' @param t numeric: Time window for prediction.
#' @param plot boolean or character: A flag indicating if a plot should be produced. Should be one of FALSE, TRUE, 'base', 'ggplot2' or 'plotly'.
#' @param pred.cred numeric: The credibility level for the C.I..
#' @param ... Extra variables necessary for prediction (covariates, etc.).
#'
#' @return A list containing:
#' \itemize{
#'    \item data data.frame: A data frame contain the mean, variance and credibility intervals for the outcomes, including both the observed data and the predictions for future observations.
#'    \item forecast data.frame: Same as data, but restricted to predictions for future observations.
#'    \item outcomes list: A named list containing predictions for each outcome. Each element of this list is a list containing predictions (mean, variance and credibility intervals), the distribution of the linear predictor for the parameter of the observational model and the parameters of the predictive distribution (if available).
#'    \item theta.mean matrix: A matrix with the values for the latent states at each time. Dimensions are n x t, where n is the number of latent states
#'    \item theta.cov array: A 3D-array with the covariance of the latent states at each time. Dimensions are n x n x t, where n is the number of latent predictors.
#'    \item lambda.mean matrix: A matrix with the values for the linear predictors at each time. Dimensions are k x t, where k is the number of linear predictors
#'    \item lambda.cov array: A 3D-array with the covariance of the linear predictors at each time. Dimensions are k x k x t, where k is the number of linear predictors.
#'    \item plot (if so chosen): A plotly or ggplot object.
#' }
#' @importFrom Rfast transpose data.frame.to_matrix
#' @import graphics
#' @import grDevices
#' @rdname forecast.fitted_dlm
#' @export
#'
#' @details
#' If an a covariate is necessary for forecasting, it should be passed as a named argument. Its name must follow this structure: <block name>.Covariate<.index>. If there is only one covariate in the associated block the index is omitted.
#' If an a pulse is necessary for forecasting, it should be passed as a named argument. Its name must follow this structure: <block name>.Pulse<.index>. If there is only one pulse in the associated block the index is omitted.
#' The user may pass the observed values at the prediction windows (optional). See example.
#' As an special case, if the model has an Multinomial outcome, the user may pass the N parameter instead of the observations.
#' If an offset is necessary for forecasting, it should be passed with the same syntax as the observed data. See example.
#'
#' @examples
#'
#' structure <-
#'   polynomial_block(p = 1, order = 2, D = 0.95) +
#'   harmonic_block(p = 1, period = 12, D = 0.975) +
#'   noise_block(p = 1, R1 = 0.1) +
#'   regression_block(
#'     p = chickenPox$date >= as.Date("2013-09-1"),
#'     # Vaccine was introduced in September of 2013
#'     name = "Vaccine"
#'   )
#'
#' outcome <- Multinom(p = c("p.1", "p.2"), data = chickenPox[, c(2, 3, 5)])
#' fitted.data <- fit_model(structure * 2,
#'   chickenPox = outcome
#' )
#'
#' forecast(fitted.data, 24,
#'   chickenPox = list(Total = rep(175, 24)), # Optional
#'   Vaccine.1.Covariate = rep(TRUE, 24),
#'   Vaccine.2.Covariate = rep(TRUE, 24)
#' )
#'
#' @return A list containing:
#' \itemize{
#'    \item data data.frame: A table with the model evaluated at each observed time, plus the forecasted period.
#'    \item forecast data.frame: A table with the model evaluated at the forecasted period.
#'    \item outcomes list: A list containing the parameters of the predictive distribution for each outcome at the forecasted period.
#'    \item theta.mean matrix: The mean of the latent states at each forecasted time. Dimensions are n x t.forecast, where t.forecast is the size of the forecast windows and n is the number of latent states.
#'    \item theta.cov array: A 3D-array containing the covariance matrix of the latent states  at each forecasted time. Dimensions are n x n x t.forecast, where t.forecast is the size of the forecast windows and n is the number of latent states.
#'    \item lambda.mean matrix: The mean of the linear predictor at each forecasted time. Dimensions are k x t.forecast, where t.forecast is the size of the forecast windows and k is the number of linear predictors.
#'    \item lambda.cov array: A 3D-array containing the covariance matrix for the linear predictor at each forecasted time. Dimensions are k x k x t.forecast, where t.forecast is the size of the forecast windows and k is the number of linear predictors.
#' }
#'
#' @family auxiliary functions for fitted_dlm objects
forecast.fitted_dlm <- function(object, t = 1,
                                plot = ifelse(requireNamespace("plotly", quietly = TRUE), "plotly", ifelse(requireNamespace("ggplot2", quietly = TRUE), "ggplot2", "base")),
                                pred.cred = 0.95,
                                ...) {
  if (plot == TRUE) {
    plot <- ifelse(requireNamespace("plotly", quietly = TRUE), "plotly", ifelse(requireNamespace("ggplot2", quietly = TRUE), "ggplot2", "base"))
  }

  n <- object$n
  t_last <- object$t
  k <- object$k
  r <- object$r
  pred.names <- object$pred.names
  pred <- matrix(NA, r, t)
  var.pred <- array(NA, c(r, t))
  icl.pred <- matrix(NA, r, t)
  icu.pred <- matrix(NA, r, t)

  time.index <- seq_len(t_last)
  time.index.foward <- (t_last + 1):(t_last + t)

  extra.args <- list(...)
  h <- H <- NULL
  if (!is.null(extra.args[["h"]])) {
    h <- extra.args[["h"]]
  }
  if (!is.null(extra.args[["H"]])) {
    H <- extra.args[["H"]]
  }

  #### Consistency check ####
  if (length(dim(h)) > 2) {
    stop(paste0("Error: H should have at most 2 dimensions. Got ", length(dim(h)), "."))
  }
  if (length(dim(H)) > 3) {
    stop(paste0("Error: H should have at most 3 dimensions. Got ", length(dim(H)), "."))
  }

  G.labs <- object$G.labs
  G.idx <- object$G.idx
  G <- array(object$G[, , t_last], c(n, n, t))
  G.flags <- G.labs == "Pulse"
  if (any(G.flags)) {
    for (i in seq_len(n)[rowSums(G.flags) > 0]) {
      for (j in seq_len(n)[G.flags[i, ]]) {
        label <- paste0(object$var.labels[j])
        if (label %in% names(extra.args)) {
          G[i, j, ] <- extra.args[[label]]
        } else {
          stop(paste0("Error: Missing extra argument: ", label))
        }
      }
    }
  }

  FF.labs <- object$FF.labs
  FF <- array(object$FF[, , t_last], c(n, k, t))
  FF.flags <- FF.labs == "Covariate"
  if (any(FF.flags)) {
    coef.names <- rep(NA, object$n)
    for (name in names(object$var.names)) {
      coef.names[object$var.names[[name]]] <- name
    }
    for (j in seq_len(k)[colSums(FF.flags) > 0]) {
      for (i in seq_len(n)[FF.flags[, j]]) {
        if (sum(FF.flags[i, ]) == 1) {
          label <- coef.names[i] |>
            paste(FF.labs[i, j], sep = ".")
        } else {
          label <- coef.names[i] |>
            paste(FF.labs[i, j], cumsum(FF.flags[i, ])[j], sep = ".")
        }
        if (label %in% names(extra.args)) {
          FF[i, j, ] <- extra.args[[label]]
        } else {
          stop(paste0("Error: Missing extra argument: ", label))
        }
      }
    }
  }


  if (is.null(h)) {
    h <- matrix(object$h[, t_last], n, t)
  }
  dim.H <- dim(H)
  if (is.null(H)) {
    H <- array(0, c(n, n, t))
    # H[, , -1] <- 0
  } else {
    if (all(dim.H == 1)) {
      H <- array(diag(n) * H, c(n, n, t))
    } else {
      if (length(dim.H) == 2 || (length(dim.H) == 3 && dim.H[3] == 1)) {
        H <- array(H, c(n, n, t))
        H[, , -1] <- 0
      }
    }
  }
  if (any(dim(H) != c(n, n, t))) {
    stop(paste0("Error: H has wrong dimesions. Expected ", paste(n, n, t, sep = "x"), ". Got ", paste(dim(H), colapse = "x")))
  }

  a1 <- object$mt[, t_last]
  R1 <- object$Ct[, , t_last]
  D <- object$W[, , t_last] - object$H[, , t_last]
  # D <- object$W[, , t_last]*0

  m1 <- matrix(NA, n, t)
  C1 <- array(NA, c(n, n, t))
  f1 <- matrix(NA, k, t)
  Q1 <- array(NA, c(k, k, t))
  D.placeholder <- R1**0

  last.m <- a1
  last.C <- R1

  outcome.forecast <- list()
  for (outcome.name in names(object$outcomes)) {
    r_i <- object$outcomes[[outcome.name]]$r
    outcome.forecast[[outcome.name]]$conj.param <- matrix(NA, t, length(object$outcomes[[outcome.name]]$param.names)) |> as.data.frame()
    names(outcome.forecast[[outcome.name]]$conj.param) <- object$outcomes[[outcome.name]]$param.names
    row.names(outcome.forecast[[outcome.name]]$conj.param) <- time.index.foward

    outcome.forecast[[outcome.name]]$ft <- matrix(NA, object$outcomes[[outcome.name]]$k, t)
    outcome.forecast[[outcome.name]]$Qt <- array(NA, c(object$outcomes[[outcome.name]]$k, object$outcomes[[outcome.name]]$k, t))

    outcome.forecast$show <- !is.null(extra.args[[outcome.name]]$data)
    outcome.forecast[[outcome.name]]$data <-
      if (!is.null(extra.args[[outcome.name]]$data)) {
        extra.args[[outcome.name]]$data |>
          as.matrix() |>
          matrix(t, r_i)
      } else if (object$outcomes[[outcome.name]]$name == "Multinomial" & !is.null(extra.args[[outcome.name]]$total)) {
        (extra.args[[outcome.name]]$total / r_i) |> matrix(t, r_i, byrow = FALSE)
      } else {
        object$outcomes[[outcome.name]]$data[t_last, ] |> matrix(t, r_i, byrow = TRUE)
      }
    outcome.forecast[[outcome.name]]$offset <-
      if (!is.null(extra.args[[outcome.name]]$offset)) {
        extra.args[[outcome.name]]$offset |>
          as.matrix() |>
          matrix(t, r_i)
      } else {
        object$outcomes[[outcome.name]]$offset[t_last, ] |> matrix(t, r_i, byrow = TRUE)
      }
  }


  for (t_i in seq_len(t)) {
    next.step <- one_step_evolve(last.m, last.C, G[, , t_i] |> matrix(n, n), G.labs, G.idx, D.placeholder, h[, t_i], H[, , t_i] + D)
    last.m <- next.step$at
    last.C <- next.step$Rt

    m1[, t_i] <- last.m
    C1[, , t_i] <- last.C

    lin.pred <- calc_lin_pred(
      last.m, last.C,
      FF[, , t_i] |> matrix(n, k, dimnames = list(NULL, pred.names)),
      FF.labs, pred.names, 1:k
    )
    f1[, t_i] <- lin.pred$ft
    Q1[, , t_i] <- lin.pred$Qt
    for (outcome.name in names(object$outcomes)) {
      model_i <- object$outcomes[[outcome.name]]
      pred.index <- match(model_i$pred.names, object$pred.names)
      lin.pred.offset <- model_i$apply_offset(
        lin.pred$ft[pred.index, , drop = FALSE],
        lin.pred$Qt[pred.index, pred.index, drop = FALSE],
        outcome.forecast[[outcome.name]]$offset[t_i, ]
      )
      if (model_i$convert.canom.flag) {
        ft.canom <- model_i$convert.mat.canom %*% lin.pred.offset$ft
        Qt.canom <- model_i$convert.mat.canom %*% lin.pred.offset$Qt %*% transpose(model_i$convert.mat.canom)
      } else {
        ft.canom <- lin.pred.offset$ft
        Qt.canom <- lin.pred.offset$Qt
      }

      conj.param <- model_i$conj_distr(ft.canom, Qt.canom, parms = model_i$parms)
      outcome.forecast[[outcome.name]]$conj.param[t_i, ] <- conj.param

      outcome.forecast[[outcome.name]]$ft[, t_i] <- lin.pred.offset$ft
      outcome.forecast[[outcome.name]]$Qt[, , t_i] <- lin.pred.offset$Qt
    }
  }

  r.acum <- 0
  out.names <- rep(NA, r)
  output <- matrix(NA, t, r)
  for (outcome.name in names(object$outcomes)) {
    model_i <- object$outcomes[[outcome.name]]
    r.cur <- model_i$r
    prediction <- model_i$calc_pred(outcome.forecast[[outcome.name]]$conj.param,
      outcome.forecast[[outcome.name]]$data,
      pred.cred = pred.cred,
      parms = model_i$parms
    )


    outcome.forecast[[outcome.name]]$pred <- prediction$pred
    outcome.forecast[[outcome.name]]$var.pred <- prediction$var.pred
    outcome.forecast[[outcome.name]]$icl.pred <- prediction$icl.pred
    outcome.forecast[[outcome.name]]$icu.pred <- prediction$icu.pred

    r.seq <- (r.acum + 1):(r.acum + r.cur)

    pred[r.seq, ] <- prediction$pred
    var.pred[r.seq, ] <- prediction$var.pred |>
      as.matrix() |>
      diag()
    icl.pred[r.seq, ] <- prediction$icl.pred
    icu.pred[r.seq, ] <- prediction$icu.pred



    out.names[r.seq] <- paste0(outcome.name, object$outcomes[[outcome.name]]$sufix)
    if (outcome.forecast$show) {
      output[, r.seq] <- outcome.forecast[[outcome.name]]$data
    }

    r.acum <- r.acum + r.cur
  }

  data <- data.frame(
    Time = time.index.foward,
    Serie = as.factor(c(sapply(out.names, function(x) {
      rep(x, t)
    }))),
    Observation = c(output),
    Variance = c(t(var.pred)),
    Prediction = c(t(pred)),
    C.I.lower = c(t(icl.pred)),
    C.I.upper = c(t(icu.pred))
  )

  data.past <- coef.fitted_dlm(object, lag = -1, pred.cred = pred.cred, eval.metric = FALSE, eval.pred = TRUE)$data
  plot.data <- rbind(
    cbind(data.past, type = "Fit"),
    cbind(data, type = "Forecast")
  )

  return.list <- list(data = plot.data, forecast = data, outcomes = outcome.forecast, theta.mean = m1, theta.cov = C1, lambda.mean = f1, lambda.cov = Q1)

  if (plot != FALSE) {
    obs.na.rm <- c(
      data.past$Observation[!is.na(data.past$Observation)],
      data$Observation[!is.na(data$Observation)]
    )
    if (!outcome.forecast$show | TRUE) {
      obs.na.rm <- c(
        obs.na.rm,
        data$Prediction[!is.na(data$Prediction)]
      )
    }
    max.value <- evaluate_max(obs.na.rm - min(obs.na.rm))[[3]] + min(obs.na.rm)
    min.value <- -evaluate_max(-(obs.na.rm - max(obs.na.rm)))[[3]] + max(obs.na.rm)
    plot.data$shape.point <- ifelse(plot.data$type == "Forecast", "Future", "Obs.")
    plot.data$group.ribbon <- paste0(plot.data$Serie, plot.data$type)
    series.names <- levels(plot.data$Serie)
    n.series <- length(series.names)
    colors <- rainbow(n.series, s = 0.8, v = 0.9)
    fills <- rainbow(n.series, s = 0.4, v = 0.9)
    if (plot == "base" || !requireNamespace("ggplot2", quietly = TRUE)) {
      if (plot != "base") {
        warning("The ggplot2 package is required for ggplot2 and plotly plots and was not found. Falling back to R base plot functions.")
      }
      fills <- paste0(fills, "33")
      points <- paste0(colors, "55")
      names(colors) <- names(points) <- names(fills) <- series.names
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
      plot(0, 0, type = "n", xlim = c(1, t + t_last), ylim = c(min.value, max.value), ylab = expression(Y[t]), xlab = "Time")


      for (serie in series.names) {
        plot.serie <- plot.data[plot.data$Serie == serie, ]
        points(plot.serie$Time, plot.serie$Observation,
          col = points[[serie]],
          pch = 16 + (seq_len(t + t_last) > t_last)
        )

        lines(plot.serie$Time[time.index], plot.serie$Prediction[time.index], col = colors[[serie]], lty = 1)
        lines(plot.serie$Time[time.index.foward], plot.serie$Prediction[time.index.foward], col = colors[[serie]], lty = 2)
        base_ribbon(plot.serie$Time, plot.serie$C.I.lower, plot.serie$C.I.upper,
          col = fills[[serie]], lty = 0
        )
      }

      par(mar = c(0, 0, 0, 0), cex = 1)
      plot(1, type = "n", axes = FALSE, xlab = "", ylab = "", xlim = c(0, 1), ylim = c(0, 1))
      legend(
        legend = c("Fit", "Forecast", "Obs.", "Future"),
        col = c("black", "black", "black", "black"),
        lty = c(1, 2, 0, 0),
        pch = c(0, 0, 16, 17),
        pt.cex = c(0, 0, 1, 1),
        fill = c("gray", "gray", "white", "white"),
        border = "#ffffff00",
        seg.len = 0.6,
        x = 0.5, y = 1, xjust = 0.5, inset = 0, cex = 0.75, bty = "n", horiz = TRUE
      )
      par(mar = c(0, 0, 0, 0), cex = 1)
      plot(1, type = "n", axes = FALSE, xlab = "", ylab = "", xlim = c(0, 1), ylim = c(0, 1))
      legend(
        legend = series.names,
        col = colors,
        lty = rep(0, n.series),
        pch = rep(22, n.series),
        pt.cex = rep(2, n.series),
        pt.bg = colors,
        x = 0.5, xjust = 0.5, y = 1, inset = 0, cex = 0.75, bty = "n",
        ncol = min(4, ceiling(n.series / count.spaces))
      )
    } else {
      # fix GeomRibbon
      # ggplot2::GeomRibbon$handle_na <- function(data, params) {  data }

      names(colors) <- names(fills) <- series.names
      plt.obj <- ggplot2::ggplot(plot.data, ggplot2::aes_string(x = "Time")) +
        ggplot2::geom_line(ggplot2::aes_string(y = "Prediction", linetype = "type", color = "Serie"), na.rm = TRUE) +
        ggplot2::geom_ribbon(ggplot2::aes_string(ymin = "C.I.lower", ymax = "C.I.upper", fill = "Serie", group = "group.ribbon"), alpha = 0.25, color = NA, na.rm = TRUE) +
        ggplot2::geom_point(ggplot2::aes_string(y = "Observation", shape = "shape.point", color = "Serie"), alpha = 0.5, na.rm = TRUE) +
        ggplot2::scale_fill_manual("", values = fills, na.value = NA) +
        ggplot2::scale_color_manual("", values = colors, na.value = NA) +
        ggplot2::scale_linetype_manual("", values = c("solid", "dashed")) +
        ggplot2::scale_shape_manual("", values = c(17, 16)) +
        ggplot2::scale_x_continuous("Time") +
        ggplot2::scale_y_continuous(expression(Y[t])) +
        ggplot2::theme_bw() +
        ggplot2::coord_cartesian(ylim = c(min.value, max.value))
      if (plot == "plotly") {
        if (!requireNamespace("plotly", quietly = TRUE)) {
          warning("The plotly package is required for plotly plots.")
        } else {
          series.names <- unique(plot.data$Serie)
          n.series <- length(series.names)
          plt.obj <- plotly::ggplotly(plt.obj + ggplot2::scale_y_continuous(plotly::TeX("Y_t"))) |> plotly::config(mathjax = "cdn")

          for (i in (1:n.series) - 1) {
            plt.obj$x$data[[2 * i + 1]]$legendgroup <-
              plt.obj$x$data[[2 * i + 1]]$name <-
              plt.obj$x$data[[2 * i + 2]]$legendgroup <-
              plt.obj$x$data[[2 * i + 2]]$name <-
              plt.obj$x$data[[i + 2 * n.series + 1]]$legendgroup <-
              plt.obj$x$data[[i + 2 * n.series + 1]]$name <-
              paste0(series.names[i + 1], ": fitted values")

            plt.obj$x$data[[2 * i + 1 + 3 * n.series]]$legendgroup <-
              plt.obj$x$data[[2 * i + 1 + 3 * n.series]]$name <-
              plt.obj$x$data[[2 * i + 2 + 3 * n.series]]$legendgroup <-
              plt.obj$x$data[[2 * i + 2 + 3 * n.series]]$name <- paste0(series.names[i + 1], ": observations")

            plt.obj$x$data[[2 * i + 1]]$showlegend <-
              plt.obj$x$data[[i + 1 + 2 * n.series]]$showlegend <-
              plt.obj$x$data[[2 * i + 1 + 3 * n.series]]$showlegend <- FALSE
          }
        }
      }
      return.list$plot <- plt.obj
    }
  }

  return(return.list)
}

#' update.fitted_dlm
#'
#' @param object fitted_dlm: The fitted model to be updated.
#' @param ... Extra variables necessary for updating (covariates, observed values, etc.).
#'
#' @return A fitted_dlm object.
#' @rdname update.fitted_dlm
#' @export
#'
#' @details
#' If an a covariate is necessary for updating, it should be passed as a named argument. Its name must follow this structure: <block name>.Covariate<.index>. If there is only one pulse in the associated block the index is omitted.
#' If an a pulse is necessary for updating, it should be passed as a named argument. Its name must follow this structure: <block name>.Pulse<.index>. If there is only one pulse in the associated block the index is omitted.
#' If an offset is necessary for updating, it should be passed along with the observed data. See example.
#'
#' @examples
#'
#' level <- polynomial_block(rate = 1, order = 2, D = 0.95)
#' season <- harmonic_block(rate = 1, order = 2, period = 12, D = 0.975)
#'
#' # Only first 100 observations (for the sake of the example)
#' outcome <- Poisson(lambda = "rate", data = c(AirPassengers)[1:100])
#'
#' fitted.data <- fit_model(level, season,
#'   AirPassengers = outcome
#' )
#'
#' updated.fit <- update(fitted.data, AirPassengers = list(data = c(AirPassengers)[101:144]))
#' # If a offset was present, the user should pass its value when updating
#' # updated.fit=update(fitted.data,
#' #                     AirPassengers=list(
#' #                      data=c(AirPassengers)[101:144],
#' #                      offset= ... ))
#'
#' @family auxiliary functions for fitted_dlm objects
update.fitted_dlm <- function(object, ...) {
  args <- list(...)
  n <- object$n
  t_last <- object$t
  k <- object$k
  r <- object$r
  outcomes.old <- object$outcomes
  outcomes.new <- list()
  for (name in names(outcomes.old)) {
    if (name %in% names(args)) {
      outcomes.new[[name]] <- outcomes.old[[name]]
      if (is.null(dim(args[[name]]$data))) {
        t <- length(args[[name]]$data)
      } else {
        t <- dim(args[[name]]$data)[1]
      }

      outcomes.new[[name]]$t <- t
      outcomes.new[[name]]$data <- args[[name]]$data |>
        as.matrix() |>
        matrix(t, r)
      if (!is.null(args[[name]]$offset)) {
        outcomes.new[[name]]$offset <- args[[name]]$offset |>
          as.matrix() |>
          matrix(t, r)
      } else {
        outcomes.new[[name]]$offset <- outcomes.new[[name]]$data**0
      }
    }
  }
  t.max <- max(sapply(outcomes.new, function(x) {
    x$t
  }))
  for (name in names(outcomes.old)) {
    outcomes.old[[name]]$t <- t.max + t_last
    if (name %in% names(outcomes.new)) {
      if (outcomes.new[[name]]$t < t.max) {
        outcomes.new[[name]]$data <- rbind(
          outcomes.new[[name]]$data,
          matrix(NA, t.max - outcomes.new[[name]]$t, outcomes.new[[name]]$r)
        )
        outcomes.new[[name]]$offset <- rbind(
          outcomes.new[[name]]$offset,
          matrix(NA, t.max - outcomes.new[[name]]$t, outcomes.new[[name]]$r)
        )
        outcomes.new[[name]]$t <- t.max
      }
      outcomes.old[[name]]$data <- rbind(outcomes.old[[name]]$data, outcomes.new[[name]]$data)
      outcomes.old[[name]]$offset <- rbind(outcomes.old[[name]]$offset, outcomes.new[[name]]$offset)
    } else {
      outcomes.old[[name]]$data <- rbind(
        outcomes.old[[name]]$data,
        matrix(NA, t.max - t_last, outcomes.old[[name]]$r)
      )
      outcomes.old[[name]]$offset <- rbind(
        outcomes.old[[name]]$offset,
        matrix(NA, t.max - t_last, outcomes.old[[name]]$r)
      )
    }
  }

  D <- h <- H <- NULL
  if (!is.null(args$D)) {
    D <- args$D
  }
  if (!is.null(args$h)) {
    h <- args$h
  }
  if (!is.null(args$H)) {
    H <- args$H
  }

  #### Consistency check ####
  if (length(dim(D)) > 3) {
    stop(paste0("Error: D should have at most 3 dimensions. Got ", length(dim(D)), "."))
  }
  if (length(dim(h)) > 2) {
    stop(paste0("Error: H should have at most 2 dimensions. Got ", length(dim(h)), "."))
  }
  if (length(dim(H)) > 3) {
    stop(paste0("Error: H should have at most 3 dimensions. Got ", length(dim(H)), "."))
  }

  G.labs <- object$G.labs
  G.idx <- object$G.idx
  G <- array(object$G[, , t_last], c(n, n, t.max))
  G.flags <- G.labs == "Pulse"
  if (any(G.flags)) {
    for (i in seq_len(n)[rowSums(G.flags) > 0]) {
      for (j in seq_len(n)[G.flags[i, ]]) {
        label <- paste0(object$var.labels[j])
        if (label %in% names(args)) {
          if (length(args[[label]]) != t.max) {
            stop(paste0("Error: ", label, " and outcomes have different size. Expected, ", t.max, ", got ", length(args[[label]]), "."))
          }
          G[i, j, ] <- args[[label]]
        } else {
          stop(paste0("Error: Missing extra argument: ", label))
        }
      }
    }
  }

  FF.labs <- object$FF.labs
  FF <- array(object$FF[, , t_last], c(n, k, t.max), dimnames = dimnames(object$FF))
  FF.flags <- FF.labs == "Covariate"
  if (any(FF.flags)) {
    coef.names <- rep(NA, object$n)
    for (name in names(object$var.names)) {
      coef.names[object$var.names[[name]]] <- name
    }
    for (j in seq_len(k)[colSums(FF.flags) > 0]) {
      for (i in seq_len(n)[FF.flags[, j]]) {
        if (sum(FF.flags[i, ]) == 1) {
          label <- coef.names[i] |>
            paste(FF.labs[i, j], sep = ".")
        } else {
          label <- coef.names[i] |>
            paste(FF.labs[i, j], cumsum(FF.flags[i, ])[j], sep = ".")
        }
        if (label %in% names(args)) {
          if (length(args[[label]]) != t.max) {
            stop(paste0("Error: ", label, " and outcomes have different size. Expected, ", t.max, ", got ", length(args[[label]]), "."))
          }
          FF[i, j, ] <- args[[label]]
        } else {
          stop(paste0("Error: Missing extra argument: ", label))
        }
      }
    }
  }

  if (is.null(h)) {
    h <- matrix(object$h[, t_last], n, t.max)
  }
  dim.D <- dim(D)
  if (is.null(D)) {
    D <- array(object$D[, , t_last], c(n, n, t.max))
  } else {
    if (all(dim.D == 1)) {
      D <- array(D, c(n, n, t.max))
    } else {
      if (length(dim.D) == 2 || (length(dim.D) == 3 && dim.D[3] == 1)) {
        D <- array(D, c(n, n, t.max))
      }
    }
  }
  dim.H <- dim(H)
  if (is.null(H)) {
    H <- array(object$H[, , t_last], c(n, n, t.max))
  } else {
    if (all(dim.H == 1)) {
      H <- array(diag(n) * H, c(n, n, t.max))
    } else {
      if (length(dim.H) == 2 || (length(dim.H) == 3 && dim.H[3] == 1)) {
        H <- array(H, c(n, n, t.max))
        H[, , -1] <- 0
      }
    }
  }
  if (any(dim(D) != c(n, n, t.max))) {
    stop(paste0("Error: D has wrong dimesions. Expected ", paste(n, n, t.max, sep = "x"), ". Got ", paste(dim(D), colapse = "x")))
  }
  if (any(dim(H) != c(n, n, t.max))) {
    stop(paste0("Error: H has wrong dimesions. Expected ", paste(n, n, t.max, sep = "x"), ". Got ", paste(dim(H), colapse = "x")))
  }
  #####
  D <- ifelse(D == 0, 1, D)

  a1 <- object$mt[, t_last, drop = FALSE]
  R1 <- object$Ct[, , t_last]

  new.data <- analytic_filter(
    outcomes = outcomes.new,
    a1 = a1,
    R1 = R1,
    FF = FF,
    FF.labs = FF.labs,
    G = G,
    G.labs = G.labs,
    G.idx = G.idx,
    D = D,
    h = h,
    H = H,
    p.monit = object$p.monit,
    monitoring = object$monitoring
  )

  object$mt <- matrix(c(object$mt, new.data$mt), c(n, t.max + t_last), dimnames = dimnames(object$mt))
  object$Ct <- array(c(object$Ct, new.data$Ct), c(n, n, t.max + t_last), dimnames = dimnames(object$Ct))
  object$at <- matrix(c(object$at, new.data$at), c(n, t.max + t_last), dimnames = dimnames(object$at))
  object$Rt <- array(c(object$Rt, new.data$Rt), c(n, n, t.max + t_last), dimnames = dimnames(object$Rt))
  object$ft <- matrix(c(object$ft, new.data$ft), c(k, t.max + t_last), dimnames = dimnames(object$ft))
  object$Qt <- array(c(object$Qt, new.data$Qt), c(k, k, t.max + t_last), dimnames = dimnames(object$Qt))
  object$ft.star <- cbind(object$ft.star, new.data$ft.star)
  object$Qt.star <- array(c(object$Qt.star, new.data$Qt.star), c(k, k, t.max + t_last), dimnames = dimnames(object$Qt.star))
  object$D <- array(c(object$D, new.data$D), c(n, n, t.max + t_last), dimnames = dimnames(object$D))
  object$H <- array(c(object$H, new.data$H), c(n, n, t.max + t_last), dimnames = dimnames(object$H))
  object$h <- matrix(c(object$h, new.data$h), c(n, t.max + t_last), dimnames = dimnames(object$h))
  object$W <- array(c(object$W, new.data$W), c(n, n, t.max + t_last), dimnames = dimnames(object$W))
  object$FF <- array(c(object$FF, new.data$FF), c(n, k, t.max + t_last), dimnames = dimnames(object$FF))
  object$G <- array(c(object$G, new.data$G), c(n, n, t.max + t_last), dimnames = dimnames(object$G))
  object$t <- t_last + t.max
  object$log.like.null <- c(object$log.like.null, new.data$log.like.null)
  object$log.like.alt <- c(object$log.like.alt, new.data$log.like.alt)
  object$alt.flags <- c(object$alt.flags, new.data$alt.flags)
  object$outcomes <- outcomes.old

  if (object$smooth) {
    object$smooth <- FALSE
    object <- smoothing(object)
  }

  return(object)
}

#' coef.fitted_dlm
#'
#' Evaluates the predictive values for the observed values used to fit the model and its latent states.
#' Predictions can be made with smoothed values, with filtered values or h-steps ahead.
#'
#' @param object fitted_dlm: The fitted model to be use for evaluation.
#' @param t.eval numeric: A vector of positive integers indicating the time index from which to extract predictions. The default is to extract to evaluate the model at all observed times.
#' @param lag integer: The relative offset for forecast. Values for time t will be calculated based on the filtered values of time t-h. If lag is negative, then the smoothed distribution for the latent states will be used.
#' @param pred.cred numeric: The credibility level for the C.I..
#' @param eval.pred boolean: A flag indicating if the predictions should be calculated.
#' @param eval.metric boolean: A flag indicating if the model density (f(M|y)) should be calculated. Only used when lag<0.
#' @param ... Extra arguments passed to the coef method.
#'
#' @return A list containing:
#' \itemize{
#'    \item data data.frame: A table with the model evaluated at each observed time.
#'    \item theta.mean matrix: The mean of the latent states at each time. Dimensions are n x t, where t is the size of t.eval and n is the number of latent states.
#'    \item theta.cov array: A 3D-array containing the covariance matrix of the latent states at each time. Dimensions are n x n x t, where t is the size of t.eval and n is the number of latent states.
#'    \item lambda.mean matrix: The mean of the linear predictor at each time. Dimensions are k x t, where t is the size of t.eval and k is the number of linear predictors.
#'    \item lambda.cov array: A 3D-array containing the covariance matrix for the linear predictor at each time. Dimensions are k x k x t, where t is the size of t.eval and k is the number of linear predictors.
#'    \item log.like, mae, mase, rae, mse, interval.score: The metric value at each time.
#'    \item conj.param list: A list containing, for each outcome, a data.frame with the parameter of the conjugated distribution at each time.
#' }
#' @importFrom Rfast transpose data.frame.to_matrix
#' @rdname coef.fitted_dlm
#' @export
#'
#' @examples
#' # Poisson case
#' data <- c(AirPassengers)
#'
#' level <- polynomial_block(rate = 1, order = 2, D = 0.95)
#' season <- harmonic_block(rate = 1, order = 2, period = 12, D = 0.975)
#'
#' outcome <- Poisson(lambda = "rate", data = data)
#'
#' fitted.data <- fit_model(level, season,
#'   AirPassengers = outcome
#' )
#'
#' var.vals <- coef(fitted.data)
#'
#' @family auxiliary functions for fitted_dlm objects
coef.fitted_dlm <- function(object, t.eval = seq_len(object$t), lag = -1, pred.cred = 0.95, eval.pred = FALSE, eval.metric = FALSE, ...) {
  if (round(lag) != lag) {
    stop(paste0("Error: lag should be a integer. Got ", lag, "."))
  }
  pred.names <- object$pred.names
  n <- object$n
  t_last <- object$t
  k <- object$k
  r <- object$r

  smoothed.log.like <- FALSE
  true.lag <- lag
  if (lag < 0) {
    if (!object$smooth) {
      object <- smoothing(object)
    }
    smoothed.log.like <- TRUE
    lag <- 0
    ref.mt <- object$mts
    ref.Ct <- object$Cts
  } else {
    ref.mt <- object$mt
    ref.Ct <- object$Ct
  }
  if (lag <= 0) {
    null.rows.flags <- diag(object$G.labs) %in% c("noise", "noise.disc")
    if (any(null.rows.flags)) {
      # time_index=colSums(null.rows.flags)>0
      var.W <- apply(object$W, 3, diag)
      # var.W=matrix(var.W[,object$t],n,object$t)
      ref.mt[null.rows.flags] <- 0
      for (i in (1:object$t)) {
        ref.Ct[null.rows.flags, , i] <- 0
        ref.Ct[, null.rows.flags, i] <- 0
        diag(ref.Ct[, , i])[null.rows.flags] <- var.W[null.rows.flags, i]
      }
    }
  }

  if (min(t.eval) < 1) {
    warning("Cannot evaluate parameters before time 1.")
    t.eval <- t.eval[t.eval - lag >= 1]
  }
  if (max(t.eval) > t_last) {
    warning("Cannot evaluate parameters after last observation.")
    t.eval <- t.eval[t.eval <= t_last]
  }
  init.t <- min(t.eval)
  final.t <- max(t.eval)
  len.t <- final.t - init.t + 1

  FF <- object$FF
  FF.labs <- object$FF.labs

  mt.pred <- matrix(NA, n, len.t)
  Ct.pred <- array(NA, c(n, n, len.t))
  ft.pred <- matrix(NA, k, len.t)
  Qt.pred <- array(NA, c(k, k, len.t))

  pred <- matrix(NA, r, len.t)
  var.pred <- matrix(NA, r, len.t)
  icl.pred <- matrix(NA, r, len.t)
  icu.pred <- matrix(NA, r, len.t)
  log.like <- rep(0, len.t)

  conj.param.list <- list()
  out.mat <- matrix(0, r, len.t)
  r.start <- 0

  for (outcome.name in names(object$outcomes)) {
    conj.param.list[[outcome.name]] <- matrix(NA, len.t, length(object$outcomes[[outcome.name]]$param.names)) |> as.data.frame()
    names(conj.param.list[[outcome.name]]) <- object$outcomes[[outcome.name]]$param.names
    row.names(conj.param.list[[outcome.name]]) <- init.t:final.t

    r.cur <- object$outcomes[[outcome.name]]$r
    out.mat[1:r.cur + r.start, ] <- t(object$outcomes[[outcome.name]]$data[init.t:final.t, ])
    r.start <- r.start + r.cur
  }

  D <- object$D
  D.inv <- 1 / D
  D.holder <- object$D[, , 1]
  h <- object$h
  W <- object$W
  G <- object$G
  G.labs <- object$G.labs
  G.idx <- object$G.idx

  for (i in c(init.t:final.t)) {
    if (i <= lag) {
      mt <- object$a1
      Ct <- object$R1
      lag_i <- i - 1
    } else if (i <= t_last) {
      mt <- ref.mt[, (i - lag):(i - lag)]
      Ct <- ref.Ct[, , (i - lag):(i - lag)]
      lag_i <- lag
    } else {
      mt <- ref.mt[, t_last]
      Ct <- ref.Ct[, , t_last]
      lag_i <- lag + i - t_last
    }
    next.step <- list("at" = mt, "Rt" = Ct)
    if (lag_i >= 1) {
      for (t in c(lag_i:1)) {
        next.step <- one_step_evolve(next.step$at, next.step$Rt, G[, , i - t + 1], G.labs, G.idx, D.holder, h[, i - t + 1], W[, , i - t + 1])
      }
    }
    lin.pred <- calc_lin_pred(
      next.step$at |> matrix(n, 1),
      next.step$Rt, FF[, , i] |> matrix(n, k, dimnames = list(NULL, pred.names)),
      FF.labs, pred.names,
      pred.index = 1:k
    )

    mt.pred[, i - init.t + 1] <- next.step$at
    Ct.pred[, , i - init.t + 1] <- next.step$Rt
    ft.pred[, i - init.t + 1] <- lin.pred$ft
    Qt.pred[, , i - init.t + 1] <- lin.pred$Qt

    r.acum <- 0
    if (eval.pred) {
      for (outcome.name in names(object$outcomes)) {
        outcome <- object$outcomes[[outcome.name]]
        r.cur <- outcome$r
        r.seq <- (r.acum + 1):(r.acum + r.cur)
        t.index <- i - init.t + 1

        pred.index <- match(outcome$pred.names, object$pred.names)

        cur.step <- outcome$apply_offset(lin.pred$ft[pred.index, , drop = FALSE], lin.pred$Qt[pred.index, pred.index, drop = FALSE], outcome$offset[i, ])

        if (outcome$convert.canom.flag) {
          ft.canom <- outcome$convert.mat.canom %*% cur.step$ft
          Qt.canom <- outcome$convert.mat.canom %*% cur.step$Qt %*% transpose(outcome$convert.mat.canom)
        } else {
          ft.canom <- cur.step$ft
          Qt.canom <- cur.step$Qt
        }

        conj.param <- outcome$conj_distr(ft.canom, Qt.canom, parms = outcome$parms)
        conj.param.list[[outcome.name]][t.index, ] <- conj.param
        prediction <- outcome$calc_pred(conj.param,
          if (i > t_last) {
            NULL
          } else {
            outcome$data[i, , drop = FALSE]
          },
          pred.cred,
          parms = outcome$parms
        )


        pred[r.seq, t.index] <- prediction$pred
        var.pred[r.seq, t.index] <- prediction$var.pred |>
          as.matrix() |>
          diag()
        icl.pred[r.seq, t.index] <- prediction$icl.pred
        icu.pred[r.seq, t.index] <- prediction$icu.pred
        log.like[t.index] <- log.like[t.index] + sum(prediction$log.like, na.rm = TRUE)
        r.acum <- r.acum + r.cur
      }
    }
  }

  mae <- mse <- mase <- interval.score <- matrix(NA, len.t, r)
  if (eval.pred) {
    mae[, ] <- t(abs(out.mat - pred))
    mse[, ] <- t((out.mat - pred)**2)
    interval.score[, ] <- t((icu.pred - icl.pred) +
      2 / (1 - pred.cred) * (icl.pred - out.mat) * (out.mat < icl.pred) +
      2 / (1 - pred.cred) * (out.mat - icu.pred) * (out.mat > icu.pred))

    if (object$period * max(lag, 1) < object$t) {
      for (i in 1:r) {
        mase[, i] <- mae[, i] / (out.mat[i, ] |> diff(lag = object$period * max(lag, 1)) |> abs() |> mean(na.rm = TRUE))
      }
    }
  }

  mae <- mse <- mase <- interval.score <- matrix(NA, len.t, r)
  if (eval.pred) {
    mae[, ] <- t(abs(out.mat - pred))
    mse[, ] <- t((out.mat - pred)**2)
    interval.score[, ] <- t((icu.pred - icl.pred) +
      2 / (1 - pred.cred) * (icl.pred - out.mat) * (out.mat < icl.pred) +
      2 / (1 - pred.cred) * (out.mat - icu.pred) * (out.mat > icu.pred))

    if (object$period * max(lag, 1) < object$t) {
      for (i in 1:r) {
        mase[, i] <- mae[, i] / (out.mat[i, ] |> diff(lag = object$period * max(lag, 1)) |> abs() |> mean(na.rm = TRUE))
      }
    }
  }

  r.acum <- 0
  out.names <- rep(NA, r)
  output <- matrix(NA, len.t, r)
  for (outcome.name in names(object$outcomes)) {
    r.cur <- object$outcomes[[outcome.name]]$r
    r.seq <- (r.acum + 1):(r.acum + r.cur)
    char.len <- floor(log10(r.cur)) + 1
    out.names[r.seq] <- paste0(outcome.name, object$outcomes[[outcome.name]]$sufix)

    1:min(final.t - init.t + 1, t_last - init.t + 1)
    output[seq_len(min(final.t - init.t + 1, t_last - init.t + 1)), r.seq] <- object$outcomes[[outcome.name]]$data[init.t:min(final.t, t_last), ]


    r.acum <- r.acum + r.cur
  }

  time.index.final <- init.t:final.t
  time.flags <- time.index.final %in% t.eval


  serie.names <- as.factor(c(sapply(out.names, function(x) {
    rep(x, final.t - init.t + 1)
  })))

  data <- data.frame(
    Time = time.index.final,
    Serie = serie.names,
    Observation = c(output),
    Prediction = c(t(pred)),
    Variance = c(t(var.pred)),
    C.I.lower = c(t(icl.pred)),
    C.I.upper = c(t(icu.pred))
  )

  rownames(mt.pred) <- rownames(Ct.pred) <- colnames(Ct.pred) <- object$var.labels
  rownames(ft.pred) <- rownames(Qt.pred) <- colnames(Qt.pred) <- object$pred.names

  metrics <- list(
    log.like = if (smoothed.log.like & eval.metric) {
      # object$mts[,]=ref.mt
      # object$Cts[,,]=ref.Ct
      eval_dlm_norm_const(object)
    } else {
      log.like[time.flags, drop = FALSE]
    },
    mae = mae[time.flags, , drop = FALSE],
    mase = mase[time.flags, , drop = FALSE],
    mse = mse[time.flags, , drop = FALSE],
    interval.score = interval.score[time.flags, , drop = FALSE]
  )

  output <- list(
    data = data[data$Time %in% t.eval, ],
    theta.mean = mt.pred[, time.flags, drop = FALSE],
    theta.cov = Ct.pred[, , time.flags, drop = FALSE],
    lambda.mean = ft.pred[, time.flags, drop = FALSE],
    lambda.cov = Qt.pred[, , time.flags, drop = FALSE],
    conj.param = conj.param.list,
    lag = true.lag,
    metrics = metrics,
    dynamic = object$dynamic
  )

  class(output) <- "dlm_coef"

  return(output)
}

#' Draw samples from the distribution of the latent states
#'
#' This is function draws samples from the latent states using the backward sampling algorithm. See \insertCite{WestHarr-DLM;textual}{kDGLM}, chapter 15, for details.
#'
#' @param object fitted_dlm: A fitted model from which to sample.
#' @param nsim integer: The number of samples to draw.
#' @param seed integer: An object specifying if and how the random number generator should be initialized.
#' @param lag integer: The relative offset for forecast. Values for time t will be calculated based on the filtered values of time t-h. If lag is negative, then the smoothed distribution for the latent states will be used.
#' @param ... Extra arguments passed to the plot method.
#'
#' @return A list containing the following values:
#' \itemize{
#'    \item theta array: An array containing a sample of the latent states. Dimensions are n x t x nsim, where n is the number of latent states in the model and t is the number of observed values.
#'    \item lambda array: An array containing a sample of the linear predictors. Dimensions are k x t x nsim, where k is the number of linear predictors in the model and t is the number of observed values.
#'    \item param list: A named list containing, for each model outcome, an array with the samples of the parameters of the observational model. Each array will have dimensions l x t x nsim, where l is the number of parameters in the observational model and t is the number of observed values.
#' }
#'
#' @importFrom Rfast transpose
#' @importFrom stats coef
#' @export
#'
#' @examples
#'
#' structure <- polynomial_block(mu = 1, D = 0.95) +
#'   polynomial_block(V = 1, D = 0.95)
#'
#' outcome <- Normal(mu = "mu", V = "V", data = cornWheat$corn.log.return[1:500])
#' fitted.data <- fit_model(structure, corn = outcome)
#'
#' sample <- simulate(fitted.data, 5000)
#'
#' @family auxiliary functions for fitted_dlm objects
simulate.fitted_dlm <- function(object, nsim, seed = NULL, lag = -1, ...) {
  G <- object$G
  G.labs <- object$G.labs
  G.idx <- object$G.idx
  FF <- object$FF
  FF.labs <- object$FF.labs
  pred.names <- object$pred.names
  t.len <- object$t
  n <- object$n
  k <- object$k
  r <- object$r
  Ct.placeholder <- matrix(0, n, n)
  outcomes <- list()
  for (outcome.name in names(object$outcomes)) {
    param.sample <- array(NA, c(object$outcomes[[outcome.name]]$l, t.len, nsim))
    rownames(param.sample) <- object$outcomes[[outcome.name]]$pred.names

    outcomes[[outcome.name]] <- list(
      inv_link = object$outcomes[[outcome.name]]$inv_link_function,
      apply_offset = object$outcomes[[outcome.name]]$apply_offset,
      offset = object$outcomes[[outcome.name]]$offset,
      l = object$outcomes[[outcome.name]]$l,
      r = object$outcomes[[outcome.name]]$r,
      convert.mat.canom = object$outcomes[[outcome.name]]$convert.mat.canom,
      convert.canom.flag = object$outcomes[[outcome.name]]$convert.canom.flag,
      param.sample = param.sample,
      pred.names = object$outcomes[[outcome.name]]$pred.names
    )
  }

  theta.sample <- array(NA, c(n, t.len, nsim))
  rownames(theta.sample) <- object$var.labels

  if (lag > -1) {
    params <- coef(object, lag = lag)

    for (i in 1:t.len) {
      theta.sample[, i, ] <- rmvnorm(nsim, params$theta.mean[, i], params$theta.cov[, , i])
    }
  } else {
    theta.mean <- object$mt
    theta.var <- object$Ct
    theta.last <- rmvnorm(nsim, theta.mean[, t.len], theta.var[, , t.len])
    theta.sample[, t.len, ] <- theta.last

    if (t.len > 1) {
      for (t in (t.len - 1):1) {
        mt.now <- theta.mean[, t]
        Ct.now <- theta.var[, , t]

        at.next <- object$at[, t + 1]
        Rt.next <- object$Rt[, , t + 1]

        G.ref <- calc_current_G(mt.now, Ct.now, G[, , t + 1], G.labs, G.idx)$G
        simple.Rt.inv <- Ct.now %*% crossprod(G.ref, ginv(Rt.next))
        simple.Rt.inv.t <- transpose(simple.Rt.inv)

        mts <- mt.now + simple.Rt.inv %*% (theta.last - at.next)
        Cts <- Ct.now - simple.Rt.inv %*% Rt.next %*% simple.Rt.inv.t

        theta.last <- rmvnorm(nsim, rep(0, n), Cts) + mts
        theta.sample[, t, ] <- theta.last
      }
    }
  }


  lambda.sample <- array(NA, c(k, t.len, nsim))
  rownames(lambda.sample) <- pred.names

  for (t in t.len:1) {
    FF.step <- FF[, , t]
    if (any(is.na(FF.step))) {
      lambda.cur <- sapply(seq_len(nsim),
        function(j) {
          calc_lin_pred(theta.sample[, t, j], Ct.placeholder, FF.step, FF.labs, pred.names)$ft
        },
        simplify = "matrix"
      )
    } else {
      lambda.cur <- crossprod(FF.step, theta.sample[, t, ])
    }
    lambda.sample[, t, ] <- lambda.cur

    for (outcome.name in names(outcomes)) {
      pred.index <- match(outcomes[[outcome.name]]$pred.names, pred.names)
      lambda.sec <- lambda.cur[pred.index, , drop = FALSE]
      offset.step <- outcomes[[outcome.name]]$offset[t, ]
      if (any(is.na(offset.step))) {
        offset.step <- 1
      }
      if (outcomes[[outcome.name]]$convert.canom.flag) {
        lambda.canom <- outcomes[[outcome.name]]$convert.mat.canom %*% lambda.sec
      } else {
        lambda.canom <- lambda.sec
      }

      lambda.out <- outcomes[[outcome.name]]$apply_offset(
        lambda.canom,
        diag(k) * 0,
        offset.step
      )$ft
      param.sample <- outcomes[[outcome.name]]$inv_link(lambda.out)
      outcomes[[outcome.name]]$param.sample[, t, ] <- param.sample
    }
  }

  return(list(
    "theta" = theta.sample,
    "lambda" = lambda.sample,
    "param" = lapply(outcomes, function(x) {
      x$param.sample
    })
  ))
}

#' Auxiliary function for evaluating the posterior density of a DLM
#'
#' Evaluates the density for a set of parameters theta in a DLM. The structure of the DLM is taken to be that of the fitted_dlm object passed as input.
#'
#' @param theta Matrix: A matrix representing the set of parameter for which to evaluate the density. Its size should be n x t, where n is the number of latent states and t is the length of the time series;
#' @param model fitted_dlm: A fitted_dlm object.
#' @param lin.pred boolean: A flag indicating if theta represents the linear predictors.
#' @return A scalar representing the log density evaluated at theta.
#' @export
#'
#' @keywords internal
#' @examples
#'
#' data <- c(AirPassengers)
#'
#' level <- polynomial_block(rate = 1, order = 2, D = 0.95)
#' season <- harmonic_block(rate = 1, order = 2, period = 12, D = 0.975)
#'
#' outcome <- Poisson(lambda = "rate", data = data)
#'
#' fitted.data <- fit_model(level, season,
#'   AirPassengers = outcome
#' )
#' eval_dlm_post(fitted.data$mts, fitted.data)
eval_dlm_post <- function(theta, model, lin.pred = FALSE) {
  t <- model$t
  n <- model$n
  k <- model$k

  FF <- model$FF
  FF.labs <- model$FF.labs
  pred.names <- model$pred.names
  G <- model$G
  G.labs <- model$G.labs
  G.idx <- model$G.idx

  if (lin.pred) {
    mt <- model$mt
    Ct <- model$Ct
    mt.step <- mt[, t, drop = FALSE]
    Ct.step <- Ct[, , t] |> matrix(n, n)
    at <- model$at
    Rt <- model$Rt

    lin.pred <- calc_lin_pred(mt.step, Ct.step, FF[, , 1] |> matrix(n, k), FF.labs, pred.names)
    ft <- lin.pred$ft
    Qt <- lin.pred$Qt
    FF.step <- lin.pred$FF
    log.post <- dmvnorm(theta[, t], ft, Qt)
    error.ft <- theta[, t] - ft
    error.Qt <- -Qt
    At <- Ct.step %*% FF.step %*% ginv(Qt)
    mts <- mt.step + At %*% error.ft
    Cts <- Ct.step + At %*% error.Qt %*% t(At)

    if (t > 1) {
      for (i in (t - 1):1) {
        mt.step <- mt[, i, drop = FALSE]
        Ct.step <- Ct[, , i] |> matrix(n, n)
        at.step <- at[, i + 1]
        Rt.step <- Rt[, , i + 1]
        G.step <- calc_current_G(mt.step, Ct.step, G[, , i + 1], G.labs, G.idx)$G

        simple.Rt.inv <- Ct.step %*% transpose(G.step) %*% ginv(Rt.step)

        mt.step <- mt.step + simple.Rt.inv %*% (mts - at.step)
        Ct.step <- Ct.step + simple.Rt.inv %*% (Cts - Rt.step) %*% transpose(simple.Rt.inv)

        lin.pred <- calc_lin_pred(mt.step, Ct.step, FF[, , i] |> matrix(n, k), FF.labs, pred.names)
        ft <- lin.pred$ft
        Qt <- lin.pred$Qt
        FF.step <- lin.pred$FF
        log.post <- log.post + dmvnorm(theta[, i], ft, Qt)
        error.ft <- theta[, i] - ft
        error.Qt <- -Qt
        At <- Ct.step %*% FF.step %*% ginv(Qt)
        mts <- mt.step + At %*% error.ft
        Cts <- Ct.step + At %*% error.Qt %*% t(At)
      }
    }
  } else {
    mts <- model$mt
    Cts <- model$Ct
    mt <- model$mt
    Ct <- model$Ct
    at <- model$at
    Rt <- model$Rt

    Ct.placeholder <- Cts[, , 1] * 0

    log.post <- dmvnorm(theta[, t], mt[, t], Ct[, , t] |> matrix(n, n))

    if (t > 1) {
      for (i in (t - 1):1) {
        mt.step <- mt[, i, drop = FALSE]
        Ct.step <- Ct[, , i]
        Rt.step <- Rt[, , i + 1]
        G.step <- calc_current_G(theta[, i], Ct.placeholder, G[, , i + 1], G.labs, G.idx)$G

        simple.Rt.inv <- Ct.step %*% transpose(G.step) %*% ginv(Rt.step)

        mts[, i] <- mt.step + simple.Rt.inv %*% (theta[, i + 1] - at[, i + 1])
        Cts[, , i] <- Ct.step - simple.Rt.inv %*% Rt.step %*% transpose(simple.Rt.inv)
        log.post <- log.post + dmvnorm(theta[, i], mts[, i], Cts[, , i])
      }
    }
  }
  return(log.post)
}

#' Auxiliary function for evaluating the prior density of a DLM
#'
#' Evaluates the prior density for a set of parameters theta in a DLM. The structure of the DLM is taken to be that of the fitted_dlm object passed as input.
#'
#' @param theta matrix: A matrix representing the set of parameter for which to evaluate the density. Its size should be n x t, where n is the number of latent states and t is the length of the time series;
#' @param model fitted_dlm object: A fitted_dlm object.
#' @param lin.pred boolean: A flag indicating if theta represents the linear predictors.
#'
#' @return A scalar representing the log density evaluated at theta.
#' @export
#'
#' @keywords internal
#' @examples
#'
#' data <- c(AirPassengers)
#'
#' level <- polynomial_block(rate = 1, order = 2, D = 0.95)
#' season <- harmonic_block(rate = 1, order = 2, period = 12, D = 0.975)
#'
#' outcome <- Poisson(lambda = "rate", data = data)
#'
#' fitted.data <- fit_model(level, season,
#'   AirPassengers = outcome
#' )
#' eval_dlm_prior(fitted.data$mts, fitted.data)
eval_dlm_prior <- function(theta, model, lin.pred = FALSE) {
  t <- model$t
  n <- model$n
  k <- model$k

  a1 <- model$a1
  R1 <- model$R1
  G <- model$G
  G.labs <- model$G.labs
  G.idx <- model$G.idx
  FF <- model$FF
  FF.labs <- model$FF.labs
  pred.names <- model$pred.names
  h <- model$h
  W <- model$W

  R1_placeholder <- R1 * 0
  D_placeholder <- R1**0

  if (lin.pred) {
    lin.pred <- calc_lin_pred(a1, R1, FF[, , 1] |> matrix(n, k), FF.labs, pred.names)
    ft <- lin.pred$ft
    Qt <- lin.pred$Qt
    FF.step <- lin.pred$FF
    log.prior <- dmvnorm(theta[, 1], ft, Qt)
    error.ft <- theta[, 1] - ft
    error.Qt <- -Qt
    At <- R1 %*% FF.step %*% ginv(Qt)
    at <- a1 + At %*% error.ft
    Rt <- R1 + At %*% error.Qt %*% t(At)
    if (t > 1) {
      for (i in 2:t) {
        next.step <- one_step_evolve(at, Rt, G[, , i], G.labs, G.idx, D_placeholder, h[, i, drop = FALSE], W[, , i])

        lin.pred <- calc_lin_pred(next.step$at, next.step$Rt, FF[, , i] |> matrix(n, k), FF.labs, pred.names)
        ft <- lin.pred$ft
        Qt <- lin.pred$Qt
        FF.step <- lin.pred$FF

        log.prior <- log.prior + dmvnorm(theta[, i], ft, Qt)
        error.ft <- theta[, i] - ft
        error.Qt <- -Qt
        At <- Rt %*% FF.step %*% ginv(Qt)
        at <- at + At %*% error.ft
        Rt <- Rt + At %*% error.Qt %*% t(At)
      }
    }
  } else {
    log.prior <- dmvnorm(theta[, 1], a1, R1)
    if (t > 1) {
      for (i in 2:t) {
        next.step <- one_step_evolve(theta[, i - 1], R1_placeholder, G[, , i], G.labs, G.idx, D_placeholder, h[, i, drop = FALSE], W[, , i])
        log.prior <- log.prior + dmvnorm(theta[, i], next.step$at, next.step$Rt)
      }
    }
  }
  return(log.prior)
}

#' Auxiliary function for evaluating the prior density of a DLM
#'
#' Evaluates the prior density for a set of parameters theta in a DLM. The structure of the DLM is taken to be that of the fitted_dlm object passed as input.
#'
#' @param theta Matrix: A matrix representing the set of parameter for which to evaluate the density. Its size should be n x t, where n is the number of latent states and t is the length of the time series;
#' @param model fitted_dlm: A fitted_dlm object.
#' @param lin.pred boolean: A flag indicating if theta represents the linear predictors.
#'
#' @return A scalar representing the log likelihood evaluated at theta.
#' @export
#'
#' @keywords internal
#' @examples
#'
#' data <- c(AirPassengers)
#'
#' level <- polynomial_block(rate = 1, order = 2, D = 0.95)
#' season <- harmonic_block(rate = 1, order = 2, period = 12, D = 0.975)
#'
#' outcome <- Poisson(lambda = "rate", data = data)
#'
#' fitted.data <- fit_model(level, season,
#'   AirPassengers = outcome
#' )
#' eval_dlm_log_like(fitted.data$mts, fitted.data)
eval_dlm_log_like <- function(theta, model, lin.pred = FALSE) {
  t <- model$t
  n <- model$n
  k <- model$k
  Ct.placeholder <- matrix(0, n, n)
  Qt.placeholder <- matrix(0, k, k)
  FF <- model$FF
  FF.labs <- model$FF.labs
  pred.names <- model$pred.names

  log.like <- 0
  if (lin.pred) {
    for (i in seq_len(t)) {
      for (outcome in model$outcomes) {
        offset.step <- outcome$offset[i, ]
        na.flag <- any(is.null(offset.step) | any(offset.step == 0) | any(is.na(offset.step)) | any(is.na(outcome$data[i, ])))
        pred.index <- match(outcome$pred.names, pred.names)
        ft.canom <- theta[pred.index, i, drop = FALSE]
        if (outcome$convert.canom.flag) {
          ft.canom <- outcome$convert.mat.canom %*% ft.canom
        }

        if (!na.flag) {
          offset.pred <- outcome$apply_offset(ft.canom, Qt.placeholder, offset.step)
          ft.canom <- offset.pred$ft
        }
        param <- outcome$inv_link_function(ft.canom)
        log.like <- log.like + outcome$log_like_function(outcome$data[i, ], param)
      }
    }
  } else {
    for (i in seq_len(t)) {
      lin.pred <- calc_lin_pred(theta[, i, drop = FALSE], Ct.placeholder, FF[, , i] |> matrix(n, k), FF.labs, pred.names)
      ft <- lin.pred$ft
      Qt <- lin.pred$Qt
      for (outcome in model$outcomes) {
        offset.step <- outcome$offset[i, ]
        na.flag <- any(is.null(offset.step) | any(offset.step == 0) | any(is.na(offset.step)) | any(is.na(outcome$data[i, ])))
        pred.index <- match(outcome$pred.names, pred.names)
        ft.canom <- ft[pred.index, , drop = FALSE]
        Qt.canom <- Qt[pred.index, pred.index, drop = FALSE]
        if (outcome$convert.canom.flag) {
          ft.canom <- outcome$convert.mat.canom %*% ft.canom
          Qt.canom <- outcome$convert.mat.canom %*% Qt.canom %*% transpose(outcome$convert.mat.canom)
        }

        if (!na.flag) {
          offset.pred <- outcome$apply_offset(ft.canom, Qt.canom, offset.step)
          ft.canom <- offset.pred$ft
          Qt.canom <- offset.pred$Qt
        }
        param <- outcome$inv_link_function(ft.canom)
        log.like <- log.like + outcome$log_like_function(outcome$data[i, ], param)
      }
    }
  }
  return(log.like)
}

#' Auxiliary function for evaluating normalizing constant for the posterior of a fitted DLM.
#'
#' Evaluates the normalizing constant for the posterior of a fitted DLM.
#'
#' @param model fitted_dlm: A fitted_dlm object.
#' @param lin.pred boolean: A flag indicating if the normalizing constant should be calculated using the linear predictors.
#'
#' @return A scalar representing the normalizing constant for the posterior of a fitted DLM.
#' @export
#'
#' @keywords internal
#' @examples
#'
#' data <- c(AirPassengers)
#'
#' level <- polynomial_block(rate = 1, order = 2, D = 0.95)
#' season <- harmonic_block(rate = 1, order = 2, period = 12, D = 0.975)
#'
#' outcome <- Poisson(lambda = "rate", data = data)
#'
#' fitted.data <- fit_model(level, season,
#'   AirPassengers = outcome
#' )
#' eval_dlm_norm_const(fitted.data)
#'
#' @family auxiliary functions for fitted_dlm objects
eval_dlm_norm_const <- function(model, lin.pred = FALSE) {
  if (!model$smooth) {
    stop("Error: The model is not smoothed.")
  }
  t <- model$t
  n <- model$n
  k <- model$k
  Ct.placeholder <- matrix(0, n, n)
  FF <- model$FF
  FF.labs <- model$FF.labs
  pred.names <- model$pred.names
  if (lin.pred) {
    fts <- model$ft
    Ct.placeholder <- model$Ct[, , t] * 0
    for (i in 1:t) {
      lin.pred.list <- calc_lin_pred(model$mts[, i], Ct.placeholder, FF[, , i] |> matrix(n, k), FF.labs, pred.names)
      ft <- lin.pred.list$ft
      fts[, i] <- ft
    }

    return(eval_dlm_prior(fts, model, lin.pred = lin.pred) +
      eval_dlm_log_like(fts, model, lin.pred = lin.pred) +
      -eval_dlm_post(fts, model, lin.pred = lin.pred))
  } else {
    eval_dlm_prior(model$mts, model, lin.pred = lin.pred) +
      eval_dlm_log_like(model$mts, model, lin.pred = lin.pred) +
      -eval_dlm_post(model$mts, model, lin.pred = lin.pred)
  }
}

#' Fitting kDGLM models
#'
#' Fit a model given its structure and the observed data. This function can be used for any supported family (see vignette).
#'
#' @param formula an object of class "formula" (or one that can be coerced to that class): a symbolic description of the model to be fitted.
#' @param ... Extra arguments, including extra formulas (multinomial case) or extra parameters (normal and gamma cases).
#' @param family a description of the error distribution to be used in the model. For kdglm this can be a character string naming a family function or a family function.
#' @param data an optional data frame, list or environment (or object coercible by as.data.frame to a data frame) containing the variables in the model. If not found in data, the variables are taken from environment(formula), typically the environment from which glm is called.
#' @param offset this can be used to specify an a priori known component to be included in the linear predictor during fitting. This should be NULL or a numeric vector of length equal to the number of cases. One or more offset terms can be included in the formula instead.
#' @param p.monit numeric (optional): The prior probability of changes in the latent space variables that are not part of its dynamic. Only used when performing sensitivity analysis.
#'
#' @importFrom stats update.formula model.matrix as.formula model.frame
#'
#' @return A fitted_dlm object.
#' @export
#'
#' @examples
#'
#' # Poisson case
#' fitted.data <- kdglm(c(AirPassengers) ~ pol(2) + har(12, order = 2), family = Poisson)
#' summary(fitted.data)
#' plot(fitted.data, plot.pkg = "base")
#'
#' ##################################################################
#'
#' # Multinomial case
#' chickenPox$Total <- rowSums(chickenPox[, c(2, 3, 4, 6, 5)])
#' chickenPox$Vaccine <- chickenPox$date >= as.Date("2013-09-01")
#' fitted.data <- kdglm(`< 5 year` ~ pol(2, D = 0.95) + har(12, D = 0.975) + noise(R1 = 0.1) + Vaccine,
#'   `5 to 9 years` ~ pol(2, D = 0.95) + har(12, D = 0.975) + noise(R1 = 0.1) + Vaccine,
#'   `10 to 14 years` ~ pol(2, D = 0.95) + har(12, D = 0.975) + noise(R1 = 0.1) + Vaccine,
#'   `50 years or more` ~ pol(2, D = 0.95) + har(12, D = 0.975) + noise(R1 = 0.1) + Vaccine,
#'   N = chickenPox$Total,
#'   family = Multinom,
#'   data = chickenPox
#' )
#' summary(fitted.data)
#' plot(fitted.data, plot.pkg = "base")
#'
#' ##################################################################
#'
#' # Univariate Normal case
#' fitted.data <- kdglm(corn.log.return ~ 1, V = ~1, family = Normal, data = cornWheat[1:500, ])
#' summary(fitted.data)
#' plot(fitted.data, plot.pkg = "base")
#'
#' ##################################################################
#'
#' # Gamma case
#' Y <- (cornWheat$corn.log.return[1:500] - mean(cornWheat$corn.log.return[1:500]))**2
#' fitted.data <- kdglm(Y ~ 1, phi = 0.5, family = Gamma, data = cornWheat)
#' summary(fitted.data)
#' plot(fitted.data, plot.pkg = "base")
#'
#' @details
#'
#' This is the main function of the kDGLM package, as it is used to fit all models.
#'
#' For the details about the implementation see  \insertCite{ArtigoPacote;textual}{kDGLM}.
#'
#' For the details about the methodology see  \insertCite{ArtigokParametrico;textual}{kDGLM}.
#'
#' For the details about the Dynamic Linear Models see  \insertCite{WestHarr-DLM;textual}{kDGLM} and \insertCite{Petris-DLM;textual}{kDGLM}.
#'
#' @seealso auxiliary functions for creating outcomes \code{\link{Poisson}}, \code{\link{Multinom}}, \code{\link{Normal}}, \code{\link{Gamma}}
#' @seealso auxiliary functions for creating structural blocks \code{\link{polynomial_block}}, \code{\link{regression_block}}, \code{\link{harmonic_block}}, \code{\link{TF_block}}
#' @seealso auxiliary functions for defining priors \code{\link{zero_sum_prior}}, \code{\link{CAR_prior}}
#'
#' @family auxiliary functions for fitted_dlm objects
kdglm <- function(formula, ..., family, data = NULL, offset = NULL, p.monit = NA) {
  # formula=c(AirPassengers)~har(12,order=2)+1+Z
  # data=NULL
  # offset=NULL
  # family=Poisson
  # p.monit=NA
  data.name <- deparse(substitute(data))

  extra.args <- list(...)
  Y <- model.frame(update.formula(formula, . ~ 1), data = data)
  name.Y <- names(Y)[1]
  Y <- Y[, 1]
  if (is.null(offset)) {
    offset <- Y**0
  }
  args <- formula.to.structure(formula, data, label = name.Y)

  if (is.character(family)) {
    if (identical(tolower(family), "normal")) {
      family <- Normal
    } else if (identical(tolower(family), "gamma")) {
      family <- Gamma
    } else if (identical(tolower(family), "multinom") | identical(tolower(family), "multinomial")) {
      family <- Multinom
    } else if (identical(tolower(family), "poisson")) {
      family <- Poisson
    }
  }

  if (identical(family, Normal)) {
    if ("V" %in% names(extra.args)) {
      formula.V <- extra.args$V
    } else {
      stop("Variance matrix (V) is not defined.")
    }
    if (is.numeric(formula.V)) {
      outcome <- Normal(mu = name.Y, V = formula.V, data = Y)
    } else {
      args <- block_superpos(args, formula.to.structure(formula.V, data, "V"))
      outcome <- Normal(mu = name.Y, V = "V", data = Y)
    }
  } else if (identical(family, Gamma)) {
    if ("phi" %in% names(extra.args)) {
      formula.phi <- extra.args$phi
    } else {
      stop("Shape parameter (phi) is not defined.")
    }
    if (is.numeric(formula.phi)) {
      outcome <- Gamma(mu = name.Y, phi = formula.phi, data = Y)
    } else {
      args <- block_superpos(args, formula.to.structure(formula.phi, data, "phi"))
      outcome <- Gamma(mu = name.Y, phi = "phi", data = Y)
    }
  } else if (identical(family, Multinom)) {
    if ("N" %in% names(extra.args)) {
      N <- extra.args$N

      extra.vals <- N - Y
      Y.frame <- as.data.frame(cbind(Y))
      names.Y <- name.Y
      for (item in extra.args) {
        if (typeof(item) == "language") {
          Y.i <- model.frame(update.formula(item, . ~ 1), data = data)
          name.Y.i <- names(Y.i)[1]
          Y.i <- Y.i[, 1]
          args.i <- formula.to.structure(item, data, label = name.Y.i)
          args <- block_superpos(args, args.i)
          Y.frame <- cbind(Y.frame, Y.i)
          names.Y <- c(names.Y, name.Y.i)
          extra.vals <- extra.vals - Y.i
        }
      }
      if (any(extra.vals < 0) | all(extra.vals == 0)) {
        stop("Invalid base category.")
      }

      Y.frame <- cbind(Y.frame, extra.vals)
      names(Y.frame) <- c(names.Y, "base")

      outcome <- Multinom(p = names.Y, data = Y.frame)
      name.Y <- data.name
    } else {
      stop("Missing number of trials (N).")
    }
  } else if (identical(family, Poisson)) {
    outcome <- family(name.Y, data = Y, offset = offset)
  } else {
    if (!inherits(family, "dlm_distr")) {
      stop("Family is not a dlm_distr object.")
    } else {
      outcome <- family(name.Y, data = Y, offset = offset)
    }
  }

  args <- list(args, p.monit = p.monit)
  args[[name.Y]] <- outcome
  model <- do.call(fit_model, args)
  # plot(model)
  model
}

#' formula.to.structure
#'
#' @param formula an object of class "formula" (or one that can be coerced to that class): a symbolic description of the model to be fitted.
#' @param data an optional data frame, list or environment (or object coercible by as.data.frame to a data frame) containing the variables in the model. If not found in data, the variables are taken from environment(formula), typically the environment from which glm is called.
#' @param label An optional character naming the linear predictor.
#'
#' @importFrom stats update.formula model.matrix
#'
#' @keywords internal
formula.to.structure <- function(formula, data, label = "mu") {
  terms <- attr(terms(formula), "term.labels")
  intercept.add <- attr(terms(formula), "intercept") & !any(grepl("pol(", terms, fixed = TRUE))
  intercept.flag <- attr(terms(formula), "intercept")
  terms <- attr(terms(formula), "term.labels")
  args <- list()
  terms.mat <- c()
  if (length(terms) >= 1) {
    for (i in 1:length(terms)) {
      var <- eval(parse(text = terms[i]), envir = data)
      if (inherits(var, "dlm_block")) {
        args <- append(args, list(eval(parse(text = terms[i]), envir = data)))
      } else {
        terms.mat <- append(terms.mat, terms[i])
      }
    }
  }
  if (length(terms.mat) > 0 | intercept.add) {
    mat.formula <- update.formula(formula, as.formula(paste0("~", paste(c(ifelse(intercept.flag, 1, 0), terms.mat), collapse = "+"))))

    X <- model.matrix(mat.formula, data = data)
    if (!intercept.add & intercept.flag) {
      X <- X[, -1, drop = FALSE]
    }
    for (j in 1:dim(X)[2]) {
      args <- append(args, list(reg(X[, j], name = colnames(X)[j])))
    }
  }
  block_rename(do.call(block_superpos, args), label)
}
