# (S3-)integration functions

#' @export
print.incubate_fit <- function(x, ...) {
  coe <- coef(x)
  rangeTime <- if (x[["twoGroup"]]) {
    ns <- x[["nobs"]]
    paste(
      round(sort(c(x[["data"]]$x[[1L]], x[["data"]]$y[[1L]]))[[1L]], 4),
      round(
        sort(c(x[["data"]]$x[[ns[["x"]]]], x[["data"]]$y[[ns[["y"]]]]))[[2L]],
        4
      ),
      sep = " to "
    )
  } else {
    paste(
      round(x$data[[1L]], 4),
      round(x[["data"]][[length(x$data)]], 4),
      sep = " to "
    )
  }
  cat(
    glue::glue_data(
      x,
      .sep = "\n",
      "Fit a {distO$dist_name}{c('', ' with two delay phases')[[1L+twoPhase]]} through{c('', ' profiled')[[1L+optimizer$profiled]]} {switch(method,
                      MPSE = 'Maximum Product of Spacings Estimation (MPSE)', MLEn = 'naive Maximum Likelihood Estimation (MLEn)',
                      MLEw = 'weighted Maximum Likelihood Estimation (MLEw)',
                      MLEc = 'corrected Maximum Likelihood Estimation (MLEc)', '???')} for {c('a single group', 'two independent groups')[[1L+twoGroup]]}.",
      "Data: {if (twoGroup) paste(nobs, collapse = ' and ') else nobs[[1L]]} observations, ranging from {rangeTime}",
      "Criterion: {signif(criterion,3)}",
      "Fitted coefficients: {if (is.null(coe)) '-' else paste(paste('\n  ', names(coe)), signif(coe,5L), sep = ': ', collapse = ' ')}"
    ),
    "\n"
  )
}

#' Coefficients of a delay-model fit.
#' @param object object that is a `incubate_fit`
#' @param transformed flag. Do we request the transformed parameters as used
#'   within the optimization?
#' @param group character string to request the canonical parameter for one
#'   group
#' @param ... further arguments, currently not used.
#' @return named coefficient vector
#' @export
coef.incubate_fit <- function(object, transformed = FALSE, group = NULL, ...) {
  stopifnot(inherits(object, "incubate_fit"))
  transformed <- isTRUE(transformed)

  rlang::env_get(rlang::fn_env(object$objFun), nm = "extractPars")(
    purrr::chuck(
      object,
      !!!if (transformed) list("optimizer", "parOpt") else "par"
    ),
    group = group,
    isOpt = transformed,
    transform = FALSE,
    named = TRUE
  )
}

#' @export
summary.incubate_fit <- function(object, ...) {
  print(object)
}


#' Plot a fitted delay-model object of class `incubate_fit`
#'
#' The fitted delay-model is plotted together with a Kaplan-Meier survival curve.
#' Optionally, a fit of a second delay-model can be added. When given, we do
#' some preliminary checks that the two models do fit together.
#'
#' @details This function requires the `ggplot2`-package to be installed.
#'
#' @param x a fitted delay-model
#' @param y optionally, a second fitted delay-model
#' @param title character. Optionally, provide a title to the plot.
#' @param subtitle character. Optionally, provide a subtitle to the plot. By
#'   default the coefficients are shown.
#' @param xlim numeric. Optionally, limits for the x-axis (time). If unspecified
#'   starts from 0 to last observation.
#' @param ... further arguments. Not in use here (it is required for generic plot function)
#' @examples
#' # fit a delay-weibull model to serial interval times from historic measles outbreak data:
#' fm <- delay_model(survival::Surv(measles_sailer$serialInterval, measles_sailer$status),
#'                   distribution = "weibull", method = "MLEw")
#' plot(fm,
#'      title = "Serial interval times of measles",
#'      subtitle = "Delay-Weibull model fit using weighted MLE (MLEw)")
#'
#' @export
plot.incubate_fit <- function(x, y, title, subtitle, xlim, ...) {
  stopifnot(inherits(x, "incubate_fit"))
  haveY <- !missing(y) &&
    !x[["twoGroup"]] &&
    inherits(y, "incubate_fit") &&
    !y[["twoGroup"]] &&
    # check for same (number of) data. Really necessary?!
    NROW(x[["data"]]) == NROW(y[["data"]]) &&
    # expect different methods (as we use it as colour labels)
    # Really necessary? (difference could be elsewhere,
    #+e.g. in optim_args$lower or optim_args$method)
    x[["method"]] != y[["method"]]

  rlang::check_installed(
    pkg = "ggplot2",
    reason = "to draw plots",
    version = "3.3"
  )

  cumFun <- x$distO$cdf
  cumFunY <- if (haveY) y$distO$cdf

  # add time = 0 per group.
  # ???survival 3.8-3: start.time= argument is ignored?!
  kmFit0 <- survival::survfit0(x[["kmFit"]], start.time = 0)
  kmFit0 <- tibble(
    group = if (is.null(kmFit0$strata)) {
      "x"
    } else {
      rep.int(c("x", "y"), times = kmFit0$strata)
    },
    time = kmFit0$time,
    n.risk = kmFit0$n.risk,
    n.event = kmFit0$n.event,
    n.censor = kmFit0$n.censor,
    surv = kmFit0$surv,
    evrate = 1 - .data$surv
  )

  # add estimated delay model
  p <- if (x[["twoGroup"]]) {
    ggplot2::ggplot(
      data = kmFit0,
      mapping = ggplot2::aes(
        x = .data$time,
        y = .data$evrate,
        col = .data$group
      )
    ) +
      ggplot2::geom_function(
        mapping = ggplot2::aes(col = rep.int("x", NROW(kmFit0))),
        fun = cumFun,
        args = coef(x, group = "x"),
        linetype = "dashed"
      ) +
      ggplot2::geom_function(
        mapping = ggplot2::aes(col = rep.int("y", NROW(kmFit0))),
        fun = cumFun,
        args = coef(x, group = "y"),
        linetype = "dashed"
      )
  } else {
    ggplot2::ggplot(
      data = kmFit0,
      mapping = ggplot2::aes(
        x = .data$time,
        y = .data$evrate
      )
    ) +
      ggplot2::geom_function(
        mapping = if (haveY) {
          ggplot2::aes(col = rep.int(x$method, NROW(kmFit0)))
        },
        inherit.aes = FALSE,
        fun = cumFun,
        args = coef(x, group = "x"),
        linetype = "dashed"
      )
  } #esle twoGroup

  if (haveY) {
    p <- p +
      ggplot2::geom_function(
        mapping = ggplot2::aes(col = rep.int(y$method, NROW(kmFit0))),
        inherit.aes = FALSE,
        fun = cumFunY,
        args = coef(y, group = "x"),
        linetype = "dashed"
      )
  }

  p <- p +
    # kaplan meier step function
    ggplot2::geom_step() +
    # mark (right-)censored observations
    ggplot2::geom_point(data = function(.x) .x[.x$n.censor > 0, ], shape = 3)

  if (missing(title)) {
    title <- glue::glue_data(
      x,
      "Fitted {x$distO$dist_name} {c('model ', 'models ')[[1L+(twoGroup || haveY)]]}",
      "{c('', 'with two delay phases')[[1L+twoPhase]]}"
    )
  } #fi

  if (missing(subtitle)) {
    coefPrint <- function(mod = x, gr, n_signif = 4) {
      co <- coef.incubate_fit(mod, group = gr)
      paste(
        names(co),
        signif(co, digits = n_signif),
        sep = ": ",
        collapse = ", "
      )
    }

    subtitle <- if (x[["twoGroup"]]) {
      paste(
        coefPrint(mod = x, "x", n_signif = 3),
        coefPrint(mod = x, "y", n_signif = 3),
        sep = " - "
      )
    } else if (haveY) {
      paste(
        coefPrint(mod = x, gr = "x", n_signif = 3),
        coefPrint(mod = y, gr = "x", n_signif = 3),
        sep = " - "
      )
    } else {
      coefPrint(mod = x, gr = "x")
    }
  } #fi subtitle

  if (missing(xlim) || is.null(xlim)) {
    xlim <- c(0L, NA)
  }

  p +
    # transforms "after_stat" which matters for stat_ecdf
    ggplot2::coord_transform(y = "reverse", xlim = xlim) +
    ggplot2::labs(
      x = "Time",
      y = "Cumulative prop. of events",
      col = if (x[["twoGroup"]]) {
        "Group"
      } else if (haveY) {
        "Model"
      } else {
        NULL
      },
      title = title,
      subtitle = subtitle
    )
}


#' Add a line fit to a plot of (another) `incubate_fit` object
#'
#' Carries over the concept of base-plot `lines` to ggplot. This function returns a
#' ggplot-layer to be added to an existing ggplot-object of an incubate fit. It allows to set
#' aesthetics of the plot manually.
#'
#' @param x `incubate_fit` object whose model fit is to be added to a ggplot
#' @param mapping a `ggplot2::mapping` object. Default to `NULL`.
#' @param ... further arguments to passed to `geom_function`, outside of the mapping. E.g., `linetype = 'dashed'`
#' @returns a `geom_function` ggplot-layer
#' @exportS3Method graphics::lines
lines.incubate_fit <- function(x, mapping = NULL, ...) {
  stopifnot(inherits(x, "incubate_fit"))

  rlang::check_installed(
    pkg = "ggplot2",
    reason = "to draw plots",
    version = "3.3"
  )

  stopifnot(is.null(mapping) || inherits(mapping, "ggplot2::mapping"))

  distO <- x$distO
  cumFun <- distO$cdf

  ggplot2::geom_function(
    mapping = mapping,
    fun = cumFun,
    args = coef(x, group = "x"),
    ...
  )
}


#' Extract Log-Likelihood
#'
#' The user has the possibility to request different flavours of log-likelihood.
#' By default the flavour matching the fitting method is used.
#' @param object an `incubate_fit` object
#' @param method Which flavour of the log-likelihood? By default, it uses the flavour from the model fit
#' @param ... further arguments passed on to the object function of the model fit object
#' @return Log-likelihood value for the model fit object
#' @export
logLik.incubate_fit <- function(object, method = NULL, ...) {
  object[["objFun"]](
    pars = object[["par"]],
    isOrig = TRUE,
    criterion = if (is.null(method)) object[["method"]] else method,
    maximum = TRUE,
    ...
  )
}


#' Transform observed data to unit interval
#'
#' The transformation used is the probability integral transform: the cumulative
#' distribution function with the estimated parameters of the model fit takes
#' the data into the 0-1 interval. All available data in the model fit is
#' transformed. Censored observations lead to censored back-transformed
#' observations as well.
#'
#' @note This S3-method implementation is quite different from its default
#' method that allows for non-standard evaluation on data frames, primarily
#' intended for interactive use. But the name `transform` fits so nicely to the
#' intended purpose that it is re-used for the probability integral transform,
#' here.
#'
#' @param _data a fitted model object of class `incubate_fit`
#' @param ... currently ignored
#' @return The transformed data, either a vector (for single group) or a list
#'   with entries x and y (in two group scenario)
#' @export
transform.incubate_fit <- function(`_data`, ...) {
  stopifnot(inherits(`_data`, "incubate_fit"))

  cdfFun <- `_data`$distO$cdf

  twoGroup <- isTRUE(`_data`$twoGroup)
  isSurv <- isTRUE(`_data`$cens$isSurv)

  x <- if (twoGroup) `_data`$data$x else `_data`$data

  tr <- NULL

  if (isSurv) {
    # currently, handle right-censored case only
    stopifnot(attr(x, which = "type", exact = TRUE) == "right")
    tr <- Surv(
      time = rlang::exec(
        cdfFun,
        !!!c(list(q = x[, 1L]), coef(`_data`, group = "x"))
      ),
      event = x[, "status"],
      type = "right"
    )
    if (twoGroup) {
      tr <- list(
        x = tr,
        y = Surv(
          time = rlang::exec(
            cdfFun,
            !!!c(list(q = `_data`$data$y[, 1L]), coef(`_data`, group = "y"))
          ),
          event = `_data`$data$y[, "status"],
          type = "right"
        )
      )
    }
  } else {
    tr <- rlang::exec(cdfFun, !!!c(list(q = x), coef(`_data`, group = "x")))
    if (twoGroup) {
      tr <- list(
        x = tr,
        y = rlang::exec(
          cdfFun,
          !!!c(list(q = `_data`$data$y), coef(`_data`, group = "y"))
        )
      )
    }
  }

  tr
}
