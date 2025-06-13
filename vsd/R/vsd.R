#' Visualizing Survival Data
#'
#' This function outputs renders of the inputted survival data analysis data
#' and/or model, and their components, into a graphically pleasing output, under
#' the [ggplot2] format.
#'
#' Depending on the kind of model passed to the function, the kind of generated
#' graphics might vary, but usually an estimation of the survival and risk
#' curves (depending if the model has covariables) is expected. The kinds of
#' graphics that can be created according to a specific R object are detailed on
#' Usage on `.include`'s definition, but non-relevant graphics can be requested
#' without error, as the function ignores them silently.
#'
#' Extra options for each graph kind can be passed to either all created
#' graphics, by having them as generic arguments, or to specific graphic types,
#' using a list in `.arguments`. Arguments are filtered, so that generic
#' arguments aren't applied if a graphic kind wouldn't use them. As an example,
#' ``` 
#' vsd(model, data, 
#'     .arguments = list(fit = (size = 3, xlab = "Weeks")),
#'     xlab = "Days") 
#' ``` 
#' would set all graphics that have an label on the x axis to
#' "Days", except the `fit` graph, which would have "Weeks" instead.
#'
#' # Generic graphical arguments
#'
#' Unless specified, all graphics are created under [ggpubr::ggpar()] and have
#' as additional options `palette`, `main`, `submain`, `xlab`, `ylab`,
#' `legend.title` and `ggtheme`. Most line graphics also allow to set the
#' options `size`, `linetype`, `alpha` and `color` to determine line styles, as
#' detailed on [survminer::ggsurvplot()].
#'
#' ## fit
#'
#' Line graphic, with a further subset of the options present in
#' [survminer::ggsurvplot()]: `censor`, `censor.shape`, `censor.size`,
#' `conf.int`, `conf.int.style`.
#'
#' ## parametric
#'
#' Line graphic, with a further subset of the options present in
#' [survminer::ggflexsurvplot()]: `conf.int.km`.
#'
#' ## forest
#'
#' Non-standard graphic(s), using all options within [survminer::ggforest()]:
#' `main`, `cpositions`, `fontsize`, `refLabel`, `noDigits`.
#'
#' ## residuals
#'
#' Line graphic(s), with a further subset of options present in
#' [survminer::ggcoxzph()]: `resid`, `se`, `df`, `nsmo`, `var`, `caption`; and
#' point style customization options as `point.col`, `point.size`,
#' `point.shape`, and `point.alpha`.
#'
#' ## hazard
#'
#' Line graphics, using the generic graphical arguments.
#'
#' @param model The survival model, or data structure, to generate graphics from
#' @param data Dataframe from where the model fetches its variables, if left
#'   blank will be extracted from the model, if possible
#' @param .interactive Allows to explore the generated graphs before returning
#'   (use with [plotly](https://plotly.com/r/) package for best results)
#' @param .include Graph types to output if relevant, defaults to all possible
#' @param .arguments Collection of list of arguments, indexed by the specific
#'   type of graph they should be passed to, has priority over \dots
#' @param ... Miscellaneous arguments, passed to ALL graphs
#'
#' @import survival
#' @import ggplot2
#' @importFrom stats model.frame
#' @export
#' @return A list of ggplot2 graphs and/or list of graphs, relevant to the model
#' 
#' @examples
#' # non-models are cohersed into a survfit object with default arguments
#' vsd(coxph(Surv(time, status) ~ sex + strata(rx) + adhere, data = colon), 
#'     .include = c("haz"))
#'
#' # parametric models are also supported with flexsurv
#' vsd(flexsurv::flexsurvreg(Surv(rectime, censrec) ~ group, data = flexsurv::bc, dist = 'gengamma'),
#'     .include = c("par"))
#' 
vsd <- function(model,
                data = NULL,
                .interactive = FALSE,
                .include =
                  c("fit", "parametric", "forest", "residuals", "hazard"),
                .arguments = list(),
                ...) {
    UseMethod("vsd")
}

#' @describeIn vsd Wraps `Surv(...) ~ (...)` in a survfit object (Kaplan-Meier
#' model)
#' @export
vsd.formula <- function(model,
                         data = NULL,
                         .interactive = FALSE,
                         .include = c("fit", "hazard"),
                         .arguments = list(),
                         ...) {
  # (Assumedly) '~' call (TODO: fail first?)
  if (is.null(data)) {
    stop("Data structure required with fit object of type call.")
  }
  new_model <- survfit(model, data)
  new_model$call$formula <- eval(model, data)

  vsd(new_model, data, .interactive, .include, .arguments, ...)
}

#' @describeIn vsd Wraps [Surv()] in a survfit object (Kaplan-Meier
#' model)
#' @export
vsd.Surv <- function(model,
                     data = NULL,
                     .interactive = FALSE,
                     .include = c("fit", "hazard"),
                     .arguments = list(),
                     ...) {
  # Surv object TODO: more than just right-censored survival
  data <- as.data.frame(as.matrix(model))
  warning("Fetched `data`: ", data, call. = FALSE, immediate. = TRUE)
  new_model <- survfit(Surv(time, status) ~ 1, data)
  warning("New model: ", new_model, call. = FALSE, immediate. = TRUE)

  vsd(new_model, data, .interactive, .include, .arguments, ...)
}

#' @describeIn vsd Wraps \code{coxph(...)} in a survfit object (Kaplan-Meier
#' model)
#' @export
vsd.coxph <- function(model,
                      data = NULL,
                      .interactive = FALSE,
                      .include = c("fit", "forest", "residuals", "hazard"),
                      .arguments = list(),
                      ...) {
  if (is.null(data)) {
    data <- eval(model$call$data)
    if (is.null(data)) {
      stop("Original data structure couldn't be extracted, ",
           "supply it to function call instead")
    }
  }

  # http://adv-r.had.co.nz/Expressions.html#capturing-call
  new_model <- survfit(model)
  new_model$call$formula <- substitute(model)
  vsd(new_model, data, .interactive, .include, .arguments, ...)
}

#' @describeIn vsd Graphical output for survfit objects (Kaplan-Meier model)
#' @export
vsd.survfit <- function(model,
                        data = NULL,
                        .interactive = FALSE,
                        .include = c("fit", "hazard"),
                        .arguments = list(),
                        ...) {
  plots <- list()
  options <- .options(.arguments, ...)
  include <-
    as.vector(match.arg(.include, several.ok = TRUE))

  # retrieving mid-objects
  if (is.null(data)) {
    if (!is.null(model$call$data)) {
      data <- eval(model$call$data)
    } else if (is.call(model$call$formula) &&
               model$call$formula[[1]] == "coxph") {
      data <- eval(eval(model$call$formula)$call$data)
    }

    if (is.null(data)) {
      stop("Original data structure couldn't be extracted,",
           " supply it to function call instead")
    }
  }

  formula <- model$call$formula
  model_frame <- model.frame(formula, data)
  strata <- .get_strata(formula, model_frame)

  surv <- as.data.frame(as.matrix(model_frame[, 1]))

  #### PLOT$FIT
  if (("fit" %in% include)) {
    fit_plots <- do.call(survminer::ggsurvplot, append(
      list(model, data), options$fit))
    plots$fit <- fit_plots$plot
  }

  #### PLOT$HAZARD
  if (("hazard" %in% include)) {
    hazard_plots <-
      do.call(plot_hazard, append(list(surv, strata), options$hazard))
    plots <- append(plots, hazard_plots)
  }

  if (.interactive && interactive()) {
    .do_interactive(plots)
  }
  return(plots)
}

#' @describeIn vsd Graphical output for survfit objects (Cox model)
#' @export
vsd.survfitcox <- function(model,
                           data = NULL,
                           .interactive = FALSE,
                           .include = c("fit", "forest", "residuals", "hazard"),
                           .arguments = list(),
                           ...) {
  plots <- list()
  options <- .options(.arguments, ...)
  include <- as.vector(match.arg(.include, several.ok = TRUE))

  # retrieving mid-objects
  if (is.null(data)) {
    data <- eval(eval(model$call$formula)$call$data)

    if (is.null(data)) {
      stop("Original data structure couldn't be extracted, ",
           "supply it to function call instead")
    }
  }

  cox_model <- eval(model$call$formula, data)
  formula <- cox_model$formula
  model_frame <- model.frame(formula, data)
  strata <- .get_strata(cox_model, model_frame)

  surv <- as.data.frame(as.matrix(model_frame[, 1]))

  #### PLOT$FIT
  if (("fit" %in% include)) {
    fit_plots <- do.call(survminer::ggsurvplot, append(
      list(model, data), options$fit))
    plots$fit <- fit_plots$plot
  }

  #### PLOT$FOREST
  if (("forest" %in% include)) {
    forest_plots <- do.call(plot_forest, append(
      list(cox_model, data, strata), options$forest))
    plots <- append(plots, forest_plots)
  }

  #### PLOT$RESIDUALS (for coxph)
  if (("residuals" %in% include)) {
    plots$residuals <- do.call(survminer::ggcoxzph, append(
      list(cox.zph(cox_model)), options$residuals))
  }

  #### PLOT$HAZARD
  if (("hazard" %in% include)) {
    hazard_plots <- do.call(plot_hazard, append(
      list(surv, strata), options$hazard))
    plots <- append(plots, hazard_plots)
  }

  if (.interactive && interactive()) {
    .do_interactive(plots)
  }
  return(plots)
}

#' @describeIn vsd Graphical output for flexsurvreg objects (various parametric models)
#' @export
vsd.flexsurvreg <- function(model,
                            data = NULL,
                            .interactive = FALSE,
                            .include = c("fit", "parametric", "hazard"),
                            .arguments = list(),
                            ...) {
  plots <- list()
  options <- .options(.arguments, ...)
  include <- as.vector(match.arg(.include, several.ok = TRUE))

  if (is.null(data)) {
    if (!is.null(model$call$data)) {
      data <- eval(model$call$data)
    }

    if (is.null(data)) {
      stop("Original data structure couldn't be extracted, ",
           "supply it to function call instead")
    }
  }

  formula <- eval(model$call$formula, data)
  model_frame <- model.frame(model)
  strata <-
    .get_strata(formula, model_frame[, !(names(model_frame) == "(weights)")])
  surv <- as.data.frame(as.matrix(model_frame[, 1]))

  km_fit <- survfit(formula, data)
  km_fit$call$formula <- eval(km_fit$call$formula, data)


  #### PLOT$FIT
  if ("fit" %in% include) {
    plot_fit <- do.call(survminer::ggsurvplot, append(
      list(km_fit, data), options$fit))
    plots$fit <- plot_fit$plot
  }

  #### PLOT$PARAMETRIC
  if (("parametric" %in% include)) {
    parametric_plots <-
      do.call(plot_parametric, append(
        list(model, km_fit, strata, data), options$parametric))
    plots <- append(plots, parametric_plots)
  }

  #### PLOT$HAZARD
  if (("hazard" %in% include)) {
    hazard_plots <- do.call(plot_hazard, append(
      list(surv, strata), options$hazard))
    plots <- append(plots, hazard_plots)
  }

  if (.interactive && interactive()) {
    .do_interactive(plots)
  }
  return(plots)
}


.do_interactive <- function(plots) {
  # TODO: make choices into two lists: plots, types ?
  choices <- .unlist_plots(plots)
  whitelist <- c("fit", "parametric", "residuals", "hazard")

  repeat {
    choice <- utils::menu(names(choices),
                          title = "Pick a graphic (or 0 to exit)")
    if (choice <= 0)
      break
    choice <- choices[[choice]]
    plot <- choice$plot
    type <- choice$type

    if (type %in% whitelist) {
      if (requireNamespace("plotly", quietly = TRUE)) {
        if (inherits(plot, "ggsurvplot")) {
          print(plotly::ggplotly(plot$plot))
        } else {
          print(plotly::ggplotly(plot))
        }
      } else {
        print(plot)
      }
    } else {
      print(plot)
    }
  }
}
