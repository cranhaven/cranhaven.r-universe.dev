#' Visualizes the path of the EM algorithm
#' @param x A fitted `survival_ln_mixture_em` object.
#' @param ... Not used.
#'
#' @returns A `ggplot` object (or `plotly`, if the package is avaiable) of the EM algorithm iterations.
#'
#' @export
plot.survival_ln_mixture_em <- function(x, ...) {
  iter <- value <- var <- NULL
  rlang::check_dots_empty()

  if (!requireNamespace("plotly")) {
    tidyr::pivot_longer(x$em_iterations, 1:(ncol(x$em_iterations) - 1),
      names_to = "var"
    ) |>
      dplyr::mutate(cat = ifelse(startsWith(var, "phi"),
        "phi",
        ifelse(startsWith(var, "eta"),
          "eta", "beta"
        )
      )) |>
      ggplot2::ggplot() +
      ggplot2::geom_path(ggplot2::aes(x = iter, y = value, color = var)) +
      ggplot2::theme_bw() +
      ggplot2::facet_wrap(~cat, scales = "free_y") +
      ggplot2::labs(
        y = "Value",
        x = "Iteration"
      ) +
      ggplot2::guides(color = "none")
  } else {
    (tidyr::pivot_longer(x$em_iterations, 1:(ncol(x$em_iterations) - 1),
      names_to = "var"
    ) |>
      dplyr::mutate(cat = ifelse(startsWith(var, "phi"),
        "phi",
        ifelse(startsWith(var, "eta"),
          "eta", "beta"
        )
      )) |>
      ggplot2::ggplot() +
      ggplot2::geom_path(ggplot2::aes(x = iter, y = value, color = var)) +
      ggplot2::theme_bw() +
      ggplot2::facet_wrap(~cat, scales = "free_y") +
      ggplot2::labs(
        y = "Value",
        x = "Iteration"
      ) +
      ggplot2::guides(color = "none")) |>
      plotly::ggplotly()
  }
}

extract_formula <- function(model) {
  # Trocar NULL por 1 para caso onde so tem intercepto
  formula <- gsub("NULL", "1", deparse(model$blueprint$formula))
  return(formula)
}

npredictors <- function(model) {
  return(ncol(model$blueprint$ptypes$predictors) + model$blueprint$intercept)
}

nobs.survival_ln_mixture_em <- function(object, ...) { # nolint: object_name_linter.
  rlang::check_dots_empty(...)
  return(object$nobs)
}

niterations <- function(model) {
  return(nrow(model$em_iterations))
}

##' @importFrom stats logLik
##' @export
logLik.survival_ln_mixture_em <- function(object, ...) {
  rlang::check_dots_empty(...)
  
  return(object$logLik)
}

##' @importFrom stats AIC
##' @export
AIC.survival_ln_mixture_em <- function(object, ..., k = 2) {
  rlang::check_dots_empty(...)
  logLik <- stats::logLik(object)
  nparam <- ncol(object$em_iteration) - 1 # remove iter column from em_iteration matrix
  
  return(round(-2 * logLik + k * nparam, 2))
}

##' @importFrom stats BIC
##' @export
BIC.survival_ln_mixture_em <- function(object, ...) {
  rlang::check_dots_empty(...)
  logLik <- stats::logLik(object)
  nparam <- ncol(object$em_iteration) - 1 # remove iter column from em_iteration matrix
  
  return(round(-2 * logLik + log(nobs(object)) * nparam, 2))
}