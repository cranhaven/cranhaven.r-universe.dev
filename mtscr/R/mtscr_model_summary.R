#' Summarise a model
#'
#' Summarise a model generated with \code{\link{mtscr_model}} with
#' some basic statistics; calculate the empirical reliability
#' and the first difference of the empirical reliability.
#'
#' @param model A model generated with \code{\link{mtscr_model}}. Can
#' be a list of models.
#'
#' @return A data frame with the following columns:
#'     \describe{
#'         \item{model}{The model number}
#'         \item{nobs}{Number of observations}
#'         \item{sigma}{The square root of the estimated residual variance}
#'         \item{logLik}{The log-likelihood of the model}
#'         \item{AIC}{The Akaike information criterion}
#'         \item{BIC}{The Bayesian information criterion}
#'         \item{df.residual}{The residual degrees of freedom}
#'         \item{emp_rel}{The empirical reliability}
#'         \item{FDI}{The first difference of the empirical reliability}
#'     }
#' @export
#'
#' @examples
#' data("mtscr_creativity", package = "mtscr")
#' mtscr_model(mtscr_creativity, id, item, SemDis_MEAN, top = 1:3) |>
#'   mtscr_model_summary()
mtscr_model_summary <- function(model) {
  if (!methods::is(model, "list")) {
    model <- list(model)
  }

  if (!any(purrr::map_lgl(model, methods::is, "glmmTMB"))) {
    cli::cli_abort(
      c(
        "The model must be a glmmTMB object or a list of glmmTMB objects.",
        "x" = "{.obj_type_friendly {model}} is not a glmmTMB object or a list of glmmTMB objects."
      )
    )
  }

  purrr::map(
    model,
    \(x) {
      st <- glmmTMB::VarCorr(x)[1]$cond[[1]][1]
      diag_cov <- x$sdr$diag.cov.random
      se <- diag_cov[seq(1, length(diag_cov) / 2)] |>
        mean()

      broom.mixed::glance(x) |>
        dplyr::mutate(
          emp_rel = 1 - se / st,
          FDI = sqrt(.data$emp_rel)
        )
    }
  ) |>
    dplyr::bind_rows(.id = "model")
}
