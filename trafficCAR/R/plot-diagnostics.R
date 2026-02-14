#' MCMC diagnostic plots
#'
#' @param fit traffic_fit
#' @importFrom posterior ess_basic
#' @export
#'
#' @return A list with components:
#' \describe{
#'   \item{plot}{A `ggplot` object of diagnostic summaries.}
#'   \item{summary}{A data frame with columns `parameter` and `ess`, giving
#'   the effective sample size for each parameter..}
#' }
plot_mcmc_diagnostics <- function(fit) {
  if (is.null(fit$draws)) stop("`fit$draws` is missing.")
  if (!is.list(fit$draws)) stop("`fit$draws` must be a list.")

  # empty draws: return correctly shaped outputs
  if (length(fit$draws) == 0L) {
    summary <- data.frame(
      parameter = character(0),
      ess = numeric(0),
      row.names = NULL
    )

    plot <- ggplot2::ggplot(
      summary,
      ggplot2::aes(x = .data$parameter, y = .data$ess)
    ) +
      ggplot2::geom_col() +
      ggplot2::coord_flip() +
      ggplot2::labs(x = NULL, y = "Effective Sample Size (ESS)")

    return(list(plot = plot, summary = summary))
  }

  bad <- vapply(
    fit$draws,
    function(x) !is.numeric(x) || any(!is.finite(x)),
    logical(1)
  )
  if (any(bad)) stop("All draws must be numeric and finite.")

  ess <- vapply(fit$draws, posterior::ess_basic, numeric(1))

  summary <- data.frame(
    parameter = names(ess),
    ess = as.numeric(ess),
    row.names = NULL
  )

  plot <- ggplot2::ggplot(
    summary,
    ggplot2::aes(x = .data$parameter, y = .data$ess)
  ) +
    ggplot2::geom_col() +
    ggplot2::coord_flip() +
    ggplot2::labs(x = NULL, y = "Effective Sample Size (ESS)")

  list(plot = plot, summary = summary)
}
