#' Summary of a BTFit object.
#'
#' Computes the relative influence of each variable in the BTFit object.
#'
#' @param object a \code{\link{BTFit}} object.
#' @param cBars the number of bars to plot. If \code{order=TRUE} only the variables with the \code{cBars} largest relative influence will appear in the barplot.
#' If \code{order=FALSE} then the first \code{cBars} variables will appear in the barplot.
#' @param n.iter the number of trees used to compute the relative influence. Only the first \code{n.iter} trees will be used.
#' @param plot_it an indicator as to whether the plot is generated.
#' @param order_it an indicator as to whether the plotted and/or returned relative influences are sorted.
#' @param method the function used to compute the relative influence. Currently, only \code{\link{.BT_relative_influence}} is available (default value as well).
#' @param normalize if \code{TRUE} returns the normalized relative influence.
#' @param ... additional argument passed to the plot function.
#'
#' @return Returns a data frame where the first component is the variable name and the second one is the computed relative influence, normalized to sum up to 100.
#' Depending on the \code{plot_it} value, the relative influence plot will be performed.
#'
#' @details Please note that the relative influence for variables having an original \strong{negative} relative influence is forced to 0.
#'
#' @author Gireg Willame \email{gireg.willame@@gmail.com}
#'
#' \emph{This package is inspired by the \code{gbm3} package. For more details, see \url{https://github.com/gbm-developers/gbm3/}}.
#'
#' @seealso \code{\link{BT}}, \code{\link{.BT_relative_influence}}.
#'
#' @references M. Denuit, D. Hainaut and J. Trufin (2019). \strong{Effective Statistical Learning Methods for Actuaries |: GLMs and Extensions}, \emph{Springer Actuarial}.
#'
#' M. Denuit, D. Hainaut and J. Trufin (2019). \strong{Effective Statistical Learning Methods for Actuaries ||: Tree-Based Methods and Extensions}, \emph{Springer Actuarial}.
#'
#' M. Denuit, D. Hainaut and J. Trufin (2019). \strong{Effective Statistical Learning Methods for Actuaries |||: Neural Networks and Extensions}, \emph{Springer Actuarial}.
#'
#' M. Denuit, D. Hainaut and J. Trufin (2022). \strong{Response versus gradient boosting trees, GLMs and neural networks under Tweedie loss and log-link}.
#' Accepted for publication in \emph{Scandinavian Actuarial Journal}.
#'
#' M. Denuit, J. Huyghe and J. Trufin (2022). \strong{Boosting cost-complexity pruned trees on Tweedie responses: The ABT machine for insurance ratemaking}.
#' Paper submitted for publication.
#'
#' M. Denuit, J. Trufin and T. Verdebout (2022). \strong{Boosting on the responses with Tweedie loss functions}. Paper submitted for publication.
#'
#' @rdname summary.BTFit
#' @export
#'
summary.BTFit <- function(object,
                          cBars = length(object$var.names),
                          n.iter = object$BTParams$n.iter,
                          plot_it = TRUE,
                          order_it = TRUE,
                          method = .BT_relative_influence,
                          normalize = TRUE,
                          ...)
{
  # Initial checks
  .check_n_iter(n.iter)
  .check_if_BT_fit(object)

  if (is.null(cBars) ||
      !(.check_if_natural_number(cBars) ||
        (cBars == 0)) ||
      (length(cBars) > 1)) {
    stop("cBars should be an integer.")
  }
  if (!is.logical(plot_it) ||
      (length(plot_it) > 1) ||
      is.na(plot_it)) {
    stop("argument plot_it must be a logical - excluding NA")
  }
  if (!is.logical(order_it) ||
      (length(order_it) > 1) ||
      is.na(order_it)) {
    stop("argument order_it must be a logical - excluding NA")
  }
  if (!is.logical(normalize) ||
      (length(normalize) > 1) ||
      is.na(normalize)) {
    stop("argument normalize must be a logical - excluding NA")
  }

  # Set inputs (if required)
  if (cBars == 0)
    cBars <- min(10, length(object$var.names))
  if (cBars > length(object$var.names))
    cBars <- length(object$var.names)
  if (n.iter > object$BTParams$n.iter)
    warning(
      "Exceeded total number of BT terms. Results use n.iter=",
      object$BTParams$n.iter,
      " terms.\n"
    )
  n.iter <- min(n.iter, object$BTParams$n.iter)

  # Calculate relative influence and order/normalize
  rel_inf <- method(object, n.iter = n.iter)
  rel_inf[rel_inf < 0] <- 0
  if (normalize)
    rel_inf <- 100 * rel_inf / sum(rel_inf)

  ordering <- seq_len(length(rel_inf))
  if (order_it) {
    ordering <- order(-rel_inf)
  }

  # Bar plot of relative influence
  if (plot_it) {
    barplot(
      rel_inf[ordering[cBars:1]],
      horiz = TRUE,
      col = rainbow(cBars, start = 3 / 6, end = 4 / 6),
      names = object$var.names[ordering[cBars:1]],
      xlab = "Relative influence",
      las = 1,
      ...
    )
  }
  return(data.frame(var = object$var.names[ordering],
                    rel_inf = rel_inf[ordering]))
}
