#' Plot a fitted epicurve
#'
#' @author Tim Taylor
#'
#' @param x An `incidence2_fit` object created by [fit_curve()].
#'
#' @param include_warnings
#'
#' Include results in plot that triggered warnings but not errors.
#'
#' Defaults to `FALSE`.
#'
#' @param ci
#'
#' Plot confidence intervals.
#'
#' Defaults to TRUE.
#'
#' @param pi
#'
#' Plot prediction intervals.
#'
#' Defaults to FALSE.
#'
#' @param ...
#'
#' Additional arguments to be passed to [incidence2::plot.incidence2()] .
#'
#' @return An incidence plot with the addition of a fitted curve.
#'
#' @importFrom rlang .data
#' @export
plot.incidence2_fit <- function(x, include_warnings = TRUE, ci = TRUE, pi = FALSE,...) {

    # pull out the variables we need
    group_vars <- attr(x, "groups")
    date_var <- attr(x, "date")
    count_variable <- attr(x, "count_variable")
    counts <- attr(x, "counts")
    estimates <- attr(x, "fitted")

    # filter results to exclude errors and, optionally, warnings
    x <- is_ok(x, include_warnings = include_warnings)

    # convert to incidence object
    x <- x[, c(count_variable, group_vars, estimates)]
    x <- unnest(x, all_of(estimates))

    # convert to incidence
    # TODO - This is bad
    # incidence2 should expose a minimal new_incidence() function
    x <- structure(
        x,
        date_index = date_var,
        count_variable = count_variable,
        count_value = counts,
        groups = group_vars,
        class = c("incidence2", "data.frame")
    )

    # plot
    graph <- plot(x, ...)

    # ribbon colour
    col_model <- "#BBB67E"

    # add estimate (prediction) to plot
    graph <- graph +
        ggplot2::geom_line(
            mapping = ggplot2::aes(x = !!rlang::sym(date_var), y = .data$estimate)
        )

    # add confidence intervals to plot
    if (ci) {
        graph <- graph +
            ggplot2::geom_ribbon(ggplot2::aes(x = !!rlang::sym(date_var),
                                              ymin = .data$lower_ci,
                                              ymax = .data$upper_ci),
                                 alpha = 0.5,
                                 fill = col_model)
    }

    # add prediction intervals to plot
    if (pi) {
        graph <- graph +
            ggplot2::geom_ribbon(ggplot2::aes(x = !!rlang::sym(date_var),
                                              ymin = .data$lower_pi,
                                              ymax = .data$upper_pi),
                                 alpha = 0.3,
                                 fill = col_model)
    }

    graph
}
