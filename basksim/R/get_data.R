#' Simulate Data Based on a Binomial Distribution
#'
#' @template k
#' @template n
#' @param p Probabilities used to simulate the data
#' @template iter
#' @param type Type of output. Use \code{bhmbasket} for the BHM and EXNED
#'   design and \code{matrix} for everything else.
#'
#' @details For \code{type = "bhmbasket"} this is simply a wraper for
#' \code{bhmbasket::simulateScenarios}.
#'
#' @return If \code{type = "matrix"} then a matrix is returned, if
#' \code{type = "bhmbasket"} then an element with class \code{scenario_list}.
#' @export
#'
#' @examples
#' get_data(k = 3, n = 20, p = c(0.2, 0.2, 0.5), iter = 1000)
get_data <- function(k, n, p, iter, type = c("matrix", "bhmbasket")) {
  type <- match.arg(type)
  if (length(p) == 1) {
    pvec <- rep(p, k)
  } else if (length(p) == k) {
    pvec <- p
  } else {
    stop("p1 must either have length 1 or k")
  }

  if (type == "matrix") {
    data <- matrix(stats::rbinom(n = iter * k, size = n, prob = pvec), ncol = k,
      byrow = TRUE)
    attr(data, "n") <- n
    attr(data, "p") <- p
    data
  } else {
    bhmbasket::simulateScenarios(
      n_subjects_list = rep(n, k),
      response_rates_list = list(pvec),
      n_trials = iter
    )
  }
}

check_data_matrix <- function(data, design, n, p, iter) {
  if (is.null(data)) {
    get_data(k = design$k, n = n, p = p, iter = iter, type = "matrix")
  } else {
    if (!inherits(data, "matrix")) {
      stop("data is not a matrix")
    }
    if (ncol(data) != design$k) {
      stop("data doesn't have k columns")
    }
    if (attr(data, "n") != n) {
      stop("data wasn't generated with the specified n")
    }
    if (!all(attr(data, "p") == p)) {
      stop("data wasn't generated with the specified p1")
    }
    if (nrow(data) != iter) {
      message("data does not have iter rows - argument iter is ignored")
    }
    data
  }
}

check_data_bhmbasket <- function(data, design, n, p, iter) {
  if (is.null(data)) {
    get_data(k = design$k, n = n, p = p, iter = iter, type = "bhmbasket")
  } else {
    if (!inherits(data, "scenario_list")) {
      stop("data is not of class scenario_list")
    }
    if (ncol(data$scenario_1$n_responders) != design$k) {
      stop("data doesn't have k columns")
    }
    if (!all(data$scenario_1$n_subjects == n)) {
      stop("data wasn't generated with the specified n")
    }
    if (!all(data$scenario_1$response_rates == p) & !is.null(p)) {
      stop("data wasn't generated with the specified p1")
    }
    if (nrow(data$scenario_1$n_responders) != iter) {
      message("data does not have iter rows - argument iter is ignored")
    }
    data
  }
}

check_data_list <- function(data, scenarios) {
  if (!is.null(data)) {
    if (!is.list(data)) {
      stop("data is not a list")
    }
    if (length(data) != ncol(scenarios)) {
      stop("data does not have an element for each scenario")
    }
  }
}

