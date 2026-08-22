#' Gather Past Results for Given Assignment Period
#' @name compute_prior
#' @description Summarizes results of prior periods to update assignment probabilities in the current period.
#' This function calculates the number of successes under each treatment and the total number of observations
#' assigned to each treatment which are used to calculate UCB1 values or Thompson sampling probabilities.
#' These values are weighted by the discount_rate provided.
#'
#' @inheritParams run_mab
#' @inheritParams mab_from_rct
#' @param current_period The current period number.
#' @param current_data A `data.frame` or `data.table` with only observations from the current sampling period.
#' @param prior_data A `data.frame` or `data.table` with only the observations from the prior index.
#' @returns A list containing 2 named vectors: the weighted number of successes, and the weighted number of assignments, where the names of each vector
#' correspond to the treatment condition.
#'
#' @details
#' When `delayed_feedback = TRUE`, the maximum value from the specified
#' `assignment_date_col` in the current data is taken as the last possible date
#' the researchers conducting the experiment could have learned about a treatment outcome.
#' All successes that occur past this date are masked and are treated as failures for the purposes
#' this period's assignments.
#'
#' @family assign
#' @keywords internal
compute_prior <- function(
  current_data,
  prior_data,
  delayed_feedback,
  assignment_date_col = NULL,
  discount_rate,
  conditions,
  current_period
) {
  # Faster execution using vectors and `tapply()` when data is small
  if (nrow(prior_data) < 30000) {
    compute_prior.fast(
      current_data = current_data,
      prior_data = prior_data,
      delayed_feedback = delayed_feedback,
      assignment_date_col = assignment_date_col,
      discount_rate = discount_rate,
      conditions = conditions,
      current_period = current_period
    )
  } else {
    UseMethod("compute_prior", current_data)
  }
}
#' @rdname compute_prior
#' @method compute_prior fast
#' @export
compute_prior.fast <- function(
  current_data,
  prior_data,
  delayed_feedback,
  assignment_date_col = NULL,
  discount_rate,
  conditions,
  current_period
) {
  if (delayed_feedback) {
    current_date <- max(current_data[[assignment_date_col]])
    known_success <- as.integer(
      current_date >= prior_data[["new_success_date"]] &
        !is.na(prior_data[["new_success_date"]]) &
        prior_data[["mab_success"]] == 1
    )
  } else {
    known_success <- prior_data[["mab_success"]]
  }
  weight <- discount_rate^(current_period -
    prior_data[["period_number"]])

  successes <- tapply(
    X = (known_success * weight),
    INDEX = prior_data[["mab_condition"]],
    FUN = sum
  )
  n <- tapply(
    X = weight,
    INDEX = prior_data[["mab_condition"]],
    FUN = sum
  )
  if (!identical(names(successes), names(n))) {
    n <- n[names(successes)]
  }

  prior_list <- list(
    mab_condition = names(successes),
    successes = successes,
    n = n
  ) |>
    finalize_prior_list(conditions = conditions)
  return(prior_list)
}

#----------------------------------------------------------------------------------
#' @method compute_prior data.frame
#' @rdname compute_prior
#' @export

compute_prior.data.frame <- function(
  current_data,
  prior_data,
  delayed_feedback,
  assignment_date_col = NULL,
  discount_rate,
  conditions,
  current_period
) {
  if (delayed_feedback) {
    current_date <- max(current_data[[assignment_date_col]])

    prior_data[["known_success"]] <- as.integer(
      current_date >= prior_data[["new_success_date"]] &
        !is.na(prior_data[["new_success_date"]]) &
        prior_data[["mab_success"]] == 1
    )
  } else {
    prior_data[["known_success"]] <- prior_data[["mab_success"]]
  }
  prior_data[["weight"]] <- discount_rate^(current_period -
    prior_data[["period_number"]])

  prior_list <- prior_data |>
    dplyr::group_by(mab_condition) |>
    dplyr::summarize(
      successes = sum(known_success * weight, na.rm = TRUE),
      n = sum(weight, na.rm = TRUE),
      .groups = "drop"
    ) |>
    as.list() |>
    finalize_prior_list(conditions = conditions)

  return(prior_list)
}
#------------------------------------------------------------------------------

#' @method compute_prior data.table
#' @rdname compute_prior
#' @export

compute_prior.data.table <- function(
  current_data,
  prior_data,
  delayed_feedback,
  assignment_date_col = NULL,
  discount_rate,
  conditions,
  current_period
) {
  if (delayed_feedback) {
    current_date <- max(current_data[[assignment_date_col]])

    prior_data[,
      known_success := as.integer(
        current_date >= new_success_date &
          !is.na(new_success_date) &
          mab_success == 1
      )
    ]
  } else {
    prior_data[, known_success := mab_success]
  }

  prior_data[, weight := discount_rate^(current_period - period_number)]
  prior_list <- prior_data[,
    .(
      successes = sum(known_success * weight, na.rm = TRUE),
      n = sum(weight, na.rm = TRUE)
    ),
    by = mab_condition
  ] |>
    as.list() |>
    finalize_prior_list(conditions = conditions)

  return(prior_list)
}
#---------------------------------------------------------------------------

#' @describeIn compute_prior Accepts the raw list output of an aggregation over `prior_data`
#' (from [compute_prior()]), names each vector by condition, fills any
#' conditions absent from the prior window with zeros, and sorts alphabetically.
#' @param prior_list Named list with elements `mab_condition`, `successes`, `n`,
#' produced by converting a summarized data.frame/data.table via [as.list()].
#' @param conditions Character vector of all treatment conditions in the trial.
#' @returns A named list with elements `successes`, `n`,
#' each a named numeric vector of length `length(conditions)`.
#' @keywords internal
finalize_prior_list <- function(prior_list, conditions) {
  nms <- prior_list[["mab_condition"]]
  prior_list[["mab_condition"]] <- NULL

  missing <- if (length(nms) != length(conditions)) {
    setdiff(conditions, nms)
  } else {
    NULL
  }

  ord <- order(c(nms, missing))

  prior_list <- lapply(
    prior_list,
    \(x) {
      names(x) <- nms
      if (!is.null(missing)) {
        x[missing] <- 0
      }
      x <- x[ord]
      return(x)
    }
  )

  return(prior_list)
}

#-------------------------------------------------------------------------------
#' Calculate Multi-Arm Bandit Decision Based on Algorithm
#' @description Calculates the best treatment for a given period using either a UCB1 or Thompson Sampling algorithm.
#' Thompson sampling is done using [bandit::best_binomial_bandit()] from
#' the \href{https://cran.r-project.org/package=bandit}{bandit}
#' package and UCB1 values are calculated using the well-defined formula that can be found
#' in \href{https://link.springer.com/article/10.1023/A:1013689704352}{Auer et al. (2002)}.
#'
#' @name compute_bandit
#' @inheritParams mab_loop
#' @inheritParams mab_from_rct
#' @param past_results A `tibble`/`data.table` containing summary of prior periods, with
#' successes, number of observations, and success rates, which is created by [compute_prior()].
#' @param current_period Numeric value of length 1; current period of the adaptive trial simulation.
#'
#' @returns A list of length 2 containing:
#' \itemize{
#' \item `bandit`: Bandit object, either a named numeric vector of Thompson sampling probabilities
#' or UCB1 values.
#' \item `assignment_probabilities`: Named numeric vector with probabilities of being assigned to the given treatment, where `names(.)` are the treatments.
#' Adjusted for control augmentation and random assignment proportion weighting}
#'
#' @details
#'
#' Control augmentation adjustment is always done last, to ensure proper probability floor.
#'
#' The Thompson `assignment_probabilities` are the same as the `bandit` vector except when
#' `control_augment` or `random_assign_prop` are greater than 0, as these arguments will alter the probabilities
#' of assignment.
#'
#' Thompson sampling is calculated using the
#' \href{https://cran.r-project.org/package=bandit}{bandit}
#' package but the direct calculation can result in errors or overflow. If this occurs, a simulation based method
#' from the same package is used instead to estimate the posterior distribution.
#' If this occurs a warning will be presented. `ndraws` specifies the number of iterations for the
#' simulation based method, and the default value is 5000.
#'
#' The UCB1 algorithm only selects 1 treatment at each period, with no probability matching
#' so `assignment_probabilities` will always have 1 element equal to 1, and the rest equal to 0, unless
#' `control_augment` or `random_assign_prop` are greater than 0, which will alter the probabilities of assignment.
#' For example, if the original vector is `(0, 0, 1)`, and `control_augment` = 0.2,
#' the new vector is `(0.2, 0, 0.8)` assuming the first element is control. If instead the 3rd element
#' were the control group the resulting vector would not be changed because it already meets the
#' control group threshold. Under ties, the assignment probability is split evenly among tied arms,
#' so `(0, 1, 1)` would become `(0, 0.5, 0.5)`.
#'
#'
#' @references
#'
#' Auer, Peter, Nicolò Cesa-Bianchi, and Paul Fischer. 2002.
#' "Finite-Time Analysis of the Multiarmed Bandit Problem." \emph{Machine Learning}
#' 47 (2): 235–56. \doi{10.1023/A:1013689704352}.
#'
#' Kuleshov, Volodymyr, and Doina Precup. 2014. "Algorithms for Multi-Armed Bandit Problems."
#' \emph{arXiv}. \doi{10.48550/arXiv.1402.6028}.
#'
#' Loecher, Thomas Lotze and Markus. 2022.
#' "Bandit: Functions for Simple a/B Split Test and Multi-Armed Bandit Analysis."
#' \url{https://cran.r-project.org/package=bandit}.
#'
#' Thompson, William R. 1933. "On the Likelihood That One Unknown Probability Exceeds Another in View of the Evidence of Two Samples."
#'  Biometrika 25 (3/4): 285–94. \doi{10.2307/2332286}
#' @keywords internal
#' @family assign

compute_bandit <- function(
  past_results = NULL,
  algorithm,
  num_conditions,
  conditions,
  current_period,
  control_augment = 0,
  random_assign_prop = 0,
  ndraws
) {
  bandit <- switch(
    algorithm,
    "thompson" = compute_bandit.thompson(
      past_results = past_results,
      conditions = conditions,
      current_period = current_period,
      ndraws = ndraws
    ),
    "ucb1" = compute_bandit.ucb1(
      past_results = past_results,
      conditions = conditions
    ),
    list(
      # Default for 'static' assignment
      bandit = rep(NA, num_conditions),
      assignment_prob = rep(1 / num_conditions, num_conditions)
    )
  )

  bandit[["assignment_prob"]] <- (1 - random_assign_prop) *
    bandit[["assignment_prob"]] +
    random_assign_prop * (1 / num_conditions)

  if (control_augment > 0) {
    ctrl <- names(conditions) == "control"

    if (bandit[["assignment_prob"]][ctrl] < control_augment) {
      bandit[["assignment_prob"]][ctrl] <- control_augment

      bandit[["assignment_prob"]][!ctrl] <-
        (bandit[["assignment_prob"]][!ctrl] /
          sum(bandit[["assignment_prob"]][!ctrl])) *
        (1 - control_augment)
    }
  }

  # Renormalization for Summing to 1
  bandit[["assignment_prob"]] <- bandit[["assignment_prob"]] /
    sum(bandit[["assignment_prob"]])

  return(bandit)
}
#-------------------------------------------------------------------
#' @rdname compute_bandit

compute_bandit.thompson <- function(
  past_results,
  conditions,
  current_period,
  ndraws
) {
  bandit <- tryCatch(
    {
      ts <- bandit::best_binomial_bandit(
        x = past_results[["successes"]],
        n = past_results[["n"]],
        alpha = 1,
        beta = 1
      ) |>
        as.vector()
      if (bandit_invalid(ts)) {
        stop("Invalid Bandit")
      }
      ts
    },
    error = function(e) {
      rlang::warn(c(
        "Thompson sampling calculation overflowed; simulation based posterior estimate was used instead",
        "i" = sprintf("Period: %d", current_period)
      ))
      ts <- bandit::best_binomial_bandit_sim(
        x = past_results[["successes"]],
        n = past_results[["n"]],
        alpha = 1,
        beta = 1,
        ndraws = ndraws
      ) |>
        as.vector()
      ts
    }
  )
  names(bandit) <- names(past_results[["successes"]])

  if (bandit_invalid(bandit)) {
    rlang::abort(c(
      "Thompson sampling simulation failed",
      "x" = sprintf(
        "Most Recent Result: %s",
        paste0(bandit, collapse = ",")
      ),
      "i" = "Consider setting `ndraws` higher or reducing `prior_periods`."
    ))
  }

  return(list(bandit = bandit, assignment_prob = bandit))
}
#' @describeIn compute_bandit Checks if the Thompson Sampling probabilities either sum arbitrarily close
#' to 0 or if any of them are NA, indicating the direct calculation failed or did not converge.
#' @param bandit a numeric vector of Thompson Sampling probabilities.
#' @returns Logical; TRUE if the vector is invalid, FALSE if valid
#' @keywords internal
bandit_invalid <- function(bandit) {
  return(any(is.na(bandit)) || isTRUE(all.equal(sum(bandit), 0)))
}
#-------------------------------------------------------------------
#' @rdname compute_bandit

compute_bandit.ucb1 <- function(
  past_results,
  conditions,
  current_period = NULL
) {
  all_pulls <- sum(past_results[["n"]], na.rm = TRUE)
  success_rates <- past_results[["successes"]] / past_results[["n"]]

  ucb1 <- success_rates +
    sqrt((2 * log(all_pulls)) / past_results[["n"]])

  ucb1[past_results[["n"]] == 0] <- Inf

  best <- names(ucb1)[ucb1 == max(ucb1)]
  assignment_probs <- stats::setNames(
    rep(0, length(ucb1)),
    names(ucb1)
  )
  assignment_probs[best] <- 1

  return(list(
    bandit = ucb1,
    assignment_prob = assignment_probs
  ))
}
