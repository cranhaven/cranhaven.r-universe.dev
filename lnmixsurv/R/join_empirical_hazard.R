#' Function used to join the empirical hazard to the data
#'
#' `join_empirical_hazard()` takes a Kaplan Meier empirical estimate which includes the survival estimates and joins the hazard estimates to it.
#'
#' @param km Kaplan-Meier estimates, i.e., object generated after running broom::tidy(survfit_obj), in which survfit_obj is a survfit object. Can also be a survfit object.
#'
#' @returns The same object as inputed, but with the hazard estimates column (hazard_estimate) joined to it.
#' 
#' @export
join_empirical_hazard <- function(km) {
  # check if the only class is a survfit object
  if (length(class(km)) == 1) {
    if (inherits(km, "survfit")) {
      km <- broom::tidy(km)
    }
  }

  output <- NULL

  if ("strata" %in% colnames(km)) { # we should separate by strata if it has the column strata
    for (i in unique(km$strata)) {
      output <- dplyr::bind_rows(
        output,
        empirical_hazard_function(km$time[km$strata == i],
          km$estimate[km$strata == i],
          strata = i
        )
      )
    }

    output <- dplyr::left_join(km, output, by = c("strata", "time"))
  } else {
    output <- empirical_hazard_function(km$time, km$estimate)
    output <- dplyr::left_join(km, output, by = "time")
  }

  return(output)
}

empirical_hazard_function <- function(time, empirical_survival, strata = NULL) {
  # check length time and empirical_survival
  if (length(time) != length(empirical_survival)) {
    stop("The length of time and empirical_survival should be the same.")
  }

  # length time should be greater or equal to 2
  if (length(time) < 2) {
    stop("The length of time should be greater or equal to 2.")
  }

  # if not NULL, strata should be character and have length 1
  if (!is.null(strata)) {
    if (!is.character(strata) | length(strata) != 1) {
      stop("The strata should be a character string with length 1.")
    }
  }

  # output object
  output <- NULL
  if (is.null(strata)) {
    for (i in 1:(length(time) - 1)) {
      if (empirical_survival[i] == 0) {
        next
      }

      haz <- (empirical_survival[i] - empirical_survival[i + 1]) / ((time[i + 1] - time[i]) * empirical_survival[i])

      output <- dplyr::bind_rows(
        output,
        tibble::tibble(
          time = time[i],
          hazard_estimate = haz
        )
      )
    }
  } else {
    for (i in 1:(length(time) - 1)) {
      if (empirical_survival[i] == 0) {
        next
      }

      haz <- (empirical_survival[i] - empirical_survival[i + 1]) / ((time[i + 1] - time[i]) * empirical_survival[i])

      output <- dplyr::bind_rows(
        output,
        tibble::tibble(
          time = time[i],
          hazard_estimate = haz,
          strata = strata
        )
      )
    }
  }

  return(output)
}
