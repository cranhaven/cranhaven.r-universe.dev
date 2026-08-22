#' Argument Check Helper Functions
#' @name check_helpers
#' @family checks
#' @param ... Arguments to check
#' @description
#' This set of functions is common across the main argument checkers, and they each
#' check a clear condition on a set of arguments, such as ensuring the proper data type.
NULL


#------------------------------------------------------------------------------
#' @describeIn check_helpers Checks for valid logical arguments
#' @returns Throws an error if any input is not TRUE or FALSE
#' @keywords internal
check_logical <- function(...) {
  args <- rlang::dots_list(..., .named = TRUE)
  purrr::iwalk(
    args,
    ~ {
      if (!is.logical(.x) || length(.x) != 1 || is.na(.x)) {
        rlang::abort(
          c(
            sprintf("`%s` must be a logical (TRUE or FALSE)", .y),
            "x" = paste0("You passed: ", deparse(.x))
          )
        )
      }
    }
  )
}
#--------------------------------------------------------------------------------
#' @describeIn check_helpers This function accepts the user's
#' settings for proportion arguments and checks if they are valid proportions between 0 and 1.
#' @returns Throws an error if any input is not a valid proportion between 0 and 1.
#' @keywords internal
check_prop <- function(...) {
  args <- rlang::dots_list(..., .named = TRUE)
  purrr::iwalk(
    args,
    ~ {
      if (is.null(.x) || !is.numeric(.x) || .x < 0 || .x > 1) {
        rlang::abort(c(
          sprintf("`%s` must be a non-null double between 0 and 1.", .y),
          "x" = paste0("You passed: ", deparse(.x))
        ))
      }
    }
  )
}
#-------------------------------------------------------------------------------
#' @describeIn check_helpers This function accepts the user's
#' settings for positive integer arguments and checks if they are valid positive integers.
#' @returns Throws an error if any input is not a valid positive integer.
#' @keywords internal
check_posint <- function(...) {
  args <- rlang::dots_list(..., .named = TRUE)
  bad <- !vapply(args, \(arg) posint(arg) || is.null(arg), logical(1))
  purrr::walk2(names(args)[bad], args[bad], function(name, val) {
    rlang::abort(c(
      sprintf(
        "`%s` must be a positive integer or vector
      of positive integers",
        name
      ),
      "x" = paste0(
        "You passed: ",
        deparse(val)
      )
    ))
  })
}
#'
posint <- function(x) {
  if (is.numeric(x) && all(!is.na(x))) {
    return(all(x > 0 & x %% 1 == 0))
  } else {
    return(FALSE)
  }
}


#-------------------------------------------------------------------------------
#' @describeIn check_helpers Checks if specified numeric vectors each sum to 1.
#' @returns Nothing; Throws an error if a numeric vector does not sum to 1.
#' @keywords internal
check_sum1 <- function(...) {
  args <- rlang::dots_list(..., .named = TRUE)
  purrr::iwalk(args, \(arg, name) {
    if (!dplyr::near(sum(arg), 1)) {
      rlang::abort(c(
        sprintf("`%s` must sum to 1", name),
        "x" = paste0("You passed: ", paste0(arg, collapse = ",")),
        "x" = paste0("Sum: ", sum(arg))
      ))
    }
  })
}

#' @describeIn check_helpers Checks specific string arguments against provided valid arguments.
#' @param arg Argument to check.
#' @param valid vector of valid arguments.
#' @param name name of the argument.
#' @returns Nothing; Throws an error if the string argument is invalid.
check_string <- function(arg, valid, name) {
  if (!arg %in% valid) {
    rlang::abort(
      c(
        sprintf("Invalid `%s`", name),
        "i" = sprintf(
          "Valid Options: %s",
          paste0(valid, collapse = ", ")
        ),
        "x" = sprintf("You Provided: '%s'", arg)
      )
    )
  }
}
#' @describeIn check_helpers Checks if provided objects have `names` attribute.
#' @returns Nothing; Throws an if an argument does not have `names` attribute.
check_names <- function(...) {
  args <- rlang::dots_list(..., .named = TRUE)
  purrr::iwalk(args, \(arg, name) {
    if (is.null(names(arg))) {
      rlang::abort(c(sprintf("%s must have the `names` attribute", name)))
    }
  })
}

#' Checking Clusters Do Not Persist Across Periods
#' @name check_clusters
#' @inheritParams prep_rct_data
#' @inheritParams run_mab
#' @inheritParams estimate_aw_aipw
#' @description
#' Checks to ensure that each cluster only exists within a single simulation period, because
#' if this is the case a true clustered design is no longer specified. See details.
#' @returns Nothing; Throws a warning if any clusters persist across multiple periods.
#' @details
#' The assignment algorithm in [mab_loop()] assumes that clusters do not persist across periods. For a
#' true clustered design, if a cluster persisted across periods, all observations within
#' it would have to be assigned to the same treatment as in the previous period. In an adaptive
#' experiment this results in no adaptation, thus this is not implemented
#' into the algorithm. Instead, the assumption is verified here.
#' @keywords internal
check_clusters <- function(
  data,
  cluster_col
) {
  UseMethod("check_clusters", data)
}

#' @method check_clusters data.frame
#' @rdname check_clusters
#' @export
check_clusters.data.frame <- function(data, cluster_col) {
  check_clusters <- data |>
    dplyr::group_by(.data[[cluster_col]]) |>
    dplyr::summarize(n_periods = dplyr::n_distinct(period_number)) |>
    dplyr::filter(n_periods > 1)

  cluster_throw(check_clusters, cluster_col)
}

#' @method check_clusters data.table
#' @rdname check_clusters
#' @export
check_clusters.data.table <- function(data, cluster_col) {
  check_clusters <- data[,
    .(n_periods = data.table::uniqueN(period_number)),
    by = cluster_col
  ][n_periods > 1]

  cluster_throw(check_clusters, cluster_col)
}

#' @describeIn check_clusters Internal Helper which handles all data wrangling agnostic
#' portions of [check_clusters()]
#' @inheritParams estimate_aw_aipw
#' @param check_clusters Object created in [check_clusters()] with all of the clusters
#' which persist across periods.
#' @returns Nothing; Throws a warning if clusters persist across periods.
#' @keywords internal
cluster_throw <- function(check_clusters, cluster_col) {
  if (nrow(check_clusters) > 0) {
    rlang::warn(
      c(
        "Clusters must only appear in a single period.",
        "x" = paste(
          "These clusters persist across multiple periods:",
          paste(check_clusters[[cluster_col]], collapse = ", ")
        )
      )
    )
  }
}
