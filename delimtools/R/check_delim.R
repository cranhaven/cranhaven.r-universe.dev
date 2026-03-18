#' Checks If Two or More Species Delimitation Outputs are (Nearly) Equal
#'
#' @description
#' `check_delim()` checks if two or more species delimitation outputs have
#' differences in its dimensions, labels, and values.
#'
#' @param list a [list][base::list] containing two or more species delimitation outputs to check.
#'
#' @details
#' `check_delim()` will check if two or more species delimitation outputs have
#' different dimensions (rows, columns), if labels are the same or if there are
#' any duplicated or absent labels, and if there are any NA values or if partitions
#' were set using non numeric values. If `TRUE` for any of the cases listed above,
#' `check_delim()` will return an error.
#'
#' @return
#' A single logical value, `TRUE` or `FALSE`.
#'
#' @author
#' Pedro S. Bittencourt, Rupert A. Collins.
#'
#' @examples
#'
#' # create dummy delimitation outputs
#' delim_1 <- tibble::tibble(
#'   labels = paste0("seq", 1:10),
#'   method_A = c(rep(1, 5), rep(2, 5))
#' )
#'
#' delim_2 <- tibble::tibble(
#'   labels = paste0("seq", 1:10),
#'   method_B = c(rep(1, 3), rep(2, 2), rep(3, 5))
#' )
#'
#' delim_3 <- tibble::tibble(
#'   labels = paste0("seq", 1:10),
#'   method_C = c(rep(1, 3), rep(2, 2), rep(3, 3), rep(4, 2))
#' )
#'
#' # check outputs
#' check_delim(list(delim_1, delim_2, delim_3))
#'
#' @export
check_delim <- function(list) {
  if (!is.list(list)) {
    cli::cli_abort(c("Please provide a {.cls list} class object."))
  }

  if (length(list) < 2) {
    cli::cli_abort(c("Please provide two or more species delimitation outputs to compare."))
  }


  check_delim.default <- function(delim_1, delim_2) {
    # check dimensions
    check_dim <- dim(delim_1) == dim(delim_2)

    if (!isTRUE(all(check_dim))) {
      cli::cli_abort(c("Dimensions are not the same across tables.",
        "x" = "You've supplied inputs with different dimensions.",
        "i" = "{.arg delim 1} has {dim(delim_1)[1]} rows and {dim(delim_1)[2]} columns",
        "i" = "{.arg delim 2} has {dim(delim_2)[1]} rows and {dim(delim_2)[2]} columns"
      ))
      return(FALSE)
    }

    # get labels
    id1 <- dplyr::pull(delim_1, 1)
    id2 <- dplyr::pull(delim_2, 1)

    # check labels
    check_labels <- identical(id1, id2)

    if (!isTRUE(check_labels)) {
      diff <- dplyr::symdiff(id1, id2)

      if (rlang::is_empty(diff)) {
        cli::cli_alert_success(c("Labels are the same across tables but they likely are unordered."))
      } else {
        diff <- vctrs::vec_chop(diff, sizes = c(length(diff) / 2, length(diff) / 2))
        names(diff) <- c(
          "Labels absent or mistyped in {.arg delim_1}",
          "Labels absent or mistyped in {.arg delim_2}"
        )

        cli::cli_abort(c("Labels must be identical across tables.",
          "x" = "The labels below are either absent or mistyped.",
          "i" = "labels absent or mistyped in {.arg delim_1}:",
          stringr::str_flatten_comma(diff[[1]]),
          "i" = "labels absent or mistyped in {.arg delim_2}:",
          stringr::str_flatten_comma(diff[[2]])
        ))
        invisible(diff)
        return(FALSE)
      }

      if (any(duplicated(id1) | duplicated(id2))) {
        cli::cli_abort(c("Duplicate labels found.",
          "x" = "You've supplied inputs with duplicated labels.",
          "i" = "Duplicated labels in {.arg delim_1}:",
          stringr::str_flatten(id1[vctrs::vec_duplicate_detect(id1)]),
          "i" = "Duplicated labels in {.arg delim_2}:",
          stringr::str_flatten(id2[vctrs::vec_duplicate_detect(id2)])
        ))
        return(FALSE)
      }
    }

    # get values
    values1 <- dplyr::pull(delim_1, 2)
    values2 <- dplyr::pull(delim_2, 2)

    # check values
    if (anyNA(c(values1, values2))) {
      cli::cli_abort(c("Missing values found across tables.",
        "x" = "You've supplied inputs with missing values.",
        "i" = "{.arg Delim 1} has {sum(vctrs::vec_detect_missing(values1))} missing values",
        "i" = "{.arg Delim 2} has {sum(vctrs::vec_detect_missing(values2))} missing values"
      ))
      return(FALSE)
    }

    if (!is.numeric(values1) | !is.numeric(values2)) {
      cli::cli_abort(c("Species partition values must be numeric.",
        "x" = "You've supplied non numeric values for species partitions.",
        "i" = "{.arg Delim 1} is {.cls {class(values1)}}",
        "i" = "{.arg Delim 2} is {.cls {class(values2)}}"
      ))
      return(FALSE)
    }
    return(TRUE)
  }

  cli::cli_inform("Checking species delimitation tables...")
  Sys.sleep(2)

  for (i in seq(2, length(list))) {
    cli::cli_progress_message("Checking table 1 against table {i}...")
    Sys.sleep(0.5)
    check_delim.default(list[[1]], list[[i]])
    cli::cli_progress_update()
  }
  cli::cli_alert_success("Checking complete!")

  return(TRUE)
}
