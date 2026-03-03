#' Set API rate limit configuration
#'
#' @param api Registry id.
#' @param max_per_hour Maximum number of calls per hour.
#'
#' @details
#' Use this to override or enforce a per-hour rate limit for a given API.
#' The default is inferred from the registry entry when available.
#'
#' @seealso
#' [bunddev_rate_limit_get()] to inspect the current setting.
#'
#' @examples
#' bunddev_rate_limit_set("smard", max_per_hour = 60)
#'
#' @return The stored rate limit configuration.
#' @export
bunddev_rate_limit_set <- function(api, max_per_hour) {
  if (missing(max_per_hour) || is.null(max_per_hour) || max_per_hour <= 0) {
    cli::cli_abort("max_per_hour must be a positive number.")
  }

  limits <- getOption("bunddev.rate_limit", list())
  limits[[api]] <- list(
    max_per_hour = as.integer(max_per_hour),
    timestamps = numeric()
  )
  options(bunddev.rate_limit = limits)
  limits[[api]]
}

#' Get API rate limit configuration
#'
#' @param api Registry id.
#'
#' @details
#' If no explicit limit was set, the function tries to infer one from the
#' registry entry. The result is used by adapter helpers when `safe = TRUE`.
#'
#' @seealso
#' [bunddev_rate_limit_set()] to override the default.
#'
#' @examples
#' bunddev_rate_limit_get("smard")
#'
#' @return The rate limit configuration.
#' @export
bunddev_rate_limit_get <- function(api) {
  limits <- getOption("bunddev.rate_limit", list())
  entry <- limits[[api]]

  if (is.null(entry)) {
    registry <- tryCatch(bunddev_info(api), error = function(e) NULL)
    rate_limit <- NULL
    if (!is.null(registry) && "rate_limit" %in% names(registry)) {
      rate_limit <- registry$rate_limit[[1]]
    }

    if (!is.null(rate_limit) && !is.na(rate_limit)) {
      limit_value <- stringr::str_extract(rate_limit, "\\d+")
      if (!is.na(limit_value)) {
        entry <- list(max_per_hour = as.integer(limit_value), timestamps = numeric())
        limits[[api]] <- entry
        options(bunddev.rate_limit = limits)
        return(entry)
      }
    }

    return(list(max_per_hour = NA_integer_, timestamps = numeric()))
  }

  entry
}

bunddev_rate_limit_wait <- function(api) {
  entry <- bunddev_rate_limit_get(api)
  if (is.na(entry$max_per_hour)) {
    return(invisible(NULL))
  }

  now <- as.numeric(Sys.time())
  window <- 3600
  timestamps <- entry$timestamps
  if (length(timestamps) > 0) {
    timestamps <- timestamps[timestamps > (now - window)]
  }

  if (length(timestamps) >= entry$max_per_hour) {
    wait_for <- window - (now - min(timestamps))
    if (wait_for > 0) {
      Sys.sleep(wait_for)
      now <- as.numeric(Sys.time())
    }
  }

  timestamps <- c(timestamps, now)
  limits <- getOption("bunddev.rate_limit", list())
  limits[[api]] <- list(max_per_hour = entry$max_per_hour, timestamps = timestamps)
  options(bunddev.rate_limit = limits)
  invisible(NULL)
}
