#' Safely set and restore a future::plan()
#'
#' Internal helper that sets up a \code{future} plan with a safe default
#' (\code{"multisession"}) and returns a reset function to restore the
#' previous plan. Supports sequential, multisession, multicore, cluster,
#' or auto selection.
#'
#' @param plan Character scalar, one of \code{c("auto","multisession","multicore",
#'   "sequential","cluster","none")}. "auto" chooses a cross-platform safe default
#'   (multisession). "none" does not change the current plan.
#' @param workers Integer number of workers. If \code{NULL}, defaults to
#'   \code{parallel::detectCores() - 1}.
#' @param cluster Optional cluster object or host specification, used only if
#'   \code{plan = "cluster"}.
#'
#' @return A function with no arguments. Calling it restores the previous plan.
#'   The function is usually invoked via \code{on.exit()}.
#'
#' @keywords internal
#' @noRd
.set_future_plan <- function(plan = c("auto","multisession","multicore","sequential","cluster","none"),
                             workers = NULL,
                             cluster = NULL) {
  plan <- match.arg(plan)
  old <- future::plan()  # capture previous plan

  # Helper to compute safe default number of workers
  pick_workers <- function(w) {
    if (is.null(w) || !is.finite(w)) {
      pmax(1L, as.integer(parallel::detectCores(logical = TRUE) - 1L))
    } else {
      pmax(1L, as.integer(w))
    }
  }

  if (plan == "none") {
    # Do not touch plan; keep sequential behavior
    return(function() try(future::plan(old), silent = TRUE))
  }

  # Choose a safe default when plan == "auto"
  if (plan == "auto") {
    # 'multisession' is the safest cross-platform default (works on Windows, macOS, Linux, RStudio).
    plan <- "multisession"
  }

  # Apply the chosen plan
  if (plan == "multisession") {
    future::plan(future::multisession, workers = pick_workers(workers))
  } else if (plan == "multicore") {
    # Note: multicore (forking) is not available on Windows and can misbehave in RStudio.
    # Use only if user explicitly asks for it.
    future::plan(future::multicore, workers = pick_workers(workers))
  } else if (plan == "sequential") {
    future::plan(future::sequential)
  } else if (plan == "cluster") {
    if (is.null(cluster)) {
      stop("For plan = 'cluster', please provide a 'cluster' object or hosts.", call. = FALSE)
    }
    future::plan(cluster, workers = cluster)
  } else {
    stop("Unknown plan: ", plan, call. = FALSE)
  }

  # Return reset function
  function() try(future::plan(old), silent = TRUE)
}
