# A minimal, dependency-free re-implementation of the core `promises`
# package API (`promise()`, `then()`, `catch()`, `finally()`, `is.promise()`),
# built entirely on later2's own event loop.
#
# The `promises` package schedules its internal fulfill/reject callbacks via
# `later::later()`, which runs on the `later` package's own C event loop.
# When code is driven by later2 (e.g. via `later2::run_now()`), those
# callbacks are registered on a completely different event loop and never
# fire. This file provides a self-contained implementation that calls
# later2's own `later()`, so promise resolution is properly synchronized
# with `run_now()`/`loop_empty()`.
#
# Unlike `promises`, this implementation has no dependencies (no R6,
# fastmap, rlang, magrittr, lifecycle) and does not support promise domains
# or `future`/`mirai` integration.

Promise <- function() {
  self <- new.env(parent = emptyenv())
  private <- new.env(parent = emptyenv())

  private$state <- "pending"
  private$value <- NULL
  private$visible <- TRUE
  private$publicResolveRejectCalled <- FALSE
  private$onFulfilled <- list()
  private$onRejected <- list()
  private$rejectionHandled <- FALSE

  private$doResolve <- function(value) {
    val <- withVisible(value)
    value <- val$value
    visible <- val$visible

    if (is.promise(value)) {
      impl <- attr(value, "promise_impl", exact = TRUE)
      if (identical(self, impl)) {
        return(private$doReject(simpleError(
          "Chaining cycle detected for promise"
        )))
      }
      value$then(private$doResolve, private$doReject)
    } else {
      private$doResolveFinalValue(value, visible)
    }
  }

  private$doReject <- function(reason) {
    if (is.promise(reason)) {
      reason$then(private$doResolve, private$doReject)
    } else {
      private$doRejectFinalReason(reason)
    }
  }

  # These "final" versions of resolve/reject are for when we've established
  # that the value/reason is not itself a promise.
  private$doResolveFinalValue <- function(value, visible) {
    private$value <- value
    private$visible <- visible
    private$state <- "fulfilled"

    later(function() {
      lapply(private$onFulfilled, function(f) {
        f(private$value, private$visible)
      })
      private$onFulfilled <- list()
    })
  }

  private$doRejectFinalReason <- function(reason) {
    private$value <- reason
    private$state <- "rejected"

    later(function() {
      lapply(private$onRejected, function(f) {
        private$rejectionHandled <- TRUE
        f(private$value)
      })
      private$onRejected <- list()

      later(function() {
        if (!private$rejectionHandled) {
          cat(
            file = stderr(),
            "Unhandled promise error: ",
            conditionMessage(reason),
            "\n",
            sep = ""
          )
        }
      })
    })
  }

  self$status <- function() {
    private$state
  }

  self$resolve <- function(value) {
    # Only allow this to be called once, then no-op.
    if (private$publicResolveRejectCalled) {
      return(invisible())
    }
    private$publicResolveRejectCalled <- TRUE

    tryCatch(
      {
        # Important: Do not trigger evaluation of value before passing to
        # doResolve. doResolve calls withVisible() on value, so evaluating
        # it before that point would cause the visibility to be lost.
        private$doResolve(value)
      },
      error = function(err) {
        private$doReject(err)
      }
    )

    invisible()
  }

  self$reject <- function(reason) {
    # Only allow this to be called once, then no-op.
    if (private$publicResolveRejectCalled) {
      return(invisible())
    }
    private$publicResolveRejectCalled <- TRUE

    tryCatch(
      {
        force(reason)
        if (is.character(reason)) {
          reason <- simpleError(reason)
        }
        private$doReject(reason)
      },
      error = function(err) {
        private$doReject(err)
      }
    )

    invisible()
  }

  self$then <- function(onFulfilled = NULL, onRejected = NULL, onFinally = NULL) {
    onFulfilled <- normalizeOnFulfilled(onFulfilled)
    onRejected <- normalizeOnRejected(onRejected)
    if (!is.function(onFinally)) {
      onFinally <- NULL
    }

    if (!is.null(onFinally)) {
      if (!is.null(onFulfilled) || !is.null(onRejected)) {
        stop(
          "A single `then` call cannot combine `onFinally` with `onFulfilled`/`onRejected`"
        )
      }
      spliced <- spliceOnFinally(onFinally)
      onFulfilled <- spliced$onFulfilled
      onRejected <- spliced$onRejected
    }

    promise2 <- promise(function(resolve, reject) {
      handleFulfill <- function(value, visible) {
        if (is.function(onFulfilled)) {
          resolve(onFulfilled(value, visible))
        } else {
          resolve(if (visible) value else invisible(value))
        }
      }

      handleReject <- function(reason) {
        if (is.function(onRejected)) {
          # Yes, resolve, not reject.
          resolve(onRejected(reason))
        } else {
          # Yes, reject, not resolve.
          reject(reason)
        }
      }

      if (private$state == "pending") {
        private$onFulfilled <- c(private$onFulfilled, list(handleFulfill))
        private$onRejected <- c(private$onRejected, list(handleReject))
      } else if (private$state == "fulfilled") {
        later(function() {
          handleFulfill(private$value, private$visible)
        })
      } else if (private$state == "rejected") {
        later(function() {
          private$rejectionHandled <- TRUE
          handleReject(private$value)
        })
      } else {
        stop("Unexpected state ", private$state)
      }
    })

    invisible(promise2)
  }

  self$catch <- function(onRejected) {
    invisible(self$then(onRejected = onRejected))
  }

  self$finally <- function(onFinally) {
    invisible(self$then(onFinally = onFinally))
  }

  self$format <- function() {
    if (private$state == "pending") {
      "<Promise [pending]>"
    } else {
      classname <- class(private$value)[[1]]
      if (length(classname) == 0) {
        classname <- ""
      }
      sprintf("<Promise [%s: %s]>", private$state, classname)
    }
  }

  class(self) <- "Promise"
  self
}

normalizeOnFulfilled <- function(onFulfilled) {
  if (!is.function(onFulfilled)) {
    if (!is.null(onFulfilled)) {
      warning("`onFulfilled` must be a function or `NULL`")
    }
    return(NULL)
  }

  args <- formals(onFulfilled)
  arg_count <- length(args)

  if (arg_count >= 2 && names(args)[[2]] == ".visible") {
    onFulfilled
  } else if (arg_count > 0) {
    function(value, .visible) {
      if (isTRUE(.visible)) {
        onFulfilled(value)
      } else {
        onFulfilled(invisible(value))
      }
    }
  } else {
    function(value, .visible) {
      onFulfilled()
    }
  }
}

normalizeOnRejected <- function(onRejected) {
  if (!is.function(onRejected)) {
    if (!is.null(onRejected)) {
      warning("`onRejected` must be a function or `NULL`")
    }
    return(NULL)
  }

  args <- formals(onRejected)
  arg_count <- length(args)

  if (arg_count >= 1) {
    onRejected
  } else {
    function(reason) {
      onRejected()
    }
  }
}

spliceOnFinally <- function(onFinally) {
  list(
    onFulfilled = finallyToFulfilled(onFinally),
    onRejected = finallyToRejected(onFinally)
  )
}

finallyToFulfilled <- function(onFinally) {
  force(onFinally)
  function(value, .visible) {
    onFinally()
    if (.visible) value else invisible(value)
  }
}

finallyToRejected <- function(onFinally) {
  force(onFinally)
  function(reason) {
    onFinally()
    stop(reason)
  }
}

#' @title Create a new promise object
#'
#' @description `promise()` creates a new promise, synchronized with later2's
#' event loop (instead of the `later` package's event loop, as the `promises`
#' package does). A promise is a placeholder object for the eventual result (or
#' error) of an asynchronous operation.
#'
#' The `action` function should be a piece of code that returns quickly, but
#' initiates a potentially long-running, asynchronous task. If/when the task
#' successfully completes, call `resolve(value)` where `value` is the result of
#' the computation. If the task fails, call `reject(reason)`, where `reason` is
#' either an error object or a character string.
#'
#' @param action A function with signature `function(resolve, reject)`.
#'
#' @return A promise object (see [then()]).
#'
#' @examples
#' p1 <- promise(function(resolve, reject) {
#'   later(function() resolve(runif(1)), delay = 2)
#' })
#' then(p1, print)
#' run_now(3)
#'
#' @export
promise <- function(action) {
  if (!is.function(action) || length(formals(action)) != 2) {
    stop("'action' must be a function with two arguments: `resolve`, `reject`")
  }

  p <- Promise()

  tryCatch(
    action(p$resolve, p$reject),
    error = function(e) {
      if (p$status() == "pending") {
        p$reject(e)
      } else {
        # Too late to do anything useful. Just notify.
        warning(e)
      }
    }
  )

  structure(
    list(
      then = p$then,
      catch = p$catch,
      finally = p$finally
    ),
    class = "promise",
    promise_impl = p
  )
}

#' @title Determine whether an object is a promise
#' @param x An R object to test.
#' @return `TRUE` if `x` is a promise object created by [promise()], `FALSE`
#'   otherwise.
#' @export
is.promise <- function(x) {
  inherits(x, "promise")
}

#' @title Format a promise
#' @param x A promise object.
#' @param ... Further arguments passed to or from other methods (currently
#'   unused).
#' @return A character string.
#' @keywords internal
#' @exportS3Method
format.promise <- function(x, ...) {
  attr(x, "promise_impl", exact = TRUE)$format()
}

#' @title Print a promise
#' @param x A promise object.
#' @param ... Further arguments passed to or from other methods (just for S3
#' complicance, unused)
#' @return `NULL`, invisibly. Called for its side effect of printing.
#' @keywords internal
#' @exportS3Method
print.promise <- function(x, ...) {
  cat(paste(format(x), collapse = "\n"), "\n", sep = "")
}

#' @title Access the results of a promise
#' 
#' @description Use `then()` to access the eventual result of a promise (or, if
#' the operation fails, the reason for that failure). The call to `then()` is
#' non-blocking: it returns immediately, and the return value is itself a
#' new promise.
#'
#' `catch()` is equivalent to `then()`, but without the `onFulfilled`
#' argument; it is typically used at the end of a promise chain to perform
#' error handling.
#'
#' `finally()` is similar to `then()`, but takes a single no-argument
#' function that is executed upon completion of the promise, regardless of
#' whether the result is success or failure. The return value of the
#' `onFinally` callback is ignored; an error thrown from it is propagated
#' forward into the returned promise.
#'
#' @param promise A promise object.
#' @param onFulfilled A function to be invoked if `promise` resolves
#'   successfully. Called with the resolved value, and optionally a second
#'   `.visible` argument indicating whether the value is
#'   [visible][base::invisible()].
#' @param onRejected A function taking a single `reason` argument, to be
#'   invoked if `promise` fails.
#'
#' @return A new promise.
#'
#' @export
then <- function(promise, onFulfilled = NULL, onRejected = NULL) {
  if (!is.promise(promise)) {
    stop("`promise` must be a promise object")
  }
  if (!is.null(onFulfilled) && !is.function(onFulfilled)) {
    stop("`onFulfilled` must be a function or `NULL`")
  }
  if (!is.null(onRejected) && !is.function(onRejected)) {
    stop("`onRejected` must be a function or `NULL`")
  }

  invisible(promise$then(onFulfilled = onFulfilled, onRejected = onRejected))
}

#' @rdname then
#' @export
catch <- function(promise, onRejected) {
  if (!is.promise(promise)) {
    stop("`promise` must be a promise object")
  }
  if (!is.function(onRejected)) {
    stop("`onRejected` must be a function")
  }

  invisible(promise$catch(onRejected))
}

#' @param onFinally A function with no arguments, called when `promise`
#'   either succeeds or fails.
#' @rdname then
#' @export
finally <- function(promise, onFinally) {
  if (!is.promise(promise)) {
    stop("`promise` must be a promise object")
  }
  if (!is.function(onFinally)) {
    stop("`onFinally` must be a function")
  }

  invisible(promise$finally(onFinally))
}
