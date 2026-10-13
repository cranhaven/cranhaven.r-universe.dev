# tests that compile snippets were moved to side pkg latertest

jitter <- 0.017 * 2 # Compensate for imprecision in system timer

source(system.file("tinytest", "helper.R", package = "later2"))

local({
  # run_now waits and returns FALSE if no tasks ----

  x <- system.time({
    result <- run_now(0.5)
  })
  expect_true(as.numeric(x[["elapsed"]]) >= 0.5 - jitter)
  expect_identical(result, FALSE)

  x <- system.time({
    result <- run_now(3)
  })
  expect_true(as.numeric(x[["elapsed"]]) >= 3 - jitter)
  expect_identical(result, FALSE)
})

local({
  # run_now returns immediately after executing a task ----

  x <- system.time({
    later(~ {}, 0)
    result <- run_now(2)
  })
  expect_true(as.numeric(x[["elapsed"]]) < 0.25)
  expect_identical(result, TRUE)
})

local({
  # run_now executes all scheduled tasks, not just one ----

  later(~ {}, 0)
  later(~ {}, 0)
  result1 <- run_now()
  result2 <- run_now()
  expect_identical(result1, TRUE)
  expect_identical(result2, FALSE)
})

local({
  # run_now executes just one scheduled task, if requested ----

  result1 <- run_now()
  expect_identical(result1, FALSE)

  later(~ {}, 0)
  later(~ {}, 0)

  result2 <- run_now(all = FALSE)
  expect_identical(result2, TRUE)

  result3 <- run_now(all = FALSE)
  expect_identical(result3, TRUE)

  result4 <- run_now()
  expect_identical(result4, FALSE)
})

local({
  # run_now doesn't go past a failed task ----

  later(~ stop("boom"), 0)
  later(~ {}, 0)
  expect_error(run_now())
  expect_true(run_now())
})


local({
  # Callbacks cannot affect the caller ----

  # This is based on a pattern used in the callCC function. Normally, simply
  # touching `throw` will cause the expression to be evaluated and f() to return
  # early. (This test does not involve later.)
  f <- function() {
    delayedAssign("throw", return(100))
    g <- function() {
      throw
    }
    g()
    return(200)
  }
  expect_equal(f(), 100)

  # When later runs callbacks, it wraps the call in R_ToplevelExec(), which
  # creates a boundary on the call stack that the early return can't cross.
  f <- function() {
    delayedAssign("throw", return(100))
    later(function() {
      throw
    })

    run_now(1)
    return(200)
  }
  # jcheng 2024-10-24: Apparently this works now, maybe because having
  # RCPP_USING_UNWIND_PROTECT means we don't need to use R_ToplevelExec to call
  # callbacks?
  # expect_error(f())
  expect_identical(f(), 100)

  # In this case, f() should return normally, and then when g() causes later to
  # run the callback with `throw`, it should be an error -- there's no function
  # to return from because it (f()) already returned.
  f <- function() {
    delayedAssign("throw", return(100))
    later(function() {
      throw
    })
    return(200)
  }
  g <- function() {
    run_now(2)
  }
  expect_equal(f(), 200)
  expect_error(g())
})


local({
  # interrupt and exception handling, R ----

  # =======================================================
  # Errors and interrupts in R callbacks
  # =======================================================

  # R error
  error_obj <- FALSE
  tryCatch(
    {
      later(function() {
        stop("oopsie")
      })
      run_now()
    },
    error = function(e) {
      error_obj <<- e
    }
  )
  expect_true(grepl("oopsie", error_obj$message))

  # interrupt
  interrupted <- FALSE
  tryCatch(
    {
      later(function() {
        simulate_interrupt()
        Sys.sleep(100)
      })
      run_now()
    },
    interrupt = function(e) {
      interrupted <<- TRUE
    }
  )
  expect_true(interrupted)
})
