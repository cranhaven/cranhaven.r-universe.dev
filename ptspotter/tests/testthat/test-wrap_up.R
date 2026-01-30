
# dependencies ------------------------------------------------------------
s_time <- Sys.time()


# tests -------------------------------------------------------------------
# -------------------------------------------------------------------------
test_that("func produces expected error", {
  expect_error(wrap_up(start_time = s_time), "wrap_up at")
  expect_error(wrap_up(), "wrap_up at")
  })

test_that("func produces expected message",
          expect_message(expect_error(wrap_up(start_time = s_time)),
                         "Time difference of"
                         )

          )
