#' Copyright(c) 2017-2024 R. Mark Sharp
#' This file is part of nprcgenekeepr
context("get_elapsed_time_str")
loadNamespace("mockery")
start_time <- structure(
  c(
    user.self = 14.724,
    sys.self = 8.324,
    elapsed = 2786.165,
    user.child = 14.473,
    sys.child = 4.668
  ),
  class = "proc_time"
)
mock_proc.time <- structure(
  c(
    user.self = 14.724,
    sys.self = 8.324,
    elapsed = 2787.165,
    user.child = 14.473,
    sys.child = 4.668
  ),
  class = "proc_time"
)

# Use mockery to mock proc.time
mockery::stub(
  get_elapsed_time_str,
  "proc.time",
  mock_proc.time
)
elapsed_time <- get_elapsed_time_str(start_time)
test_that("get_elapsed_time_str gets time string diff back", {
  expect_identical(elapsed_time, "1 seconds.")
})
