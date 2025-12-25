test_that("Test conjecture recognizes incompatible arguments", {
  expect_error(conjecture(comms, station, type, "send"), "must be coercible to <double>")
  expect_error(conjecture(comms, timestamp, msg_code, 1), "must contain exactly 2 levels")
  expect_error(conjecture(dplyr::mutate(comms, type = c(NA, type[-1])), timestamp, type, "send"), "must not contain any missing values")
  expect_error(conjecture(comms, timestamp1, type, "send"), "not found")
  expect_error(conjecture(comms, timestamp, type1, "send1"), "not found")
  expect_error(conjecture(comms, timestamp, type, "send1"), "not found")
})

test_that("Test break_join handles evil input", {
  expect_error(conjecture(setNames(comms, c("station", "timestamp", "send", "type")), timestamp, type, "send"), "matches column name")
  expect_equal(
    conjecture(setNames(comms, c("station", "timestamp", "msg_code", "sort_by")), timestamp, sort_by, "send"),
    conjecture(comms, timestamp, type, "send")
  )
  expect_equal(
    conjecture(setNames(comms, c("station", "sort_by", "msg_code", "type")), sort_by, type, "send"),
    conjecture(comms, timestamp, type, "send")
  )
  expect_equal(
    conjecture(setNames(comms, c("station", "timestamp", "msg_code", "names_from")), timestamp, names_from, "send"),
    conjecture(comms, timestamp, type, "send")
  )
})

test_that("Test break_join produces expected output", {
  expect_equal(nrow(conjecture(dplyr::mutate(comms, timestamp = NA_real_), timestamp, type, "send")), 0)
  expect_equal(
    nrow(dplyr::distinct(conjecture(comms, timestamp, type, "send"), station, msg_code)),
    nrow(dplyr::distinct(dplyr::filter(comms, type == "send"), station, msg_code))
  )
})

test_that("Test conjecture grouped df handling", {
  expect_equal(
    conjecture(dplyr::group_by(comms, station), timestamp, type, "send"),
    conjecture(comms, timestamp, type, "send")
  )
})






