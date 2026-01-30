with(globalenv(), {
  .old_wd <- setwd(tempdir())
})

# fileOps -----------------------------------------------------------------

seq_file_ops(n = 5, target_dir = "n_is_5")
seq_file_ops(n = c(1:5), target_dir = "n_is_1_to_5")
seq_file_ops(n = 100, target_dir = "n_is_100")
seq_file_ops(n = c(1:100), target_dir = "n_is_1_to_100")
seq_file_ops(n = c(1, 3, 5, 7, 9), target_dir = "n_is_odd_nums")
seq_file_ops(n = c(1, 3:8, 10), target_dir = "n_is_mixed_vec")
seq_file_ops(n = 5, target_dir = "testing_filetype", filetype = "txt")
seq_file_ops(n = 5, target_dir = "testing_force")
writeLines("testing force", "testing_force/01-.R")

# tests -------------------------------------------------------------------
# expect message ----------------------------------------------------------

test_that("func produces expected messages", {
  expect_message(
    seq_file_ops(1, target_dir = "test_seq"),
    "New files created:"
  )

  expect_message(
    seq_file_ops(1, target_dir = "test_new_dir"),
    "created test_new_dir/ as it was not found."
  )
})

# n -----------------------------------------------------------------------

test_that("different n formats are supported", {
  expect_identical(
    length(list.files("n_is_5")),
    length(list.files("n_is_1_to_5"))
  )

  expect_identical(
    length(list.files("n_is_100")),
    length(list.files("n_is_1_to_100"))
  )

  expect_identical(as.integer(5), length(list.files("n_is_odd_nums")))

  expect_identical(as.integer(8), length(list.files("n_is_mixed_vec")))
})

# target_dir --------------------------------------------------------------

test_that("target_dir has been created", {
  expect_true(dir.exists("n_is_5"))
  expect_true(dir.exists("n_is_1_to_5"))
  expect_true(dir.exists("n_is_100"))
  expect_true(dir.exists("n_is_1_to_100"))
  expect_true(dir.exists("n_is_odd_nums"))
  expect_true(dir.exists("n_is_mixed_vec"))
})

# filetype ----------------------------------------------------------------

test_that("specified filetype is found", {
  expect_true(all(str_extract(list.files("testing_filetype"),
    pattern = "[^\\.]*$"
  ) == "txt"))

  expect_true(all(str_extract(list.files("n_is_5"),
    pattern = "[^\\.]*$"
  ) == "R"))
})

# force -------------------------------------------------------------------

test_that(
  "func messages for pre-existing sequences",
  expect_message(
    seq_file_ops(n = 5, target_dir = "testing_force"),
    "Following found files will not be overwritten:"
  )
)

test_that(
  "force has not allowed additional files to be written",
  expect_true(length(list.files("testing_force")) == 5)
)

test_that(
  "force results in message for part sequences",
  expect_message(
    seq_file_ops(n = 7, target_dir = "testing_force"),
    "Following found files will not be overwritten:"
  )
)

test_that(
  "content of pre-existing sequence scripts is unaffected",
  expect_identical(readLines("testing_force/01-.R"), "testing force")
)

test_that(
  "func warns when force == TRUE",
  expect_warning(
    seq_file_ops(1, target_dir = "testing_force", force = TRUE),
    "force = TRUE. Files may be overwritten."
  )
)

test_that(
  "force == TRUE overwites pre-existing sequence files",
  expect_true(length(readLines("testing_force/01-.R")) == 0)
)

# leading 0s --------------------------------------------------------------

# regex used matches single digit at the start of a string that is NOT followed
# by another digit. These shouldn't exist: 1-.R, 2-.R etc.
pat <- "^\\d(?!\\d)"
test_that("there are no single digits in sequence filenames", {
  expect_false(all(stringr::str_detect(list.files("n_is_5"), pat)))
  expect_false(all(stringr::str_detect(list.files("n_is_1_to_5"), pat)))
  expect_false(all(stringr::str_detect(list.files("n_is_100"), pat)))
  expect_false(all(stringr::str_detect(list.files("n_is_1_to_100"), pat)))
  expect_false(all(stringr::str_detect(list.files("n_is_odd_nums"), pat)))
  expect_false(all(stringr::str_detect(list.files("n_is_mixed_vec"), pat)))
  expect_false(all(stringr::str_detect(list.files("testing_filetype"), pat)))
  expect_false(all(stringr::str_detect(list.files("testing_force"), pat)))
})

with(globalenv(), {
  setwd(.old_wd)
})
