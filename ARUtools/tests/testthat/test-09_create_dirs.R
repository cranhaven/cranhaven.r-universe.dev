test_that("create_dirs()", {
  bd <- test_path("Recordings")

  # Default is to do a dry-run (don't actually create the directories)
  expect_message(
    d <- create_dirs(
      plots = c("river1", "river2", "river3"),
      site_ids = c(
        "river1_sm01", "river1_sm02", "river2_sm03", "river2_sm04",
        "river3_sm05", "river3_sm06"
      ),
      base_dir = bd
    ), "This is a dry run"
  )
  expect_null(d)

  # Get a list of directories which would be created
  expect_message(
    d <- create_dirs(
      plots = c("river1", "river2", "river3"),
      site_ids = c(
        "river1_sm01", "river1_sm02", "river2_sm03", "river2_sm04",
        "river3_sm05", "river3_sm06"
      ),
      base_dir = bd, dir_list = TRUE
    ), "This is a dry run"
  )
  expect_type(d, "character")
  expect_match(d, "Recordings/river")
  expect_length(d, 6)
  expect_false(any(fs::file_exists(d)))


  # Create directories AND return a list of those created
  expect_silent(
    d <- create_dirs(
      plots = c("river1", "river2", "river3"),
      site_ids = c(
        "river1_sm01", "river1_sm02", "river2_sm03", "river2_sm04",
        "river3_sm05", "river3_sm06"
      ),
      base_dir = bd, dir_list = TRUE,
      dry_run = FALSE
    )
  )
  expect_type(d, "character")
  expect_match(d, "Recordings/river")
  expect_length(d, 6)
  expect_true(all(fs::file_exists(d)))

  # Don't create dirs if they already exist
  expect_error(
    d <- create_dirs(
      plots = c("river1", "river2", "river3"),
      site_ids = c(
        "river1_sm01", "river1_sm02", "river2_sm03", "river2_sm04",
        "river3_sm05", "river3_sm06"
      ),
      base_dir = bd, dir_list = TRUE,
      dry_run = FALSE
    ),
    "Trying to create directories"
  )

  expect_message(
    d <- create_dirs(
      plots = c("river1", "river2", "river3"),
      site_ids = c(
        "river1_sm01", "river1_sm02", "river2_sm03", "river2_sm04",
        "river3_sm05", "river3_sm06"
      ),
      base_dir = bd, dir_list = TRUE,
      dry_run = TRUE
    ),
    "This will create directories that already exist"
  )

  expect_silent(d <- create_dirs(
    plots = c("river1", "river2", "river3"),
    site_ids = c(
      "river1_sm01", "river1_sm02", "river2_sm03", "river2_sm04",
      "river3_sm05", "river3_sm06"
    ),
    base_dir = bd, dir_list = TRUE,
    dry_run = FALSE, expect_dirs = TRUE
  ))

  unlink(bd, recursive = TRUE)
})
