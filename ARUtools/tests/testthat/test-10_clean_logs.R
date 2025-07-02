test_that("clean_logs() single", {
  # log_file <- test_path("..", "..", "..", "ARUtools - Extra", "aru_log_files",
  #                       "P028/1A_BARLT10962/logfile_00010962_SD1.txt")
  log_file <- system.file("extdata", "logfile_00015141_SD1.txt", package = "ARUtools")

  skip_if_not(fs::file_exists(log_file))

  expect_silent(l <- clean_logs(log_file))
  expect_s3_class(l, "data.frame")
  expect_named(l)
  expect_true(nrow(l) > 0)

  expect_silent(l1 <- clean_logs(log_file, return = "gps"))
  expect_silent(l2 <- clean_logs(log_file, return = "recordings"))

  expect_equal(
    dplyr::filter(l, event == "gps") |>
      dplyr::select(-dplyr::starts_with("rec_")),
    l1
  )
  expect_equal(
    dplyr::filter(l, event == "recording") |>
      dplyr::select(-"lat", -"lon"),
    l2
  )
  skip_on_ci()
  l <- withr::with_seed(1234, dplyr::slice_sample(l, n = 100))
  l$path <- l$path |> stringr::str_remove(fs::path_package("ARUtools"))
  expect_snapshot_value(l, style = "json2")
})

test_that("clean_logs() multiple", {
  log_files <- fs::dir_ls(fs::path_package("extdata", package = "ARUtools"),
    recurse = TRUE, glob = "*logfile*"
  )
  # log_files <- log_files[1:5]

  skip_if_not(all(fs::file_exists(log_files)) | length(log_files) > 0)

  expect_silent(l <- clean_logs(log_files, progress = FALSE))
  expect_s3_class(l, "data.frame")
  expect_named(l)
  expect_true(nrow(l) > 0)

  expect_silent(l1 <- clean_logs(log_files, progress = FALSE, return = "gps"))
  expect_silent(l2 <- clean_logs(log_files, progress = FALSE, return = "recordings"))

  expect_equal(
    dplyr::filter(l, event == "gps") |>
      dplyr::select(-dplyr::starts_with("rec_")),
    l1
  )
  expect_equal(
    dplyr::filter(l, event == "recording") |>
      dplyr::select(-"lat", -"lon"),
    l2
  )
  skip_on_ci()
  l <- withr::with_seed(1234, dplyr::slice_sample(l, n = 100))
  l$path <- l$path |> stringr::str_remove(fs::path_package("ARUtools"))
  expect_snapshot_value(l, style = "json2")
})

test_that("meta_clean_logs()",{
  file_vec <- fs::dir_ls(fs::path_package("extdata", package = "ARUtools"),
                               recurse = TRUE,
                             )
  expect_message(
    m <- clean_metadata(project_files = file_vec, file_type = 'json',pattern_site_id = "000\\d+" )
  )

  expect_silent(logs <- meta_clean_logs(m))


})
