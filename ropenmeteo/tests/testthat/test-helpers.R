test_that("write_glm_format works", {

  file <- system.file("extdata", "test-data.csv", package="ropenmeteo")
  df <- readr::read_csv(file, show_col_types = FALSE)

  df <- df |>
    add_longwave()

  expect_s3_class(df, "data.frame")

  path <- tempdir()
  df |>
    write_glm_format(path = path)

  file_names <- read.csv(list.files(path = path, full.names = TRUE, pattern = ".csv")[1])

  expect_s3_class(file_names, "data.frame")

  efi <- df |>
  convert_to_efi_standard()

  expect_s3_class(efi, "data.frame")
})

