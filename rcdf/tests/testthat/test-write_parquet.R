# Mock key
mock_aes_key <- list(
  aes_key = "3e5bdf7031653ddcffca50831f6d9822f73cce44948c5a0861540cfb5620a633",
  aes_iv = "9A5D8517510F8E26C564C6C8DD39EA68"
)

# Test `write_parquet()` with encryption
test_that("write_parquet writes encrypted Parquet files", {
  data <- tibble::tibble(a = 1:5, b = letters[1:5])
  temp_file <- tempfile(fileext = ".parquet")

  # Writing without encryption
  write_parquet(data, temp_file, encryption_key = mock_aes_key)

  # Check if file is created
  expect_true(file.exists(temp_file))
  unlink(temp_file, force = T)
})


# Test `write_parquet()` without encryption
test_that("write_parquet writes regular Parquet files", {
  data <- tibble::tibble(a = 1:5, b = letters[1:5])
  temp_file <- tempfile(fileext = ".parquet")

  # Writing without encryption
  write_parquet(data, temp_file)

  # Check if file is created
  expect_true(file.exists(temp_file))
  unlink(temp_file, force = T)
})



# Test `write_rcdf_parquet()` with RCDF data
test_that("write_rcdf_parquet writes RCDF data to Parquet files", {

  data_list <- list(
    df1 = tibble::tibble(a = 1:5),
    df2 = tibble::tibble(b = 6:10)
  )

  data_list <- as_rcdf(data_list)

  temp_dir <- tempfile()

  # Writing the RCDF data
  written_files <- write_rcdf_parquet(data_list, temp_dir)

  # Check if the correct files were written
  expect_true(all(grepl("\\.parquet$", written_files)))

  # Check if the files are located in the temp_dir
  expect_true(all(file.exists(written_files)))
  unlink(written_files, force = T)

})

