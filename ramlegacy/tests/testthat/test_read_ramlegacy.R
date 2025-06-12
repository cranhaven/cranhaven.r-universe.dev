context("Checking read_ramlegacy works as expected")

in_url <- "https://zenodo.org/record/2542919/files/RLSADB%20v4.44.zip?download=1"

test_that("read_ramlegacy writes the database as RDS for v4.44", {
  skip_on_cran()
  temp_db_path <- file.path(tempfile("ramlegacy", tempdir()), "4.44")
  dir.create(temp_db_path, showWarnings = FALSE, recursive = TRUE)
  # download and unzip the database
  download_db_path <- file.path(temp_db_path, "v4.44.zip")
  download.file(in_url, download_db_path)
  utils::unzip(download_db_path, exdir = temp_db_path, overwrite = TRUE)
  read_path <- file.path(temp_db_path, "RLSADB v4.44")
  read_path <- file.path(read_path, "DB Files With Assessment Data")
  suppressWarnings(read_ramlegacy(read_path, "4.44"))
  # check RDS exists
  rds_file <- file.path(read_path, "v4.44.rds")
  expect_true(file.exists(rds_file))
  expect_is(readRDS(rds_file), "list")
  unlink(rds_file, recursive = TRUE)
})
