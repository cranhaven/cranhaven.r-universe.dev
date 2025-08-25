library(testthat)

# Check setup.R

# @@@@@@@@@@@@@@@@
# Save Cache ----
# @@@@@@@@@@@@@@@@

song <- c("bella", "ciao", "bella", "ciao", "ciao", "ciao")

test_that("Save to cache works", {
  expect_message(
    {
      file_path <- save_to_cache(
        dat = song,
        file_name = "song.txt",
        verbose = TRUE
      )
    },
    "Saving"
  )

  exp_path <- normalizePath(file.path(
    Sys.getenv("R_USER_CACHE_DIR"), "R",
    "extractox", "song.txt"
  ))
  file_path <- normalizePath(file_path)

  expect_equal(exp_path, file_path)
  message(file_path)
})

test_that("Save to cache overwrites cache if present", {
  expect_message(
    {
      file_path <- save_to_cache(dat = song, file_name = "song.txt", verbose = TRUE)
    },
    "Overwriting"
  )

  expect_no_message({
    file_path <- save_to_cache(
      dat = song,
      file_name = "song.txt",
      verbose = FALSE
    )
  })
})

# @@@@@@@@@@@@@@@@
# Load Cache ----
# @@@@@@@@@@@@@@@@

test_that("load cache verbose, load silent", {
  expect_message(
    {
      dat <- read_from_cache(file_name = "song.txt", verbose = TRUE)
    },
    "Successfully"
  )

  expect_no_message({
    dat2 <- read_from_cache(file_name = "song.txt", verbose = FALSE)
  })

  expect_equal(dat, song)
})


test_that("load cache fail if no file", {
  expect_error({
    read_from_cache(file_name = "non_esisto.txt", verbose = TRUE)
  })
})

# @@@@@@@@@@@@@@@@
# Sandbox ----
# @@@@@@@@@@@@@@@@


# test_that("Sandbox works", {
#   to_check <- normalizePath(tempdir(), mustWork = FALSE)

#   with_extr_sandbox(temp_dir = to_check, code = {
#     out <- normalizePath(Sys.getenv("R_USER_CACHE_DIR"), mustWork = FALSE)
#   })

#   expect_equal(to_check, normalizePath(out, mustWork = FALSE))
# })
