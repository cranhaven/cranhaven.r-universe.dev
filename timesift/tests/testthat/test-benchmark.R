# The shipped scripts under inst/ reach into the namespace for a few helpers the package does not
# export. Each one is named here as the scripts name it, so a rename inside the package fails this
# suite rather than the next benchmark launch.
test_that("every internal the shipped scripts reach still exists under that name", {
  dirs <- Filter(nzchar, c(system.file("benchmark", package = "timesift"),
                           system.file("reproduce", package = "timesift")))
  skip_if(!length(dirs), "the scripts are not in the built package")
  files <- unlist(lapply(dirs, list.files, pattern = "[.]R$", full.names = TRUE))
  reached <- unique(unlist(lapply(files, function(f) {
    text <- paste(readLines(f, warn = FALSE), collapse = "\n")
    m <- gregexpr("timesift:::([.A-Za-z_][.A-Za-z0-9_]*)", text)
    sub("^timesift:::", "", regmatches(text, m)[[1L]])
  })))
  expect_gt(length(reached), 0L)
  ns <- asNamespace("timesift")
  for (name in reached) {
    expect_true(exists(name, envir = ns, inherits = FALSE), label = paste0("timesift:::", name))
  }
})

test_that("the benchmark refuses a package that is not the checkout it stamps", {
  dir <- system.file("benchmark", package = "timesift")
  skip_if(!nzchar(dir), "the scripts are not in the built package")
  env <- new.env()
  sys.source(file.path(dir, "design.R"), envir = env)
  other <- tempfile("checkout")
  dir.create(other)
  writeLines(c("Package: timesift", "Version: 0.0.0.9999"), file.path(other, "DESCRIPTION"))
  expect_error(env$bench_assert_package(other, dirty = FALSE), "declares 0.0.0.9999")
  here <- tempfile("checkout")
  dir.create(here)
  writeLines(c("Package: timesift",
               paste("Version:", as.character(utils::packageVersion("timesift")))),
             file.path(here, "DESCRIPTION"))
  expect_true(env$bench_assert_package(here, dirty = FALSE))
  env$BENCH$scale <- "full"
  expect_error(env$bench_assert_package(here, dirty = TRUE), "uncommitted changes")
  env$BENCH$scale <- "smoke"
  expect_true(env$bench_assert_package(here, dirty = TRUE))
})

test_that("a quantity is paired with another on the replicate, not on the row order", {
  dir <- system.file("benchmark", package = "timesift")
  skip_if(!nzchar(dir), "the scripts are not in the built package")
  env <- new.env()
  sys.source(file.path(dir, "design.R"), envir = env)

  rows <- function(arm, replicate, value) {
    data.frame(cell_id = "cell", arm = arm, quantity = "true", metric = env$BENCH$metric,
               replicate = replicate, value = value, stringsAsFactors = FALSE)
  }
  # The second arm arrives in the reverse order and is still read replicate by replicate.
  d <- rbind(rows("nested", 1:3, c(0.1, 0.2, 0.3)), rows("oracle", 3:1, c(0.6, 0.5, 0.4)))
  expect_equal(env$bench_paired(d, c("oracle", "true"), c("nested", "true")),
               c(`1` = 0.3, `2` = 0.3, `3` = 0.3))

  # Two arms of the same length over different replicates: the case that used to difference across
  # replicates and report a number.
  d <- rbind(rows("nested", 1:3, c(0.1, 0.2, 0.3)), rows("oracle", 2:4, c(0.4, 0.5, 0.6)))
  expect_error(env$bench_paired(d, c("oracle", "true"), c("nested", "true")),
               "cell oracle/true alone holds {4}", fixed = TRUE)

  # And two of unequal length, where the shorter would have been recycled.
  d <- rbind(rows("nested", 1:4, c(0.1, 0.2, 0.3, 0.4)), rows("oracle", 1:2, c(0.5, 0.6)))
  expect_error(env$bench_paired(d, c("oracle", "true"), c("nested", "true")),
               "cell nested/true alone holds {3,4}", fixed = TRUE)

  expect_equal(env$bench_align(c(a = 1, b = 2), c(a = 4, b = 8)), c(a = -3, b = -6))
  expect_error(env$bench_align(c(a = 1), c(b = 1)), "not the same replicates")
})
