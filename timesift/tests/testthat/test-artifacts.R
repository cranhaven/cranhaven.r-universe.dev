# The three artifacts that cross the language boundary, and the deterministic numbers read off
# them. Every assertion here has a twin in python/tests/test_artifacts.py reading the same files.

# na.strings is emptied so a value the fixture writes as NA, which is how a metric a case defines
# none of is recorded, reads as the string it is rather than as a missing value.
read_fixture <- function(dir, file) {
  utils::read.csv(file.path(dir, file), stringsAsFactors = FALSE, colClasses = "character",
                  check.names = FALSE, na.strings = character(0))
}

test_that("a fold map, a response and a mask round-trip through the file byte for byte", {
  set.seed(3)
  y <- matrix(rbinom(150, 1, 0.3), nrow = 25,
              dimnames = list(sprintf("p%02d", 1:25), paste0("sp", 1:6)))
  f <- fold_map(y, v = 5)
  cells <- scorable_cells(y, f)

  dir <- temp_dir()
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)
  write_folds(f, file.path(dir, "folds.csv"))
  write_response(y, file.path(dir, "response.csv"))
  write_cells(cells, file.path(dir, "cells.csv"))

  expect_equal(read_folds(file.path(dir, "folds.csv"), names(f)), f[names(f)],
               ignore_attr = TRUE)
  expect_equal(read_response(file.path(dir, "response.csv"), rownames(y)), y)
  expect_equal(as.data.frame(read_cells(file.path(dir, "cells.csv"))), as.data.frame(cells))

  # Writing what was read gives the same bytes, which is what makes the format a contract rather
  # than a convention: a reader that quietly reordered or reformatted would show up here.
  again <- file.path(dir, "again.csv")
  for (nm in c("folds", "response", "cells")) {
    original <- file.path(dir, paste0(nm, ".csv"))
    switch(nm,
           folds = write_folds(read_folds(original), again),
           response = write_response(read_response(original), again),
           cells = write_cells(read_cells(original), again))
    expect_identical(readBin(again, "raw", file.size(again)),
                     readBin(original, "raw", file.size(original)), info = nm)
  }
})

test_that("the artifacts are written with LF line endings and twelve significant digits", {
  dir <- temp_dir()
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)
  y <- matrix(c(0, 1, 0.5, 1 / 3), nrow = 2,
              dimnames = list(c("b", "a"), c("v2", "v1")))
  path <- file.path(dir, "response.csv")
  write_response(y, path)
  bytes <- readBin(path, "raw", file.size(path))
  expect_false(any(bytes == as.raw(13L)))
  # Rows by the identifier under C collation, columns in the response's own order.
  expect_identical(readLines(path, warn = FALSE),
                   c("id,v2,v1", "a,1,0.333333333333", "b,0,0.5"))
})

test_that("a unit the artifact has no row for is an error naming it", {
  dir <- temp_dir()
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)
  y <- matrix(rbinom(30, 1, 0.4), nrow = 10,
              dimnames = list(sprintf("p%02d", 1:10), paste0("sp", 1:3)))
  write_folds(fold_map(y, v = 3), file.path(dir, "folds.csv"))
  write_response(y, file.path(dir, "response.csv"))

  expect_error(read_folds(file.path(dir, "folds.csv"), c("p01", "p99")),
               "no row in the fold map, first: p99")
  expect_error(read_response(file.path(dir, "response.csv"), c("p01", "p98", "p99")),
               "2 units have no row in the response, first: p98")
  # A unit the file carries beyond those asked for is dropped: reading a subset of a fold map
  # covering a whole study is a normal thing to do.
  expect_length(read_folds(file.path(dir, "folds.csv"), c("p03", "p01")), 2L)
  expect_identical(names(read_folds(file.path(dir, "folds.csv"), c("p03", "p01"))),
                   c("p03", "p01"))
})

test_that("a malformed artifact is refused rather than read as something else", {
  dir <- temp_dir()
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)
  writeLines(c("id,fold", "p01,1", "p01,2"), file.path(dir, "twice.csv"))
  writeLines(c("id,fold", "p01,first"), file.path(dir, "word.csv"))
  writeLines(c("id", "p01"), file.path(dir, "bare.csv"))
  expect_error(read_folds(file.path(dir, "twice.csv")), "names p01 more than once")
  expect_error(read_folds(file.path(dir, "word.csv")), "not a whole number")
  expect_error(read_folds(file.path(dir, "bare.csv")), "has no fold column")
  expect_error(read_response(file.path(dir, "bare.csv")), "no variable")
  expect_error(read_folds(file.path(dir, "absent.csv")), "no file at")
})

test_that("the scorable mask the fixtures pin is the mask this implementation builds", {
  dir <- fixture_dir()
  skip_if(is.null(dir), "fixtures are not in the built package")

  y <- read_response(file.path(dir, "response.csv"))
  f <- read_folds(file.path(dir, "folds.csv"), rownames(y))
  expected <- read_cells(file.path(dir, "cells.csv"))
  cells <- scorable_cells(y, f)

  # Cell by cell rather than in aggregate: a mask that agreed on how many cells are scorable and
  # disagreed on which would score two arms on different units and report neither.
  expect_identical(cells$variable, expected$variable)
  expect_identical(cells$fold, expected$fold)
  expect_identical(cells$scorable, expected$scorable)
  for (nm in c("n_occ", "pres_train", "abs_train", "pres_test", "abs_test")) {
    expect_identical(cells[[nm]], expected[[nm]], info = nm)
  }

  # The fixture is only a test if it holds a cell of each verdict, and holds a variable that has
  # no scorable fold at all.
  expect_true(any(expected$scorable))
  expect_true(any(!expected$scorable))
  by_variable <- tapply(expected$scorable, expected$variable, any)
  expect_true(any(!by_variable))
})

test_that("every threshold metric matches the value the fixtures pin", {
  dir <- fixture_dir()
  skip_if(is.null(dir), "fixtures are not in the built package")

  cases <- read_fixture(dir, "metric_cases.csv")
  expected <- read_fixture(dir, "metrics.csv")
  fns <- list(tss = tss, roc_auc = roc_auc, average_precision = average_precision,
              kappa = function(y, p) kappa_score(y, p, "prevalence"),
              kappa_youden = function(y, p) kappa_score(y, p, "youden"),
              threshold_youden = function(y, p) decision_threshold(y, p, "youden"),
              threshold_kappa = function(y, p) decision_threshold(y, p, "kappa"),
              threshold_prevalence = function(y, p) decision_threshold(y, p, "prevalence"))

  expect_setequal(unique(expected$metric), names(fns))
  for (i in seq_len(nrow(expected))) {
    row <- expected[i, ]
    case <- cases[cases$case == row$case, ]
    label <- paste(row$case, row$metric)
    got <- fns[[row$metric]](as.integer(case$y), as.numeric(case$p))
    if (row$value == "NA") {
      # A case a metric defines no value on is pinned as that, so a silent number is a failure.
      expect_false(is.finite(got), info = label)
    } else {
      expect_identical(sprintf("%.12g", got), row$value, info = label)
    }
  }

  # The cases where the tie rule is the whole answer are the reason the file exists.
  expect_true(all(c("all_tied", "some_tied", "tied_across_classes", "one_presence", "one_absence",
                    "all_presence", "all_absence", "perfect", "reversed") %in% expected$case))
})

test_that("the paired contrast matches the value the fixtures pin", {
  dir <- fixture_dir()
  skip_if(is.null(dir), "fixtures are not in the built package")

  cells <- read_fixture(dir, "contrast_cells.csv")
  expected <- read_fixture(dir, "contrast.csv")
  arm <- function(name, column) {
    raw <- cells[[column]]
    score <- ifelse(raw == "NA", NA_real_, suppressWarnings(as.numeric(raw)))
    data.frame(grain = "week", learner = name, variable = cells$variable,
               fold = as.integer(cells$fold), score = score, scorable = !is.na(score),
               stringsAsFactors = FALSE)
  }
  ladder <- structure(rbind(arm("a", "a"), arm("b", "b")),
                      class = c("timesift_ladder", "data.frame"))
  got <- paired_contrast(ladder, "week|a", "week|b")

  for (i in seq_len(nrow(expected))) {
    v <- got[[expected$quantity[i]]]
    expect_identical(if (is.character(v)) v else sprintf("%.12g", v), expected$value[i],
                     info = expected$quantity[i])
  }
  # A table where both arms scored every cell would pin the pairing at its easiest. The fixture
  # holds cells only one arm scored, so the count it rests on is below the table's height.
  expect_lt(got$n_cell, nrow(cells))
})

test_that("the inflation of a self-selected threshold is stated to the tolerance the spec gives", {
  dir <- fixture_dir()
  skip_if(is.null(dir), "fixtures are not in the built package")

  # It draws replicates, so it is the one thing here that cannot be a digest. The fixture holds
  # this side's value; the Python suite asserts its own against the same file, within the band the
  # spec states. Here the seed makes it exact.
  y <- read_response(file.path(dir, "response.csv"))
  f <- read_folds(file.path(dir, "folds.csv"), rownames(y))
  expected <- read_fixture(dir, "inflation.csv")
  out <- tss_inflation(y, f, skill = as.numeric(expected$skill),
                       replicates = as.integer(expected$replicates[1L]), seed = 1L)
  expect_identical(sprintf("%.12g", out$reported), expected$reported)
  expect_identical(sprintf("%.12g", out$inflation), expected$inflation)
  # A self-selected threshold reports above the truth, which is the claim the number carries.
  expect_true(all(out$inflation > 0))
})
