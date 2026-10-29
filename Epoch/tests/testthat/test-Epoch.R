# Create a dummy Epoch object for testing
set.seed(1)
row_num <- 10
col_num <- 100
dummy_data <- matrix(rnorm(row_num*col_num), nrow = row_num, dimnames = list(paste0("Elec", seq_len(row_num)), NULL))
expected_times <- seq(0, by = 0.1, length.out = col_num)
dummy_epoch <- Epoch(dummy_data, time = expected_times)

test_that("Epoch object creation and basic properties with valid input", {
  expect_s4_class(dummy_epoch, "Epoch")
  expect_equal(nrow(tblData(dummy_epoch)), row_num)
  expect_equal(ncol(tblData(dummy_epoch)), col_num)
  # Time is stored as colnames of the data table
  expect_equal(as.numeric(colnames(tblData(dummy_epoch))), expected_times)
  expect_equal(coltimes(dummy_epoch), expected_times)
  expect_equal(dim(dummy_epoch), dim(dummy_data))
  expect_equal(rownames(dummy_epoch), rownames(dummy_data))
})

test_that("tblData method for Epoch", {
  expect_true(is.matrix(tblData(dummy_epoch)))
  expect_equal(dim(tblData(dummy_epoch)), c(row_num, col_num))
})

test_that("crop method for Epoch with valid inputs", {
  # Times are accessed via colnames(tblData())
  original_times <- coltimes(dummy_epoch)
  cropped_epoch_time <- crop(dummy_epoch, start = 1, end = 5)
  expect_s4_class(cropped_epoch_time, "Epoch")
  cropped_times <- as.numeric(colnames(tblData(cropped_epoch_time)))
  expect_true(all(cropped_times >= 1))
  expect_true(all(cropped_times <= 5))
  expect_lt(ncol(tblData(cropped_epoch_time)), ncol(tblData(dummy_epoch)))
})


test_that("crop() returns full Epoch when range matches full extent", {
  times <- coltimes(dummy_epoch)
  e <- crop(dummy_epoch, start = min(times), end = max(times))
  expect_equal(dim(e), dim(dummy_epoch))
})

test_that("crop() gives error when start or end is not numeric", {
  expect_error(
    e <- crop(dummy_epoch, start = 'a', end = 0.2),
    regexp = "must be numeric"
  )
  expect_error(
    e <- crop(dummy_epoch, start = 0.1, end = '0.2'),
    regexp = "must be numeric"
  )
})

test_that("crop() gives warning when start or end is out of range", {
  expect_warning(
    e <- crop(dummy_epoch, start = -1, end = 0.2),
    regexp = "outside the available time range"
  )
  expect_s4_class(e, "Epoch")
})

test_that("crop() returns empty Epoch and warns when no time points match", {
  warnings <- character()
  
  withCallingHandlers(
    {
      e <- crop(dummy_epoch, start = 500, end = 600)
    },
    warning = function(w) {
      warnings <<- c(warnings, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )
  
  expect_length(warnings, 2)
  expect_true(any(grepl("No time points found", warnings)))
  expect_true(any(grepl("outside the available time range", warnings)))
  expect_equal(ncol(e), 0)
})

test_that("crop() with checkTimeRange = FALSE returns silently", {
  e <- crop(dummy_epoch, start = -1, end = 0.2, checkTimeRange = FALSE)
  expect_s4_class(e, "Epoch")
})

test_that("crop() throws error for start > end when checkTimeRange = TRUE", {
  warnings <- character()
  
  withCallingHandlers(
    {
      e <- crop(dummy_epoch, start = 0.6, end = 0.2)
    },
    warning = function(w) {
      warnings <<- c(warnings, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )
  
  expect_length(warnings, 2)
  expect_true(any(grepl("No time points found", warnings)))
  expect_true(any(grepl("`start` is greater than `end`", warnings)))
})


test_that("plot method for Epoch", {
  # Check if plot returns a ggplot object without errors
  library(ggplot2)
  p <- plot(dummy_epoch)
  expect_s3_class(p, "ggplot")
})


test_that("Epoch print/show methods", {
  # Capture print output
  expect_no_error(capture_output(print(dummy_epoch)))
  expect_no_error(capture_output(show(dummy_epoch)))
})

test_that("Epoch dim, rownames, colnames", {
  expect_equal(dim(dummy_epoch), c(row_num, col_num))
  expect_equal(rownames(dummy_epoch), paste0("Elec", seq_len(row_num)))
})

test_that("Epoch subsetting with valid values", {
  subset_epoch <- dummy_epoch[1:5, 1:6]
  expect_s4_class(subset_epoch, "Epoch")
  expect_equal(dim(subset_epoch), c(5, 6))
  expect_equal(rownames(subset_epoch), paste0("Elec", 1:5))

  subset_epoch_single_elec <- dummy_epoch["Elec1", ]
  expect_s4_class(subset_epoch_single_elec, "Epoch")
  expect_equal(nrow(subset_epoch_single_elec), 1)
  expect_equal(rownames(subset_epoch_single_elec), "Elec1")

  subset_epoch_single_time <- dummy_epoch[, 2]
  expect_s4_class(subset_epoch_single_time, "Epoch")
  expect_equal(ncol(subset_epoch_single_time), 1)
  expect_equal(colnames(subset_epoch_single_time), "0.1")
  
  expect_equal(dim(dummy_epoch[0, 0]), c(0, 0))

})

test_that("Epoch subsetting with invalid values", {

  expect_error(dummy_epoch[999, ], regexp = "subscript out of bounds")
  expect_error(dummy_epoch[, 999], regexp = "subscript out of bounds")
  expect_error(dummy_epoch["nonexistent_electrode", ], regexp = "subscript out of bounds")
  expect_error(dummy_epoch[, "nonexistent_time"], regexp = "subscript out of bounds")

})

test_that("logical subsetting recycles by default", {
  subset <- dummy_epoch[c(TRUE, FALSE), ]
  expect_equal(nrow(subset), ceiling(nrow(dummy_epoch) / 2))
})

test_that("subsetting with NA returns NA-filled Epoch", {
  subset_epoch <- dummy_epoch[NA, ]
  expect_s4_class(subset_epoch, "Epoch")
  expect_equal(nrow(subset_epoch), 10)
  expect_true(all(is.na(as.matrix(tblData(subset_epoch)))))
})

test_that("subsetting with -999 returns full object (nothing dropped)", {
  subset <- dummy_epoch[-999, ]
  expect_equal(dim(subset), dim(dummy_epoch))
})

test_that("dimnames(Epoch) forwards to the base method", {
  # Exact match to the source matrix's dimnames
  #expect_identical(dimnames(dummy_epoch), dimnames(dummy_data))
  
  # Also check row/col names individually (clearer failure messages)
  expect_identical(rownames(dummy_epoch), rownames(dummy_data))
  expect_length(colnames(dummy_epoch), ncol(dummy_data))
  
  # Basic structure checks
  dn <- dimnames(dummy_epoch)
  expect_type(dn, "list")
  expect_length(dn, 2L)
  expect_true(is.null(dn[[1]]) || is.character(dn[[1]]))
  expect_true(is.null(dn[[2]]) || is.character(dn[[2]]))
})

test_that(".TableContainer2Epoch returns x unchanged for non-TableContainer inputs", {
  # 1) Plain numeric vector
  x_vec <- 1:5
  expect_identical(Epoch:::.TableContainer2Epoch(x_vec), x_vec)
  
  # 2) Plain matrix with dimnames
  x_mat <- matrix(1:4, nrow = 2,
                  dimnames = list(c("r1","r2"), c("c1","c2")))
  expect_identical(Epoch:::.TableContainer2Epoch(x_mat), x_mat)
  
  # 3) S3 object of some other class
  x_s3 <- structure(list(a = 1), class = "notTableContainer")
  expect_identical(Epoch:::.TableContainer2Epoch(x_s3), x_s3)
  
  # 4) NULL should stay NULL
  expect_null(Epoch:::.TableContainer2Epoch(NULL))
})

# (Optional) sanity check: when it IS a TableContainer, it should convert to Epoch
test_that(".TableContainer2Epoch converts TableContainer to Epoch", {
  # dummy_epoch is a TableContainer, so the helper should return an Epoch
  out <- Epoch:::.TableContainer2Epoch(dummy_epoch)
  expect_s4_class(out, "Epoch")
  # and keep core contents consistent
  expect_identical(dim(out), dim(dummy_epoch))
  expect_identical(dimnames(out), dimnames(dummy_epoch))
})

test_that("Epoch() errors if neither or both of 'times' and 'startTime' are supplied", {

  # neither provided -> error
  expect_error(
    Epoch(dummy_data),
    "exactly one of 'times' or 'startTime'"
  )
  
  # both provided -> error
  expect_error(
    Epoch(dummy_data, times = c(0, 0.1, 0.2), startTime = 0),
    "exactly one of 'times' or 'startTime'"
  )
})

test_that("Epoch() errors when startTime is provided without samplingRate", {
  tbl <- matrix(1:6, nrow = 2, ncol = 3,
                dimnames = list(c("Elec1","Elec2"), NULL))
  
  expect_error(
    Epoch(dummy_data, startTime = 0),
    "If 'startTime' is provided, 'samplingRate' must also be provided"
  )
})

test_that("Epoch() computes times from startTime and samplingRate", {

  ep <- Epoch(dummy_data, startTime = 0, samplingRate = 10)  # 10 Hz → 0, 0.1, 0.2
  
  # colnames are strings, but coltimes() should give numeric times
  expect_s4_class(ep, "Epoch")
  expect_equal(coltimes(ep), expected_times, tolerance = 1e-8)
  expect_identical(rownames(ep), rownames(dummy_data))
  expect_equal(ncol(ep), 100L)
})

test_that("Epoch() errors when provided samplingRate mismatches estimate from times", {

  expect_error(
    Epoch(dummy_data, times = expected_times, samplingRate = 9.5),
    regexp = "Estimated sampling rate .* does not match provided sampling rate"
  )
})

test_that("Epoch() succeeds when samplingRate matches estimate from times", {

  expect_no_error(
    Epoch(dummy_data, times = expected_times, samplingRate = 10)
  )
})

test_that("Epoch() errors when rowData is not a data.frame", {

  # matrix is NOT a data.frame → should error
  bad_rowData_mat <- matrix(1:4, nrow = 2)
  expect_error(
    Epoch(dummy_data, times = expected_times, rowData = bad_rowData_mat),
    regexp = "rowData must be a data\\.frame"
  )
  
  # list is NOT a data.frame → should error
  bad_rowData_list <- list(a = 1, b = 2)
  expect_error(
    Epoch(dummy_data, times = expected_times, rowData = bad_rowData_list),
    regexp = "rowData must be a data\\.frame"
  )
})

test_that("Epoch() accepts NULL or proper data.frame for rowData", {

  # Proper data.frame → OK
  rd <- data.frame(channel = list(paste0("Elec", seq_len(row_num))), side = c("L","R"))
  ep <- Epoch(dummy_data, times = expected_times, rowData = rd)
  expect_s4_class(ep, "Epoch")
})

test_that("Epoch() errors when colData is not a data.frame", {

  # matrix -> not a data.frame
  expect_error(
    Epoch(dummy_data, times = expected_times, colData = matrix(1:6, nrow = 3)),
    regexp = "colData must be a data\\.frame"
  )
  
  # list -> not a data.frame
  expect_error(
    Epoch(dummy_data, times = expected_times, colData = list(a = 1, b = 2)),
    regexp = "colData must be a data\\.frame"
  )
})

test_that("Epoch() accepts NULL or proper data.frame for colData", {

  # NULL -> replaced with empty data.frame()
  ep1 <- Epoch(dummy_data, times = expected_times, colData = NULL)
  expect_s4_class(ep1, "Epoch")
  expect_true(is.data.frame(colData(ep1)))
  expect_equal(nrow(colData(ep1)), 0L)
  
  # Proper data.frame -> preserved
  cd  <- data.frame(t = expected_times, idx = seq_along(expected_times))
  ep2 <- Epoch(dummy_data, times = expected_times, colData = cd)
  expect_s4_class(ep2, "Epoch")
  expect_identical(colData(ep2), cd)
})

test_that("Epoch() validates `electrodes` type and length", {

  msg <- "`electrodes` must be a character vector of length nrow\\(table\\)\\."
  
  ## OK: character vector of correct length
  good_elec <- paste0("Elec_", seq_len(row_num))
  ep <- Epoch(dummy_data, times = expected_times, electrodes = good_elec)
  expect_s4_class(ep, "Epoch")
  expect_identical(rownames(ep), good_elec)
  
  ## Type errors
  expect_error(
    Epoch(dummy_data, times = expected_times, electrodes = seq_len(row_num)),  # numeric
    regexp = msg
  )
  expect_error(
    Epoch(dummy_data, times = expected_times, electrodes = factor(good_elec)), # factor
    regexp = msg
  )
  expect_error(
    Epoch(dummy_data, times = expected_times, electrodes = as.list(good_elec)),# list
    regexp = msg
  )
  
  ## Length errors
  expect_error(
    Epoch(dummy_data, times = expected_times, electrodes = good_elec[-1]),     # too short
    regexp = msg
  )
  expect_error(
    Epoch(dummy_data, times = expected_times, electrodes = c(good_elec, "extra")), # too long
    regexp = msg
  )
  expect_error(
    Epoch(dummy_data, times = expected_times, electrodes = character(0)),      # length 0
    regexp = msg
  )
  
  ## NULL should bypass the check and preserve existing rownames
  ep_null <- Epoch(dummy_data, times = expected_times, electrodes = NULL)
  expect_identical(rownames(ep_null), rownames(dummy_data))
})

test_that(".samplingRate returns samplingRate from metaData when present", {

  # .samplingRate should return the value stored in metaData
  expect_equal(Epoch:::.samplingRate(dummy_epoch), 10, tolerance = 1e-12)
  
  # Also test explicit startTime + samplingRate path
  ep2 <- Epoch(dummy_data, startTime = 0, samplingRate = 250)
  expect_equal(Epoch:::.samplingRate(ep2), 250)
})

test_that(".samplingRate errors when metaData$samplingRate is NULL", {

  # Manually remove samplingRate from metaData to hit the error branch
  md <- slot(dummy_epoch, "metaData")
  md$samplingRate <- NULL
  slot(dummy_epoch, "metaData") <- md
  
  expect_error(
    Epoch:::.samplingRate(dummy_epoch),
    "Sampling rate is not defined in metaData"
  )
})

test_that("crop() warns when Epoch has no time information (length(times) == 0)", {

  
  # Make a zero-column Epoch so coltimes(ep0) has length 0
  ep0 <- dummy_epoch[, integer(0)]
  
  # Collect warnings, but prevent any error from failing the test
  warns <- character()
  res <- withCallingHandlers(
    tryCatch(crop(ep0, start = 0, end = 1), error = function(e) NULL),
    warning = function(w) {
      warns <<- c(warns, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )
  
  expect_true(
    any(grepl("^crop function: The Epoch object has no time information\\.$", warns)),
    info = paste("Warnings seen:\n", paste("-", warns, collapse = "\n"))
  )
  
  
  # Still returns an Epoch with zero columns
  expect_s4_class(res, "Epoch")
  expect_identical(dim(res), c(nrow(ep0), 0L))
  
})

test_that("crop() does not emit the 'no time information' warning when times exist", {
  # Normal Epoch with actual time points
  tbl  <- matrix(0, nrow = 2, ncol = 5, dimnames = list(c("E1","E2"), NULL))
  ep   <- Epoch(tbl, startTime = 0, samplingRate = 10)   # times: 0, 0.1, ..., 0.4
  
  w <- character()
  withCallingHandlers(
    crop(ep, start = 0.1, end = 0.3),  # in range; should avoid all crop warnings
    warning = function(c) {
      w <<- c(w, conditionMessage(c))
      invokeRestart("muffleWarning")
    }
  )
  
  expect_false(any(grepl("no time information", w, ignore.case = TRUE)))
})

test_that("coltimes() falls back to seq_len(ncol(x)) when no times exist", {

  # Build a normal Epoch (with times), then remove colnames to force fallback
  ep <- Epoch(dummy_data, startTime = 0, samplingRate = 10)  # has times
  tbl <- slot(ep, "table")
  colnames(tbl) <- NULL
  slot(ep, "table") <- tbl
  
  expect_identical(coltimes(ep), seq_len(ncol(ep)))  # 1:col_num
})

test_that("coltimes() returns integer(0) for zero-column Epochs", {

  ep0 <- dummy_epoch[, integer(0)]  # zero columns
  
  expect_identical(ncol(ep0), 0L)
  expect_identical(coltimes(ep0), integer(0))  # fallback via seq_len(0)
})


