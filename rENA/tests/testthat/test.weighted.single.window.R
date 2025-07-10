suppressMessages(library(rENA, quietly = T, verbose = F))
context("Test binary versus weighted accumulations on windows of 1");


fake.data <- function(rows = 30, num.units = 3, units = LETTERS[1:num.units], binary = T, num.codes = 3, codes = LETTERS[1:num.codes]) {
  if(is.logical(binary) && binary == F) {
    binary = runif(rows, 0.1, 1)
  } else if (!is.logical(binary) && !(length(binary) %in% c(1, rows))) {
    stop("binary must be T, F, a vector of length 1, or a vector with length equal to rows")
  }
  data.table(
    unit = sample(units, rows, replace=T),
    conv = sample(1:2, rows, replace=T),
    (sapply(codes, function(c) sample(0:1, rows, replace=T)) * binary)
  )
}

test_that("Verify binary/weighted accumulations with window > 1 are not equal", {
  dat = fake.data(binary = F)

  win.4.binary = ena.accumulate.data(
    units = dat[,"unit"], conversation = dat[,"conv"], codes = dat[,c("A","B","C")],
    window.size.back = 4
  )
  win.4.sum = ena.accumulate.data(
    units = dat[,"unit"], conversation = dat[,"conv"], codes = dat[,c("A","B","C")],
    window.size.back = 4,
    weight.by = sum
  )
  # expect_equal(object = any(as.matrix(win.4.binary$connection.counts) == as.matrix(win.4.sum$connection.counts)), expected = F)
})


test_that("Verify binary/weighted accumulations with window == 1 are not equal", {
  dat = fake.data(binary = F)

  win.1.binary = ena.accumulate.data(
    units = dat[,"unit"], conversation = dat[,"conv"], codes = dat[,c("A","B","C")],
    window.size.back = 1
  )
  win.1.sum = ena.accumulate.data(
    units = dat[,"unit"], conversation = dat[,"conv"], codes = dat[,c("A","B","C")],
    window.size.back = 1,
    weight.by = sum
  )

  # Single row binary and sum shouldn't equal
  # expect_false(object = all(win.1.binary$connection.counts == win.1.sum$connection.counts))
})

test_that("Verify binary/weighted accumulations with window == 1 are not equal", {
  dat = fake.data(binary = F)

  win.1.sum = ena.accumulate.data(
    units = dat[,"unit"], conversation = dat[,"conv"], codes = dat[,c("A","B","C")],
    window.size.back = 1,
    weight.by = sum
  )
  win.c.sum = ena.accumulate.data(
    units = dat[,"unit"], conversation = as.data.frame(rownames(dat)), codes = dat[,c("A","B","C")],
    window = "C",
    weight.by = sum
  )

  # Single row sum and conversations by line and weight.sum should be equal
  # expect_true(object = all(win.c.sum$connection.counts == win.1.sum$connection.counts))
})
