test_that("function works for each input type", {
  d <- getAffectedRCW("2007-08", "HB 1001", type = "df")
  l <- getAffectedRCW("2007-08", "HB 1001", type = "list")
  x <- getAffectedRCW("2007-08", "HB 1001", type = "xml")

  expect_true(any(class(d) == "data.frame",is.null(d)))
  expect_true(any(typeof(l) == "list",is.null(l)))
  expect_true(any(typeof(x[[1]]) == "externalptr",is.null(x)))
})

test_that("function handles vector inputs", {
  bills <- c("HB 1001", "HB 1002")

  d <- getAffectedRCW("2007-08", bills, type = "df")
  l <- getAffectedRCW("2007-08", bills, type = "list")
  x <- getAffectedRCW("2007-08", bills, type = "xml")

  expect_true(any(identical(unique(d$BillId),bills),is.null(d)))
  expect_true(any(identical(names(l),bills),is.null(l)))
  expect_true(any(identical(names(x),paste("2007-08", bills, sep = "//")),is.null(x)))
})

test_that("function checks for proper formatting", {
  expect_error(getAffectedRCW("2007-2008", "HB 1001"))
  expect_error(getAffectedRCW("2007-08", "1001"))
  expect_error(getAffectedRCW("1989-90", "HB 1001"))
})
