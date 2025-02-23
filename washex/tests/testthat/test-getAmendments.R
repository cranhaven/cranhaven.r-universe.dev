test_that("function works for each input type", {
  d <- getAmendments("2007-08", "1001", type = "df")
  l <- getAmendments("2007-08", "1001", type = "list")
  x <- getAmendments("2007-08", "1001", type = "xml")

  expect_true(any(class(d) == "data.frame",is.null(d)))
  expect_true(any(typeof(l) == "list",is.null(l)))
  expect_true(any(typeof(x[[1]]) == "externalptr",is.null(x)))
})

test_that("function handles vector inputs", {
  bills <- c("1001", "1002")

  d <- getAmendments("2007-08", bills, type = "df")
  l <- getAmendments("2007-08", bills, type = "list")
  x <- getAmendments("2007-08", bills, type = "xml")

  expect_true(any(identical(unique(d$BillNumber),bills),is.null(d)))
  expect_true(any(identical(names(l),bills),is.null(l)))
  expect_true(any(identical(names(x),paste("2007-08", bills, sep = "//")),is.null(x)))
})

test_that("function checks for proper formatting", {
  expect_error(getAmendments("2007-2008", "1001"))
  expect_error(getAmendments("2007-08", "HB 1001"))
  expect_error(getAmendments("2007-08", "10o1"))
  expect_error(getAmendments("1989-90", "1001"))
})

test_that("function handles integer inputs", {
  x <- getAmendments("2007-08", "1001")
  y <- getAmendments("2007-08", 1001)

  expect_true(any(x==y,is.null(x),is.null(y)))
})
