test_that("function works for each input type", {
  d <- getCommittees("2007-08", type = "df")
  l <- getCommittees("2007-08", type = "list")
  x <- getCommittees("2007-08", type = "xml")

  expect_true(any(class(d) == "data.frame",is.null(d)))
  expect_true(any(typeof(l) == "list",is.null(l)))
  expect_true(any(typeof(x[[1]]) == "externalptr",is.null(x)))
})

test_that("function handles vector inputs", {
  bienns <- c("2007-08", "2009-10")

  d <- getCommittees(bienns, type = "df")
  l <- getCommittees(bienns, type = "list")
  x <- getCommittees(bienns, type = "xml")

  expect_true(any(identical(unique(d$Biennium),bienns),is.null(d)))
  expect_true(any(identical(names(l),bienns),is.null(l)))
  expect_true(any(identical(names(x),bienns),is.null(x)))
})

test_that("function checks for proper formatting", {
  expect_error(getCommittees("2007-2008"))
  expect_error(getCommittees("1989-90"))
})
