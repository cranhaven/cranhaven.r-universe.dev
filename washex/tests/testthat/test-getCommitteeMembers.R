test_that("function works for each input type", {
  d <- getCommitteeMembers("2007-08", "House", "Rules", type = "df")
  l <- getCommitteeMembers("2007-08", "House", "Rules", type = "list")
  x <- getCommitteeMembers("2007-08", "House", "Rules", type = "xml")

  expect_true(any(class(d) == "data.frame",is.null(d)))
  expect_true(any(typeof(l) == "list",is.null(l)))
  expect_true(any(typeof(x[[1]]) == "externalptr",is.null(x)))
})

test_that("function handles vector inputs", {
  comms <- c("Rules", "Judiciary")

  d <- getCommitteeMembers("2007-08", "House", comms, type = "df")
  l <- getCommitteeMembers("2007-08", "House", comms, type = "list")
  x <- getCommitteeMembers("2007-08", "House", comms, type = "xml")

  expect_true(any(identical(unique(d$CommitteeName),comms),is.null(d)))
  expect_true(any(identical(names(l),comms),is.null(l)))
  expect_true(any(identical(names(x),paste("2007-08", "House", comms, sep = "//")),is.null(x)))
})

test_that("function checks for proper formatting", {
  expect_error(getCommitteeMembers("1989-90", "House", "Rules"))
  expect_error(getCommitteeMembers("2007-2008", "House", "Rules"))
})

test_that("function properly addresses capitalization", {
  x <- getCommitteeMembers("2007-08", "house", "Rules")
  y <- getCommitteeMembers("2007-08", "House", "Rules")

  expect_true(any(x==y,is.null(x),is.null(y)))
  expect_equal(x, y)
})
