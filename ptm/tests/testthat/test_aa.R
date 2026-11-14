library(ptm)

## ----------------------------------------------- ##
#         Testing the function aa.at                #
## ----------------------------------------------- ##
test_that("aa.at() using named arguments with order", {

  skip_on_cran()
  skip_on_travis()

  a <- aa.at(at=28 , target='P01009' , uniprot=TRUE)
  b <- aa.at(at=500, target='P01009' , uniprot=TRUE)

  if (!is.null(a)){
    expect_is(a, 'character')
    expect_equal(a, "Q")
  }
  expect_is(b, 'NULL')
})

test_that("aa.at() using named arguments without order", {

  skip_on_cran()
  skip_on_travis()

  a <- aa.at(target='P01009' , uniprot=TRUE, at=28)
  b <- aa.at(at=500, uniprot=TRUE, target='P01009')

  if (!is.null(a)){
    expect_is(a, 'character')
    expect_equal(a, "Q")
    expect_equal(length(a), 1)
    expect_equal(nchar(a), 1)
  }
  expect_is(b, 'NULL')

})

test_that("aa.at using uniprot = FALSE",{

  expect_equal(aa.at(6, "MARSSRAM", FALSE), 'R')
  expect_is(aa.at(10, "MARSSRAM", FALSE), 'NULL')

})


## ---------------------------------------------- ##
#            Testing the function is.at            #
## ---------------------------------------------- ##
test_that("is.at() works properly", {

  skip_on_cran()
  skip_on_travis()

  a <- is.at(at = 5, target = 'MARTMTRAM', uniprot = FALSE)
  b <- is.at(28, 'P01009', aa = 'Q')
  c <- is.at(at =5, target = 'MARTMTRAM', aa = "P", uniprot = FALSE)
  d <- is.at(28, 'P01009', aa = 'P')
  e <- is.at(28, 'P010091')

  expect_is(a, 'logical')
  expect_true(a)

  if (!is.null(b)){
    expect_is(b, 'logical')
    expect_true(b)
  }

  if (!is.null(c)){
    expect_is(c, 'logical')
    expect_false(c)
  }

  if (!is.null(d)){
    expect_is(d, 'logical')
    expect_false(d)
  }

  expect_is(e, 'NULL')

})

## ---------------------------------------------- ##
#        Testing the function aa.comp              #
## ---------------------------------------------- ##
test_that("the function aa.comp() works properly", {

  skip_on_cran()
  skip_on_travis()

  a <- suppressWarnings(aa.comp("ACEEAGRKDNW", uniprot = FALSE))
  b <- aa.comp("P01009")
  c <- aa.comp('P010091')

  expect_is(a, 'list')
  expect_equal(dim(a[[1]]), c(20, 6))

  if (!is.null(b)){
    expect_is(b, 'list')
    expect_equal(dim(b[[1]]), c(20, 6))
    expect_equal(sum(b[[1]]$observed), 418)
    expect_equal(length(b[[2]]), 9)
    expect_lt(b[[2]]$p.value, 0.5)
  }

  expect_is(c, 'NULL')
})

