library(ptm)

## ---------------------------------------------- ##
#               Testing get.seq                    #
## ---------------------------------------------- ##
test_that("get.seq() works properly with UniProt",{

  skip_on_cran()
  skip_on_travis()

  a <- get.seq('P01009')
  b <- get.seq('P01009', as.string = FALSE)
  c <- get.seq('P010091')

  if(!is.null(a)){
    expect_is(a, 'character')
    expect_equal(nchar(a), 418)
  }

  if (!is.null(b)){
    expect_is(b, 'list')
    expect_equal(length(b[[1]]), 418)
    expect_equal(b[[1]][28], 'Q')
  }

  expect_is(c, 'NULL')
})

test_that("get.seq() works properly with MetOSite",{

  skip_on_cran()
  skip_on_travis()

  a <- get.seq('P01009', db = 'metosite')
  t <- get.seq('P01009', db = 'metosite', as.string = FALSE)

  if (!is.null(a)){
    expect_is(a, 'character')
    expect_equal(nchar(a), 394)
  }
  if (!is.null(t)){
    expect_is(t, 'list')
    expect_equal(length(t[[1]]), 394)
    expect_equal(t[[1]][28], 'P')
  }

})
