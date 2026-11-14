library(ptm)
context("S-Aromatic Motifs")

## ----------------------------------------------- ##
#            Testing the saro.dist                  #
## ----------------------------------------------- ##
test_that('saro.dist() works properly', {

  skip_on_cran()

  a <- saro.dist('1cll', rawdata = TRUE)
  b <- saro.dist('3ug0')
  c <- saro.dist('2lo1')
  d <- saro.dist('xxxx')

  expect_equal(typeof(a), 'list')
  expect_equal(class(a[[1]]), 'data.frame')
  expect_equal(nrow(a[[1]]), 9)
  expect_equal(ncol(a[[1]]), 10)
  expect_equal(class(a[[2]]), 'data.frame')
  expect_equal(nrow(a[[2]]), 9)

  expect_equal(typeof(b), 'list')
  expect_equal(class(b), 'data.frame')
  expect_equal(nrow(b), 3)
  expect_equal(ncol(b), 10)
  expect_true(is.na(b$Wd[1]))

  expect_true(is.null(d))
})


## ----------------------------------------------- ##
#            Testing the saro.geometry              #
## ----------------------------------------------- ##
test_that('saro.geometry() works properly', {

  skip_on_cran()

  a <- saro.geometry('1cll', rA = 145, rB = 141)
  b <- saro.geometry('3ug0', rA = 74, rB = 78)
  c <- saro.geometry(pdb = '1d0g', rA = 99, chainA = 'R', rB = 237, chainB = 'A')
  d <- saro.geometry('xxxx', rA = 1, rB = 5)

  expect_equal(typeof(a), 'list')
  expect_equal(class(a), 'data.frame')
  expect_equal(nrow(a), 2)
  expect_equal(ncol(a), 8)
  expect_equal(a$resid[1], 'MET')
  expect_equal(a$resid[2], 'PHE')
  expect_lt(a$length[2], 7)

  expect_equal(typeof(b), 'list')
  expect_equal(class(b), 'data.frame')
  expect_equal(nrow(b), 2)
  expect_equal(ncol(b), 8)
  expect_equal(b$resid[1], 'MET')
  expect_equal(b$resid[2], 'TYR')
  expect_lt(b$length[2], 7)

  expect_equal(typeof(c), 'list')
  expect_equal(class(c), 'data.frame')
  expect_equal(nrow(c), 2)
  expect_equal(ncol(c), 8)
  expect_equal(c$resid[1], 'MET')
  expect_equal(c$resid[2], 'TYR')
  expect_lt(c$length[2], 7)

  expect_true(is.null(d))
})


## ----------------------------------------------- ##
#            Testing the saro.motif                 #
## ----------------------------------------------- ##
test_that('saro.motif() works properly', {

  skip_on_cran()

  a <- saro.motif('1cll', onlySaro = FALSE)
  b <- saro.motif(pdb = '1d0g', threshold = 5)
  c <- saro.motif(pdb = '2lo1', threshold = 7) # No motifs
  d <- saro.motif('xxxx')

  expect_equal(class(a), 'data.frame')
  expect_equal(nrow(a), 10)
  expect_true(max(a$Length, na.rm = TRUE) < 7)

  expect_equal(class(b), 'data.frame')
  expect_equal(nrow(b), 12)
  expect_true(max(b$Length) < 5)

  expect_is(c, 'data.frame')
  expect_equal(nrow(c), 0)
  expect_equal(ncol(c), 4)

  expect_true(is.null(d))

})

