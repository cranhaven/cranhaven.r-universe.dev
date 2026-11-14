library(ptm)

## ---------------------------------------------- ##
#             Testing gracefully_fail              #
## ---------------------------------------------- ##
test_that('gracefully_fail() works properly', {

  skip_on_cran()

  a <- gracefully_fail("https://metosite.uma.es/api/sites/mapping/111/222222")
  b <- gracefully_fail("https://metosite.umap.es/api/sites/mapping/111/222222")
  c <- gracefully_fail("http://httpbin.org/status/404")
  d <- gracefully_fail("http://httpbin.org/delay/2", timeout(1))

  expect_is(a, "character")
  expect_true(is.null(b))
  expect_true(is.null(c))
  expect_true(is.null(d))


})
