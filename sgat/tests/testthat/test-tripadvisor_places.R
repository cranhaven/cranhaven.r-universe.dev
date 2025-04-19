testthat::skip_on_cran()
testthat::skip_if_offline(host = "r-project.org")

try(initialization_sgat(), silent = TRUE)

test_that("gives correct character vector if information is found", {
  skip_if(!exists("remDr"), "Server not initialized")
  skip_on_cran()
  skip_if_offline(host = "r-project.org")
  pinamar <- tripadvisor_places("Pinamar, Argentina", 10)
  expect_type(pinamar, "character")
  expect_length(pinamar, 10)
})

test_that("gives NA if information is not found", {
  skip_if(!exists("remDr"), "Server not initialized")
  skip_on_cran()
  skip_if_offline(host = "r-project.org")
  bad_example <- tripadvisor_places("bad examples")
  expect_type(bad_example, "character")
  expect_length(bad_example, 1)
})

test_that("error if ciudad is not typed", {
  if(!exists("remDr")){
    remDr <- 1
  }
  expect_error(tripadvisor_places())
})
