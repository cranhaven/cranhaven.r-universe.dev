test_that(desc = "Test the dates", {

  expect_equal(base::class(covid19swiss$date) == "Date", TRUE)
  expect_equal(base::min(covid19swiss$date) == as.Date("2020-01-24"), TRUE)
})


test_that(desc = "Test the structure", {

  expect_equal(base::ncol(covid19swiss) == 7, TRUE)
  expect_equal(base::nrow(covid19swiss) >= 6800, TRUE)


})
