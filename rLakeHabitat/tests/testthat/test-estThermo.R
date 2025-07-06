#estThermo test

#load test data
prof <- read.csv(system.file("extdata", "example_profile_data.csv", package = 'rLakeHabitat')) %>%
  dplyr::mutate(date = as.Date(date))

#create incorrect data frame
wrong <- as.data.frame(matrix(data = NA, nrow = 1, ncol = 4))
colnames(wrong) <- c("date", "site", "depth", "temp")
wrong[1,1:4] <- c(1)
wrong$site <- as.numeric(wrong$site)
vect <- list(wrong)

#input check
test_that("estThermo input data check", {
  expect_error(estThermo(vect, site, date, depth, temp, combine = "all"), info = "data must be a data.frame!")
  expect_error(estThermo(wrong, site = 1, date, depth, temp, combine = "all"), info = "site must be a character giving the site column name")
  expect_error(estThermo(wrong, site = "sites", date, depth, temp, combine = "all"), info = "The value of site does not appear to be a valid column name")
  expect_error(estThermo(wrong, site, date = 1, depth, temp, combine = "all"), info = "date must be a character giving the date column name")
  expect_error(estThermo(wrong, site, date = "dates", depth, temp, combine = "all"), info = "The value of date does not appear to be a valid column name")
  expect_error(estThermo(wrong, site, date, depth, temp, combine = "all"), info = "data in date column is not formatted as date")
  expect_error(estThermo(wrong, site, date, depth = 1, temp, combine = "all"), info = "depth must be a character giving the depth column name")
  expect_error(estThermo(wrong, site, date, depth = "depths", temp, combine = "all"), info = "The value of depth does not appear to be a valid column name")
  expect_error(estThermo(wrong, site, date, depth, temp, combine = "all"), info = "data in depth column is not numeric")
  expect_error(estThermo(wrong, site, date, depth, temp = 1, combine = "all"), info = "temp must be a character giving the temp column name")
  expect_error(estThermo(wrong, site, date, depth, temp = "temps", combine = "all"), info = "The value of temp does not appear to be a valid column name")
  expect_error(estThermo(wrong, site, date, depth, temp, combine = "all"), info = "data in temp column is not numeric")
  expect_error(estThermo(wrong, site, date, depth, temp, combine = 1), info = "combine must be either 'sites', 'dates', or 'all'")
  expect_error(estThermo(wrong, site, date, depth, temp, combine = "test"), info = "combine must be either 'sites', 'dates', or 'all'")
})

#test output
test_that("estThermo output check", {
  expect_s3_class(estThermo(prof, site = "site", date = "date", depth = "depth", temp = "temp", combine = "sites"), class = "data.frame")
  expect_s3_class(estThermo(prof, site = "site", date = "date", depth = "depth", temp = "temp", combine = "dates"), class = "data.frame")
  expect_s3_class(estThermo(prof, site = "site", date = "date", depth = "depth", temp = "temp", combine = "all"), class = "data.frame")
})
