context("rwl tidiers")

library(dplR)
data('ca533')
crn <- chron(ca533)

test_that("conversion followed by backconversion equals initial object", {
  expect_that(untidy_rwl(tidy_rwl(ca533)), equals(ca533))

  expect_that(untidy_crn(tidy_crn(crn)), equals(crn))

})
