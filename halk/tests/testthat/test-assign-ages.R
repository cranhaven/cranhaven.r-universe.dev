test_that("assign_ages correctly estimates age from basic alk", {
  test_laa_data <- data.frame(
    age = c(2,2,3,3,4),
    length = c(10, 11, 12, 13, 14)
  )
  test_length_data <- c(10.2, 11.1, 12.5, 13.6, 14.5)
  test_alk <- make_alk(
    test_laa_data,
    min_age_sample_size = 1,
    min_age_groups = 1
  )
  test_est_ages <- assign_ages(test_length_data, test_alk)
  expect_equal(test_est_ages, c(2, 2, 3, 3, 4))
})
