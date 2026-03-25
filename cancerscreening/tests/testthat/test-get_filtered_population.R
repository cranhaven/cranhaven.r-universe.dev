test_that("get_filtered_population works correct", {

  expect_error(get_filtered_population(), 'year')
  expect_error(get_filtered_population(2022), 'min_age')
  expect_error(get_filtered_population(2022, 5), 'max_age')
  expect_error(get_filtered_population(2022, 5, 34, level='invalid'), 'level')
  expect_no_error(get_filtered_population(2022, 5, 34, level='county'),)
  expect_error(get_filtered_population(2022, 5, 4, pop_sex='other'), 'pop_sex')
  expect_no_error(get_filtered_population(2022, 5, 4, pop_sex='both'))

  expect_error(get_filtered_population(c(2022, 2023), 5, 30), 'year')
  expect_error(get_filtered_population('2022', 5, 30),'year')
  expect_error(get_filtered_population(2022, '5', 30), 'min_age')
  expect_error(get_filtered_population(2022, 5, '55'),'max_age')
})
