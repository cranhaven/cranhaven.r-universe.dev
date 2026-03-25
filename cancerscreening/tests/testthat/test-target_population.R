test_that("target_population works correctly", {

  expect_error(get_cervical_target_population(), 'year')
  expect_error(get_breast_cbe_target_population(), 'year')
  expect_error(get_breast_mammogram_target_population(), 'year')
  expect_error(get_colorectal_target_population(), 'year')

  expect_error(get_cervical_target_population('dfdfdf'), 'year')
  expect_error(get_breast_cbe_target_population('dfdfdf'), 'year')
  expect_error(get_breast_mammogram_target_population('dfdfdf'), 'year')
  expect_error(get_colorectal_target_population('dfdfdf'), 'year')

  expect_error(get_cervical_target_population(2022, 'dfdfdf'), 'level')
  expect_error(get_breast_cbe_target_population(2022, 'dfdfdf'), 'level')
  expect_error(get_breast_mammogram_target_population(2022, 'dfdfdf'), 'level')
  expect_error(get_colorectal_target_population(2022, 'dfdfdf'), 'level')

  expect_no_error(get_cervical_target_population(2022))
  expect_no_error(get_breast_cbe_target_population(2022))
  expect_no_error(get_breast_mammogram_target_population(2022))
  expect_no_error(get_colorectal_target_population(2022))
})
