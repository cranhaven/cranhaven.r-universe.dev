test_that(
  'uniqueness_probability',
  expect_true(uniqueness_probability(400, 50) < 0.5)
)

test_that(
  'uniqueness_max_size',
  expect_equal(round(uniqueness_max_size(2048, 0.01)), 6)
)
