# microbenchmark(
#   SEM_likelihood(
#     0.5,
#     generate_test_feature_standard_data(),
#     times, entities, dep_var
#   )
# )
# Unit: milliseconds
# min      lq       mean.    median   uq       max        neval
# 25.80372 26.05294 27.14582 26.22264 27.25252 36.02822   100

test_that("SEM likelihood is calculated correctly for default feature standardization parameters", {
  skip_on_os(c("windows", "linux"))
  set.seed(1)
  sem_value <- SEM_likelihood(
    0.5,
    generate_test_feature_standard_data(),
    times, entities, dep_var
  )
  expect_equal(sem_value, 133.223858)
})

test_that("SEM likelihood is calculated correctly for cross_sectional TRUE", {
  skip_on_os(c("windows", "linux"))
  set.seed(1)
  sem_value <- SEM_likelihood(
    0.5,
    generate_test_feature_standard_data(cross_sectional = TRUE),
    times, entities, dep_var
  )
  expect_equal(sem_value, 217.693805)
})

test_that("SEM likelihood is calculated correctly for cross_sectional TRUE and scale FALSE", {
  skip_on_os(c("windows", "linux"))
  set.seed(1)
  sem_value <- SEM_likelihood(
    0.5,
    generate_test_feature_standard_data(cross_sectional = TRUE, scale = FALSE),
    times, entities, dep_var
  )
  expect_equal(sem_value, 225.54665)
})

test_that("SEM likelihood is calculated correctly for cross_sectional FALSE and scale FALSE", {
  skip_on_os(c("windows", "linux"))
  set.seed(1)
  sem_value <- SEM_likelihood(
    0.5,
    generate_test_feature_standard_data(cross_sectional = FALSE, scale = FALSE),
    times, entities, dep_var
  )
  expect_equal(sem_value, 140.498138)
})

test_that("SEM likelihood is calculated incorrectly for specific data", {
  skip_on_os(c("windows", "linux"))
  set.seed(2)
  # TODO: That produces NaN for that particular seed.
  testthat::expect_warning(
    sem_value <- SEM_likelihood(
      0.5,
      generate_test_feature_standard_data(cross_sectional = FALSE, scale = FALSE),
      times, entities, dep_var
    )
  )
})
