# Test that improper data structure will trigger an error or warning
# Such as missing required columns, mismatch of subjects
test_that("data check works", {
  data("regddm_data")

  # test that duplicate id will trigger an error
  data1 = regddm_data$data1
  data2 = regddm_data$data2
  data1$id[1:2] = c(1,1)
  model = list()
  expect_error(regddm(data1, data2, model, fit_model = FALSE))

  # test that missing id will trigger an error
  data1 = dplyr::select(regddm_data$data1, -"id")
  expect_error(regddm(data1, data2, model, fit_model = FALSE))

  data1 = regddm_data$data1
  data2 = dplyr::select(regddm_data$data2, -"id")
  expect_error(regddm(data1, data2, model, fit_model = FALSE))

  data2 = dplyr::select(regddm_data$data2, -"rt")
  expect_error(regddm(data1, data2, model, fit_model = FALSE))

  data2 = dplyr::select(regddm_data$data2, -"response")
  expect_error(regddm(data1, data2, model, fit_model = FALSE))

  # test that improper coding of response will trigger an error
  data2 = dplyr::mutate(regddm_data$data2, response = ifelse(response == 1, "upper", "lower"))
  expect_error(regddm(data1, data2, model, fit_model = FALSE))

  # test that abnormal value of response time rt will trigger an warning
  data2 = dplyr::mutate(regddm_data$data2, rt = rt* 100)
  expect_warning(regddm(data1, data2, model, fit_model = FALSE))

  # test that missing values in data2 will trigger an error
  data2 = regddm_data$data2
  data2[["memload"]][1] = NA
  model = list(v ~ memload)
  expect_error(regddm(data1, data2, model, fit_model = FALSE))

  # test that data1 and data2 must have exactly the same subjects
  data1 = regddm_data$data1
  data2 = regddm_data$data2
  data1[["id"]][1] = 1001
  expect_error(regddm(data1, data2, model, fit_model = FALSE))

})

# Test that improper specification of models will trigger an error
test_that("model check works", {

  data("regddm_data")
  data1 = regddm_data$data1
  data2 = regddm_data$data2

  # check that including a covariate that does not exist will trigger an error
  model = list(
    iq ~ c
  )
  expect_error(regddm(data1, data2, model, fit_model = FALSE))

  # check that including a non-existing trial level variable will trigger an error
  model = list(
    v ~ x
  )
  expect_error(regddm(data1, data2, model, fit_model = FALSE))

  # check that improper naming of variables will trigger an error
  data1 = dplyr::rename(regddm_data$data1, v_0 = age)
  model = list(iq ~ v_0)
  expect_error(regddm(data1, data2, model, fit_model = FALSE))

  # check that multiple regression models will trigger an error
  data1 = regddm_data$data1
  model = list(iq ~ v_0, age ~ v_0)
  expect_error(regddm(data1, data2, model, fit_model = FALSE))

})

# Test that the simulated data generation function works
# this function is mostly used as a testing tool for RegDDM
test_that("simulated data generation works", {
  expect_no_error(
    generate_sim_data()
  )
  expect_no_error(
    generate_sim_data(y_family = "bernoulli", n = 50, n_each = 50)
  )
  expect_no_error(
    generate_sim_data(y_family = "poisson", n_xvar = 0, beta_v_0 = 1, sigma_v = 0.3)
  )
})


# the following codes needs to fit stan models, which takes a long time
# some tests will not be performed on CRAN but locally.
# warnings are suppressed because it doesn't influence RegDDM functionality
# only a few chains were run to save time
test_that("example code works", {suppressWarnings({
  skip_on_cran()

  # Example analysis over the simulated tutorial dataset.
  data(regddm_data)
  model = list(v ~ memload, iq ~ v_memload + v_0 + age + education)
  expect_no_error(regddm(
       regddm_data$data1,
       regddm_data$data2,
       model,
       chains = 1,
       cores = 1,
       iter = 10,
       warmup = 5
    ))

  # Alternatively, subjects' DDM parameters can be used as the outcome.
  model = list(v ~ memload, v_memload ~ gender)
  expect_no_error(regddm(
    regddm_data$data1,
    regddm_data$data2,
    model,
    chains = 1,
    cores = 1,
    iter = 10,
    warmup = 5
  ))
})})

test_that("summary function works", {suppressWarnings({
  skip_on_cran()

  # Example analysis over the simulated tutorial dataset.
  data(regddm_data)
  model = list()
  expect_no_error(summary(regddm(
    regddm_data$data1,
    regddm_data$data2,
    model,
    chains = 1,
    cores = 1,
    iter = 10,
    warmup = 5
  )))
})})

test_that("model with interaction works", {suppressWarnings({
  skip_on_cran()

  # interaction in trial-level variable
  sim_data = generate_sim_data(N = 30, n_xvar = 2, n_each = 100)
  model = list(
    v ~ x1 * x2,
    y ~ v_0 + v_x1_x2
  )
  expect_no_error(
    regddm(
      sim_data$data1,
      sim_data$data2,
      model,
      chains = 1,
      cores = 1,
      iter = 10,
      warmup = 5
    )
  )

  # interaction in subject-level variable
  sim_data = generate_sim_data(N = 30, n_xvar = 1, n_each = 100)
  model = list(
    v ~ x1,
    y ~ v_0 + v_x1 * c1
  )
  expect_no_error(
    regddm(
      sim_data$data1,
      sim_data$data2,
      model,
      chains = 1,
      cores = 1,
      iter = 10,
      warmup = 5
    )
  )
})})


test_that("model works for factor variables", {suppressWarnings({
  skip_on_cran()
  data("regddm_data")

  # factor in trial-level variable
  data1 = regddm_data$data1
  data2 = regddm_data$data2
  data2$memload = factor(data2$memload)

  model = list(
    v ~ memload,
    iq ~ v_memload
  )
  expect_no_error(
    regddm(
      regddm_data$data1,
      regddm_data$data2,
      model,
      chains = 1,
      cores = 1,
      iter = 10,
      warmup = 5
    )
  )

  # factor in subject-level variable
  data2 = regddm_data$data2

  model = list(
    iq ~ race + v_0
  )
  expect_no_error(
    regddm(
      regddm_data$data1,
      regddm_data$data2,
      model,
      chains = 1,
      cores = 1,
      iter = 10,
      warmup = 5
    )
  )

})})


test_that("model works for Bernoulli and Poisson family", {suppressWarnings({
  skip_on_cran()

  # for bernoulli family
  sim_data = generate_sim_data(N = 100, n_xvar = 0, n_each = 50, y_family = "bernoulli")
  model = list(y ~ v_0)
  expect_no_error(
    regddm(
      sim_data$data1,
      sim_data$data2,
      model,
      family = "bernoulli",
      chains = 1,
      cores = 1,
      iter = 10,
      warmup = 5
    )
  )

  # for poisson family
  sim_data = generate_sim_data(N = 50, n_xvar = 0, n_each = 50, y_family = "poisson")
  model = list(y ~ v_0)
  expect_no_error(
    regddm(
      sim_data$data1,
      sim_data$data2,
      model,
      family = "poisson",
      chains = 1,
      cores = 1,
      iter = 10,
      warmup = 5
    )
  )
})})


test_that("model works with prior = FALSE", {suppressWarnings({
  data("regddm_data")
  skip_on_cran()

  model = list()
  expect_no_error(
    regddm(
      regddm_data$data1,
      regddm_data$data2,
      model,
      prior =  FALSE,
      chains = 1,
      cores = 1,
      iter = 10,
      warmup = 5
    )
  )
})})




