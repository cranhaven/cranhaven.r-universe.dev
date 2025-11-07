test_that("mmcif_x function calls in the manual pages gives the same", {
  skip_if_not_installed("mets")

  # prepare the data
  library(mets)
  data(prt)

  # truncate the time
  max_time <- 90
  prt <- within(prt, {
    status[time >= max_time] <- 0
    time <- pmin(time, max_time)
  })

  # select the DZ twins and re-code the status
  prt_use <- subset(prt, zyg == "DZ") |>
    transform(status = ifelse(status == 0, 3L, status))

  # randomly sub-sample
  set.seed(1)
  prt_use <- subset(
    prt_use, id %in% sample(unique(id), length(unique(id)) %/% 10L))

  ghq_data <- list(
    node = c(-4.49999070730939, -3.66995037340445, -2.9671669279056, -2.32573248617386, -1.71999257518649, -1.13611558521092, -0.565069583255576, -3.84143836181876e-16, 0.565069583255576, 1.13611558521092, 1.71999257518649, 2.32573248617386, 2.9671669279056, 3.66995037340445, 4.49999070730939),
    weight = c(1.52247580425352e-09, 1.05911554771106e-06, 0.0001000044412325, 0.00277806884291276, 0.0307800338725461, 0.158488915795936, 0.412028687498898, 0.564100308726418, 0.412028687498898, 0.158488915795935, 0.030780033872546, 0.00277806884291278, 0.0001000044412325, 1.05911554771107e-06, 1.52247580425352e-09))

  mmcif_obj <- mmcif_data(
    ~ country - 1, prt_use, status, time, id, max_time,
    2L, strata = country, ghq_data = ghq_data)

  # get the staring values
  n_threads <- 2L
  start_vals <- mmcif_start_values(mmcif_obj, n_threads = n_threads)
  expect_snapshot_value(
    start_vals, cran = TRUE, style = "json2", tolerance = 1e-4)

  # mmcif_logLik(
  #   mmcif_obj, start_vals$upper, is_log_chol = TRUE, n_threads = n_threads) |>
  #   dput()
  expect_equal(
    mmcif_logLik(
      mmcif_obj, start_vals$upper, is_log_chol = TRUE, n_threads = n_threads),
    -2389.42390562039, tolerance = 1e-6)

  expect_snapshot_value(
    mmcif_logLik_grad(
      mmcif_obj, start_vals$upper, is_log_chol = TRUE, n_threads = n_threads),
    cran = TRUE, style = "json2", tolerance = 1e-4)

  # estimate the parameters
  skip_on_cran()
  ests <- mmcif_fit(start_vals$upper, mmcif_obj, n_threads = n_threads)
  expect_snapshot_value(ests[c("par", "value")],
                        style = "serialize", tolerance = 1e-2)
})
