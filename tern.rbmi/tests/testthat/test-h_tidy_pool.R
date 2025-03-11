data(rbmi_test_data)
dat <- rbmi_test_data

testthat::test_that("h_tidy_pool is produced correctly", {
  result <- h_tidy_pool(dat$pars[1:3])[, -1]

  expected <- data.frame(
    c("-1.61581995766697", "-1.70762640404516"),
    c("0.486231596928312", "0.474957263044525"),
    c("-2.57577141468279", "-2.64531930504159"),
    c("-0.655868500651147", "-0.769933503048726"),
    c(NA, "-0.0918064463781925"),
    c(NA, "0.68262790574849"),
    c(NA, "-1.43949684096095"),
    c(NA, "1.25588394820457"),
    c(NA, "0.893177242657404"),
    c(NA, "0.056817249930957"),
    stringsAsFactors = FALSE
  ) %>% dplyr::mutate(dplyr::across(everything(), as.numeric))

  colnames(expected) <- c(
    "est", "se_est", "lower_cl_est", "upper_cl_est", "est_contr", "se_contr",
    "lower_cl_contr", "upper_cl_contr", "p_value", "relative_reduc"
  )
  testthat::expect_identical(result, expected, tolerance = 0.000001)
})
