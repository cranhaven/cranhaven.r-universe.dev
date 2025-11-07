test_that("mmic_pd_univaraite works", {
  skip_if_not_installed("mets")
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

  # Gauss Hermite quadrature nodes and weights from fastGHQuad::gaussHermiteData
  ghq_data <- list(
    node = c(-3.43615911883774, -2.53273167423279, -1.75668364929988, -1.03661082978951,
             -0.342901327223705, 0.342901327223705, 1.03661082978951, 1.75668364929988,
             2.53273167423279, 3.43615911883774),
    weight = c(7.6404328552326e-06, 0.00134364574678124, 0.0338743944554811, 0.240138611082314,
               0.610862633735326,0.610862633735326, 0.240138611082315, 0.033874394455481,
               0.00134364574678124, 7.64043285523265e-06))

  # setup the object for the computation
  mmcif_obj <- mmcif_data(
    ~ country - 1, prt_use, status, time, id, max_time,
    2L, strata = country, ghq_data = ghq_data)

  # previous estimates
  par <- c(0.727279974859164, 0.640534073288067, 0.429437766165371, 0.434367104339573,
           -2.4737847536253, -1.49576564624673, -1.89966050143904, -1.58881346649412,
           -5.5431198001029, -3.5328359024178, -5.82305147022587, -3.4531896212114,
           -5.29132887832377, -3.36106297109548, -6.03690322125729, -3.49516746825624,
           2.55000711185704, 2.71995985605891, 2.61971498736444, 3.05976391058032,
           -5.97173564860957, -3.37912051983482, -5.14324860374941, -3.36396780694965,
           -6.02337246348561, -3.03754644968859, -5.51267338700737, -3.01148582224673,
           2.69665543753264, 2.59359057553995, 2.7938341786374, 2.70689750644755,
           -0.362056555418564, 0.24088005091276, 0.124070380635372, -0.246152029808377,
           -0.0445628476462479, -0.911485513197845, -0.27911988106887, -0.359648419277058,
           -0.242711959678559, -6.84897302527358)

  # the test data we will use
  test_dat <- data.frame(country = factor("Norway", levels(prt_use$country)))

  # TODO: test that the density is correct

  # the CIFs add up to one minus the survival prob
  cum <- Vectorize(function(status){
    mmcif_pd_univariate(
      par = par, object = mmcif_obj, newdata = test_dat, cause = status,
      strata = country, ghq_data = ghq_data, time = 75, type = "cumulative")
  })

  expect_equal(1 - sum(cum(1:2)), cum(3L), tolerance = 1e-5)

  # the derivative integrates the the cumulative
  deriv <- Vectorize(function(ti, status){
    mmcif_pd_univariate(
      par = par, object = mmcif_obj, newdata = test_dat, cause = status,
      strata = country, ghq_data = ghq_data, time = ti, type = "derivative")
  })

  tol <- sqrt(.Machine$double.eps)
  expect_equal(integrate(deriv, 0, 75, status = 1L, rel.tol = tol)$value,
               cum(1L), tolerance = tol * 10)

  tol <- sqrt(.Machine$double.eps)
  expect_equal(integrate(deriv, 0, 75, status = 2L, rel.tol = tol)$value,
               cum(2L), tolerance = tol * 10)
})
