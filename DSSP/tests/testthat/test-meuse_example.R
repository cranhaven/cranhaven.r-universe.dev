data("meuse.all", package = "gstat")

test_that("fitting model with coords specified work", {
  m <- DSSP(
    formula = log(zinc) ~ 1, data = meuse.all, N = 1000, function(x) -2 * log(1 + x),
    pars = c(0.001, 0.001), coords = ~ x + y
  )
  expect_true(class(m) == "dsspMod")
})

sp::coordinates(meuse.all) <- ~ x + y
N <- 1000

meuse.fit <- DSSP(
  formula = log(zinc) ~ 1, data = meuse.all[1:155, ], N = N, function(x) -2 * log(1 + x),
  pars = c(0.001, 0.001)
)
ETA <- meuse.fit$eta
DELTA <- meuse.fit$delta
NU <- meuse.fit$nu

test_that("fitting model work", {
  expect_true(is.numeric(ETA))
  expect_true(is.numeric(DELTA))
  expect_true(is.numeric(NU))
})

test_that("fitted values are good", {
  Yhat <- rowMeans(exp(NU * meuse.fit$y_scaling$scale + meuse.fit$y_scaling$center))
  Ytrue <- meuse.all$zinc[1:155]
  mse <- mean((Ytrue - Yhat)^2)
  expect_true(mse < 40000)
  expect_true(cor(Ytrue, Yhat) > 0.8)
})

test_that("predictions are good", {
  Y.pred <- predict(meuse.fit, meuse.all[156:164, ])
  Y.pred <- exp(Y.pred)
  Y.pred <- rowMeans(Y.pred)
  Y.true <- meuse.all$zinc[156:164]
  expect_true(mean((Y.true - Y.pred)^2) < 20000)
  expect_true(cor(Y.pred, Y.true) > 0.8)
})

test_that("summary runs without errors", {
  expect_true(class(summary(meuse.fit)) == "dsspModsummary")
})
