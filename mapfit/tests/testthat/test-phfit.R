test_that("phfit.point", {
  RNGkind(kind = "Mersenne-Twister")
  set.seed(1234)

  wsample <- rweibull(n=100, shape=2, scale=1)

  ## PH fitting for general PH
  (result1 <- phfit.point(ph=ph(2), x=wsample))

  ## PH fitting for CF1
  (result2 <- phfit.point(ph=cf1(2), x=wsample))

  ## PH fitting for hyper Erlang
  (result3 <- phfit.point(ph=herlang(3), x=wsample))

  ## mean
  ph.mean(result1$model)
  ph.mean(result2$model)
  ph.mean(result3$model)

  ## variance
  ph.var(result1$model)
  ph.var(result2$model)
  ph.var(result3$model)

  ## up to 5 moments
  ph.moment(5, result1$model)
  ph.moment(5, result2$model)
  ph.moment(5, result3$model)
})

test_that("phfit.group", {
  RNGkind(kind = "Mersenne-Twister")
  set.seed(1234)

  ## make sample
  wsample <- rweibull(n=100, shape=2, scale=1)
  wgroup <- hist(x=wsample, breaks="fd", plot=FALSE)

  ## PH fitting for general PH
  (result1 <- phfit.group(ph=ph(2), counts=wgroup$counts, breaks=wgroup$breaks))

  ## PH fitting for CF1
  (result2 <- phfit.group(ph=cf1(2), counts=wgroup$counts, breaks=wgroup$breaks))

  ## PH fitting for hyper Erlang
  (result3 <- phfit.group(ph=herlang(3), counts=wgroup$counts, breaks=wgroup$breaks))

  ## mean
  ph.mean(result1$model)
  ph.mean(result2$model)
  ph.mean(result3$model)

  ## variance
  ph.var(result1$model)
  ph.var(result2$model)
  ph.var(result3$model)

  ## up to 5 moments
  ph.moment(5, result1$model)
  ph.moment(5, result2$model)
  ph.moment(5, result3$model)
})

test_that("phfit.density", {
  ## PH fitting for general PH
  (result1 <- phfit.density(ph=ph(2), f=dnorm, mean=3, sd=1))

  ## PH fitting for CF1
  (result2 <- phfit.density(ph=cf1(2), f=dnorm, mean=3, sd=1))

  ## PH fitting for hyper Erlang
  (result3 <- phfit.density(ph=herlang(3), f=dnorm, mean=3, sd=1))

  ## mean
  ph.mean(result1$model)
  ph.mean(result2$model)
  ph.mean(result3$model)

  ## variance
  ph.var(result1$model)
  ph.var(result2$model)
  ph.var(result3$model)

  ## up to 5 moments
  ph.moment(5, result1$model)
  ph.moment(5, result2$model)
  ph.moment(5, result3$model)
})



