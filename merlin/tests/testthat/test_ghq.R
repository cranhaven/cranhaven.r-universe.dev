testthat::context("ghq")

#------------------------------------------------------------------------------
# method: test gauss-hermite quadrature

# GAUSSIAN
#------------------------------------------------------------------------------
testthat::test_that("Test 1.1", {
  data("pbc.merlin", package = "merlin")
  lmemod <- nlme::lme(logb ~ 1, random = ~ 1 | id, data = pbc.merlin)
  lmemodres <- as.numeric(c(summary(lmemod)$tTable[1, 1], log(as.numeric(nlme::VarCorr(lmemod)[c(2, 1), 2])), lmemod$logLik))
  comp <- rep(0, length(lmemodres))
  mod <- merlin::merlin(
    model = list(logb ~ M1[id] * 1),
    levels = c("id"),
    family = "gaussian",
    data = pbc.merlin,
    control = list(ip = 25)
  )
  modres <- as.numeric(c(mod$coefficients, mod$loglikelihood))
  testthat::expect_equal(object = (abs(modres - lmemodres) / (abs(lmemodres) + 1)), expected = comp, tolerance = 5e-2)
})

testthat::test_that("Test 1.2a", {
  data("pbc.merlin", package = "merlin")
  lmemod <- nlme::lme(logp ~ year, random = ~ 1 | id, data = pbc.merlin)
  lmemodres <- as.numeric(c(summary(lmemod)$tTable[c(2, 1), 1], log(as.numeric(nlme::VarCorr(lmemod)[c(2, 1), 2])), lmemod$logLik))
  comp <- rep(0, length(lmemodres))
  mod <- merlin::merlin(
    model = list(logp ~ year + M1[id] * 1),
    levels = c("id"),
    family = "gaussian",
    data = pbc.merlin,
    control = list(ip = 25)
  )
  modres <- as.numeric(c(mod$coefficients, mod$loglikelihood))
  testthat::expect_equal(object = (abs(modres - lmemodres) / (abs(lmemodres) + 1)), expected = comp, tolerance = 1e-2)
})

testthat::test_that("Test 1.3a", {
  testthat::skip_on_cran()
  data("pbc.merlin", package = "merlin")
  lmemod <- nlme::lme(logb ~ year, random = list(id = nlme::pdDiag(~year)), data = pbc.merlin)
  lmemodres <- as.numeric(c(summary(lmemod)$tTable[c(2, 1), 1], log(as.numeric(nlme::VarCorr(lmemod)[c(3, 1, 2), 2])), lmemod$logLik))
  comp <- rep(0, length(lmemodres))
  mod <- merlin::merlin(
    model = list(logb ~ year + M1[id] * 1 + year:M2[id] * 1),
    levels = c("id"),
    family = "gaussian",
    data = pbc.merlin,
    control = list(ip = 9)
  )
  modres <- as.numeric(c(mod$coefficients, mod$loglikelihood))
  testthat::expect_equal(object = (abs(modres - lmemodres) / (abs(lmemodres) + 1)), expected = comp, tolerance = 1e-1)
})

testthat::test_that("Test 1.3b", {
  data("pbc.merlin", package = "merlin")
  pbc.merlin$year_2 <- pbc.merlin$year^2
  lmemod <- nlme::lme(logp ~ year + year_2, random = ~ 1 | id, data = pbc.merlin)
  lmemodres <- as.numeric(c(summary(lmemod)$tTable[c(2, 3, 1), 1], log(as.numeric(nlme::VarCorr(lmemod)[c(2, 1), 2])), lmemod$logLik))
  comp <- rep(0, length(lmemodres))
  mod <- merlin::merlin(
    model = list(logp ~ year + fp(year, powers = c(2)) + M1[id] * 1),
    timevar = "year",
    levels = c("id"),
    family = "gaussian",
    data = pbc.merlin,
    control = list(ip = 11)
  )
  modres <- as.numeric(c(mod$coefficients, mod$loglikelihood))
  testthat::expect_equal(object = (abs(modres - lmemodres) / (abs(lmemodres) + 1)), expected = comp, tolerance = 5e-2)
})

testthat::test_that("Test 1.4a", {
  data("pbc.merlin", package = "merlin")
  pbc.merlin$year <- pbc.merlin$year + 0.001
  pbc.merlin$logyear <- log(pbc.merlin$year)
  lmemod <- nlme::lme(logb ~ logyear, random = ~ 1 | id, data = pbc.merlin)
  lmemodres <- as.numeric(c(summary(lmemod)$tTable[c(2, 1), 1], log(as.numeric(nlme::VarCorr(lmemod)[c(2, 1), 2])), lmemod$logLik))
  comp <- rep(0, length(lmemodres))
  mod <- merlin::merlin(
    model = list(logb ~ fp(year, powers = c(0)) + M1[id] * 1),
    levels = c("id"),
    family = "gaussian",
    data = pbc.merlin,
    control = list(ip = 41)
  )
  modres <- as.numeric(c(mod$coefficients, mod$loglikelihood))
  testthat::expect_equal(object = (abs(modres - lmemodres) / (abs(lmemodres) + 1)), expected = comp, tolerance = 1e-2)
  pbc.merlin$year <- pbc.merlin$year - 0.001
})

# testthat::test_that("Test 1.4b", {
#   testthat::skip_on_cran()
#   data("pbc.merlin", package = "merlin")
#   pbc.merlin$year <- pbc.merlin$year + 0.001
#   pbc.merlin$logyear <- log(pbc.merlin$year)
#   lmemod <- nlme::lme(logb ~ logyear, random = list(id = nlme::pdDiag(~logyear)), data = pbc.merlin)
#   lmemodres <- as.numeric(c(summary(lmemod)$tTable[c(2, 1), 1], log(as.numeric(nlme::VarCorr(lmemod)[c(3, 1, 2), 2])), lmemod$logLik))
#   comp <- rep(0, length(lmemodres))
#   mod <- merlin::merlin(
#     model = list(logb ~ fp(year, powers = c(0)) + M1[id] * 1 + fp(year, powers = c(0)):M2[id] * 1),
#     levels = c("id"),
#     family = "gaussian",
#     data = pbc.merlin,
#     timevar = "year",
#     control = list(ip = 25)
#   )
#   modres <- as.numeric(c(mod$coefficients, mod$loglikelihood))
#   testthat::expect_equal(object = (abs(modres - lmemodres) / (abs(lmemodres) + 1)), expected = comp, tolerance = 5e-2)
#   pbc.merlin$year <- pbc.merlin$year - 0.001
# })

# testthat::test_that("Test 1.5a", {
#   data("pbc.merlin", package = "merlin")
#   statares <- c(.0124268, .0030361, -.0011979, .000164, 2.368558, log(.0846013), log(.0711746), 1802.8308)
#   comp <- rep(0, length(statares))
#   mod <- merlin::merlin(
#     model = list(logp ~ rcs(year, df = 4) + M1[id] * 1),
#     levels = c("id"),
#     family = "gaussian",
#     data = pbc.merlin,
#     timevar = "year",
#     control = list(ip = 25)
#   )
#   modres <- as.numeric(c(mod$coefficients, mod$loglikelihood))
#   testthat::expect_equal(object = (abs(modres - statares) / (abs(statares) + 1)), expected = comp, tolerance = 5e-2)
# })

# BERNOULLI
#------------------------------------------------------------------------------
testthat::test_that("Test 2.1", {
  data("pbc.merlin", package = "merlin")
  pbc.merlin$group <- round(pbc.merlin$age / 10)
  statares <- c(.0141224, log(1.121833), -205.85846)
  comp <- rep(0, length(statares))
  mod <- merlin::merlin(
    model = list(died ~ M1[group] * 1),
    levels = c("group"),
    family = "bernoulli",
    covariance = "identity",
    data = pbc.merlin,
    control = list(ip = 15)
  )
  modres <- as.numeric(c(mod$coefficients, mod$loglikelihood))
  testthat::expect_equal(object = (abs(modres - statares) / (abs(statares) + 1)), expected = comp, tolerance = 1e-3)
})

testthat::test_that("Test 2.2", {
  data("pbc.merlin", package = "merlin")
  pbc.merlin$group <- round(pbc.merlin$age / 10)
  statares <- c(14.39803, -34.05126, log(1.051053), -171.97422)
  comp <- rep(0, length(statares))
  mod <- merlin::merlin(
    model = list(died ~ logp + M1[group] * 1),
    levels = c("group"),
    family = "bernoulli",
    covariance = "identity",
    data = pbc.merlin,
    control = list(ip = 15)
  )
  modres <- as.numeric(c(mod$coefficients, mod$loglikelihood))
  testthat::expect_equal(object = (abs(modres - statares) / (abs(statares) + 1)), expected = comp, tolerance = 1e-3)
})

testthat::test_that("Test 2.3", {
  data("pbc.merlin", package = "merlin")
  pbc.merlin$group <- round(pbc.merlin$age / 10)
  statares <- c(12.3279, 2.888777, -.7888563, -29.56543, log(1.364638), -150.64338)
  comp <- rep(0, length(statares))
  mod <- merlin::merlin(
    model = list(died ~ logp + logb + logp:logb + M1[group] * 1),
    levels = c("group"),
    family = "bernoulli",
    covariance = "identity",
    data = pbc.merlin,
    control = list(ip = 15)
  )
  modres <- as.numeric(c(mod$coefficients, mod$loglikelihood))
  testthat::expect_equal(object = (abs(modres - statares) / (abs(statares) + 1)), expected = comp, tolerance = 1e-2)
})

# EXPONENTIAL
#------------------------------------------------------------------------------
testthat::test_that("Test 8.1a", {
  data("pbc.merlin", package = "merlin")
  pbc.merlin$group <- round(pbc.merlin$age / 10)
  statares <- c(-2.567219, log(.6763707), -504.46228)
  comp <- rep(0, length(statares))
  mod <- merlin::merlin(
    model = list(Surv(stime, died) ~ M1[group] * 1),
    levels = c("group"),
    family = "exponential",
    covariance = "identity",
    data = pbc.merlin,
    control = list(ip = 21)
  )
  modres <- as.numeric(c(mod$coefficients, mod$loglikelihood))
  testthat::expect_equal(object = (abs(modres - statares) / (abs(statares) + 1)), expected = comp, tolerance = 5e-2)
})

testthat::test_that("Test 8.2a", {
  data("pbc.merlin", package = "merlin")
  pbc.merlin$trt <- as.numeric(pbc.merlin$trt)
  pbc.merlin$group <- round(pbc.merlin$age / 10)
  statares <- c(-.1259977, -2.438703, log(.9471302), -504.30085)
  comp <- rep(0, length(statares))
  mod <- merlin::merlin(
    model = list(Surv(stime, died) ~ trt + M1[group] * 1),
    levels = c("group"),
    family = "exponential",
    covariance = "identity",
    data = pbc.merlin,
    control = list(ip = 15)
  )
  modres <- as.numeric(c(mod$coefficients, mod$loglikelihood))
  testthat::expect_equal(object = (abs(modres - statares) / (abs(statares) + 1)), expected = comp, tolerance = 1e-4)
})

# GOMPERTZ
# #------------------------------------------------------------------------------
# test_that("Test 9.1", {
#   pbc.merlin$group <- round(pbc.merlin$age/10)
#   statares <- c(-2.608133,.2697497,log(.2006814),-2902.1221)
#   mod <- merlin(model=list(Surv(stime,died) ~ M1[group]*1),
#                   ip=5,
#                   timevar="stime",
#                   levels=c("group"),
#                   family="gompertz",
#                   data=pbc.merlin)
#   modres <- as.numeric(c(mod$coefftable[,1],mod$loglikelihood))
#   expect_equal(modres, statares, tolerance=1e-4)
# })

# # test_that("Test 9.2", { # doesn't run
# #   statares <- c(-2.408634,-.4532081,.2776478,log(.1825546),-2877.0077)
# #   mod <- merlin(model=list(Surv(stime,status) ~ trt + M1[practice]*1),
# #                   ip=25,
# #                   levels=c("practice"),
# #                   family="gompertz",
# #                   covariance="identity",
# #                   data=data_surv3)
# #   modres <- as.numeric(c(mod$coefftable[,1],mod$loglikelihood))
# #   expect_equal(modres, statares, tolerance=1e-4)
# # })


# WEIBULL
# ------------------------------------------------------------------------------
testthat::test_that("Test 11.1", {
  data("pbc.merlin", package = "merlin")
  pbc.merlin$trt <- as.numeric(pbc.merlin$trt)
  pbc.merlin$group <- round(pbc.merlin$age / 10)
  statares <- c(-2.717169, .098435, log(.5982627), -502.90265)
  comp <- rep(0, length(statares))
  mod <- merlin::merlin(
    model = list(surv(stime, died) ~ M1[group] * 1),
    levels = c("group"),
    family = "weibull",
    data = pbc.merlin,
    control = list(ip = 5)
  )
  modres <- as.numeric(c(mod$coefficients, mod$loglikelihood))
  testthat::expect_equal(object = (abs(modres - statares) / (abs(statares) + 1)), expected = comp, tolerance = 1e-4)
})

testthat::test_that("Test 11.2", {
  data("pbc.merlin", package = "merlin")
  pbc.merlin$trt <- as.numeric(pbc.merlin$trt)
  pbc.merlin$group <- round(pbc.merlin$age / 10)
  statares <- c(-.1289869, -2.650166, .0993116, log(.6112077), -502.62058)
  comp <- rep(0, length(statares))
  mod <- merlin::merlin(
    model = list(Surv(stime, died) ~ trt + M1[group] * 1),
    levels = c("group"),
    family = "weibull",
    covariance = "identity",
    data = pbc.merlin,
    control = list(ip = 5)
  )
  modres <- as.numeric(c(mod$coefficients, mod$loglikelihood))
  testthat::expect_equal(object = (abs(modres - statares) / (abs(statares) + 1)), expected = comp, tolerance = 1e-3)
})

# test_that("Test 11.3", {
#   statares <- c(-.1289869,-2.650166,.0993116,log(.6112077),-502.62058)
#   comp <- rep(0,length(statares))
#   mod <- merlin(model=list(Surv(stime,died) ~ trt + M1[group]*1),
#                 ip=5,
#                 levels=c("group"),
#                 family="weibull",
#                 covariance="identity",
#                 data=sim3)
#   modres <- as.numeric(c(mod$coefftable[,1],mod$loglikelihood))
#   expect_equal((abs(modres-statares)/(abs(statares)+1)),comp,tolerance=1e-2)
# })


# ROYSTON-PARMAR MODELS
#------------------------------------------------------------------------------
testthat::test_that("Test 13.1", {
  data("pbc.merlin", package = "merlin")
  statares <- c(.94136462, .02136717, -.02597223, -1.0880669)
  comp <- rep(0, length(statares))
  mod <- merlin::merlin(
    model = list(Surv(stime, died) ~ srcs(stime, df = 3)),
    timevar = "stime",
    family = c("rp"),
    data = pbc.merlin,
    control = list(ip = 25)
  )
  modres <- as.numeric(c(mod$coefficients))
  testthat::expect_equal(object = (abs(modres - statares) / (abs(statares) + 1)), expected = comp, tolerance = 1e-4)
})

# test_that("Test 13.2", {
#   statares <- c(.04453543,.95872581,.01529192,-.02923702,-3.3843425)
#   comp <- rep(0,length(statares))
#   mod <- merlin(model=list(Surv(stime,died) ~ fp(age,powers=1) + rcs(stime,df=3,orthog=T)),
#                 ip=15,
#                 family=c("rp"),
#                 timevar="stime",
#                 data=pbc.merlin)
#   modres <- as.numeric(c(mod$coefftable[,1]))
#   expect_equal((abs(modres-statares)/(abs(statares)+1)),comp,tolerance=1e-2)
# })

testthat::test_that("Test 13.3", {
  data("pbc.merlin", package = "merlin")
  statares <- c(.01593183, .22151683, .00158001, -.02484696, -2.3727601)
  comp <- rep(0, length(statares))
  mod <- merlin::merlin(
    model = list(Surv(stime, died) ~ age:fp(stime, powers = c(0)) + rcs(stime, df = 3, orthog = T)),
    timevar = "stime",
    family = c("rp"),
    data = pbc.merlin,
    control = list(ip = 25)
  )
  modres <- as.numeric(c(mod$coefficients))
  testthat::expect_equal(object = (abs(modres - statares) / (abs(statares) + 1)), expected = comp, tolerance = 5e-2)
})


# GAUSSIAN - GAUSSIAN
#------------------------------------------------------------------------------
# testthat::test_that("Test 14.1a", {
#   testthat::skip_on_cran()
#   data("pbc.merlin", package = "merlin")
#   statares <- c(2.394502, log(.0923497), .8521971, log(.5701065), log(.0620281), log(.9758374), -418.43076)
#   comp <- rep(0, length(statares))
#   mod <- merlin::merlin(
#     model = list(
#       logp ~ M1[id] * 1,
#       logb ~ M2[id] * 1
#     ),
#     family = c("gaussian", "gaussian"),
#     levels = c("id"),
#     data = pbc.merlin,
#     control = list(ip = 25)
#   )
#   modres <- as.numeric(c(mod$coefficients, mod$loglikelihood))
#   testthat::expect_equal(object = (abs(modres - statares) / (abs(statares) + 1)), expected = comp, tolerance = 1e-4)
# })

testthat::test_that("Test 14.1b", {
  testthat::skip_on_cran()
  data("pbc.merlin", package = "merlin")
  statares <- c(2.400873, log(.1023105), 25.3716, .8531628, log(.5720524), log(.0388209), -438.76059)
  comp <- rep(0, length(statares))
  mod <- merlin::merlin(
    model = list(
      logp ~ M1[id] * 1,
      logb ~ M1[id]
    ),
    family = c("gaussian", "gaussian"),
    levels = c("id"),
    covariance = "identity",
    data = pbc.merlin,
    control = list(ip = 25)
  )
  modres <- as.numeric(c(mod$coefficients, mod$loglikelihood))
  testthat::expect_equal(object = (abs(modres - statares) / (abs(statares) + 1)), expected = comp, tolerance = 1e-4)
})

# testthat::test_that("Test 14.2a", {
#   testthat::skip_on_cran()
#   data("pbc.merlin", package = "merlin")
#   statares <- c(.0118483, 2.366806, log(.0846979), .0936, .5465511, log(.5003448), log(.0704125), log(.9729097), -95.757833)
#   comp <- rep(0, length(statares))
#   mod <- merlin::merlin(
#     model = list(
#       logp ~ year + M1[id] * 1,
#       logb ~ year + M2[id] * 1
#     ),
#     family = c("gaussian", "gaussian"),
#     levels = c("id"),
#     data = pbc.merlin,
#     control = list(ip = 27)
#   )
#   modres <- as.numeric(c(mod$coefficients, mod$loglikelihood))
#   testthat::expect_equal(object = (abs(modres - statares) / (abs(statares) + 1)), expected = comp, tolerance = 1e-2)
# })


# 15 - WEIBULL GAUSSIAN
#------------------------------------------------------------------------------
# testthat::test_that("Test 15.1", {
#   testthat::skip_on_cran()
#   data("pbc.merlin", package = "merlin")
#   pbc.merlin$trt <- as.numeric(pbc.merlin$trt)
#   statares <- c(.032736, -3.879407, .4126188, .0956745, .8100579, .5345633, log(.4991925), log(1.198969), -2322.2555)
#   comp <- rep(0, length(statares))
#   mod <- merlin::merlin(
#     model = list(
#       Surv(stime, died) ~ trt + M1[id] * 1,
#       logb ~ year + M1[id]
#     ),
#     timevar = c("stime", "year"),
#     family = c("weibull", "gaussian"),
#     levels = c("id"),
#     data = pbc.merlin,
#     control = list(ip = 25)
#   )
#   modres <- as.numeric(c(mod$coefficients, mod$loglikelihood))
#   testthat::expect_equal(object = (abs(modres - statares) / (abs(statares) + 1)), expected = comp, tolerance = 1e-3)
# })
