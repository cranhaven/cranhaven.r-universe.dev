test_that("devRateQlStat 3 ds 3 nls", {
  dfList <- list(
    data.frame(
      T = seq(from = 0, to = 50, by = 10),
      rT = c(0.001, 0.008, 0.02, 0.03, 0.018, 0.004)),
    data.frame(
      T = seq(from = 0, to = 50, by = 10),
      rT = c(0.01, 0.08, 0.2, 0.3, 0.18, 0.04)),
    data.frame(
      T = seq(from = 0, to = 50, by = 10),
      rT = c(0.0012, 0.0082, 0.025, 0.032, 0.0182, 0.0045))
  )
  fitList <- lapply(seq_along(dfList), function(i){
    devRateModel(
      eq = janisch_32,
      temp = dfList[[i]][, 1],
      devRate = dfList[[i]][, 2],
      startValues = list(Dmin = 5, Topt = 30, aa = 0.2, bb = 0.1))})
  # qlStat <- devRateQlStat(eq = rep(list(janisch_32), 3), nlsDR = fitList, df = dfList)
  qlStat <- devRateQlStat(nlsDR = fitList)
  expect_is(qlStat, "data.frame")
})

test_that("devRateQlStat 1 df 3 nls", {
  dfList <- list(
    data.frame(
      T = seq(from = 0, to = 50, by = 10),
      rT = c(0.001, 0.008, 0.02, 0.03, 0.018, 0.004)))
  fitList <- list(
    devRateModel(
      eq = janisch_32, temp = dfList[[1]][, 1],
      devRate = dfList[[1]][, 2],
      startValues = list(Dmin = 5, Topt = 30, aa = 0.2, bb = 0.1)),
    devRateModel(
      eq = kontodimas_04,
      temp = dfList[[1]][, 1],
      devRate = dfList[[1]][, 2],
      startValues = list(aa = 1, Tmin = 7, Tmax = 40), algo = "LM"),
    devRateModel(
      eq = poly2,
      temp = dfList[[1]][, 1],
      devRate = dfList[[1]][, 2],
      startValues = list(a0 = 1, a1 = 1, a2 = 1), algo = "LM"))
  # qlStat <- devRateQlStat(eq = list(janisch_32, kontodimas_04, poly2), nlsDR = fitList, df = dfList)
  qlStat <- devRateQlStat(nlsDR = fitList)
  expect_is(qlStat, "data.frame")
})

# test_that("devRateQlStat df no list", {
#   dfList <- data.frame(
#     T = seq(from = 0, to = 50, by = 10),
#     rT = c(0.001, 0.008, 0.02, 0.03, 0.018, 0.004))
#   fitList <- list(
#     devRateModel(
#       eq = janisch_32,
#       temp = dfList[, 1],
#       devRate = dfList[, 2],
#       startValues = list(Dmin = 5, Topt = 30, aa = 0.2, bb = 0.1)),
#     devRateModel(
#       eq = kontodimas_04,
#       temp = dfList[, 1],
#       devRate = dfList[, 2],
#       startValues = list(aa = 1, Tmin = 7, Tmax = 40), algo = "LM"),
#     devRateModel(
#       eq = poly2,
#       temp = dfList[, 1],
#       devRate = dfList[, 2],
#       startValues = list(a0 = 1, a1 = 1, a2 = 1), algo = "LM"))
#   # qlStat <- devRateQlStat(eq = list(janisch_32, kontodimas_04, poly2), nlsDR = fitList, df = dfList)
#   qlStat <- devRateQlStat(nlsDR = fitList)
#   expect_is(qlStat, "NULL")
# })

test_that("devRateQlStat nls no list", {
  dfList <- list(
    data.frame(
      T = seq(from = 0, to = 50, by = 10),
      rT = c(0.001, 0.008, 0.02, 0.03, 0.018, 0.004)))
  fitList <- devRateModel(
    eq = janisch_32,
    temp = dfList[[1]][, 1],
    devRate = dfList[[1]][, 2],
    startValues = list(Dmin = 5, Topt = 30, aa = 0.2, bb = 0.1))
  # qlStat <- devRateQlStat(eq = list(janisch_32), nlsDR = fitList, df = dfList)
  qlStat <- devRateQlStat(nlsDR = fitList)
  expect_is(qlStat, "NULL")
})

# test_that("devRateQlStat eq no list", {
#   dfList <- list(
#     data.frame(
#       T = seq(from = 0, to = 50, by = 10),
#       rT = c(0.001, 0.008, 0.02, 0.03, 0.018, 0.004)))
#   fitList <- list(
#     devRateModel(
#       eq = janisch_32,
#       temp = dfList[[1]][, 1],
#       devRate = dfList[[1]][, 2],
#       startValues = list(Dmin = 5, Topt = 30, aa = 0.2, bb = 0.1)))
#   # qlStat <- devRateQlStat(eq = janisch_32, nlsDR = fitList, df = dfList)
#   qlStat <- devRateQlStat(nlsDR = fitList)
#   expect_is(qlStat, "NULL")
# })

test_that("devRateQlStat eq janisch_32", {
  dfList <- list(
    data.frame(
      T = seq(from = 0, to = 50, by = 10),
      rT = c(0.001, 0.008, 0.02, 0.03, 0.018, 0.004)))
  fitList <- devRateModel(
    eq = janisch_32,
    temp = dfList[[1]][, 1],
    devRate = dfList[[1]][, 2],
    startValues = list(Dmin = 5, Topt = 30, aa = 0.2, bb = 0.1))
  # qlStat <- devRateQlStat(eq = list(stinner_74), nlsDR = list(fitList), df = dfList)
  qlStat <- devRateQlStat(nlsDR = list(fitList))
  expect_is(qlStat, "data.frame")
})

test_that("devRateQlStat nls NULL", {
  dfList <- list(
    data.frame(
      T = seq(from = 0, to = 50, by = 10),
      rT = c(0.001, 0.008, 0.02, 0.03, 0.018, 0.004)))
  fitList <- list(NULL)
  # qlStat <- devRateQlStat(eq = list(janisch_32), nlsDR = fitList, df = dfList)
  qlStat <- devRateQlStat(nlsDR = fitList)
  expect_is(qlStat, "data.frame")
})

test_that("devRateQlBio", {
  dfList <- list(
    data.frame(
      T = seq(from = 0, to = 50, by = 10),
      rT = c(0.001, 0.008, 0.02, 0.03, 0.018, 0.004)),
    data.frame(
      T = seq(from = 0, to = 50, by = 10),
      rT = c(0.01, 0.08, 0.2, 0.3, 0.18, 0.04)),
    data.frame(
      T = seq(from = 0, to = 50, by = 10),
      rT = c(0.0012, 0.0082, 0.025, 0.032, 0.0182, 0.0045)))
  fitList <- lapply(seq_along(dfList), function(i){
    devRateModel(
      eq = janisch_32,
      temp = dfList[[i]][, 1],
      devRate = dfList[[i]][, 2],
      startValues = list(Dmin = 5, Topt = 30, aa = 0.2, bb = 0.1))})
  qlBio <- devRateQlBio(nlsDR = fitList, propThresh = 0.1, eq = rep(list(janisch_32), 3))
  expect_is(qlBio, "data.frame")
})

test_that("devRateQlBio nls NULL", {
  qlBio <- devRateQlBio(nlsDR = list(NULL), propThresh = 0.1, eq = list(janisch_32))
  expect_is(qlBio, "data.frame")
})

test_that("devRateQlBio campbell_74", {
  dfList <- list(data.frame(T = seq(from = 0, to = 50, by = 10),
                            rT = c(0.001, 0.008, 0.02, 0.03, 0.018, 0.004)),
                 data.frame(T = seq(from = 0, to = 50, by = 10),
                            rT = c(0.01, 0.08, 0.2, 0.3, 0.18, 0.04)),
                 data.frame(T = seq(from = 0, to = 50, by = 10),
                            rT = c(0.0012, 0.0082, 0.025, 0.032, 0.0182, 0.0045)))
  fitList <- lapply(seq_along(dfList), function(i){
    devRateModel(eq = campbell_74, temp = dfList[[i]][, 1], devRate = dfList[[i]][, 2],
                 startValues = list(aa = 1, bb = 1))})
  qlBio <- devRateQlBio(nlsDR = fitList, propThresh = 0.1, eq = rep(list(campbell_74), 3))
  expect_is(qlBio, "data.frame")
})
