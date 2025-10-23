
# ------------------------------------------------------------------------------
# Tests for devRatePlot
# ------------------------------------------------------------------------------

test_that("devRatePlot returns NULL",{
  xx <- seq(from = 4, to = 35, by = 1)
  yy <- c(0.0011177457, 0.0000000000, 0.0027744302, 0.0004460741, 0.0020882305,
          0.0048659816, 0.0046385278, 0.0083962283, 0.0089383489, 0.0076518397,
          0.0109503542, 0.0111564471, 0.0150364540, 0.0165235404, 0.0189938734,
          0.0238332060, 0.0229612488, 0.0271481105, 0.0292819680, 0.0346557231,
          0.0360398266, 0.0397014187, 0.0413783578, 0.0439369633, 0.0481031628,
          0.0489109593, 0.0515116666, 0.0539285523, 0.0533571975, 0.0552395531,
          0.0552867174, 0.0534671900)
  testNLS <- devRateModel(eq = lamb_92, temp = xx, devRate = yy,
    startValues = list(list(Rm = 0.05393, Tmax = 33.4000, To = 10.7500), list(T1 = 3.377000)))
  res <- devRatePlot(
    eq = lamb_92,
    nlsDR = testNLS,
    rangeT = 10,
    optText = TRUE,
    spe = TRUE)
  expect_equal(
    object = res,
    expected = NULL
  )

  xx <- seq(from = 0, to = 50, by = 1)
  yy <- c(0.000000e+00, 0.000000e+00, 0.000000e+00, 0.000000e+00, 0.000000e+00,
          3.243648e-03, 7.814825e-04, 5.814596e-03, 9.819378e-03, 7.814990e-03,
          0.000000e+00, 5.430783e-05, 1.056332e-02, 0.000000e+00, 1.295533e-02,
          0.000000e+00, 0.000000e+00, 7.823721e-03, 1.247843e-03, 5.706782e-03,
          8.286514e-03, 7.482038e-03, 2.459962e-02, 5.102975e-03, 3.485625e-02,
          3.060154e-02, 1.493131e-02, 2.509750e-02, 2.822221e-02, 2.497632e-02,
          3.772156e-02, 3.043415e-02, 3.034768e-02, 3.756987e-02, 5.572242e-02,
          5.400489e-02, 5.740899e-02, 4.994192e-02, 5.559589e-02, 5.372400e-02,
          4.867726e-02, 5.998619e-02, 4.504179e-02, 2.689224e-02, 4.373311e-02,
          4.414157e-02, 3.948465e-02, 3.335472e-02, 4.921421e-02, 3.708899e-02,
          1.838826e-02)
  testNLS <- devRateModel(eq = taylor_81, temp = xx, devRate = yy,
    startValues = list(Rm = 0.0550, Tm = 37.2000, To = 8.8000))
  res <- devRatePlot(
    eq = taylor_81,
    nlsDR = testNLS,
    rangeT = 10,
    optText = TRUE,
    spe = TRUE)
  expect_equal(
    object = res,
    expected = NULL
  )

  rawDevEggs <- matrix(c(10, 0.031, 10, 0.039, 15, 0.047, 15, 0.059, 15.5, 0.066,
    13, 0.072, 16, 0.083, 16, 0.100, 17, 0.100, 20, 0.100, 20, 0.143, 25, 0.171,
    25, 0.200, 30, 0.200, 30, 0.180, 35, 0.001), ncol = 2, byrow = TRUE)
  testNLS <- devRateModel(eq = stinner_74, temp = rawDevEggs[,1], devRate = rawDevEggs[,2],
    startValues = list(list(C = 0.1, k1 = 4, k2 = -0.2), list(Topt = 30)))
  res <- devRatePlot(
    eq = stinner_74,
    nlsDR = testNLS,
    rangeT = 10,
    optText = TRUE,
    spe = TRUE)
  expect_equal(
    object = res,
    expected = NULL
  )


  rawDevEggs <- matrix(c(10, 0.031, 10, 0.039, 15, 0.047, 15, 0.059, 15.5, 0.066,
    13, 0.072, 16, 0.083, 16, 0.100, 17, 0.100, 20, 0.100, 20, 0.143, 25, 0.171,
    25, 0.200, 30, 0.200, 30, 0.180), ncol = 2, byrow = TRUE)
  testNLS <- devRateModel(eq = campbell_74, temp = rawDevEggs[,1], devRate = rawDevEggs[,2],
    startValues = list(aa = 0, bb = 0))
  res <- devRatePlot(
    eq = campbell_74,
    nlsDR = testNLS,
    rangeT = 10,
    optText = TRUE,
    spe = TRUE)
  expect_equal(
    object = res,
    expected = NULL
  )

  rawDevEggs <- matrix(c(10, 0.031, 10, 0.039, 15, 0.047, 15, 0.059, 15.5, 0.066,
    13, 0.072, 16, 0.083, 16, 0.100, 17, 0.100, 20, 0.100, 20, 0.143, 25, 0.171,
    25, 0.200, 30, 0.200, 30, 0.180), ncol = 2, byrow = TRUE)
  testNLS <- devRateModel(eq = davidson_44, temp = rawDevEggs[,1], devRate = rawDevEggs[,2],
    startValues = list(aa = 5, bb = -0.2, K = 0.3))
  res <- devRatePlot(
    eq = davidson_44,
    nlsDR = testNLS,
    rangeT = 10,
    optText = TRUE,
    spe = TRUE)
  expect_equal(
    object = res,
    expected = NULL
  )

  rawDevEggs <- matrix(c(10, 0.031, 10, 0.039, 15, 0.047, 15, 0.059, 15.5, 0.066,
    13, 0.072, 16, 0.083, 16, 0.100, 17, 0.100, 20, 0.100, 20, 0.143, 25, 0.171,
    25, 0.200, 30, 0.200, 30, 0.180, 35, 0.001), ncol = 2, byrow = TRUE)
  testNLS <- devRateModel(eq = janisch_32, temp = rawDevEggs[,1], devRate = rawDevEggs[,2],
    startValues = list(Dmin = 5, Topt = 30, aa = 0.2, bb = 0.1))
  res <- devRatePlot(
    eq = janisch_32,
    nlsDR = testNLS,
    rangeT = 10,
    optText = TRUE,
    spe = TRUE)
  expect_equal(
    object = res,
    expected = NULL
  )

  xx <- seq(from = 0, to = 33, by = 1)
  yy <- c(0.0005252046, 0.0002251286, 0.0006460717, 0.0017880213, 0.0015611891,
          0.0012853886, 0.0010841702, 0.0000000000, 0.0029119885, 0.0051535582,
          0.0049455628, 0.0049839564, 0.0042565719, 0.0075039314, 0.0073359365,
          0.0069161169, 0.0104414407, 0.0109888804, 0.0121261595, 0.0153213235,
          0.0147817972, 0.0193366467, 0.0202140539, 0.0221893465, 0.0260741628,
          0.0275310077, 0.0298545694, 0.0317042536, 0.0332244901, 0.0302047526,
          0.0292701892, 0.0267738132, 0.0155003324, 0.0011891955)
  testNLS <- devRateModel(eq = logan6_76, temp = xx, devRate = yy,
    startValues = list(phi = 0.000800, bb = 0.169, Tmax = 33.003, deltaT = 3.895))
  res <- devRatePlot(
    eq = logan6_76,
    nlsDR = testNLS,
    rangeT = 10,
    optText = TRUE,
    spe = TRUE)
  expect_equal(
    object = res,
    expected = NULL
  )

  xx <- seq(from = 0, to = 33, by = 1)
  yy <- c(0.0005252046, 0.0002251286, 0.0006460717, 0.0017880213, 0.0015611891,
          0.0012853886, 0.0010841702, 0.0000000000, 0.0029119885, 0.0051535582,
          0.0049455628, 0.0049839564, 0.0042565719, 0.0075039314, 0.0073359365,
          0.0069161169, 0.0104414407, 0.0109888804, 0.0121261595, 0.0153213235,
          0.0147817972, 0.0193366467, 0.0202140539, 0.0221893465, 0.0260741628,
          0.0275310077, 0.0298545694, 0.0317042536, 0.0332244901, 0.0302047526,
          0.0292701892, 0.0267738132, 0.0155003324, 0.0011891955)
  testNLS <- devRateModel(eq = logan6_76, temp = xx, devRate = yy,
    startValues = list(phi = 0.000800, bb = 0.169, Tmax = 33.003, deltaT = 3.895))
  res <- devRatePlot(
    eq = logan6_76,
    nlsDR = testNLS,
    rangeT = 10,
    optText = TRUE,
    spe = FALSE)
  expect_equal(
    object = res,
    expected = NULL
  )
})

# devtools::test()
# covr::package_coverage()
# covr::report()
# devtools::document()
# devtools::check()
