### TESTING MODELS

test_that("devRateModel janisch_32 Phthorimaea operculella (Lepidoptera:Gelechiidae)", {
  rawDevEggs <- matrix(c(10, 0.031, 10, 0.039, 15, 0.047, 15, 0.059, 15.5, 0.066,
                         13, 0.072, 16, 0.083, 16, 0.100, 17, 0.100, 20, 0.100, 20, 0.143, 25, 0.171,
                         25, 0.200, 30, 0.200, 30, 0.180, 35, 0.001), ncol = 2, byrow = TRUE)
  rawDevLarva <- matrix(c(10, 0.010, 10, 0.014, 10, 0.019, 13, 0.034, 15, 0.024,
                          15.5, 0.029, 15.5, 0.034, 15.5, 0.039, 17, 0.067, 20, 0.050, 25, 0.076,
                          25, 0.056, 30, 0.0003, 35, 0.0002), ncol = 2, byrow = TRUE)
  rawDevPupa <- matrix(c(10, 0.001, 10, 0.008, 10, 0.012, 13, 0.044, 15, 0.017,
                         15, 0.044, 15.5, 0.039, 16, 0.034, 15.5, 0.037, 16, 0.051, 17, 0.051,
                         20, 0.080, 20, 0.092, 25, 0.102, 25, 0.073, 30, 0.005,
                         35, 0.0002), ncol = 2, byrow = TRUE)
  mEggs <- devRateModel(eq = janisch_32, temp = rawDevEggs[,1], devRate = rawDevEggs[,2],
                        startValues = list(Dmin = 5, Topt = 30, aa = 0.2, bb = 0.1))
  mLarva <- devRateModel(eq = janisch_32, temp = rawDevLarva[,1], devRate = rawDevLarva[,2],
                         startValues = list(Dmin = 5, Topt = 30, aa = 0.2, bb = 0.1))
  mPupa <- devRateModel(eq = janisch_32, temp = rawDevPupa[,1], devRate = rawDevPupa[,2],
                        startValues = list(Dmin = 5, Topt = 30, aa = 0.2, bb = 0.1))
  expect_is(mEggs, "nls")
  expect_is(mLarva, "nls")
  expect_is(mPupa, "nls")
  expect_gte(stats::cor(rawDevEggs[, 2], stats::predict(mEggs)), 0.90)
  expect_gte(stats::cor(rawDevLarva[, 2], stats::predict(mLarva)), 0.90)
  expect_gte(stats::cor(rawDevPupa[, 2], stats::predict(mPupa)), 0.90)
})

test_that("devRateModel davidson_44 Phthorimaea operculella (Lepidoptera:Gelechiidae)", {
  rawDevEggs <- matrix(c(10, 0.031, 10, 0.039, 15, 0.047, 15, 0.059, 15.5, 0.066,
                         13, 0.072, 16, 0.083, 16, 0.100, 17, 0.100, 20, 0.100, 20, 0.143, 25, 0.171,
                         25, 0.200, 30, 0.200, 30, 0.180), ncol = 2, byrow = TRUE)
  rawDevLarva <- matrix(c(10, 0.010, 10, 0.014, 10, 0.019, 13, 0.034, 15, 0.024,
                          15.5, 0.029, 15.5, 0.034, 15.5, 0.039, 17, 0.067, 20, 0.050, 25, 0.076,
                          25, 0.056), ncol = 2, byrow = TRUE)
  rawDevPupa <- matrix(c(10, 0.001, 10, 0.008, 10, 0.012, 13, 0.044, 15, 0.017,
                         15, 0.044, 15.5, 0.039, 16, 0.034, 15.5, 0.037, 16, 0.051, 17, 0.051,
                         20, 0.080, 20, 0.092, 25, 0.102, 25, 0.073), ncol = 2, byrow = TRUE)
  mEggs <- devRateModel(eq = davidson_44, temp = rawDevEggs[,1], devRate = rawDevEggs[,2],
                        startValues = list(aa = 5, bb = -0.2, K = 0.3))
  mLarva <- devRateModel(eq = davidson_44, temp = rawDevLarva[,1], devRate = rawDevLarva[,2],
                         startValues = list(aa = 5, bb = -0.25, K = 0.08))
  mPupa <- devRateModel(eq = davidson_44, temp = rawDevPupa[,1], devRate = rawDevPupa[,2],
                        startValues = list(aa = 5, bb = -0.2, K = 0.3))
  expect_is(mEggs, "nls")
  expect_is(mLarva, "nls")
  expect_is(mPupa, "nls")
  expect_gte(stats::cor(rawDevEggs[, 2], stats::predict(mEggs)), 0.90)
  expect_gte(stats::cor(rawDevLarva[, 2], stats::predict(mLarva)), 0.80)
  expect_gte(stats::cor(rawDevPupa[, 2], stats::predict(mPupa)), 0.90)
})

test_that("devRateModel campbell_74 Phthorimaea operculella (Lepidoptera:Gelechiidae)", {
  rawDevEggs <- matrix(c(10, 0.031, 10, 0.039, 15, 0.047, 15, 0.059, 15.5, 0.066,
                         13, 0.072, 16, 0.083, 16, 0.100, 17, 0.100, 20, 0.100, 20, 0.143, 25, 0.171,
                         25, 0.200, 30, 0.200, 30, 0.180), ncol = 2, byrow = TRUE)
  rawDevLarva <- matrix(c(10, 0.010, 10, 0.014, 10, 0.019, 13, 0.034, 15, 0.024,
                          15.5, 0.029, 15.5, 0.034, 15.5, 0.039, 17, 0.067, 20, 0.050, 25, 0.076,
                          25, 0.056), ncol = 2, byrow = TRUE)
  rawDevPupa <- matrix(c(10, 0.001, 10, 0.008, 10, 0.012, 13, 0.044, 15, 0.017,
                         15, 0.044, 15.5, 0.039, 16, 0.034, 15.5, 0.037, 16, 0.051, 17, 0.051,
                         20, 0.080, 20, 0.092, 25, 0.102, 25, 0.073), ncol = 2, byrow = TRUE)
  mEggs <- devRateModel(eq = campbell_74, temp = rawDevEggs[,1], devRate = rawDevEggs[,2],
                        startValues = list(aa = 0, bb = 0))
  mLarva <- devRateModel(eq = campbell_74, temp = rawDevLarva[,1], devRate = rawDevLarva[,2],
                         startValues = list(aa = 0, bb = 0))
  mPupa <- devRateModel(eq = campbell_74, temp = rawDevPupa[,1], devRate = rawDevPupa[,2],
                        startValues = list(aa = 0, bb = 0))
  expect_is(mEggs, "nls")
  expect_is(mLarva, "nls")
  expect_is(mPupa, "nls")
  expect_gte(stats::cor(rawDevEggs[, 2], stats::predict(mEggs)), 0.90)
  expect_gte(stats::cor(rawDevLarva[, 2], stats::predict(mLarva)), 0.80)
  expect_gte(stats::cor(rawDevPupa[, 2], stats::predict(mPupa)), 0.90)
})

test_that("devRateModel stinner_74 Phthorimaea operculella (Lepidoptera:Gelechiidae)", {
  rawDevEggs <- matrix(c(10, 0.031, 10, 0.039, 15, 0.047, 15, 0.059, 15.5, 0.066,
                         13, 0.072, 16, 0.083, 16, 0.100, 17, 0.100, 20, 0.100, 20, 0.143, 25, 0.171,
                         25, 0.200, 30, 0.200, 30, 0.180, 35, 0.001), ncol = 2, byrow = TRUE)
  rawDevLarva <- matrix(c(10, 0.010, 10, 0.014, 10, 0.019, 13, 0.034, 15, 0.024,
                          15.5, 0.029, 15.5, 0.034, 15.5, 0.039, 17, 0.067, 20, 0.050, 25, 0.076,
                          25, 0.056, 30, 0.0003, 35, 0.0002), ncol = 2, byrow = TRUE)
  rawDevPupa <- matrix(c(10, 0.001, 10, 0.008, 10, 0.012, 13, 0.044, 15, 0.017,
                         15, 0.044, 15.5, 0.039, 16, 0.034, 15.5, 0.037, 16, 0.051, 17, 0.051,
                         20, 0.080, 20, 0.092, 25, 0.102, 25, 0.073, 30, 0.005,
                         35, 0.0002), ncol = 2, byrow = TRUE)
  mEggs <- devRateModel(eq = stinner_74, temp = rawDevEggs[,1], devRate = rawDevEggs[,2],
                        startValues = list(list(C = 0.1, k1 = 4, k2 = -0.2), list(Topt = 30)))
  mLarva <- devRateModel(eq = stinner_74, temp = rawDevLarva[,1], devRate = rawDevLarva[,2],
                         startValues = list(list(C = 0.08, k1 = 3, k2 = -0.2), list(Topt = 25)))
  mPupa <- devRateModel(eq = stinner_74, temp = rawDevPupa[,1], devRate = rawDevPupa[,2],
                        startValues = list(list(C = 0.1, k1 = 4, k2 = -0.2), list(Topt = 25)))
  expect_is(mEggs[[1]], "nls")
  expect_is(mLarva[[1]], "nls")
  expect_is(mPupa[[1]], "nls")
  expect_is(mEggs[[2]], "nls")
  expect_is(mLarva[[2]], "nls")
  expect_is(mPupa[[2]], "nls")
  expect_gte(stats::cor(rawDevEggs[1:15, 2], stats::predict(mEggs[[1]])), 0.90)
  expect_gte(stats::cor(rawDevLarva[1:12, 2], stats::predict(mLarva[[1]])), 0.80)
  expect_gte(stats::cor(rawDevPupa[1:15, 2], stats::predict(mPupa[[1]])), 0.90)
})

test_that("devRateModel logan6_76 Episimus utilis (Lepidoptera:Tortricidae)", {
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
  expect_is(testNLS, "nls")
  expect_gte(stats::cor(yy, stats::predict(testNLS)), 0.90)
})

test_that("devRateModel logan10_76 Cydia pomonella (Lepidoptera:Tortricidae)", {
  xx <- seq(from = 0, to = 36, by = 1)
  yy <- c(0.0007729213, 0.0001253524, 0.0007717062, 0.0010629336, 0.0014643530,
          0.0004597164, 0.0013529063, 0.0016926055, 0.0020639653, 0.0029521922,
          0.0015773307, 0.0026982888, 0.0045119329, 0.0046683817, 0.0052173935,
          0.0060213544, 0.0077865111, 0.0087443970, 0.0092949713, 0.0107126719,
          0.0116619374, 0.0136489680, 0.0146848849, 0.0162357028, 0.0171430946,
          0.0178698728, 0.0194753703, 0.0191674932, 0.0193544746, 0.0172297562,
          0.0153346383, 0.0126856251, 0.0083347235, 0.0016592859, 0.0000000000,
          0.0000000000, 0.0000000000)
  testNLS <- devRateModel(eq = logan10_76, temp = xx, devRate = yy,
                          startValues = list(alpha = 0.08450, bb = 0.173400, cc = 102.2182, Tmax = 38.7735, deltaT = 6.40240))
  expect_is(testNLS, "nls")
  expect_gte(stats::cor(yy, stats::predict(testNLS)), 0.90)
})

test_that("devRateModel sharpeDeMichele_77 Drosophila melanogaster (Diptera:Drosophilidae)", {
  xx <- seq(from = 0, to = 36, by = 1)
  yy <- c(0.000000000, 0.000000000, 0.000000000, 0.007279878, 0.010780900,
          0.002023176, 0.004599434, 0.030801276, 0.003611483, 0.022008346,
          0.022513837, 0.029477274, 0.038279933, 0.049745442, 0.020752379,
          0.042697777, 0.044835351, 0.055853683, 0.072617792, 0.064999242,
          0.068376525, 0.067887024, 0.087232515, 0.096994747, 0.069810120,
          0.099206646, 0.094854215, 0.098206028, 0.090197283, 0.101336347,
          0.090258319, 0.071312951, 0.077009260, 0.068088433, 0.064604557,
          0.037714537, 0.053549824)
  testNLS <- devRateModel(eq = sharpeDeMichele_77, temp = xx, devRate = yy,
                          startValues = list(aa = 19.43, bb = 10490, cc = -156.9, dd = -44373, ff = 226.6, gg = 69113))
  expect_is(testNLS, "nls")
  expect_gte(stats::cor(yy, stats::predict(testNLS)), 0.90)
})

test_that("devRateModel analytis_77 Nephus bisignatus (Coleoptera:Coccinellidae)", {
  xx <- seq(from = 5, to = 33, by = 1)
  yy <- c(0.003624885, 0.000000000, 0.001950942, 0.001846166, 0.001969783,
          0.004169708, 0.003567134, 0.004989870, 0.007621175, 0.009034580,
          0.007648763, 0.011395174, 0.014138975, 0.014511722, 0.018423493,
          0.019787992, 0.020790183, 0.023420162, 0.024488401, 0.028266525,
          0.031751702, 0.031811957, 0.032410839, 0.035262203, 0.035145591,
          0.038331467, 0.038510757, 0.036173298, 0.023066068)
  testNLS <- devRateModel(eq = analytis_77, temp = xx, devRate = yy,
                          startValues = list(aa = 1.0e-04, bb = 1.7766, cc = 0.1740, Tmin = 4.9125, Tmax = 33.0781))
  expect_is(testNLS, "nls")
  expect_gte(stats::cor(yy, stats::predict(testNLS)), 0.90)
})

test_that("devRateModel schoolfield_81 Melanoplus sanguinipes (Orthoptera:Acrididae)", {
  xx <- seq(from = 0, to = 50, by = 1)
  yy <- c(0.0005738992, 0.0037753270, 0.0019711243, 0.0013993445, 0.0000000000,
          0.0018258747, 0.0015857669, 0.0000000000, 0.0028721502, 0.0053867347,
          0.0068066704, 0.0071001306, 0.0066717568, 0.0048745851, 0.0080201138,
          0.0067507888, 0.0085149901, 0.0111779350, 0.0091441154, 0.0120987686,
          0.0147621464, 0.0194343995, 0.0198100212, 0.0193765233, 0.0238609486,
          0.0232133053, 0.0198710750, 0.0292669974, 0.0252871388, 0.0282169641,
          0.0349077453, 0.0390198824, 0.0391263725, 0.0438380968, 0.0453967965,
          0.0487420375, 0.0521477311, 0.0523109706, 0.0553442142, 0.0544577634,
          0.0566073337, 0.0527249056, 0.0529586715, 0.0537847167, 0.0489292577,
          0.0486422935, 0.0449730753, 0.0400146305, 0.0346307260, 0.0352004855,
          0.0263990779)

  testNLS <- devRateModel(eq = schoolfield_81, temp = xx, devRate = yy,
                          startValues = list(p25 = 0.0455, aa = 8814.36, bb = -14877.95, cc = 298.81, dd = 47258.52, ee = 316.695))
  expect_is(testNLS, "nls")
  expect_gte(stats::cor(yy, stats::predict(testNLS)), 0.90)
})

test_that("devRateModel schoolfieldHigh_81 Melanoplus sanguinipes (Orthoptera:Acrididae)", {
  xx <- seq(from = -5, to = 30, by = 1)
  yy <- c(0.0005315358, 0.0081099947, 0.0166569706, 0.0045348182, 0.0023591495,
          0.0085177076, 0.0122157105, 0.0193775061, 0.0037427740, 0.0241785522,
          0.0147844460, 0.0216203187, 0.0246449326, 0.0194719553, 0.0147972427,
          0.0261072237, 0.0265193061, 0.0270178896, 0.0252153311, 0.0241325856,
          0.0239119132, 0.0379386934, 0.0186132170, 0.0282008402, 0.0306502169,
          0.0354469603, 0.0277423794, 0.0374357233, 0.0381423032, 0.0473560141,
          0.0419966062, 0.0522594310, 0.0473526517, 0.0509561741, 0.0474212992,
          0.0567305166)
  testNLS <- devRateModel(eq = schoolfieldHigh_81, temp = xx, devRate = yy,
                          startValues = list(p25 = 0.0455, aa = 8814.36, dd = 47258.52, ee = 316.695))
  expect_is(testNLS, "nls")
  expect_gte(stats::cor(yy, stats::predict(testNLS)), 0.90)
})

test_that("devRateModel schoolfieldLow_81 Melanoplus sanguinipes (Orthoptera:Acrididae)", {
  xx <- seq(from = 15, to = 40, by = 1)
  yy <- c(0.007408536, 0.008365150, 0.009468100, 0.010714038, 0.012081506,
          0.013243063, 0.014691625, 0.016625860, 0.018090679, 0.020252121,
          0.022297409, 0.024531523, 0.026979481, 0.029683609, 0.032354563,
          0.035106640, 0.038020835, 0.041563266, 0.044988835, 0.048447192,
          0.052393433, 0.056512979, 0.060679163, 0.065064254, 0.069769445,
          0.074526602)
  testNLS <- devRateModel(eq = schoolfieldLow_81, temp = xx, devRate = yy,
                          startValues = list(p25 = 0.0455, aa = 8814.36, bb = -14877.95, cc = 298.81))
  expect_is(testNLS, "nls")
  expect_gte(stats::cor(yy, stats::predict(testNLS)), 0.90)
})

test_that("devRateModel taylor_81 Geocoris articolor (Hemiptera:Lygaeidae)", {
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
  expect_is(testNLS, "nls")
  expect_gte(stats::cor(yy, stats::predict(testNLS)), 0.90)
})

test_that("devRateModel wang_82 Nurscia albofasciata (Araneae:Titanoecidae)", {
  xx <- seq(from = 10, to = 40, by = 1)
  yy <- c(0.000000e+00, 0.000000e+00, 0.000000e+00, 2.189162e-04, 2.153789e-03,
          2.826689e-03, 3.093816e-03, 4.513258e-03, 3.204745e-03, 6.003303e-03,
          4.793105e-03, 7.987115e-03, 1.027355e-02, 1.158569e-02, 1.157333e-02,
          1.267494e-02, 1.388047e-02, 1.725236e-02, 1.623073e-02, 1.928007e-02,
          2.160226e-02, 2.136966e-02, 2.274513e-02, 2.475865e-02, 2.655935e-02,
          2.790608e-02, 2.371863e-02, 2.379099e-02, 2.017513e-02, 1.228895e-02,
          5.543522e-05)
  testNLS <- devRateModel(eq = wang_82, temp = xx, devRate = yy,
                          startValues = list(K = 0.045, r = 0.15, T0 = 31, TL = 13, TH = 40,  aa =2))
  expect_is(testNLS, "nls")
  expect_gte(stats::cor(yy, stats::predict(testNLS)), 0.90)
})

test_that("devRateModel poly2 Plodia interplInctella (Lepidoptera:Pyralidae)", {
  xx <- seq(from = 15, to = 40, by = 1)
  yy <- c(0.000000000, 0.000000000, 0.008714114, 0.009226288, 0.008974961,
          0.012786594, 0.022019003, 0.036118170, 0.029093556, 0.032973127,
          0.031510768, 0.042017463, 0.032786228, 0.042400972, 0.052407155,
          0.046048385, 0.044539889, 0.047925037, 0.051333902, 0.045657589,
          0.044514010, 0.041387678, 0.038145555, 0.048391190, 0.045908961,
          0.050747954)
  testNLS <- devRateModel(eq = poly2, temp = xx, devRate = yy,
                          startValues = list(a0 = -0.13003, a1 = 0.01022, a2 = -0.000144))
  expect_is(testNLS, "nls")
  expect_gte(stats::cor(yy, stats::predict(testNLS)), 0.90)
})

test_that("devRateModel harcourtYee_82 Hypera postica (Coleoptera:Curculionidae)", {
  xx <- seq(from = 15, to = 40, by = 1)
  yy <- c(0.04787425, 0.07107279, 0.05511158, 0.07248420, 0.08392225, 0.08329395,
          0.07825801, 0.11772809, 0.14566401, 0.15290571, 0.17476144, 0.16426242,
          0.20056165, 0.18852016, 0.20116038, 0.23733901, 0.22235097, 0.22490219,
          0.25511832, 0.21187438, 0.24725272, 0.24015939, 0.25575154, 0.25208030,
          0.22387521, 0.22734355)
  testNLS <- devRateModel(eq = harcourtYee_82, temp = xx, devRate = yy,
                          startValues = list(a0 = 0.1434000, a1 = -0.028270, a2 = 1.8240e-03, a3 = -0.000026629))
  expect_is(testNLS, "nls")
  expect_gte(stats::cor(yy, stats::predict(testNLS)), 0.90)
})

test_that("devRateModel poly4 Tuta absoluta (Lepidoptera:Gelechiidae)", {
  xx <- seq(from = 10, to = 50, by = 1)
  yy <- c(0.00000000, 0.01740821, 0.01220815, 0.01662167, 0.02894427, 0.02778375,
          0.04298098, 0.06337377, 0.04259963, 0.04382139, 0.05071260, 0.05552906,
          0.08036814, 0.06739671, 0.09720684, 0.08217325, 0.10354591, 0.09996859,
          0.11375681, 0.11507291, 0.14883151, 0.13347678, 0.14945011, 0.17115580,
          0.16503220, 0.18094458, 0.15573351, 0.17484868, 0.18130818, 0.18935105,
          0.18944839, 0.19123078, 0.17856466, 0.15369201, 0.14431060, 0.13409134,
          0.10092687, 0.05206789, 0.02728394, 0.00000000, 0.00000000)
  testNLS <- devRateModel(eq = poly4, temp = xx, devRate = yy,
                          startValues = list(a0 = -0.27, a1 = 0.049, a2 = -0.00310, a3 = 0.000094, a4 = -1.00e-06))
  expect_is(testNLS, "nls")
  expect_gte(stats::cor(yy, stats::predict(testNLS)), 0.90)
})

test_that("devRateModel rootsq_82 Symmetrischema tangolias (Lepidoptera:Gelechiidae)", {
  xx <- seq(from = 10, to = 50, by = 1)
  yy <- c(0.05076648, 0.04130829, 0.03100395, 0.05420404, 0.07027619, 0.09081254,
          0.07973797, 0.09178988, 0.09120504, 0.10319485, 0.12150123, 0.13037226,
          0.15253398, 0.14760324, 0.17405035, 0.17703873, 0.18051293, 0.20000342,
          0.22038633, 0.21933672, 0.24568362, 0.27005526, 0.27972188, 0.29105867,
          0.31291853, 0.33053615, 0.35858438, 0.36573842, 0.37366271, 0.40343608,
          0.41379410, 0.44065095, 0.46758776, 0.47282415, 0.48604381, 0.52820252,
          0.55619338, 0.56618472, 0.58957850, 0.62538123, 0.64705128)
  testNLS <- devRateModel(eq = rootsq_82, temp = xx, devRate = yy,
                          startValues = list(bb = -0.0151, Tb = -2.8397))
  expect_is(testNLS, "nls")
  expect_gte(stats::cor(yy, stats::predict(testNLS)), 0.90)
})

test_that("devRateModel hilbertLogan_83 Melanoplus sanguinipes (Orthoptera:Acrididae)", {
  xx <- seq(from = 8, to = 45, by = 1)
  yy <- c(0.0011014477, 0.0012507442, 0.0001167703, 0.0003532791, 0.0004263743,
          0.0015807307, 0.0025007541, 0.0024451171, 0.0038479143, 0.0064245233,
          0.0069100382, 0.0097557497, 0.0119485173, 0.0142912696, 0.0149056976,
          0.0173977196, 0.0193806360, 0.0212603286, 0.0246371582, 0.0260012154,
          0.0294022145, 0.0309236323, 0.0339868565, 0.0357495426, 0.0384880874,
          0.0402338705, 0.0431885192, 0.0472998302, 0.0478493169, 0.0506640531,
          0.0527050289, 0.0516846788, 0.0551387014, 0.0556545999, 0.0562771387,
          0.0552666161, 0.0521032164, 0.0477044579)
  testNLS <- devRateModel(eq = hilbertLogan_83, temp = xx, devRate = yy,
                          startValues = list(phi = 0.2676, aa = 58.6200, Tb = 7.6570, Tmax = 45.0200, deltaT = 3.4330))
  expect_is(testNLS, "nls")
  expect_gte(stats::cor(yy, stats::predict(testNLS)), 0.90)
})

test_that("devRateModel wagner_88 Rhyzopertha dominica (Coleoptera:Bostrichidae)", {
  xx <- seq(from = 0, to = 40, by = 1)
  yy <- c(0.0022212491, 0.0019379943, 0.0046610670, 0.0038109754, 0.0034486857,
          0.0047077040, 0.0029604436, 0.0052875661, 0.0053446159, 0.0063670157,
          0.0061276112, 0.0060130713, 0.0070109376, 0.0053224850, 0.0072778832,
          0.0093596225, 0.0100493711, 0.0102531971, 0.0103169375, 0.0096810912,
          0.0115920321, 0.0135770996, 0.0140990536, 0.0155335820, 0.0154300307,
          0.0179327532, 0.0178314388, 0.0208542864, 0.0196719180, 0.0210781947,
          0.0242664001, 0.0265959275, 0.0260000039, 0.0283018708, 0.0310243408,
          0.0333888139, 0.0270995388, 0.0002436405, 0.0000000000, 0.0000000000,
          0.0022229075)
  testNLS <- devRateModel(eq = wagner_88, temp = xx, devRate = yy,
                          startValues = list(aa = 0.017, bb = 11172, cc = 952361, dd = 309.4))
  expect_is(testNLS, "nls")
  expect_gte(stats::cor(yy, stats::predict(testNLS)), 0.90)
})

test_that("devRateModel lamb_92 Entomoscelis americana (Coleoptera:Chrysomelidae)", {
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
  expect_is(testNLS[[1]], "nls")
  expect_is(testNLS[[2]], "nls")
  expect_gte(stats::cor(yy[1:31], stats::predict(testNLS[[1]])), 0.90)
  expect_gte(stats::cor(yy[31:32], stats::predict(testNLS[[2]])), 0.90)
})

test_that("devRateModel lactin1_95 Plutella xylostella (Lepidoptera:Yponomeutidae)", {
  xx <- seq(from = 0, to = 35, by = 1)
  yy <- c(0.1822941, 0.2219479, 0.2255983, 0.2449956, 0.3109943, 0.3653696,
          0.4123151, 0.4554988, 0.5404501, 0.6054672, 0.7141738, 0.7866237,
          0.9141232, 1.0533414, 1.1567953, 1.3182321, 1.5054745, 1.6934138,
          1.8920394, 2.1144703, 2.3537863, 2.6178237, 2.8850753, 3.1874894,
          3.4406528, 3.7307796, 4.0232947, 4.2308143, 4.4163537, 4.5410291,
          4.4970305, 4.2682834, 3.8589188, 3.0975230, 1.9257092, 0.2139277)
  testNLS <- devRateModel(eq = lactin1_95, temp = xx, devRate = yy,
                          startValues = list(aa = 0.170000, Tmax = 35.10000, deltaT = 5.700000))
  expect_is(testNLS, "nls")
  expect_gte(stats::cor(yy, stats::predict(testNLS)), 0.90)
})

test_that("devRateModel lactin2_95 Leptinotarsa decemlineata (Coleoptera:Chrysomelidae)", {
  xx <- seq(from = 0, to = 38, by = 1)
  yy <- c(0.000000000, 0.000000000, 0.001150229, 0.000000000, 0.010329007, 0.002114154,
          0.012467492, 0.011841950, 0.020890812, 0.025269013, 0.025211605, 0.022588623,
          0.053243659, 0.059586772, 0.066315001, 0.062347109, 0.072354588, 0.072628676,
          0.089246183, 0.127517415, 0.118350089, 0.125706150, 0.136130564, 0.172161385,
          0.163746042, 0.194455160, 0.205405549, 0.200541211, 0.242366873, 0.248637282,
          0.264764651, 0.269341417, 0.263910144, 0.254994983, 0.265915585, 0.220850779,
          0.180261610, 0.134250223, 0.063380383)
  testNLS <- devRateModel(eq = lactin2_95, temp = xx, devRate = yy,
                          startValues = list(aa = 0.139034, Tmax = 38.89003, deltaT = 7.167110, bb = -0.026410))
  expect_is(testNLS, "nls")
  expect_gte(stats::cor(yy, stats::predict(testNLS)), 0.90)
})

test_that("devRateModel beta_95 NA", {
  xx <- seq(from = 15, to = 35, by = 1)
  yy <- c(0.001417876, 0.005636010, 0.013581678, 0.019082356, 0.026517216,
          0.032528973, 0.038774090, 0.044050438, 0.048498157, 0.056179586,
          0.058087298, 0.062558419, 0.065349262, 0.067477422, 0.066865668,
          0.069423345, 0.067166592, 0.066033683, 0.063193324, 0.055528744,
          0.046781069)
  testNLS <- devRateModel(eq = beta_95, temp = xx, devRate = yy,
                          startValues = list(mu = -6.484, Tb = 14.8, aa = 1.071, Tc = 36.6, bb = 0.469))
  expect_is(testNLS, "nls")
  expect_gte(stats::cor(yy, stats::predict(testNLS)), 0.90)
})

test_that("devRateModel wangengel_98 NA", {
  xx <- seq(from = 5, to = 35, by = 1)
  yy <- c(0.1885586, 0.2447508, 0.2675420, 0.3352446, 0.3836423, 0.4204840,
          0.5131225, 0.5717431, 0.5975940, 0.6562028, 0.7149159, 0.7541717,
          0.7938599, 0.8322312, 0.8853133, 0.9172352, 0.9438028, 0.9536710,
          0.9751491, 0.9954896, 0.9969244, 1.0128322, 0.9841107, 0.9726438,
          0.9411715, 0.9080856, 0.8694295, 0.7953146, 0.7258664, 0.6700754,
          0.5787964)
  testNLS <- devRateModel(eq = wangengel_98, temp = xx, devRate = yy,
                          startValues = list(Tmin = 0, aa = 1.5, Topt = 25))
  expect_is(testNLS, "nls")
  expect_gte(stats::cor(yy, stats::predict(testNLS)), 0.90)
})

test_that("devRateModel briere1_99 Cydia pomonella (Lepidoptera:Tortricidae)", {
  xx <- seq(from = 5, to = 35, by = 1)
  yy <- c(0.0000000000, 0.0000000000, 0.0000000000, 0.0000000000, 0.0000000000,
          0.0008276912, 0.0024161151, 0.0038645223, 0.0043578848, 0.0072354110,
          0.0072349070, 0.0098385738, 0.0108333796, 0.0125452909, 0.0148372325,
          0.0174393359, 0.0190593441, 0.0200491951, 0.0224374464, 0.0245829079,
          0.0278148202, 0.0264121875, 0.0279165289, 0.0303138541, 0.0306765425,
          0.0287505897, 0.0324110598, 0.0320097185, 0.0310495736, 0.0271481739,
          0.0223007786)
  testNLS <- devRateModel(eq = briere1_99, temp = xx, devRate = yy,
                          startValues = list(aa = 2.000e-05, Tmin = 9.6188, Tmax = 36.5508))
  expect_is(testNLS, "nls")
  expect_gte(stats::cor(yy, stats::predict(testNLS)), 0.90)
})

test_that("devRateModel briere2_99 Cydia pomonella (Lepidoptera:Tortricidae)", {
  xx <- seq(from = 10, to = 30, by = 1)
  yy <- c(0.003592012, 0.002441060, 0.005466671, 0.004373278, 0.005762749, 0.009782370,
          0.009475021, 0.011327031, 0.012687551, 0.015284688, 0.014736900, 0.018212742,
          0.019424187, 0.022882716, 0.024458473, 0.025876630, 0.027911330, 0.027726436,
          0.029162758, 0.033745400, 0.035172396)

  testNLS <- devRateModel(eq = briere2_99, temp = xx, devRate = yy,
                          startValues = list(aa = 0.0000340, Tmin = 7.7659, Tmax = 34.1260, bb = 3.8342))
  expect_is(testNLS, "nls")
  expect_gte(stats::cor(yy, stats::predict(testNLS)), 0.90)
})

test_that("devRateModel bayoh_03 Anopheles gambiae (Diptera:Culicidae)", {
  xx <- seq(from = 16, to = 34, by = 1)
  yy <- c(0.00000000, 0.02360451, 0.03679854, 0.04256779, 0.05106298,
          0.05494146, 0.05910256, 0.06544544, 0.07125289, 0.07310096,
          0.07760755, 0.08423758, 0.08910340, 0.09361966, 0.09687718,
          0.09940690, 0.09378216, 0.06982750, 0.00000000)
  testNLS <- devRateModel(eq = bayoh_03, temp = xx, devRate = yy,
                          startValues = list(aa = -0.05, bb = 0.005, cc = -2.139e-16, dd = -281357.7))
  expect_is(testNLS, "nls")
  expect_gte(stats::cor(yy, stats::predict(testNLS)), 0.90)
})

test_that("devRateModel kontodimas_04 Cydia pomonella (Lepidoptera:Tortricidae)", {
  xx <- seq(from = 10, to = 40, by = 1)
  yy <- c(0.0006747102, 0.0027825987, 0.0033743047, 0.0047231486, 0.0064218586,
          0.0081886365, 0.0097703062, 0.0125784934, 0.0145758820, 0.0159444966,
          0.0183775852, 0.0206127228, 0.0245370442, 0.0266028103, 0.0291879954,
          0.0305220770, 0.0312787956, 0.0321701920, 0.0364631361, 0.0353332002,
          0.0392814342, 0.0368932876, 0.0374896984, 0.0385335372, 0.0372273938,
          0.0367543093, 0.0336959171, 0.0321736453, 0.0266896329, 0.0227111416,
          0.0190010898)
  testNLS <- devRateModel(eq = kontodimas_04, temp = xx, devRate = yy,
                          startValues = list(aa = 5.7e-06, Tmin = 7.9376, Tmax = 43.3850))
  expect_is(testNLS, "nls")
  expect_gte(stats::cor(yy, stats::predict(testNLS)), 0.90)
})

test_that("devRateModel damos_08 Anarsia lineatella (Lepidoptera:Gelechiidae)", {
  xx <- seq(from = 5, to = 35, by = 1)
  yy <- c(0.000000000, 0.000000000, 0.013844075, 0.000000000, 0.009361758,
          0.001454269, 0.000000000, 0.013183609, 0.000000000, 0.004798529,
          0.014066903, 0.003628415, 0.009424874, 0.017446812, 0.010623413,
          0.031509888, 0.033779900, 0.030489820, 0.023386860, 0.027721306,
          0.049432465, 0.051402218, 0.054979934, 0.057700327, 0.060598896,
          0.065206405, 0.061546110, 0.055924618, 0.057930173, 0.055963462,
          0.031124983)
  testNLS <- devRateModel(eq = damos_08, temp = xx, devRate = yy,
                          startValues = list(aa = 3e-04, bb = 3.8297, cc = 4.876))
  expect_is(testNLS, "nls")
  expect_gte(stats::cor(yy, stats::predict(testNLS)), 0.90)
})

test_that("devRateModel damos_11 NA", {
  xx <- seq(from = 0, to = 35, by = 1)
  yy <- c(0.08239590, 0.08072370, 0.10070833, 0.08878272, 0.13047990, 0.12654245,
          0.11403194, 0.11003217, 0.11405621, 0.13394459, 0.12004746, 0.14363508,
          0.13795026, 0.16381752, 0.15539005, 0.17926441, 0.15370672, 0.16926040,
          0.18565617, 0.20515092, 0.20139358, 0.19017785, 0.21421971, 0.21199808,
          0.22879180, 0.23448626, 0.20004777, 0.21789319, 0.20348710, 0.21354517,
          0.21559372, 0.21210905, 0.17523718, 0.18001217, 0.18948697, 0.17359905)
  testNLS <- devRateModel(eq = damos_11, temp = xx, devRate = yy,
                          startValues = list(aa = 0.08, bb = -0.05, cc = 0.001))
  expect_is(testNLS, "nls")
  expect_gte(stats::cor(yy, stats::predict(testNLS)), 0.90)
})

test_that("devRateModel shi_11 NA", {
  xx <- seq(from = 5, to = 30, by = 1)
  yy <- c(0.0005995735, 0.0079170372, 0.0171991089, 0.0236537918, 0.0281298713,
          0.0341454964, 0.0392060208, 0.0420963368, 0.0443013526, 0.0467620208,
          0.0478385221, 0.0494913515, 0.0514356761, 0.0504533097, 0.0500366659,
          0.0492914131, 0.0478050231, 0.0459566307, 0.0415589540, 0.0375018619,
          0.0347461935, 0.0272569814, 0.0224289054, 0.0176117947, 0.0081157601,
          0.0018878189)
  testNLS <- devRateModel(eq = shi_11, temp = xx, devRate = yy,
                          startValues = list(cc = 0.1, k1 = 0.1, k2 = 0.1, T1 = 5, T2 = 30))
  expect_is(testNLS, "nls")
  expect_gte(stats::cor(yy, stats::predict(testNLS)), 0.90)
})

test_that("devRateModel perf2_11 NA", {
  xx <- seq(from = 5, to = 30, by = 1)
  yy <- c(0.00000000, 0.09659644, 0.17580803, 0.27372645, 0.34321944,
          0.43498266, 0.50303890, 0.60765488, 0.66796542, 0.72033052,
          0.78447114, 0.86140402, 0.87338736, 0.90814892, 0.93825509,
          0.95255293, 0.92937469, 0.92991574, 0.92638381, 0.86106986,
          0.78175846, 0.69621604, 0.56833233, 0.41577785, 0.21972773,
          0.00000000)
  testNLS <- devRateModel(eq = perf2_11, temp = xx, devRate = yy,
                          startValues = list(cc = 0.1, T1 = 5, k = 0.1, T2 = 30))
  expect_is(testNLS, "nls")
  expect_gte(stats::cor(yy, stats::predict(testNLS)), 0.90)
})

test_that("devRateModel regniere_12 Choristoneura fumiferana (Lepidoptera:Tortricidae)", {
  xx <- seq(from = 10, to = 37, by = 1)
  yy <- c(0.02270952, 0.03016126, 0.03866354, 0.04770442, 0.05706395, 0.06602862,
          0.07615793, 0.08304121, 0.09257196, 0.10285969, 0.11100158, 0.12039677,
          0.13045528, 0.13883317, 0.14702621, 0.15699092, 0.16491603, 0.17207691,
          0.17885112, 0.18642188, 0.18838618, 0.19262171, 0.19107004, 0.18812192,
          0.18038079, 0.16780250, 0.14956308, 0.12607016)
  testNLS <- devRateModel(eq = regniere_12, temp = xx, devRate = yy,
                          startValues = list(phi = 0.0469, bb = 0.109, Tb = 7.2, Tm = 39.9, deltab = 1.7, deltam = 8.7))
  expect_is(testNLS, "nls")
  expect_gte(stats::cor(yy, stats::predict(testNLS)), 0.90)
})

