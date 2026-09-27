# mkuhn, 2021-10-11
# test utility/helper functions of this package

testthat::skip_if_not_installed("numDeriv")
library("numDeriv")

test_that("MLEw weight objects", {
  # we consider the package-internal list MLEw_approx
  expect_true(exists("MLEw_approx"))
  expect_named(
    MLEw_approx,
    expected = c(
      "MCsim",
      "MCsim_cousineau2009",
      "MCsim_cousineauGH",
      "coef",
      "fun"
    )
  )
  expect_type(MLEw_approx[["MCsim"]], type = "list")
  expect_named(
    MLEw_approx[["MCsim"]],
    expected = c("nObs", "W1", "W1gamma", "W2")
  )
  # simulation results are close to median of gamma distribution
  with(MLEw_approx[["MCsim"]], expect_equal(W1, W1gamma, tolerance = 1e-4))

  # MC results from Cousineau (2009)
  MCsim_cous09 <- MLEw_approx[["MCsim_cousineau2009"]]
  expect_type(MCsim_cous09, type = "list")
  expect_named(
    MCsim_cous09,
    expected = c("type", "location", "n", "shape", "value")
  )
  # we have a location J
  expect_true("J" %in% MCsim_cous09$location)
  # range of shape
  expect_equal(min(MCsim_cous09$shape, na.rm = TRUE), expected = 0.5)
  expect_equal(max(MCsim_cous09$shape, na.rm = TRUE), expected = 2.5)
  # W3 is ordered for shape (ascending) per given n
  purrr::walk(
    .x = unique(MCsim_cous09$n[MCsim_cous09$type == "W3"]),
    .f = \(nObs) {
      shapes <- MCsim_cous09$shape[
        MCsim_cous09$type == "W3" &
          MCsim_cous09$location == "J" &
          MCsim_cous09$n == nObs
      ]
      # shapes are increasing
      expect_identical(shapes, c(0.5, 1, 1.5, 2, 2.5))
    }
  )
  with(MCsim_cous09, {
    # W1: cf Cousineau (2009), table 2
    expect_equal(
      value[type == "W1" & location == "J" & n %in% c(3, 5, 15)],
      expected = c(0.891, 0.934, 0.978)
    )
    expect_equal(
      value[type == "W1" & location == "G" & n %in% c(2, 7, 13)],
      expected = c(0.763, 0.929, 0.962)
    )
    # W2: cf Cousineau (2009), table 3
    expect_equal(
      value[type == "W2" & location == "J" & n %in% c(3, 5, 15)],
      expected = c(0.517, 0.711, 0.902)
    )
    expect_equal(
      value[type == "W2" & location == "G" & n %in% c(2, 7, 13)],
      expected = c(0.163, 0.742, 0.860)
    )
    # W3: cf Cousineau (2009), table 4
    expect_equal(
      value[type == "W3" & location == "J" & shape == 0.5 & n %in% c(3, 5, 15)],
      expected = c(3.081, 4.806, 12.069)
    )
    expect_equal(
      value[type == "W3" & location == "G" & shape == 1.5 & n %in% c(2, 7, 13)],
      expected = c(1.524, 2.184, 2.403)
    )
  })

  # MC results from Cousineau (2009)
  MCsim_cousGH <- MLEw_approx[["MCsim_cousineauGH"]]
  expect_type(MCsim_cousGH, type = "list")
  expect_named(
    MCsim_cousGH,
    expected = c("type", "location", "n", "shape", "value")
  )
  # we have a location J
  expect_true("J" %in% MCsim_cousGH$location)
  # range of shape
  expect_equal(min(MCsim_cousGH$shape, na.rm = TRUE), expected = 0.1)
  expect_equal(max(MCsim_cousGH$shape, na.rm = TRUE), expected = 5.0)
  # W3 is ordered for shape (ascending) per given n
  purrr::walk(
    .x = unique(MCsim_cousGH$n[MCsim_cousGH$type == "W3"]),
    .f = \(nObs) {
      shapes <- MCsim_cousGH$shape[
        MCsim_cousGH$type == "W3" &
          MCsim_cousGH$location == "J" &
          MCsim_cousGH$n == nObs
      ]
      # shapes are increasing
      expect_equal(shapes, seq.int(from = .1, to = 5.0, by = .1))
    }
  )
  with(MCsim_cousGH, {
    # see <https://github.com/dcousin3/wMLE> weights files
    # W1
    expect_equal(
      value[type == "W1" & location == "J" & n %in% c(3, 15, 35)],
      expected = c(0.8920774880443304, 0.9778152771433289, 0.9904665402384888)
    )
    expect_equal(
      value[type == "W1" & location == "G" & n %in% c(2, 18, 65)],
      expected = c(
        0.7625930582854074334,
        0.97251232790835328476,
        0.992397831076367396186
      )
    )
    # W2
    expect_equal(
      value[type == "W2" & location == "J" & n %in% c(3, 26)],
      expected = c(0.5184046467893484, 0.942983924204849)
    )
    expect_equal(
      value[type == "W2" & location == "G" & n %in% c(2, 80)],
      expected = c(0.1616905964032776532, 0.97742105623729829933)
    )
    # W3
    expect_equal(
      value[type == "W3" & location == "J" & shape == 0.5 & n %in% c(3, 5, 15)],
      expected = c(3.083128646237427, 4.830973130538364, 12.113435707789740)
    )
    expect_equal(
      value[type == "W3" & location == "G" & shape == 1.5 & n %in% c(2, 7)],
      expected = c(1.518931457913954, 2.185921790685063)
    )
  })

  expect_named(MLEw_approx[["coef"]], expected = c("W2", "W3_richards"))

  expect_length(w1Fint(seq_len(3)), n = 3)
  expect_type(w1Fint(5), type = "double")
  expect_equal(w1Fint(1), expected = log(2))
  expect_equal(w1Fint(-1), expected = log(2))
  expect_equal(w1Fint(1.5), expected = log(2))
  # W1 is (mostly) monotonically increasing
  expect_gte(min(diff(w1Fint(seq_len(40)))), expected = 0)

  # Cousineau: Nearly unbiased.. (Table 2)
  expect_equal(w1Fint(6), expected = 0.945, tolerance = 1e-3)
  expect_equal(w1Fint(14), expected = 0.976, tolerance = 1e-3)

  expect_length(w2Fint(seq_len(3)), n = 3)
  expect_type(w2Fint(5), type = "double")
  expect_equal(w2Fint(1), expected = 0)
  expect_equal(w2Fint(-1), expected = 0)
  expect_equal(w2Fint(1.5), expected = 0)
  # W2 is monotonically increasing
  expect_gte(min(diff(w2Fint(seq_len(100)))), expected = 0)

  # Cousineau: Nearly unbiased.. (Table 3)
  expect_equal(w2Fint(8), expected = 0.817, tolerance = 1e-3)
  expect_equal(w2Fint(15), expected = 0.902, tolerance = 1e-3)

  # Cousineau: Nearly unbiased.. (Table 4)
  # agreement here is not as high: W3 is more challenging, in particular for small shape
  #+our median is based on higher sample size in our MC-sim
  shapes <- seq.int(0.5, 2.5, by = .5)
  expect_equal(
    w3FFint(6)(shapes),
    expected = c(5.631, 2.808, 2.004, 1.669, 1.492),
    tolerance = 5e-2
  )
  expect_equal(
    w3FFint(11)(shapes),
    expected = c(9.319, 3.462, 2.207, 1.774, 1.555),
    tolerance = 4e-2
  )
  expect_equal(
    w3FFint(12)(shapes),
    expected = c(10.051, 3.560, 2.239, 1.782, 1.565),
    tolerance = 3e-2
  )
  expect_equal(
    w3FFint(16)(shapes),
    expected = c(12.743, 3.854, 2.324, 1.820, 1.586),
    tolerance = 2e-2
  )

  # W3 for neighbouring nObs and some shapes
  # we use also fractional nObs to test if the spline interpolation of coefficients works properly
  w3Ex_mat <- c(
    15.999,
    16,
    16.1,
    16.11,
    16.25,
    16.3,
    16.5,
    16.6,
    16.7,
    16.9,
    17,
    17.1,
    17.2,
    17.3,
    17.5,
    18,
    19.1,
    50,
    50.01,
    50.1,
    50.2,
    51,
    52,
    53,
    54,
    55,
    55.1,
    55.11,
    55.13,
    59,
    60,
    61,
    61.01,
    61.02,
    61.5,
    61.6,
    61.9,
    61.99,
    62
  ) |>
    sort.int() |>
    purrr::map(.f = function(n_) w3FFint(n_)(shapes)) |>
    unlist() |>
    matrix(
      ncol = length(shapes),
      byrow = TRUE,
      dimnames = list(list(), shape = paste0("k=", shapes))
    )
  # W3 decreases as function of shape (for given n)
  expect_lte(
    w3Ex_mat |>
      apply(MARGIN = 1, FUN = diff) |>
      # diffs between shapes are put in columns: hence, next apply with MARGIN=2
      apply(MARGIN = 2, FUN = max, simplify = TRUE) |>
      max(),
    expected = 0
  )

  # W3 increases as function of n (for given shape)
  expect_gte(
    w3Ex_mat |>
      apply(MARGIN = 2, FUN = diff) |>
      apply(MARGIN = 2, FUN = min, simplify = TRUE) |>
      min(),
    expected = 0
  )

  # W3 function gradient
  w3Fn6 <- w3FFint(nObs = 6)
  expect_type(w3Fn6, type = "closure")
  expect_named(formals(w3Fn6), "k")
  expect_true("gradient" %in% names(attributes(w3Fn6)))
  w3Fn6_gr <- attr(w3Fn6, which = "gradient", exact = TRUE)
  expect_type(w3Fn6_gr, type = "closure")
  expect_named(formals(w3Fn6_gr), "k")

  xVals <- c(.1, .5, sqrt(unique(stats::rpois(n = 3, lambda = 3.5))))
  expect_equal(
    object = w3Fn6_gr(xVals),
    expected = numDeriv::grad(w3Fn6, x = xVals),
    tolerance = 1e-7
  )
})


test_that("Richards's generalized logistic function", {
  # we consider the package-internal list MLEw_approx
  expect_true(exists("MLEw_approx"))

  expect_named(
    MLEw_approx[["fun"]],
    expected = c("genLogisticF", "genLogisticD", "genLogisticJ")
  )
  # MLEw_approx[["fun"]] contains only functions
  purrr::walk(.x = names(MLEw_approx[["fun"]]), .f = function(nam) {
    expect_type(MLEw_approx[["fun"]][[nam]], "closure")
  })

  myN <- sample(x = seq.int(from = 2, to = 1000), size = 1)
  # negative log shapes
  myNLShapes <- -log(sample(
    x = seq.int(from = .05, to = 25, by = .05),
    size = 5
  ))
  # some parameters values
  startL <- c(
    L = .1 + stats::runif(n = 1, min = -.1, max = .2),
    A = log1p(log(2 * myN)),
    d = log(myN + 7),
    K = .5 + stats::rnorm(n = 1, sd = .1),
    Xi = .1 + stats::rnorm(n = 1, sd = .2)
  )

  # test 1st derivative (for k)
  expect_equal(
    MLEw_approx$fun$genLogisticD(xVal = myNLShapes, theta = startL),
    expected = numDeriv::grad(
      func = MLEw_approx$fun$genLogisticF,
      x = myNLShapes,
      theta = startL
    ),
    tolerance = 1e-7
  )

  # test gradient function (for parameters)
  expect_equal(
    MLEw_approx$fun$genLogisticJ(theta = startL, xVal = myNLShapes),
    expected = purrr::map(.x = myNLShapes, .f = function(.x) {
      numDeriv::grad(func = MLEw_approx$fun$genLogisticF, x = startL, xVal = .x)
    }) |>
      # convert to single matrix, columns = nbr of parameters
      unlist() |>
      matrix(
        ncol = 5,
        byrow = TRUE,
        dimnames = list(NULL, c("L", "A", "d", "K", "Xi"))
      ),
    tolerance = 1e-7
  )
})


test_that("Estimate rounding error from sample", {
  set.seed(1234)
  # some random data (around 0)
  obsList <- list(
    obs1 = rnorm(31L),
    obs2 = rt(37, df = 3),
    obs3 = sqrt(rpois(51, lambda = 11))
  )

  # cross observation vectors and rounding digits 0:3
  purrr::walk2(
    .x = rep(obsList, 4),
    # rounding digits
    .y = rep(0:3, each = length(obsList)),
    .f = ~ expect_identical(
      estimRoundingError(round(.x, .y)),
      expected = 10**-.y
    )
  )

  # rounds everything to 0
  expect_identical(estimRoundingError(round(obsList$obs1, -2)), expected = 1)
  expect_identical(estimRoundingError(round(obsList$obs2, -1)), expected = 1)
  expect_identical(estimRoundingError(round(obsList$obs3, -1)), expected = 1)

  # zeros at the end
  expect_identical(
    estimRoundingError(round(obsList$obs1, 3) * 10000),
    expected = 10
  )
  expect_identical(
    estimRoundingError(round(obsList$obs2, 2) * 1000),
    expected = 10
  )
  expect_identical(
    estimRoundingError(round(obsList$obs3, 1) * 100),
    expected = 10
  )
  expect_identical(
    estimRoundingError(round(obsList$obs1, 0) * 10),
    expected = 10
  )

  expect_identical(
    estimRoundingError(round(obsList$obs1, 1) * 1000),
    expected = 100
  )
  expect_identical(
    estimRoundingError(round(obsList$obs2, 1) * 1000),
    expected = 100
  )
  expect_identical(
    estimRoundingError(round(obsList$obs3, 1) * 1000),
    expected = 100
  )

  # exceeding the specified precision (e.g. here max(roundDigits) is 5) we expect to have one more
  expect_identical(
    estimRoundingError(obsList$obs1, roundDigits = -2:5),
    expected = 1e-6
  )
  expect_identical(
    estimRoundingError(obsList$obs2, roundDigits = -13:5),
    expected = 1e-6
  )
  expect_identical(
    estimRoundingError(obsList$obs1, roundDigits = 4:8),
    expected = 1e-9
  )
  expect_identical(
    estimRoundingError(obsList$obs3, roundDigits = -5:6),
    expected = 1e-7
  )
  expect_identical(
    estimRoundingError(obsList$obs3, roundDigits = -3:7),
    expected = 1e-8
  )
  expect_identical(
    estimRoundingError(obsList$obs3, roundDigits = -1:8),
    expected = 1e-9
  )

  # exceeding the specified precision on the negative side
  expect_identical(
    estimRoundingError(round(obsList$obs3, 0) * 1000, roundDigits = -2:5),
    expected = 10**3
  )
})


test_that("Ties in data", {
  set.seed(2023 - 04 - 17)
  # draw random data
  x <- sqrt(5 + stats::rpois(n = 17L, lambda = 9))
  # add ties
  x <- sort(sample(x = x, size = length(x) + 1, replace = TRUE))
  xs <- sort(survival::Surv(
    time = x,
    event = sample(x = c(0, 1, 1), size = length(x), replace = TRUE),
    type = "right"
  ))

  expect_error(
    objFunFactory(
      x = x,
      distO = buildDist("exponential"),
      control = buildControl(
        verbose = 0,
        profiled = FALSE,
        pen_shape = FALSE,
        ties = "error"
      )
    ),
    regexp = "ties"
  )

  # objFunEqui1 <- objFunFactory(x = x, control = buildControl(ties = "equi"))
  # x_pp <- rlang::env_get(rlang::fn_env(objFunEqui1), nm = "x")
  # # there are no duplicates any more!
  # expect_false(any(duplicated(x_pp)))
  # #waldo::compare(x, y=rlang::env_get(rlang::fn_env(objFunEqui1), nm = "x"))
  # # deviations through tie-break are below and above tie and sum to zer0
  # expect_identical(sum(x_pp - x), expected = 0)

  # objFunEqui2 <- objFunFactory(x = xs, control = buildControl(ties = "equi"))
  # xs_pp <- rlang::env_get(rlang::fn_env(objFunEqui2), nm = "x")
  # # there are no duplicates any more!
  # #+ for array/matrix: it means no duplicate rows, here: no time + status duplicates!
  # #+ this means, we allow for same times that are once as observed time and once as a (right-) censoring time
  # expect_false(any(duplicated(xs_pp)))
})


test_that("S3-integration functions", {
  expect_type(logLik.incubate_fit, type = "closure")
  expect_named(
    formals(logLik.incubate_fit),
    expected = c("object", "method", "...")
  )
})


test_that("Lambert W function", {
  # we consider the package-internal function lambertW0
  expect_true(exists("lambertW0_cpp"))
  expect_type(lambertW0_cpp, type = "closure")
  expect_named(formals(lambertW0_cpp), expected = "x")

  # test some known values
  expect_equal(lambertW0_cpp(0), expected = 0)
  expect_equal(lambertW0_cpp(-exp(-1)), expected = -1)
  expect_equal(lambertW0_cpp(exp(1)), expected = 1)

  # x-arguments to evaluate
  x_vals <- c(
    seq.int(from = -exp(-1) + .0001, to = exp(1), length.out = 47),
    5,
    7,
    exp(2),
    9.2,
    10,
    11.5,
    13,
    50
  )
  ## expected results from lamW (v2.2.7 from CRAN)
  ##lamW::lambertW0(x = x_vals) |> dput()
  W0_vals_expected <- c(
    -0.976862865574426,
    -0.491617685777098,
    -0.322509223411854,
    -0.204246949165174,
    -0.111110729590312,
    -0.0334375846930903,
    0.0336020103451618,
    0.0928120892163823,
    0.145982070413602,
    0.194331346342889,
    0.238732108154064,
    0.279831921925831,
    0.318126057507369,
    0.354002577818024,
    0.387771719819175,
    0.419685751482768,
    0.449952809625704,
    0.478746798236195,
    0.506214630106121,
    0.532481629876748,
    0.557655635547172,
    0.58183016004334,
    0.605086861830733,
    0.627497499468096,
    0.649125495177315,
    0.670027198330716,
    0.690252915894688,
    0.70984775993406,
    0.728852350084642,
    0.7473033999967,
    0.765234210169733,
    0.782675084676732,
    0.799653685556196,
    0.81619533581081,
    0.832323279764542,
    0.848058907830504,
    0.863421951410968,
    0.878430652600534,
    0.893101912528694,
    0.90745142151014,
    0.921493773633452,
    0.935242567983309,
    0.948710498336646,
    0.96190943288277,
    0.97485048527842,
    0.987544078151124,
    1,
    1.3267246652422,
    1.52434520498414,
    1.55714559899761,
    1.69281227191955,
    1.7455280027407,
    1.83519583660931,
    1.91515223953636,
    2.86089017798221
  )
  expect_identical(
    length(x_vals),
    expected = length(W0_vals_expected)
  )
  for (i in seq_along(x_vals)) {
    expect_equal(
      lambertW0_cpp(x_vals[i]),
      expected = W0_vals_expected[i],
      tolerance = 1e-7
    )
  }
})
