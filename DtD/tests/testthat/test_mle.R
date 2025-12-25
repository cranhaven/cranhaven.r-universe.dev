context("Testing MLE method")

test_that("mle method gives previous output without supplying a starting value for the vol", {
  set.seed(83486778)
  sims <- BS_sim(
    vol = .1, mu = .05, dt = .1, V_0 = 100, T. = 1, D = rep(80, 20), r = .01)

  # with(sims, dput(BS_fit(S = S, D = D, T. = T, r = r, time = time, method = "mle")))
  with(sims,
       expect_equal(
         BS_fit(S = S, D = D, T. = T, r = r, time = time, method = "mle")$est,
         structure(c(0.0805287001624244, 0.101322187871702),
                   .Names = c("mu", "vol")),
         tolerance = .Machine$double.eps^(1/3)))
})

test_that("mle method gives previous output when supplying a starting value for the vol", {
  set.seed(79156879)
  sims <- BS_sim(
    vol = .1, mu = .05, dt = .2, V_0 = 100, T. = 1, D = rep(80, 20), r = .01)

  # with(sims, dput(BS_fit(S = S, D = D, T. = T, r = r, time = time,
  #                        method = "mle", vol_start = 1)))
  with(sims,
       expect_equal(
         BS_fit(S = S, D = D, T. = T, r = r, time = time, method = "mle",
                vol_start = 1)$est,
         structure(c(0.102792869281686, 0.118660088139721),
                   .Names = c("mu", "vol")),
         tolerance = .Machine$double.eps^(1/3)))
})
