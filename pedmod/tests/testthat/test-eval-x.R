context("testing eval functions")

test_that("A previous problematic case gives the correct result", {
  dat <- list(list(y = 1L, X = matrix(c(1, 1, 34), 1L),
                   scale_mats = list(matrix(1, 1, 1))))
  ptr <- pedigree_ll_terms(dat, 1L)

  par <- c(-3.64537483404081, 0.06815334989362926, 0.0162241770644372,
           -.487601434379729)

  fn_func <- function(par)
    pnorm(sum(dat[[1L]]$X * par[-4]) / sqrt(1 + exp(par[4])), log.p = TRUE)

  # check that we get the right function value
  fn <- eval_pedigree_ll(ptr, par, maxvls = 1000L, abs_eps = 0,
                         minvls = 100, rel_eps = 1e-3, n_threads = 1L)
  expect_equal(c(fn), fn_func(par))

  fn <- eval_pedigree_ll(ptr, par, maxvls = 1000L, abs_eps = 0,
                         minvls = 100, rel_eps = 1e-3, n_threads = 1L,
                         use_aprx = TRUE)
  expect_equal(c(fn), fn_func(par))

  # dput(numDeriv::grad(fn_func, par))
  true_grad <- c(2.13706257290667, 2.13706257261275, 72.6601274806369, 1.23000330372289)
  gr <- eval_pedigree_grad(ptr, par, maxvls = 1000L, abs_eps = 0,
                           minvls = 100, rel_eps = 1e-3, n_threads = 1L)
  expect_equal(true_grad, c(gr), tolerance = .Machine$double.eps^(4/11))

  fn_func_org <- function(par)
    pnorm(sum(dat[[1L]]$X * par[-4]) / sqrt(1 + par[4]), log.p = TRUE)
  # dput(numDeriv::hessian(fn_func, par))
  # par_org <- par
  # par_org[4] <- exp(par[4])
  # dput(numDeriv::hessian(fn_func_org, par_org))
  true_hess <- structure(
    c(-0.561147673156897, -0.561147673148975, -19.0790208874682, -0.729505091366542, -0.561147673148975, -0.56114767150749, -19.0790208957654, -0.729505091362509, -19.0790208874682, -19.0790208957654, -648.68671007521, -24.8031731069689, -0.729505091366542, -0.729505091362509, -24.8031731069689, 0.34216532063984),
    .Dim = c(4L, 4L))
  true_hess_org <- structure(
    c(-0.561147673156897, -0.561147673148975, -19.0790208874682, -1.18793024464694, -0.561147673148975, -0.56114767150749, -19.0790208957654, -1.18793024464153, -19.0790208874682, -19.0790208957654, -648.68671007521, -40.389628319246, -1.18793024464694, -1.18793024464153, -40.389628319246, -2.35428451205402),
    .Dim = c(4L, 4L))

  hess <- eval_pedigree_hess(ptr, par, maxvls = 1000L, abs_eps = 0,
                             minvls = 100, rel_eps = 1e-3, n_threads = 1L)

  expect_equal(true_hess, hess, tolerance = .Machine$double.eps^(4/11),
               check.attributes = FALSE)
  expect_equal(true_hess_org, attr(hess, "hess_org"),
               tolerance = .Machine$double.eps^(4/11))
  expect_equal(true_grad, attr(hess, "grad"),
               tolerance = .Machine$double.eps^(4/11))
  expect_equal(fn_func(par), attr(hess, "logLik"),
               tolerance = .Machine$double.eps^(4/11))
})
