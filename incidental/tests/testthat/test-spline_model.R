library(incidental)

#'Test parameter initialization
test_that("want equality", {
	expect_equal(length(init_params(10)), 10)
})


#' test make_spline_basis
test_that('want equality', {
	expect_equal(dim(make_spline_basis(10, 1:25)), c(25, 10))
})

#' test make_spline_basis values
test_that('cr spline output is as expected', {
  expected_data_frame_splines <- data.frame(
    "1" = c(1.00000000,  0.72496571,  0.46639232,  0.24074074,  0.06447188, -0.04663923, -0.09259259, -0.08916324, -0.05281207,  0.00000000),
    "2" = c(0.0000000, 0.3278464, 0.6227709, 0.8518519, 0.9821674, 0.9821674, 0.8518519, 0.6227709, 0.3278464, 0.0000000),
    "3" = c(0.00000000, -0.05281207, -0.08916324, -0.09259259, -0.04663923,  0.06447188,  0.24074074,  0.46639232, 0.72496571,  1.00000000)
  )
  names(expected_data_frame_splines) = c("1", "2", "3")
  expect_equal(as.data.frame(make_spline_basis(3, 1:10)), expected_data_frame_splines, tol = 1E-7)
})

#' likelihood matrix
test_that('make_likelihood_matrix', {
  delay_dist = c(.2, .3, .4, .1)
  Pres <- rbind(c(.2, 0, 0, 0),
                c(.3, .2, 0, 0),
                c(.4, .3, .2, 0),
                c(.1, .4, .3, .2))
	expect_equal(make_likelihood_matrix(delay_dist), Pres)
})


#' Incidence
test_that('compute_log_incidence', {
  beta = c(-.51, .5, -1, 0.1, -.4)
  tgrid = 1:80
  Q = make_spline_basis(length(beta), tgrid)
  Tobs  = 40
  expect_equal(length(compute_log_incidence(beta, Q, Tobs)), Tobs)
})

#' cases
test_that('compute expected cases', {
  reported = c(1, 2, 3, 4, 65, 6, 6)
  beta = c(1, 2, 3, 4)
  Q = make_spline_basis(4, 1:20)
  delay_dist <- c(.05, .1, .4, .3, .1, .05, 0)
  Pmat = make_likelihood_matrix(delay_dist)
  lnPmat = log(Pmat)
  EC = compute_expected_cases(beta, Q, lnPmat, Tobs=length(reported))
  EC_true = c(0.1359141, 0.4309894, 1.5920198,
              2.6798081, 3.4099985, 4.1291687, 4.8354338)
  expect_equal(EC, EC_true, tolerance=1e-4)
})

#' marginal likelihood
test_that('marg_loglike_poisson', {
  reported = c(1, 2, 3, 4, 65, 6, 6)
  beta = c(1, 2, 3, 4)
  Q = make_spline_basis(4, 1:20)
  delay_dist <- c(.05, .1, .4, .3, .1, .05, 0)
  Pmat = make_likelihood_matrix(delay_dist)
  lnPmat = log(Pmat)
  expect_equal(marg_loglike_poisson(beta, reported, Q, lnPmat),
               -146.0179, tolerance=1E-4)
  beta=c(0, 0, 0, 0)
  expect_equal(marg_loglike_poisson(beta, reported, Q, lnPmat),
               -245.2817, tolerance=1E-4)
})

#' beta regularization
test_that('regfun', {
  beta = c(1, 2, 3, 4)
  expect_equal(regfun(beta, 1), 3)
  expect_equal(regfun(beta, 2), 0)
  expect_equal(regfun(beta, 0), sum(beta**2))
})

#' Poisson objective function
test_that('full objective', {
  reg_diff = 2
  lam = .5
  reported = c(1, 2, 3, 4, 65, 6, 6)
  beta = c(1, 2, 3, 4)
  Q = make_spline_basis(4, 1:20)
  delay_dist <- c(.05, .1, .4, .3, .1, .05, 0)
  Pmat = make_likelihood_matrix(delay_dist)
  lnPmat = log(Pmat)
  expect_equal(poisson_objective(beta, lam, reported, Q, lnPmat, reg_diff),
               1.68, tolerance=1E-2)
  beta = c(0, 0, 0, 0)
  expect_equal(poisson_objective(beta, lam, reported, Q, lnPmat, reg_diff),
               2.81933, tolerance=1E-5)
})



#' train and validate

test_that('train_and_validate fitting error', {
  reported = c(1, 2, 3, 4, 6, 5, 6)
  beta = c(1, 2, 3, 4, 1, 2, 3, 4)
  delay_dist = c(.05, .1, .4, .3, .1, .05, 0)
  end_pad = 10
  reg_diff = 2
  lam = .1
  dof = 8
  set.seed(1)
  # make sure MAPE is less than or equal to .3859 for this case
  res = train_and_validate(reported, delay_dist, lam, dof, beta0=beta)
  mape = mean(abs(res$Chat - reported)/reported)
  expect_lt(mape, 0.3949408*1.001)
  set.seed(1)
  res = train_and_validate(reported, delay_dist, lam, dof, beta0=NULL)
  mape = mean(abs(res$Chat - reported)/reported)
  expect_lt(mape, 0.3949408*1.001)
})

#' scan degrees of freedom

test_that('scan spline df chooses and scan with aic', {
  reported = c(1, 2, 3, 4, 6, 5, 6)
  delay_dist <- c(.05, .1, .4, .3, .1, .05, 0)
  dof_grid = c(4, 6, 8)
  set.seed(2)
  res = scan_spline_dof(reported, delay_dist, dof_grid, lam=.001)
  expect_equal(res$best_dof, 4)

  dof_grid = c(6)
  res = scan_spline_dof(reported, delay_dist, dof_grid, lam=.001)
  expect_equal(res$best_dof, 6)
  set.seed(2)
  dof_grid = c(4, 6, 8)
  res = scan_spline_dof(reported, delay_dist, dof_grid, method='bic', lam=.001)
  expect_equal(res$best_dof, 4)
})

#' scan lambda

test_that('scan spline df chooses with aic', {
  reported = c(1, 2, 3, 4, 6, 5, 6)
  delay_dist <- c(.05, .1, .4, .3, .1, .05)
  lam_grid = c(.1, .01, .001)
  set.seed(2)
  res = scan_spline_lam(reported, delay_dist,
                        lam_grid=lam_grid,
                        reported_val=reported,
                        dof=6)
  expect_equal(res$best_lam, .1)

  res = scan_spline_lam(reported, delay_dist,
                        lam_grid=c(1),
                        reported_val=reported,
                        dof=6)
  expect_equal(res$best_lam, 1)

  set.seed(2)
  res = scan_spline_lam(reported, delay_dist,
                        lam_grid=lam_grid,
                        reported_val=NULL,
                        method='aic',
                        dof=6)
  expect_equal(res$best_lam, .001)
})


#''''''''''''''''''''''''''''
#' Gradient checks
#''''''''''''''''''''''''''''

#' numerical gradient function
num_grad_fun <- function(fun, beta) {
  dbeta = rep(0, length(beta))
  for(p in 1:length(beta)){
    de = rep(0, length(beta))
    de[p] = 1e-5
    lhi = fun(beta+de)
    llo = fun(beta-de)
    dbeta[p] = (lhi-llo)/(2*1e-5)
  }
  return(dbeta)
}


#' Marg Loglike Gradient tests
test_that('marg_loglike_poisson grad', {
  reported = c(1, 2, 3, 4, 10, 6, 6)
  Q = make_spline_basis(4, 1:length(reported))
  delay_dist <- c(.05, .1, .4, .3, .1, .05, 0)
  Pmat = make_likelihood_matrix(delay_dist)
  lnPmat = log(Pmat)

  # test different betas
  beta = c(1, 2, 3, 4)
  dbeta_num = num_grad_fun(function(beta){marg_loglike_poisson(beta, reported, Q, lnPmat)}, beta)
  dbeta_an = marg_loglike_poisson_grad(beta, reported, Q, lnPmat)
  expect_equal(dbeta_num, dbeta_an, tolerance=1E-7)
  beta = c(-2, 1, 3, 1.1)
  dbeta_num = num_grad_fun(function(beta){marg_loglike_poisson(beta, reported, Q, lnPmat)}, beta)
  dbeta_an = marg_loglike_poisson_grad(beta, reported, Q, lnPmat)
  expect_equal(dbeta_num, dbeta_an, tolerance=1E-7)

  # test different Q length
  beta = c(-2, 1, 3, 1.1)
  Q = make_spline_basis(4, 1:20)
  dbeta_num = num_grad_fun(function(beta){marg_loglike_poisson(beta, reported, Q, lnPmat)}, beta)
  dbeta_an = marg_loglike_poisson_grad(beta, reported, Q, lnPmat)
  expect_equal(dbeta_num, dbeta_an, tolerance=1E-7)

  # test different data sequence
  reported = c(1, 0, 1, 4, 10, 6, 11)
  dbeta_num = num_grad_fun(function(beta){marg_loglike_poisson(beta, reported, Q, lnPmat)}, beta)
  dbeta_an = marg_loglike_poisson_grad(beta, reported, Q, lnPmat)
  expect_equal(dbeta_num, dbeta_an, tolerance=1E-7)
})

#' Marg Loglike Fisher
test_that('marg_loglike_poisson fisher', {
  reported = c(1, 2, 3, 4, 10, 6, 6)
  Q = make_spline_basis(4, 1:length(reported))
  delay_dist <- c(.05, .1, .4, .3, .1, .05, 0)
  Pmat = make_likelihood_matrix(delay_dist)
  lnPmat = log(Pmat)
  # test different betas
  beta = c(1, 2, 3, 4)
  fisher = matrix(marg_loglike_poisson_fisher(beta, reported, Q, lnPmat), 4,4)
  fisher_fixed = matrix(c(1.87682523, 1.9498205, 0.0475954, -0.01573467,
                          1.94982045, 7.5307570,  5.3853163,  0.61203036,
                          0.04759540, 5.3853163, 11.8172139,  2.68016564,
                         -0.01573467, 0.6120304,  2.6801656,  0.77578152), 4,4)
  expect_equal(fisher, fisher_fixed, tolerance=1E-5)
})


#' Regfun tests

#' test reg fun gradient for different values of reg_diff and beta

test_that('reg fun gradient diff=0, 1, 2', {
  beta = c(-2, 1, 3, 1.1, -1.)
  reg_diff = 0
  dbeta_num = num_grad_fun(function(beta){regfun(beta, reg_diff)}, beta)
  dbeta_an = regfun_grad(beta, reg_diff)
  expect_equal(dbeta_num, dbeta_an, tolerance=1E-7)

  reg_diff = 1
  dbeta_num = num_grad_fun(function(beta){regfun(beta, reg_diff)}, beta)
  dbeta_an = regfun_grad(beta, reg_diff)
  expect_equal(dbeta_num, dbeta_an, tolerance=1E-7)

  reg_diff = 2
  dbeta_num = num_grad_fun(function(beta){regfun(beta, reg_diff)}, beta)
  dbeta_an = regfun_grad(beta, reg_diff)
  expect_equal(dbeta_num, dbeta_an, tolerance=1E-7)
})


#' hessian tests
test_that('reg fun hessian diff=0, 1, 2', {
  beta = c(-2, 1, 1.1, -1.)
  reg_diff = 0
  H = regfun_hess(beta, reg_diff)
  expect_equal(H, diag(rep(2, length(beta))), tolerance=1E-6)

  beta = c(-2, 1, 3, 4, 3, 1.1, -1.)
  reg_diff = 1
  Hnum = numDeriv::hessian(function(beta){ regfun(beta, reg_diff) }, beta)
  H = regfun_hess(beta, reg_diff)
  expect_equal(Hnum, H, tolerance=1E-6)

  beta = c(-2, 1, 3, 4, 3, 1.1, -1.)
  reg_diff = 2
  Hnum = numDeriv::hessian(function(beta){ regfun(beta, reg_diff) }, beta)
  H = regfun_hess(beta, reg_diff)
  expect_equal(Hnum, H, tolerance=1E-6)
})


#' Poisson optimization objective gradient tests

test_that('poisson_objective_grad reg_diff=1, 2', {
  reported = c(1, 2, 3, 4, 10, 6, 6)
  Q = make_spline_basis(7, 1:length(reported))
  delay_dist <- c(.05, .1, .4, .3, .1, .05, 0)
  Pmat = make_likelihood_matrix(delay_dist)
  lnPmat = log(Pmat)

  # test different data sequence
  lam = .1
  reg_diff = 1
  beta = c(.1, -.5, -2.4, 4, 3.4, -1., 7.)
  dbeta_num = num_grad_fun(function(beta){poisson_objective(beta, lam, reported, Q, lnPmat, reg_diff)}, beta)
  dbeta_an = poisson_objective_grad(beta, lam, reported, Q, lnPmat, reg_diff)
  expect_equal(dbeta_num, dbeta_an, tolerance=1E-7)

  # test different data sequence
  lam = .001
  reg_diff = 2
  beta = c(.1, -.5, -2.4, 4, 3.4, -5., 7.)
  dbeta_num = num_grad_fun(function(beta){poisson_objective(beta, lam, reported, Q, lnPmat, reg_diff)}, beta)
  dbeta_an = poisson_objective_grad(beta, lam, reported, Q, lnPmat, reg_diff)
  expect_equal(dbeta_num, dbeta_an, tolerance=1E-7)
})


