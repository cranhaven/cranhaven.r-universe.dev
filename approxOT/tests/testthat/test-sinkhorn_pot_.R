testthat::test_that("sinkhorn potiential works log scale", {
  set.seed(20980)
  
  n <- 2^3
  m <- n/2
  d <- 5
  
  x <- matrix(rnorm(n*d),n,d)
  y <- matrix(rnorm(m*d),m,d)
  cost_matrix_A <- approxOT::cost_calc(t(x),t(x),2.0)^2
  cost_matrix_B <- approxOT::cost_calc(t(y),t(y),2.0)^2
  cost_matrix <- approxOT::cost_calc(t(x),t(y),2.0)^2
  mass_a <- rep(1/n,n)
  mass_b <- rep(1/m,m)
  epsilon <- 0.05
  eta <- 1/(0.05 * median(cost_matrix))
  max_e <- max(-cost_matrix * eta)
  
  potentials <- approxOT:::sinkhorn_pot_log_(
    mass_a = mass_a, mass_b = mass_b,
    cost_matrix = cost_matrix, epsilon = 0.05,
    niterations = 1e6,
    unbiased = FALSE, cost_matrix_A = cost_matrix_A,
    cost_matrix_B = cost_matrix_B
  )
  pot_exp <- approxOT:::sinkhorn_pot_(
    mass_a = mass_a, mass_b = mass_b,
    cost_matrix = cost_matrix, epsilon = 0.05,
    niterations = 1e6,
    unbiased = FALSE, cost_matrix_A = cost_matrix_A,
    cost_matrix_B = cost_matrix_B
  )
  # debugonce(approxOT:::log_sinkhorn_test)
  # r_pot_log <- approxOT:::log_sinkhorn_test(mass_a, mass_b, cost_matrix, eps = 0.05,
  #                                niterations = 1e4)
  
  # debugonce(approxOT:::log_sinkhorn_test_nomax)
  r_pot_log_nm <- approxOT:::log_sinkhorn_test_nomax(mass_a, mass_b, cost_matrix, eps = 0.05,
                                 niterations = 1e6)
  
  # testthat::expect_equal(potentials$f, r_pot_log_nm$f, tol = 1e-3)
  testthat::expect_equal(potentials$g, r_pot_log_nm$g, tol = 1e-3)
  # cbind(r_pot_log$f, r_pot_log_nm$f)
  # cbind(r_pot_log$g, r_pot_log_nm$g)
  # cbind(potentials$f, r_pot_log_nm$f)
  # cbind(potentials$g, r_pot_log_nm$g)
  # cbind(pot_exp$f, r_pot_log_nm$f)
  # cbind(pot_exp$g, r_pot_log_nm$g)
  # 7.192881
  exact_wass_p <- wasserstein(X = x, Y = y, a = mass_a, b = mass_b, cost = sqrt(cost_matrix), p = 2, method = "exact")^2
  sink_round <- wasserstein(X = x, Y = y, a = mass_a, b = mass_b, cost = sqrt(cost_matrix), p = 2, method = "sinkhorn", unbiased = FALSE)^2
  testthat::expect_gt( sum(potentials$f * mass_a) + sum(potentials$g * mass_b),
                       sink_round)
  # wasserstein(X = x, Y = y, a = mass_a, b = mass_b, cost = sqrt(cost_matrix), p = 2, method = "exact")^2
  testthat::expect_gt( sum(potentials$f * mass_a) + sum(potentials$g * mass_b),
                       exact_wass_p)
  # testthat::expect_equal(sum(potentials$f * mass_a) + sum(potentials$g * mass_b), sum(r_pot_log_nm$f * mass_a) + sum(r_pot_log_nm$g * mass_b))
  # sum(potentials$f * mass_a) + sum(potentials$g * mass_b)
  # sum(pot_exp$f * mass_a) + sum(pot_exp$g * mass_b)
  # sum(r_pot_log$f * mass_a) + sum(r_pot_log$g * mass_b)
  # sum(r_pot_log_nm$f * mass_a) + sum(r_pot_log_nm$g * mass_b)
  # sum(exp(approxOT:::generate_S(cost_matrix, potentials$f, potentials$g, eta))/sum(exp(approxOT:::generate_S(cost_matrix, potentials$f, potentials$g, eta))) * cost_matrix)
  # testthat::expect_equal(rowSums(exp(approxOT:::generate_S(cost_matrix, potentials$f, potentials$g, eta))/sum(exp(approxOT:::generate_S(cost_matrix, potentials$f, potentials$g, eta)))), mass_a)
  # testthat::expect_equal(colSums(exp(approxOT:::generate_S(cost_matrix, potentials$f, potentials$g, eta))/sum(exp(approxOT:::generate_S(cost_matrix, potentials$f, potentials$g, eta)))), mass_b)
  potentials_small <- approxOT:::sinkhorn_pot_log_(
    mass_a = mass_a, mass_b = mass_b,
    cost_matrix = cost_matrix, epsilon = 1e-10,
    niterations = 1e1,
    unbiased = FALSE, cost_matrix_A = cost_matrix_A,
    cost_matrix_B = cost_matrix_B
  )
  r_pot_log_nm_small <- approxOT:::log_sinkhorn_test_nomax(mass_a, mass_b, cost_matrix, eps = 1e-10,
                                                     niterations = 1e6)
  testthat::expect_equal(sum(potentials_small$f * mass_a) + sum(potentials_small$g * mass_b),
  sum(r_pot_log_nm_small$f * mass_a) + sum(r_pot_log_nm_small$g * mass_b))
  # sum(potentials_small$f * mass_a) + sum(potentials_small$g * mass_b)
  # exact_wass_p
  
  potentials_large <- approxOT:::sinkhorn_pot_log_(
    mass_a = mass_a, mass_b = mass_b,
    cost_matrix = cost_matrix, epsilon = 100,
    niterations = 1e6,
    unbiased = FALSE, cost_matrix_A = cost_matrix_A,
    cost_matrix_B = cost_matrix_B
  )
  pot_exp_large <- approxOT:::sinkhorn_pot_(
    mass_a = mass_a, mass_b = mass_b,
    cost_matrix = cost_matrix, epsilon = 100,
    niterations = 1e6,
    unbiased = FALSE, cost_matrix_A = cost_matrix_A,
    cost_matrix_B = cost_matrix_B
  )
  r_pot_KL_large <- approxOT:::log_sinkhorn_test_nomax_KL(mass_a, mass_b, cost_matrix, eps = 100,
                                                          niterations = 1e6)
  testthat::expect_equal(r_pot_KL_large$f, potentials_large$f)
  # sum(potentials_large$f * mass_a) + sum(potentials_large$g * mass_b)
  # sum(r_pot_log_nm_large$f * mass_a) + sum(r_pot_log_nm_large$g * mass_b)
  # sum(r_pot_KL_large$f * mass_a) + sum(r_pot_KL_large$g * mass_b)
  # sum(pot_exp_large$f * mass_a) + sum(pot_exp_large$g * mass_b)
  # exact_wass_p
  
  
})


testthat::test_that("support functions work", {
  set.seed(20980)
  
  n <- 2^3
  m <- n/2
  d <- 5
  
  x <- matrix(rnorm(n*d),n,d)
  y <- matrix(rnorm(m*d),m,d)
  cost_matrix_A <- approxOT::cost_calc(t(x),t(x),2.0)^2
  cost_matrix_B <- approxOT::cost_calc(t(y),t(y),2.0)^2
  cost_matrix <- approxOT::cost_calc(t(x),t(y),2.0)^2
  mass_a <- rep(1/n,n)
  mass_b <- rep(1/m,m)
  epsilon <- 0.5
  eta <- 1/(0.5 * median(cost_matrix))
  max_e <- max(-cost_matrix * eta)
  f <- rep(0, n)
  g <- rep(0, m)
  
  testthat::expect_equal(approxOT:::generate_S(cost_matrix,f,g,eta),
            sweep(sweep(cost_matrix, 1, f), 2, g) * -eta
            )
  
  testthat::expect_equal(approxOT:::colLogSumExp(cost_matrix), 
            approxOT:::col_logsumexp(cost_matrix))
  
  
  # maxes <- apply(cost_matrix,1,max)
  # sweeps <- sweep(cost_matrix, 1, maxes)
  # sums <- log(rowSums(exp(sweep(cost_matrix, 1, maxes))))
  # maxes
  # sweeps
  # sums
  # maxes + sums
  testthat::expect_equal(approxOT:::rowLogSumExp(cost_matrix), 
            approxOT:::row_logsumexp(cost_matrix))
  
  testthat::expect_equal(approxOT:::col_softmin(cost_matrix*(-eta))/(eta), 
            approxOT:::colMin_eps(cost_matrix, f, g, eta))
  testthat::expect_equal(approxOT:::row_softmin(cost_matrix*(-eta))/(eta), 
            approxOT:::rowMin_eps(cost_matrix, f, g, eta))
  
  f <- 1:n
  g <- 1:m
  testthat::expect_equal(approxOT:::col_softmin(sweep(sweep(cost_matrix, 1, f), 2, g) * -eta)/(eta), 
            approxOT:::colMin_eps(cost_matrix, f, g, eta))
  testthat::expect_equal(approxOT:::row_softmin(sweep(sweep(cost_matrix, 1, f), 2, g) * -eta)/(eta), 
            approxOT:::rowMin_eps(cost_matrix, f, g, eta))
  
  
})