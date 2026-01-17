context('data: GE, GV')

test_that('mhEdge infers the correct graph using PMR',{

  # Adjacency matrix for M1 - M4 -----------------------------------------------

  # Fully connected adjacency matrix for M1 - M4
  am_m <- matrix(c(0, 1, 1,
                   0, 0, 1,
                   0, 0, 0),
                 byrow = TRUE,
                 nrow = 3)

  # Simulate data for M1 and M3 ------------------------------------------------

  set.seed(3)

  data_m1 <- simdata(b0 = 0,
                     N = 500,
                     s = 1,
                     ss = 1,
                     q = 0.1,
                     graph = 'm1_gv')

  data_m3 <- simdata(b0 = 0,
                     N = 500,
                     s = 1,
                     ss = 1,
                     q = 0.1,
                     graph = 'm3_gv')

  # Run baycn on M1 and M3 -----------------------------------------------------

  baycn_m1 <- mhEdge(adjMatrix = am_m,
                     burnIn = 0.2,
                     data = data_m1,
                     iterations = 1000,
                     nCPh = 0,
                     nGV = 1,
                     pmr = TRUE,
                     prior = c(0.05,
                               0.05,
                               0.9),
                     progress = FALSE,
                     thinTo = 500)

  baycn_m3 <- mhEdge(adjMatrix = am_m,
                     burnIn = 0.2,
                     data = data_m3,
                     iterations = 1000,
                     nCPh = 0,
                     nGV = 1,
                     pmr = TRUE,
                     prior = c(0.05,
                               0.05,
                               0.9),
                     progress = TRUE,
                     thinTo = 500)

  # Calculate the MSE for M1 and M3 --------------------------------------------

  # Expected probabilities for M1 when using PMR
  ep_m1 <- matrix(c(1, 0, 0,
                    0, 0, 1,
                    1, 0, 0),
                  byrow = TRUE,
                  nrow = 3)

  mse_m1 <- sum((baycn_m1@posteriorES[, 2:4] - ep_m1)^2)

  # Expected probabilities for M3 when using PMR
  ep_m3 <- matrix(c(1, 0, 0,
                    1, 0, 0,
                    0, 0, 1),
                  byrow = TRUE,
                  nrow = 3)

  mse_m3 <- sum((baycn_m3@posteriorES[, 2:4] - ep_m3)^2)

  expect_true(mse_m1 < 0.1)
  expect_true(mse_m3 < 0.1)

})
