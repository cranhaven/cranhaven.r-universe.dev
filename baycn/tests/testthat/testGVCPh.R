context('data: GE, GV, CPh')

test_that('mhEdge infers the correct graph with clinical phenotypes present',{

  # Adjacency matrix for M1 with one clinical phenotype ------------------------

  # Fully connected adjacency matrix for M1 with one clinical phenotype
  am_m1_cph <- matrix(c(0, 1, 1, 1,
                        0, 0, 1, 1,
                        0, 0, 0, 1,
                        0, 0, 0, 0),
                      byrow = TRUE,
                      nrow = 4)

  # Simulate data for M1_CPh -------------------------------------------------

  set.seed(22)

  data_m1_cph <- simdata(b0 = 0,
                         N = 500,
                         s = 1,
                         ss = 1,
                         q = 0.1,
                         p = 0.6,
                         graph = 'm1_cph')

  # Run baycn on M1_CPh -- -----------------------------------------------------

  baycn_m1_cph <- mhEdge(adjMatrix = am_m1_cph,
                         burnIn = 0.2,
                         data = data_m1_cph,
                         iterations = 1000,
                         nGV = 1,
                         nCPh = 1,
                         pmr = TRUE,
                         prior = c(0.05,
                                   0.05,
                                   0.9),
                         progress = FALSE,
                         thinTo = 500)

  # Calculate the MSE for M1_CPh -----------------------------------------------

  # Expected probabilities for M1_CPh when using PMR and one clinical phenotype.
  ep_m1_cph <- matrix(c(1, 0, 0,
                        0, 0, 1,
                        0, 0, 1,
                        1, 0, 0,
                        1, 0, 0,
                        0, 0, 1),
                      byrow = TRUE,
                      ncol = 3)

  mse_m1_cph <- sum((baycn_m1_cph@posteriorES[, 2:4] - ep_m1_cph)^2)

  expect_true(mse_m1_cph < 0.1)

})
