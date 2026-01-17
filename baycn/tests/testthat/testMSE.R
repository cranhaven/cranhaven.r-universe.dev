context('MSE')

test_that('the edgewise and whole graph MSE are calculated correctly',{

  # Load the baycn output for gn4 ----------------------------------------------

  load(system.file('testdata',
                   'baycn_gn4.RData',
                   package = 'baycn'))

  # Adjacency matrices ---------------------------------------------------------

  # Fully connected adjacency matrix for topology gn4.
  am_gn4 <- matrix(c(0, 1, 1, 1,
                     0, 0, 1, 1,
                     0, 0, 0, 1,
                     0, 0, 0, 0),
                   byrow = TRUE,
                   nrow = 4)

  # Expected probability matrix ------------------------------------------------

  # Expected probabilities for GN4 - fully connected graph.
  ep_gn4_fc <- matrix(c(1/3, 2/3, 0,
                        1, 0, 0,
                        0, 0, 1,
                        0, 0, 1,
                        2/3, 1/3, 0,
                        0, 1, 0),
                      byrow = TRUE,
                      ncol = 3)

  # Calculate the eMSE and gMSE for gn4 ----------------------------------------

  emse_gn4 <- mse(posterior = list(baycn_gn4),
                  expected = ep_gn4_fc,
                  type = 'emse')

  expect_equal(as.vector(emse_gn4), c(0.006058963,
                                      0.004482667,
                                      0.028712000,
                                      0.124234667,
                                      0.032462519,
                                      0.001176000))

  gmse_gn4 <- mse(posterior = list(baycn_gn4),
                  expected = ep_gn4_fc,
                  type = 'gmse')

  expect_equal(as.vector(gmse_gn4), 0.03285447)

  # Use a random posterior probability adjacency matrix ------------------------

  rand_gn4 <- matrix(c(0, 0.160, 0.384, 0.216,
                       0.103, 0, 0.322, 0.272,
                       0.399, 0.145, 0, 0.069,
                       0.325, 0.466, 0.044, 0),
                     byrow = TRUE,
                     nrow = 4)

  # Calculate the eMSE and gMSE for the random output --------------------------

  emse_rand_gn4 <- mse(posterior = list(rand_gn4),
                       expected = ep_gn4_fc,
                       adjMatrix = am_gn4,
                       type = 'emse')

  expect_equal(as.vector(emse_rand_gn4), c(0.29697785,
                                           0.19524867,
                                           0.14832067,
                                           0.11426600,
                                           0.08066874,
                                           0.56848867))

  gmse_rand_gn4 <- mse(posterior = list(rand_gn4),
                       expected = ep_gn4_fc,
                       adjMatrix = am_gn4,
                       type = 'gmse')

  expect_equal(as.vector(gmse_rand_gn4), 0.2339951)

})
