context('Log likelihood')

test_that('the likelihood functions calculate the log likelihood correctly',{

  # Simulate data for gn4, m2_gv, and m1_cph -----------------------------------

  set.seed(33)

  data_gn4 <- simdata(b0 = 0,
                      N = 200,
                      s = 1,
                      ss = 1,
                      graph = 'gn4')

  data_m2_gv <- simdata(b0 = 0,
                        N = 200,
                        s = 1,
                        ss = 1,
                        q = 0.45,
                        graph = 'm2_gv')

  data_m1_cph <- simdata(b0 = 0,
                         N = 200,
                         s = 1,
                         ss = 1,
                         p = 0.6,
                         q = 0.45,
                         graph = 'm1_cph')

  # Adjacency matrices ---------------------------------------------------------

  # True adjacency matrix for GN4.
  am_gn4 <- matrix(c(0, 1, 1, 0,
                     0, 0, 0, 1,
                     0, 0, 0, 0,
                     0, 0, 1, 0),
                   byrow = TRUE,
                   nrow = 4)

  # True adjacency matrix for M2.
  am_m2 <- matrix(c(0, 1, 0,
                    0, 0, 0,
                    0, 1, 0),
                  byrow = TRUE,
                  nrow = 3)

  # True adjacency matrix for M1 with a clinical phenotype node.
  am_m1_cph <- matrix(c(0, 1, 0, 0,
                        0, 0, 1, 1,
                        0, 0, 0, 0,
                        0, 0, 0, 0),
                      byrow = TRUE,
                      nrow = 4)

  # Create the likelihood environments -----------------------------------------

  logle_gn4 <- logLikEnv(data = data_gn4,
                         nCPh = 0,
                         nGV = 0,
                         nNodes = 4)

  logle_m2_gv <- logLikEnv(data = data_m2_gv,
                           nCPh = 0,
                           nGV = 1,
                           nNodes = 3)

  logle_m1_cph <- logLikEnv(data = data_m1_cph,
                            nCPh = 1,
                            nGV = 1,
                            nNodes = 4)

  # Calculate the log likelihood for gn4 ---------------------------------------

  ll_gn4 <- lull(data = data_gn4,
                 am = am_gn4,
                 likelihood = vector(length = 4),
                 llenv = logle_gn4,
                 nCPh = 0,
                 nGV = 0,
                 nNodes = 4,
                 wNodes = 1:4)

  expect_equal(round(sum(ll_gn4), 3), -1121.318)

  # Calculate the log likelihood for m2_gv -------------------------------------

  ll_m2_gv <- lull(data = data_m2_gv,
                   am = am_m2,
                   likelihood = vector(length = 3),
                   llenv = logle_m2_gv,
                   nCPh = 0,
                   nGV = 1,
                   nNodes = 3,
                   wNodes = 1:3)

  expect_equal(round(sum(ll_m2_gv), 3), -774.512)

  # Calculate the log likelihood for m1_cph ------------------------------------

  ll_m1_cph <- lull(data = data_m1_cph,
                    am = am_m1_cph,
                    likelihood = vector(length = 4),
                    llenv = logle_m1_cph,
                    nCPh = 1,
                    nGV = 1,
                    nNodes = 4,
                    wNodes = 1:4)

  expect_equal(round(sum(ll_m1_cph), 3), -874.837)

})
