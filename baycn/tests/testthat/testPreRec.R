context('precision/recall')

test_that('precision and recall are calculated correctly',{

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

  # The true undirected adjacency matrix for gn4.
  am_gn4_true <- matrix(c(0, 1, 1, 0,
                          1, 0, 0, 1,
                          1, 0, 0, 1,
                          0, 1, 1, 0),
                        byrow = TRUE,
                        nrow = 4)

  # Calculate the precision and recall for gn4 ---------------------------------

  prerec_gn4 <- prerec(amInferred = baycn_gn4,
                       amTrue = am_gn4_true,
                       cutoff = 0.4)

  expect_equal(prerec_gn4$precision, 0.8)
  expect_equal(prerec_gn4$recall, 1)

  # Use a random posterior probability adjacency matrix ------------------------

  rand_gn4 <- matrix(c(0, 0.160, 0.384, 0.216,
                       0.103, 0, 0.322, 0.272,
                       0.399, 0.145, 0, 0.069,
                       0.325, 0.466, 0.044, 0),
                     byrow = TRUE,
                     nrow = 4)

  # Calculate the precision and recall for the random output -------------------

  prerec_rand_gn4 <- prerec(amInferred = rand_gn4,
                            amTrue = am_gn4_true,
                            cutoff = 0.4)

  expect_equal(prerec_rand_gn4$precision, 0.5)
  expect_equal(prerec_rand_gn4$recall, 0.5)

})
