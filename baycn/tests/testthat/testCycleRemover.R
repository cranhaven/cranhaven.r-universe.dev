context('Remove cycles')

test_that('cycleRm can remove all cycles when cycles are present',{

  # Nested cycles --------------------------------------------------------------

  # Matrix of directed cycles
  cycles <- matrix(c(0, 1, 9, 0, 9, 9, 9,
                     1, 0, 9, 1, 9, 9, 9,
                     9, 0, 1, 9, 0, 0, 0,
                     9, 1, 0, 9, 1, 1, 1,
                     0, 9, 1, 0, 0, 0, 0,
                     1, 9, 0, 1, 1, 1, 1),
                   byrow = TRUE,
                   nrow = 6)

  # Edge indices for each directed cycle
  edgeID <- list(c(1, 4, 2),
                 c(2, 4, 1),
                 c(2, 5, 6, 7, 3),
                 c(3, 7, 6, 5, 2),
                 c(1, 4, 5, 6, 7, 3),
                 c(3, 7, 6, 5, 4, 1))

  # Edge types for each edge: 1 for gv-ge and 0 otherwise
  edgeType <- c(0, 0, 0, 0, 0, 0, 0)

  # Edge states for the current individual
  currentES <- c(0, 0, 1, 0, 0, 0, 0)

  # Remove the directed cycles.
  cr <- cycleRmvr(cycles = cycles,
                  edgeID = edgeID,
                  edgeType = edgeType,
                  currentES = currentES,
                  nCycles = 6,
                  nEdges = 7,
                  pmr = FALSE,
                  prior = c(0.05, 0.05, 0.9))

  # Calculate the number of edge chagnes
  nChanges <- sum(currentES != cr)

  # Check that currentEs is not the same vector as cr
  expect_false(identical(currentES, cr))
  # Chech that up to three edges were chagned
  expect_true(nChanges <= 3)

  # Disjoint cycles ------------------------------------------------------------

  # Matrix of directed cycles
  cycles <- matrix(c(0, 1, 0, 9, 9, 9,
                     1, 0, 1, 9, 9, 9,
                     9, 9, 9, 0, 1, 0,
                     9, 9, 9, 1, 0, 1),
                   byrow = TRUE,
                   nrow = 4)

  # Edge indices for each directed cycle
  edgeID <- list(c(1, 3, 2),
                 c(2, 3, 1),
                 c(4, 6, 5),
                 c(5, 6, 4))

  # Edge types for each edge: 1 for gv-ge and 0 otherwise
  edgeType <- c(0, 0, 0, 0, 0, 0)

  # Edge states for the current individual
  currentES <- c(0, 1, 0, 1, 0, 1)

  # Remove the directed cycles.
  cr <- cycleRmvr(cycles = cycles,
                  edgeID = edgeID,
                  edgeType = edgeType,
                  currentES = currentES,
                  nCycles = 4,
                  nEdges = 6,
                  pmr = FALSE,
                  prior = c(0.05, 0.05, 0.9))

  # Calculate the number of edge chagnes
  nChanges <- sum(currentES != cr)

  # Check that currentEs is not the same vector as cr
  expect_false(identical(currentES, cr))
  # Chech that only 2 edges were chagned
  expect_true(nChanges == 2)

  # Single cycle ---------------------------------------------------------------

  # Matrix of directed cycles
  cycles <- matrix(c(0, 1, 0,
                     1, 0, 1),
                   byrow = TRUE,
                   nrow = 2)

  # Edge indices for each directed cycle
  edgeID <- list(c(1, 3, 2),
                 c(2, 3, 1))

  # Edge types for each edge: 1 for gv-ge and 0 otherwise
  edgeType <- c(0, 0, 0)

  # Edge states for the current individual
  currentES <- c(0, 1, 0)

  # Remove the directed cycles.
  cr <- cycleRmvr(cycles = cycles,
                  edgeID = edgeID,
                  edgeType = edgeType,
                  currentES = currentES,
                  nCycles = 2,
                  nEdges = 3,
                  pmr = FALSE,
                  prior = c(0.05, 0.05, 0.9))

  # Calculate the number of edge chagnes
  nChanges <- sum(currentES != cr)

  # Check that currentEs is not the same vector as cr
  expect_false(identical(currentES, cr))
  # Chech that only 1 edge was chagned
  expect_true(nChanges == 1)

})
