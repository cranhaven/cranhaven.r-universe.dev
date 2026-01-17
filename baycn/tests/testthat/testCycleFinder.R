context('Identify cycles')

test_that('cycleFndr can find all cycles when cycles are present',{

  # Nested cycles --------------------------------------------------------------

  # Fully connected four node graph.
  adjMatrix<- matrix(1,
                     nrow = 4,
                     ncol = 4)

  diag(adjMatrix) <- 0

  # Get the coordinates of each edge in the network
  coord <- coordinates(adjMatrix)

  cf <- cycleFndr(adjMatrix = adjMatrix,
                  nEdges = 6,
                  nCPh = 0,
                  nGV = 0,
                  pmr = FALSE,
                  position = coord)

  # Check that all the cycles are found.
  expect_true(cf$nCycles == 14)
  # Check that the correct cycles were found.
  expect_identical(cf$cycles, matrix(c(0, 1, 9, 0, 9, 9,
                                       1, 0, 9, 1, 9, 9,
                                       0, 9, 1, 9, 0, 9,
                                       1, 9, 0, 9, 1, 9,
                                       9, 0, 1, 9, 9, 0,
                                       9, 1, 0, 9, 9, 1,
                                       9, 9, 9, 0, 1, 0,
                                       9, 9, 9, 1, 0, 1,
                                       0, 9, 1, 0, 9, 0,
                                       0, 1, 9, 9, 0, 1,
                                       9, 0, 1, 1, 0, 9,
                                       1, 0, 9, 9, 1, 0,
                                       9, 1, 0, 0, 1, 9,
                                       1, 9, 0, 1, 9, 1),
                                     nrow = 14,
                                     byrow = TRUE))



  # Disjoint cycles ------------------------------------------------------------

  # Two disjoint cycles (one three node cycle and one four node cycle). The four
  # node cycle also has nested three node cycles in it.
  adjMatrix <- matrix(c(0, 1, 1, 0, 0, 0, 0,
                        1, 0, 1, 0, 0, 0, 0,
                        1, 1, 0, 0, 0, 0, 0,
                        0, 0, 0, 0, 1, 1, 0,
                        0, 0, 0, 1, 0, 1, 1,
                        0, 0, 0, 1, 1, 0, 1,
                        0, 0, 0, 0, 1, 1, 0),
                      nrow = 7,
                      byrow = TRUE)

  # Get the coordinates of each edge in the network
  coord <- coordinates(adjMatrix)

  cf <- cycleFndr(adjMatrix = adjMatrix,
                  nEdges = 8,
                  nCPh = 0,
                  nGV = 0,
                  pmr = FALSE,
                  position = coord)

  # Check that all the cycles are found.
  expect_true(cf$nCycles == 8)
  # Check that the correct cycles were found.
  expect_identical(cf$cycles, matrix(c(1, 0, 1, 9, 9, 9, 9, 9,
                                       0, 1, 0, 9, 9, 9, 9, 9,
                                       9, 9, 9, 1, 0, 1, 9, 9,
                                       9, 9, 9, 9, 9, 1, 0, 1,
                                       9, 9, 9, 9, 9, 0, 1, 0,
                                       9, 9, 9, 0, 1, 0, 9, 9,
                                       9, 9, 9, 1, 0, 9, 1, 0,
                                       9, 9, 9, 0, 1, 9, 0, 1),
                                     nrow = 8,
                                     byrow = TRUE))

  # A tail between two cycles ----------------------------------------------------

  # Two cycles connected by a tail.
  adjMatrix <- matrix(c(0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0,
                        1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0,
                        1, 1, 0, 0, 0, 0, 0, 1, 0, 0, 0,
                        0, 0, 0, 0, 1, 0, 1, 0, 0, 0, 0,
                        0, 0, 0, 1, 0, 1, 0, 0, 0, 0, 1,
                        0, 0, 0, 0, 1, 0, 1, 0, 0, 0, 0,
                        0, 0, 0, 1, 0, 1, 0, 0, 0, 0, 0,
                        0, 0, 1, 0, 0, 0, 0, 0, 1, 0, 0,
                        0, 0, 0, 0, 0, 0, 0, 1, 0, 1, 0,
                        0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 1,
                        0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0),
                      byrow = TRUE,
                      nrow = 11)

  # Get the coordinates of each edge in the network
  coord <- coordinates(adjMatrix)

  cf <- cycleFndr(adjMatrix = adjMatrix,
                  nEdges = 12,
                  nCPh = 0,
                  nGV = 0,
                  pmr = FALSE,
                  position = coord)

  # Check that all the cycles are found.
  expect_true(cf$nCycles == 4)
  # Check that the correct cycles were found.
  expect_identical(cf$cycles, matrix(c(1, 0, 1, 9, 9, 9, 9, 9, 9, 9, 9, 9,
                                       0, 1, 0, 9, 9, 9, 9, 9, 9, 9, 9, 9,
                                       9, 9, 9, 9, 0, 1, 0, 9, 0, 9, 9, 9,
                                       9, 9, 9, 9, 1, 0, 1, 9, 1, 9, 9, 9),
                                     nrow = 4,
                                     byrow = TRUE))

  # Single cycle ---------------------------------------------------------------

  # The following adjacency matrix is from topology GN5
  adjMatrix <- matrix(c(0, 1, 1, 0, 0,
                        0, 0, 0, 1, 0,
                        0, 0, 0, 0, 1,
                        0, 0, 0, 0, 1,
                        0, 0, 0, 0, 0),
                      byrow = TRUE,
                      nrow = 5)

  # Get the coordinates of each edge in the network
  coord <- coordinates(adjMatrix)

  cf <- cycleFndr(adjMatrix = adjMatrix,
                  nEdges = 5,
                  nCPh = 0,
                  nGV = 0,
                  pmr = FALSE,
                  position = coord)

  # Check that all the cycles are found.
  expect_true(cf$nCycles == 2)
  # Check that the correct cycles were found.
  expect_identical(cf$cycles, matrix(c(1, 0, 1, 0, 1,
                                       0, 1, 0, 1, 0),
                                     nrow = 2,
                                     byrow = TRUE))

})

test_that('cycleFndr returns NULL if there are no cycles', {

  #           T4
  #           ^
  #     T1 -> T3 <- T2
  adjMatrix <- matrix(c(0, 0, 1, 0,
                        0, 0, 1, 0,
                        0, 0, 0, 1,
                        0, 0, 0, 0),
                      byrow = TRUE,
                      nrow = 4)

  # Get the coordinates of each edge in the network
  coord <- coordinates(adjMatrix)

  cf <- cycleFndr(adjMatrix = adjMatrix,
                  nEdges = 3,
                  nCPh = 0,
                  nGV = 0,
                  pmr = FALSE,
                  position = coord)

  # Check that NULL is returned when there are no cycles in the graph.
  expect_null(cf$cycles)

  # T1 - T3 - T5 - T7
  # |    |    |    |
  # T2   T4   T6   T8
  adjMatrix <- matrix(c(0, 1, 1, 0, 0, 0, 0, 0,
                        1, 0, 0, 0, 0, 0, 0, 0,
                        1, 0, 0, 1, 1, 0, 0, 0,
                        0, 0, 1, 0, 0, 0, 0, 0,
                        0, 0, 1, 0, 0, 1, 1, 0,
                        0, 0, 0, 0, 1, 0, 0, 0,
                        0, 0, 0, 0, 1, 0, 0, 1,
                        0, 0, 0, 0, 0, 0, 1, 0),
                      byrow = TRUE,
                      nrow = 8)

  # Get the coordinates of each edge in the network
  coord <- coordinates(adjMatrix)

  cf <- cycleFndr(adjMatrix = adjMatrix,
                  nEdges = 7,
                  nCPh = 0,
                  nGV = 0,
                  pmr = FALSE,
                  position = coord)

  # Check that NULL is returned when there are no cycles in the graph.
  expect_null(cf$cycles)

})

test_that('cycleFndr correctly uses the PMR', {

  # Fully connected three node graph.
  adjMatrix <- matrix(1,
                      nrow = 3,
                      ncol = 3)

  diag(adjMatrix) <- 0

  # Get the coordinates of each edge in the network
  coord <- coordinates(adjMatrix)

  cf <- cycleFndr(adjMatrix = adjMatrix,
                  nEdges = 3,
                  nCPh = 0,
                  nGV = 1,
                  pmr = TRUE,
                  position = coord)

  # Check that NULL is returned for a three node graph when using the PMR.
  expect_null(cf$cycles)

  # Fully connected five node graph.
  adjMatrix <- matrix(1,
                      nrow = 5,
                      ncol = 5)

  diag(adjMatrix) <- 0

  # Get the coordinates of each edge in the network
  coord <- coordinates(adjMatrix)

  cf <- cycleFndr(adjMatrix = adjMatrix,
                  nEdges = 10,
                  nCPh = 0,
                  nGV = 1,
                  pmr = TRUE,
                  position = coord)

  # Check that all the cycles are found.
  expect_true(cf$nCycles == 14)
  # Check that the correct cycles were found.
  expect_identical(cf$cycles, matrix(c(9, 9, 9, 9, 0, 1, 9, 0, 9, 9,
                                       9, 9, 9, 9, 1, 0, 9, 1, 9, 9,
                                       9, 9, 9, 9, 0, 9, 1, 9, 0, 9,
                                       9, 9, 9, 9, 1, 9, 0, 9, 1, 9,
                                       9, 9, 9, 9, 9, 0, 1, 9, 9, 0,
                                       9, 9, 9, 9, 9, 1, 0, 9, 9, 1,
                                       9, 9, 9, 9, 9, 9, 9, 0, 1, 0,
                                       9, 9, 9, 9, 9, 9, 9, 1, 0, 1,
                                       9, 9, 9, 9, 0, 9, 1, 0, 9, 0,
                                       9, 9, 9, 9, 0, 1, 9, 9, 0, 1,
                                       9, 9, 9, 9, 9, 0, 1, 1, 0, 9,
                                       9, 9, 9, 9, 1, 0, 9, 9, 1, 0,
                                       9, 9, 9, 9, 9, 1, 0, 0, 1, 9,
                                       9, 9, 9, 9, 1, 9, 0, 1, 9, 1),
                                     nrow = 14,
                                     byrow = TRUE))

})

test_that('cycleFndr correctly uses clinical phenotypes', {

  # Fully connected four node graph.
  adjMatrix <- matrix(1,
                      nrow = 4,
                      ncol = 4)

  diag(adjMatrix) <- 0

  # Get the coordinates of each edge in the network
  coord <- coordinates(adjMatrix)

  cf <- cycleFndr(adjMatrix = adjMatrix,
                  nEdges = 6,
                  nCPh = 1,
                  nGV = 1,
                  pmr = TRUE,
                  position = coord)

  # Check that NULL is returned for a four node graph when using the PMR and
  # there are clinical phenotypes present.
  expect_null(cf$cycles)

  # Fully connected five node graph.
  adjMatrix <- matrix(1,
                      nrow = 5,
                      ncol = 5)

  diag(adjMatrix) <- 0

  # Get the coordinates of each edge in the network
  coord <- coordinates(adjMatrix)

  cf <- cycleFndr(adjMatrix = adjMatrix,
                  nEdges = 10,
                  nCPh = 1,
                  nGV = 1,
                  pmr = TRUE,
                  position = coord)

  # Check that all the cycles are found.
  expect_true(cf$nCycles == 2)
  # Check that the correct cycles were found.
  expect_identical(cf$cycles, matrix(c(9, 9, 9, 9, 0, 1, 9, 0, 9, 9,
                                       9, 9, 9, 9, 1, 0, 9, 1, 9, 9),
                                     nrow = 2,
                                     byrow = TRUE))

})
