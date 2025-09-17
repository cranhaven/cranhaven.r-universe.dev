context("getAdjacenyMatrix")

test_that("test getAdjacenyMatrix can return the correct outputs in the case where individuals are represented by single letters and zeros represent no nomination.", {
  
  actualAdjacencyMatrix = as.data.frame(matrix(c(0, 0, 1, 0, 1, 0, 0, 0, 0, 1, 0, 1, 0, 0, 1, 0), ncol = 4))
  colnames(actualAdjacencyMatrix) = c("A", "B", "C", "D")
  rownames(actualAdjacencyMatrix) = c("A", "B", "C", "D")
  actualNonnominators = "There are no nonnominators! :)"
  actualVertexNoOutdegrees = character(0)
  actualVertexNoIndegrees = character(0)
  actualVertexIsolates = character(0)
  
  rawNetwork = matrix(NA, 4, 3)
  rawNetwork = as.data.frame(rawNetwork)
  colnames(rawNetwork)[1] = "labels"
  rawNetwork[, 1] = c("A", "B", "C", "D")
  rawNetwork[, 2] = c(0, "C", "D", 0)
  rawNetwork[, 3] = c("B", 0, "A", "C")
  adjacencyMatrixInfo = getAdjacencyMatrix(rawNetwork)
  adjacencyMatrix = adjacencyMatrixInfo$adjacencyMatrix
  nonnominators = adjacencyMatrixInfo$nonnominators
  vertexNoOutdegrees = adjacencyMatrixInfo$vertexNoOutdegrees
  vertexNoIndegrees = adjacencyMatrixInfo$vertexNoIndegrees
  vertexIsolates = adjacencyMatrixInfo$vertexIsolates
  
  expect_equal(adjacencyMatrix, actualAdjacencyMatrix)
  expect_equal(nonnominators, actualNonnominators)
  expect_equal(vertexNoOutdegrees, actualVertexNoOutdegrees)
  expect_equal(vertexNoIndegrees, actualVertexNoIndegrees)
  expect_equal(vertexIsolates, actualVertexIsolates)

})

test_that("test getAdjacenyMatrix can return the correct outputs in the case where individuals are represented by integers and zeros/NA represent no nomination.", {
  
  actualAdjacencyMatrix = as.data.frame(matrix(c(0, 0, 1, 0, 1, 0, 0, 0, 0, 1, 0, 1, 0, 0, 1, 0), ncol = 4))
  colnames(actualAdjacencyMatrix) = c(245, 344, 234, 104)
  rownames(actualAdjacencyMatrix) = c(245, 344, 234, 104)
  actualNonnominators = "There are no nonnominators! :)"
  actualVertexNoOutdegrees = character(0)
  actualVertexNoIndegrees = character(0)
  actualVertexIsolates = character(0)
  
  rawNetwork = matrix(NA, 4, 3)
  rawNetwork = as.data.frame(rawNetwork)
  colnames(rawNetwork)[1] = "labels"
  rawNetwork[, 1] = c(245, 344, 234, 104)
  rawNetwork[, 2] = c(NA, 234, 104, NA)
  rawNetwork[, 3] = c(344, 0, 245, 234)
  adjacencyMatrixInfo = getAdjacencyMatrix(rawNetwork)
  adjacencyMatrix = adjacencyMatrixInfo$adjacencyMatrix
  nonnominators = adjacencyMatrixInfo$nonnominators
  vertexNoOutdegrees = adjacencyMatrixInfo$vertexNoOutdegrees
  vertexNoIndegrees = adjacencyMatrixInfo$vertexNoIndegrees
  vertexIsolates = adjacencyMatrixInfo$vertexIsolates
  
  expect_equal(adjacencyMatrix, actualAdjacencyMatrix)
  expect_equal(nonnominators, actualNonnominators)
  expect_equal(vertexNoOutdegrees, actualVertexNoOutdegrees)
  expect_equal(vertexNoIndegrees, actualVertexNoIndegrees)
  expect_equal(vertexIsolates, actualVertexIsolates)
  
})


test_that("test getAdjacenyMatrix can return the correct outputs in the case where individuals are represented by names, zeros/NA represent no nomination, and there is one nonnominator.", {
  
  actualAdjacencyMatrix = as.data.frame(matrix(c(0, 0, 1, 0, 0, 1, 0, 0, 0, 0, 0, 1, 0, 1, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0), ncol = 5))
  colnames(actualAdjacencyMatrix) = c("Alistar", "Bob", "Charlie", "David", "Blaine")
  rownames(actualAdjacencyMatrix) = c("Alistar", "Bob", "Charlie", "David", "Blaine")
  actualNonnominators = "Blaine"
  actualVertexNoOutdegrees = "Blaine"
  actualVertexNoIndegrees = character(0)
  actualVertexIsolates = character(0)
  
  rawNetwork = matrix(NA, 4, 3)
  rawNetwork = as.data.frame(rawNetwork)
  colnames(rawNetwork)[1] = "labels"
  rawNetwork[, 1] = c("Alistar", "Bob", "Charlie", "David")
  rawNetwork[, 2] = c(NA, "Charlie", "David", 0)
  rawNetwork[, 3] = c("Bob", "Blaine", "Alistar", "Charlie")
  adjacencyMatrixInfo = getAdjacencyMatrix(rawNetwork)
  adjacencyMatrix = adjacencyMatrixInfo$adjacencyMatrix
  nonnominators = adjacencyMatrixInfo$nonnominators
  vertexNoOutdegrees = adjacencyMatrixInfo$vertexNoOutdegrees
  vertexNoIndegrees = adjacencyMatrixInfo$vertexNoIndegrees
  vertexIsolates = adjacencyMatrixInfo$vertexIsolates
  
  expect_equal(adjacencyMatrix, actualAdjacencyMatrix)
  expect_equal(nonnominators, actualNonnominators)
  expect_equal(vertexNoOutdegrees, actualVertexNoOutdegrees)
  expect_equal(vertexNoIndegrees, actualVertexNoIndegrees)
  expect_equal(vertexIsolates, actualVertexIsolates)
  
})
