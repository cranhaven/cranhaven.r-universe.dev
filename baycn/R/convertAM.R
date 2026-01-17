# convertAM
#
# Creates an adjacency matrix from a current adjacency matrix. This function
# will be used to change the current adjacency matrix currentAM to the proposed
# adjacency matrix proposedAM using the proposed edge states proposedES. This
# matrix will be used to look up the log likelihood for the proposed graph.
#
# @param adjMatrix The adjacency matrix for the current graph.
#
# @param coordinates A matrix with the row and column coordinates of the
# nonzero elements in the adjacency matrix. The row numbers of the nonzero
# elements appear in the first row and the column numbers of the nonzero
# elements appear in the second row.
#
# @param edgeStates A vector containing the directions of the edges in the
# graph and the log likelihood of the graph.
#
# @param wEdges The edge indices that have changed state.
#
# @return An adjacency matrix given a new vector of edge states.
#
convertAM <- function (adjMatrix,
                       coordinates,
                       edgeStates,
                       wEdges) {

  # Check if wEdges has length zero. If it doesn't then the proposed and current
  # edge state vectors are different.
  if (length(wEdges) != 0) {

    # Change the elements of the adjacency matrix according to the edge states.
    for (e in wEdges) {

      if (edgeStates[[e]] == 0) {

        # Change the i, j and j, i (where i < j) elements to represent an edge
        # pointing from node i to node j.
        adjMatrix[coordinates[1, e], coordinates[2, e]] <- 1
        adjMatrix[coordinates[2, e], coordinates[1, e]] <- 0

      } else if (edgeStates[[e]] == 1) {

        # Change the i, j and j, i (where i < j) elements to represent an edge
        # pointing from node j to node i.
        adjMatrix[coordinates[2, e], coordinates[1, e]] <- 1
        adjMatrix[coordinates[1, e], coordinates[2, e]] <- 0

      } else {

        # Change the i, j and j, i (where i < j) elements to represent the
        # absence of an edge between nodes i and j.
        adjMatrix[coordinates[1, e], coordinates[2, e]] <- 0
        adjMatrix[coordinates[2, e], coordinates[1, e]] <- 0

      }

    }

  }

  return (adjMatrix)

}
