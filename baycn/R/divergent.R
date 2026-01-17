# caRatio
#
# Calculates the acceptance ratio for the Metropolis-Hastings and Genetic
# Algorithm. It takes the ratio of the original vector of edge directions, old
# individual, and the proposed vector of edge directions, new individual.
#
# @param coordinates A matrix with the row and column coordinates of the
# nonzero elements in the adjacency matrix. The row numbers of the nonzero
# elements appear in the first row and the column numbers of the nonzero
# elements appear in the second row.
#
# @param currentES A vector containing the current edge states for each edge.
#
# @param proposedES A vector containing the proposed edge states for each edge.
#
# @return A list. The first element contains a vector of edge positions that are
# different between the current and proposed graphs. The second element contains
# the nodes whose parents have changed.
#
divergent <- function (coordinates,
                       currentES,
                       proposedES){

  # Determine which edges are different between the current and proposed graphs.
  dEdges <- which(currentES != proposedES)

  # Determine which nodes have a different parent structure based on the edges
  # that have changed state.
  dNodes <- unique(as.vector(coordinates[, dEdges]))

  return (list(dEdges = dEdges,
               dNodes = dNodes))

}
