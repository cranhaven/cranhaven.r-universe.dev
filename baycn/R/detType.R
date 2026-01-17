# detType
#
# Determines the type of edge between two nodes. For example, gv-ge, gv-gv,
# ge-ge, ge-cph.
#
# @param coord A matrix of row and column indices for each edge in the
# network. The indices are from the adjacency matrix.
#
# @param nEdges The number of edges in the graph.
#
# @param nCPh The number of clinical phenotypes in the graph.
#
# @param nGV The number of genetic variants in the graph.
#
# @param nNodes The number of nodes in the network
#
# @param pmr Logical. If true the Metropolis-Hastings algorithm will use the
# Principle of Mendelian Randomization, PMR. This prevents the direction of an
# edge pointing from a gene expression node to a genetic variant node.
#
# @return A vector of edge types: 1 if edge is between gv-ge and  0 otherwise.
#
detType <- function (coord,
                     nEdges,
                     nCPh,
                     nGV,
                     nNodes,
                     pmr) {

  # Initialize the edge type vector with zeros. This vector will be returned
  # unchanged if PMR is not being used and there are no clinical phenotypes.
  forbidden <- c(rep(0, nEdges))

  # When using the PMR or if clinical phenotypes are present there are
  # restrictions on the states some edges can take.
  if (pmr || nCPh >= 1) {

    # Determine the number of gv and ge nodes.
    nGVGE <- nNodes - nCPh

    # Loop through the coordinates matrix and determine the edge type based on
    # where the coordinates fall in the adjacency matrix.
    for (e in 1:nEdges) {

      # Determine the type when using PMR and clinical phenotypes are present.
      if (pmr && nCPh >= 1) {

        # Edges between gv-ge, gv-cph, or ge-cph nodes are of type 1.
        # Edges between gv-gv, ge-ge, or cph-cph nodes are of type 0.
        if (((coord[1, e] <= nGV && coord[2, e] > nGV) ||
             (coord[1, e] <= nGVGE && coord[2, e] > nGVGE))) {

          forbidden[[e]] <- 1

        }

        # Determine the type when using PMR and clinical phenotypes are not
        # present.
      } else if (pmr && nCPh == 0) {

        # Edges between gv-ge nodes are of type 1.
        # Edges between gv-gv or ge-ge nodes are of type 0.
        if (coord[1, e] <= nGV && coord[2, e] > nGV) {

          forbidden[[e]] <- 1

        }

        # Determine the type when not using PMR and clinical phenotypes are
        # present.
      } else {

        # Edges between gv-cph or ge-cph nodes are of type 1.
        # Edges between gv-gv, gv-ge, ge-ge, or cph-cph nodes are of type 0.
        if (coord[1, e] <= nGVGE && coord[2, e] > nGVGE) {

          forbidden[[e]] <- 1

        }

      }

    }

  }

  return (forbidden)

}
