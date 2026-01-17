# carPrior
#
# Calculates the prior depending on the current edge state and edge type for
# the caRatio function. For example, gv-ge, gv-gv, ge-ge.
#
# @param edgeDir1 A scalar indicating the state of the edge for graph one (the
# graph - or state - being moved from).
#
# @param edgeDir2 A scalar indicating the state of the edge for graph two (the
# graph - or state - being moved to).
#
# @param edgeType A 0 or 1 indicating whether the edge is a gv-ge edge (1) or
# a gv-gv or ge-ge edge (1).
#
# @param nCPh The number of clinical phenotypes in the graph.
#
# @param pmr Logical. If true the Metropolis-Hastings algorithm will use the
# Principle of Mendelian Randomization, PMR. This prevents the direction of an
# edge pointing from a gene expression node to a genetic variant node.
#
# @param prior A vector containing the prior probability of seeing each edge
# direction.
#
# @return The probability of the specified edge state
#
carPrior <- function (edgeDir1,
                      edgeDir2,
                      edgeType,
                      nCPh,
                      pmr,
                      prior) {

  if (edgeType == 1 && (pmr || nCPh >= 1)) {

    # When using the pmr or if there are clinical phenotypes in the graph the
    # probability of a gv-ge, gv-cph, or ge-cph edge moving to edge state one is
    # zero. The prior of edge state zero is now the sum of states zero and one.
    prior <- c(prior[[1]] + prior[[2]], 0, prior[[3]])

  }

  # Extract the prior for the edge state in graph one.
  priors <- log(prior[[edgeDir1 + 1]])

  # Calculate the probability of moving from the state in graph one to the
  # edge state in graph two.
  transition <- log(prior[[edgeDir2 + 1]] /
                      sum(prior[-(edgeDir1 + 1)]))

  return (c(priors, transition))

}
