# cPrior
#
# Calculates the prior depending on the current edge state and edge type for
# the mutate function. For example, gv-ge, gv-gv, ge-ge.
#
# @param prPos A vector indicating the position in the prior vector of the prior
# probability for the two edge states that the current edge can move to.
#
# @param edgeType A 0 or 1 indicating whether the edge is a gv-ge edge (1) or
# a gv-gv or ge-ge edge (0).
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
cPrior <- function (prPos,
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

  # Calculate the probability of moving to the two possible edge states.
  return (c(prior[[prPos[[1]]]] / (prior[[prPos[[1]]]] + prior[[prPos[[2]]]]),
            prior[[prPos[[2]]]] / (prior[[prPos[[1]]]] + prior[[prPos[[2]]]])))

}
