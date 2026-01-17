# mutate
#
# Takes the current graph and changes at least one of the edges from its
# current edge state to a different edge state.
#
# @param edgeType A 0 or 1 indicating whether the edge is a gv-ge edge (1) or
# a gv-gv or ge-ge edge (0).
#
# @param graph A vector containing the current edge states for each edge.
#
# @param nCPh The number of clinical phenotypes in the graph.
#
# @param nEdges The number of edges in the graph.
#
# @param pmr Logical. If true the Metropolis-Hastings algorithm will use the
# Principle of Mendelian Randomization, PMR. This prevents the direction of an
# edge pointing from a gene expression node to a genetic variant node.
#
# @param prior A vector containing the prior probability of seeing each edge
# direction.
#
# @param ztbProb A vector of probabilities for 1:nEdges from a zero truncated
# binomial distribution.
#
# @return A vector of the edge directions.
#
#' @importFrom stats rbinom
#'
mutate <- function (edgeType,
                    graph,
                    nCPh,
                    nEdges,
                    pmr,
                    prior,
                    ztbProb) {

  # Generate a zero truncated binomial random variable to determine the number
  # of edges to change states.
  nChange <- sample(x = 1:nEdges,
                    size = 1,
                    prob = ztbProb)

  # Select which edges will be changed from their current state to a different
  # state.
  wEdges <- sample(x = 1:nEdges,
                   size = nChange,
                   replace = FALSE)

  # Loop through the edges to be mutated
  for (e in 1:nChange) {

    if (graph[[wEdges[[e]]]] == 0) {

      # The position in the prior vector of the prior probability for the two
      # edge states that the current edge can move to.
      prPos <- c(2, 3)

      # The states the current edge can move to.
      states <- c(1, 2)

    } else if (graph[[wEdges[[e]]]] == 1) {

      # The position in the prior vector of the prior probability for the two
      # edge states that the current edge can move to.
      prPos <- c(1, 3)

      # The states the current edge can move to.
      states <- c(0, 2)

    } else {

      # The position in the prior vector of the prior probability for the two
      # edge states that the current edge can move to.
      prPos <- c(1, 2)

      # The states the current edge can move to.
      states <- c(0, 1)

    }

    # Calculate the probability of moving to the two possible edge states.
    probability <- cPrior(prPos = prPos,
                          edgeType = edgeType[[wEdges[[e]]]],
                          nCPh = nCPh,
                          pmr = pmr,
                          prior = prior)

    # Select one of the two possible edge states according to the prior.
    graph[[wEdges[[e]]]] <- sample(x = states,
                                   size = 1,
                                   prob = probability)

  }

  return (graph)

}
