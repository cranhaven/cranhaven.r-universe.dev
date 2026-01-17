# caRatio
#
# Calculates the acceptance ratio for the Metropolis-Hastings and Genetic
# Algorithm. It takes the ratio of the original vector of edge directions, old
# individual, and the proposed vector of edge directions, new individual.
#
# @param current A vector of edge directions. This is the graph from
# the start of the current interation.
#
# @param currentLL A vector containing the log likelihood for each node in the
# current graph.
#
# @param edgeType A 0 or 1 indicating whether the edge is a gv-ge edge (0) or
# a gv-gv or ge-ge edge (1).
#
# @param nCPh The number of clinical phenotypes in the graph.
#
# @param pmr Logical. If true the Metropolis-Hastings algorithm will use the
# Principle of Mendelian Randomization, PMR. This prevents the direction of an
# edge pointing from a gene expression node to a genetic variant node.
#
# @param proposed A vector of edge directions. This the the proposed
# vector of edge directions.
#
# @param proposedLL A vector containing the log likelihood for each node in the
# proposed graph.
#
# @param prior A vector containing the prior probability of seeing each edge
# direction.
#
# @param wEdges A vector of edge indices for the edges that differ between the
# current and proposed graphs.
#
# @return A character string indicating whether the 'proposed' or 'current'
# graph was accepted.
#
#' @importFrom stats runif
#'
caRatio <- function (current,
                     currentLL,
                     edgeType,
                     nCPh,
                     pmr,
                     proposed,
                     proposedLL,
                     prior,
                     wEdges) {

  # Check if wEdges has length zero. If it doesn't then the proposed and current
  # edge state vectors are different.
  if (length(wEdges) != 0) {

    # Ater getting the locations of the differences of the edge directions
    # between the current and proposed graphs we need to get the directions of
    # the edges.
    edgeDirP <- proposed[wEdges]
    edgeDirC <- current[wEdges]

    # Extract the edge type for the edges that have changed.
    etDiff <- edgeType[wEdges]

    # Number of changed edges.
    nDiff <- length(edgeDirP)

    # After getting the edge directions for each edge that is different between
    # the current and proposed graphs we need to get the prior probability
    # associated with each edge direction.
    priorP <- vector(mode = 'numeric',
                     length = nDiff)
    priorC <- vector(mode = 'numeric',
                     length = nDiff)

    # The following vectors will contain the transition probabilities for the
    # current and proposed graphs. transProbP will hold the probabilities for
    # moving from the proposed graph to the current graph and transProbC will
    # hold the probabilities of moving from the current graph to the proposed
    # graph.
    transProbP <- vector(mode = 'numeric',
                         length = nDiff)
    transProbC <- vector(mode = 'numeric',
                         length = nDiff)

    # Attach the correct prior probability to each edge direction for both the
    # current and proposed edge state vectors.
    for(e in 1:nDiff) {

      # I can use the edge direction (0, 1, or 2) to select the correct prior by
      # adding a 1 to it and using that number to subset the prior vector. For
      # example, if the edge direction is 0 the corresponding prior is in the
      # first position of the prior vector so prior[[0 + 1]] will give 0.05
      # which is the default prior for an edge being 0.

      # Calculate the log(prior) for the current edge state and the probability
      # of moving from the proposed graph to the current graph.
      ptProposed <- carPrior(edgeDir1 = edgeDirP[[e]],
                             edgeDir2 = edgeDirC[[e]],
                             edgeType = etDiff[[e]],
                             nCPh = nCPh,
                             pmr = pmr,
                             prior = prior)

      priorP[[e]] <- ptProposed[1]
      transProbP[[e]] <- ptProposed[2]

      # Calculate the log(prior) for the current edge state and the probability
      # of moving from the current graph to the proposed graph.
      ptCurrent <- carPrior(edgeDir1 = edgeDirC[[e]],
                            edgeDir2 = edgeDirP[[e]],
                            edgeType = etDiff[[e]],
                            nCPh = nCPh,
                            pmr = pmr,
                            prior = prior)

      priorC[[e]] <- ptCurrent[1]
      transProbC[[e]] <- ptCurrent[2]

    }

    ratio <- ((sum(priorP) + sum(proposedLL) + sum(transProbP))
              - (sum(priorC) + sum(currentLL) + sum(transProbC)))

    # Generate log uniform(0, 1) to compare to alpha which is
    # min(ratio, 0).
    logU <- log(runif(n = 1,
                      min = 0,
                      max = 1))

    alpha <- min(ratio, 0)

    # Determine if the proposed graph should be accepted.
    if (logU < alpha) {

      return ('proposed')

    }

  }

  # If the proposed and current edge state vectors are identical or if logU is
  # less than alpha return 'current'.
  return ('current')

}
