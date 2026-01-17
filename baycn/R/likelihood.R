# logLikEnv
#
# Creates an environment for each node in the graph and calculates the log
# likelihood for each node without any parents.
#
# @param data A matrix with the variables across the columns and the
# observations down the rows. If there are genetic variants in the data these
# variables must come before the remaining variables. If there are clinical
# phenotypes in the data these variables must come after all other variables.
# For example, if there is a data set with one genetic variant variable, three
# gene expression variables, and one clinical phenotype variable the first
# column in the data matrix must contain the genetic variant data, the next
# three columns will contain the gene expression data, and the last column will
# contain the clinical phenotype data.
#
# @param nCPh The number of clinical phenotypes in the graph.
#
# @param nGV The number of genetic variants in the graph.
#
# @param nNodes The number of nodes in the graph.
#
logLikEnv <- function (data,
                       nCPh,
                       nGV,
                       nNodes) {

  # Create an environment that will hold all of the node environments. This
  # environment will be returned at the end of the function.
  allNodes <- new.env(parent = emptyenv())

  # Create a vector of names for each node
  nodeNames <- paste0('node', 1:nNodes)

  # Create an environment for each element in nodeNames.
  for (e in 1:nNodes) {

    # Create an environment for each node in the network.
    assign(eval(nodeNames[[e]]),
           new.env(parent = emptyenv()),
           envir = allNodes)

    # Calculate the log likelihood for each node with no parents.
    assign(paste0('dn', 0),
           cll(data = data,
               nodeIndex = e,
               nCPh = nCPh,
               nGV = nGV,
               nNodes = nNodes,
               pVector = rep(0, nNodes)),
           envir = allNodes[[eval(nodeNames[[e]])]])

  }

  return (allNodes)

}

# lull
#
# A function to look up the likelihood for a given set of nodes based on the
# current parent configuration for each node. If the log likelihood for the node
# has not been calculated previously it will then be calculated.
#
# @param data A matrix with the variables across the columns and the
# observations down the rows. If there are genetic variants in the data these
# variables must come before the remaining variables. If there are clinical
# phenotypes in the data these variables must come after all other variables.
# For example, if there is a data set with one genetic variant variable, three
# gene expression variables, and one clinical phenotype variable the first
# column in the data matrix must contain the genetic variant data, the next
# three columns will contain the gene expression data, and the last column will
# contain the clinical phenotype data.
#
# @param am The adjacency matrix of the current graph
#
# @param likelihood A vector containing the log likelihood for each node.
#
# @param llenv The environment that has the likelihood for each parent combintation
# for each node.
#
# @param nCPh The number of clinical phenotypes in the graph.
#
# @param nGV The number of genetic variants in the graph.
#
# @param nNodes The number of nodes in the graph.
#
# @param wNodes the nodes for which the likelihood will be looked up.
#
lull <- function (data,
                  am,
                  likelihood,
                  llenv,
                  nCPh,
                  nGV,
                  nNodes,
                  wNodes) {

  # Check if wNodes has length zero. If it doesn't then the proposed and current
  # edge state vectors are different.
  if (length(wNodes) != 0) {

    # Loop through each node that has changed
    for (e in wNodes) {

      # Create the name for the current node. This will be used to find the
      # environment containing the likelihood values for the current node.
      node <- paste0('node', e)

      # Create the name for the current parent configuration. This will be used
      # to look up the likelihood based on the current parent configuration.
      parents <- paste0('dn',
                        sum(am[, e] * 2^(0:(nNodes - 1))))

      # Check if the log likelihood has been calculated previously.
      if (is.null(llenv[[node]][[parents]])) {

        # Calculate the log likelihood for the current parent combination.
        assign(parents,
               cll(data = data,
                   nodeIndex = e,
                   nCPh = nCPh,
                   nGV = nGV,
                   nNodes = nNodes,
                   pVector = am[, e]),
               envir = llenv[[node]])

      }

      # Assign the likelihood for the eth node to the likelihood vector.
      likelihood[[e]] <- llenv[[node]][[parents]]

    }

  }

  return (likelihood)

}

# cll
#
# Calculates the log likelihood of the current node.
#
# @param data A matrix with the nodes across the columns and the observations
# along the rows.
#
# @param nodeIndex The index of the for loop for the nodes.
#
# @param nCPh The number of clinical phenotypes in the graph.
#
# @param nGV The number of genetic variants in the graph.
#
# @param nNodes The number of nodes in the graph.
#
# @param pVector The vector of parent nodes for the current node.
#
# @return The log likelihood of the graph according to the orientation of the
# edges denoted by the DNA of the current graph.
#
#' @importFrom stats dnorm
#'
cll <- function (data,
                 nodeIndex,
                 nCPh,
                 nGV,
                 nNodes,
                 pVector) {

  # Check if the current node is a genetic variant. If e is less than the number
  # of genetic variants then the current node is a genetic variant.
  if (nodeIndex <= nGV) {

    # Calculate the log likelihood for the multinomial variables.
    logLikelihood <- cllMultinom(data = data,
                                 nodeIndex = nodeIndex,
                                 pVector = pVector)

    # If e is greater than nGV but less than nNodes - nCPh then the node is a
    # gene expression node.
  } else if (nodeIndex <= (nNodes - nCPh)) {

    # Calculate the log likelihood for the normal variables.
    logLikelihood <- cllNormal(data = data,
                               nodeIndex = nodeIndex,
                               pVector = pVector)

    # If e is greater than nNodes - nCPh then the node is a clinical phenotype
    # node.
  } else {

    # Calculate the log likelihood for the binomial variables.
    logLikelihood <- cllBinom(data = data,
                              nodeIndex = nodeIndex,
                              pVector = pVector)

  }

  return (logLikelihood)

}

# cllMultinom
#
# Calculates the log likelihood for a node with multinomial data.
#
# @param data The data matrix.
#
# @param nodeIndex The index of the for loop for the nodes
#
# @param pVector The vector of parent nodes for the current node.
#
# @return The log likelihood of the current node.
#
#' @importFrom MASS polr
#'
#' @importFrom stats dmultinom logLik
#'
cllMultinom <- function (data,
                         nodeIndex,
                         pVector){

  # Check if the current node has any parents.
  if (sum(pVector) == 0) {

    # Calculate the counts for each level of the current genetic variant.
    counts <- as.vector(table(data[, nodeIndex]))

    # Calculate the probability of each level of counts.
    lprob <- log(counts / sum(counts))

    # Store the log likelihood for the current node.
    ll <- sum(lprob * counts)

  } else {

    # Check for the number of levels for the current node. If there are three or
    # more levels the polr function will be used to calculate the likelihood. If
    # there are only two levels to the current node then the glm function will
    # be used to calculate the log likelihood.
    if (length(as.vector(table(data[, nodeIndex]))) > 2) {

      # Calculate the log likelihood for the current node when it has three or
      # more levels.
      ll <- logLik(polr(as.factor(data[, nodeIndex]) ~
                          data[, which(pVector != 0 )]))[1]

    } else {

      # Calculate the log likelihood for the current node when it only has two
      # levels.
      ll <- logLik(glm(data[, nodeIndex] ~ data[, which(pVector != 0)],
                       family = binomial(link = 'logit')))[1]

    }

  }

  return (ll)

}

# cllNormal
#
# Calculates the log likelihood for a node with normal data.
#
# @param data The data matrix.
#
# @param nodeIndex The index of the for loop for the nodes
#
# @param pVector The vector of parent nodes for the current node.
#
# @return The log likelihood for the current node.
#
#' @importFrom stats lm logLik sd
#'
cllNormal <- function (data,
                       nodeIndex,
                       pVector){

  # Check if the current node has any parents.
  if (sum(pVector) == 0) {

    # Calculate the log likelihood for the current continuous node.
    ll <- sum(dnorm(x = data[, nodeIndex],
                    mean = mean(data[, nodeIndex]),
                    sd = sd(data[, nodeIndex]),
                    log = TRUE))

  } else {

    # Store the log likelihood for the current node.
    ll <- logLik(lm(data[, nodeIndex] ~ data[, which(pVector != 0)]))[1]

  }

  return (ll)

}

# cllBinom
#
# Calculates the log likelihood for a node with binomial data.
#
# @param data The data matrix.
#
# @param nodeIndex The index of the for loop for the nodes
#
# @param pVector The vector of parent nodes for the current node.
#
# @return The log likelihood of the current node.
#
#' @importFrom stats binomial dbinom glm logLik
#'
cllBinom <- function (data,
                      nodeIndex,
                      pVector){

  # Check if the current node has any parents.
  if (sum(pVector) == 0) {

    # Calculate the counts for the two levels of the current clinical phenotype.
    counts <- as.vector(table(data[, nodeIndex]))

    # Calculate the probability of each level of counts.
    lprob <- log(counts / sum(counts))

    # Store the log likelihood for the current node.
    ll <- sum(lprob * counts)

  } else {

    # Calculate the log likelihood for the current node.
    ll <- logLik(glm(data[, nodeIndex] ~ data[, which(pVector != 0)],
                     family = binomial(link = 'logit')))[1]

  }

  return (ll)

}

