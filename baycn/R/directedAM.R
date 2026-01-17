# directedAM
#
# Takes a posterior proability adjacency matrix and using the cutoff for edge
# presence and direction converts it to a directed adjacency matrix. This
# matrix can then be used as input to the functions in the igraph package to
# create a plot of the network.
#
# @param ppm A posterior proability adjacency matrix.
#
# @param presence A scalar between 0 and 1. This is the cutoff for considering
# an edge to be present. For example, if presence = 0.4 then an edge is
# considered to be present if the sum of the posterior proability for the two
# edge directions is greater than 0.4. The edge will be considered to be absent
# if this sum is less than 0.4.
#
# @param direction A scalar between 0 and 1. This is the cutoff for determining
# the direction of an edge. For example, if direction = 0.2 then an edge is
# considered to be directed if the difference between the posterior proability
# for the two edge directions is greater than 0.2. An edge will be considered
# undirected if the difference is less than 0.2.
#
# @return An adjacency matrix that can be used as the input to create a plot of
# the network.
#
directedAM <- function (ppm,
                        presence,
                        direction) {

  # Extract the upper triangular matrix by row and the lower triangular matrix
  # by column and combine the two vectors in a matrix.
  directions <- rbind(t(ppm)[lower.tri(ppm)], # upper triangular matrix
                      ppm[lower.tri(ppm)])    # lower triangular matrix

  # Loop through the elements of the adjacency matrix and determine which is
  # larger the upper or lower element. This will be used to determine the
  # direction of the edge. If the two values are within a certain value, say
  # 0.2, then the edge will be considered bidirected.
  for (e in 1:dim(directions)[2]) {

    # Determine which value is larger.
    grande <- which.max(directions[, e])

    # Calculate the absolute value of the difference between the posterior
    # probablity of the two edge directions.
    difference <- abs(directions[1, e] - directions[2, e])

    # Calcualte the sum of the two directions.
    total <- sum(directions[1, e],
                 directions[2, e])

    # If the total is less than presence then set both elements to 0.
    if (total < presence) {

      directions[1, e] <- 0
      directions[2, e] <- 0

    }

    # If the difference is greater than 0.2 trun the larger value to 1 and the
    # smaller value to 0.
    if (difference > direction) {

      # If the larger value is in the first row set the value in the second row
      # to 0.
      if (grande == 1) {

        directions[2, e] <- 0

        # If the larger value is in the second row set the value in the first
        # row to 0.
      } else {

        directions[1, e] <- 0

      }

    }

  }

  # Create an empty matrix that will become a directed adjacency matrix.
  dAM <- matrix(0,
                nrow = dim(ppm)[1],
                ncol = dim(ppm)[2])

  # Name the rows and columns of the dAM matrix to match the ppm input.
  rownames(dAM) <- rownames(ppm)
  colnames(dAM) <- colnames(ppm)

  # Fill in the upper triangular matrix of the directed adjacency matrix.
  dAM[lower.tri(ppm)] <- directions[1, ]
  dAM <- t(dAM)

  # Fill in the lower triangular matrix of the directed adjacency matrix.
  dAM[lower.tri(ppm)] <- directions[2, ]

  return (dAM)

}
