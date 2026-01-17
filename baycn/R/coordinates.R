# coordinates
#
# Takes the adjacency matrix and extracts the row and column numbers for each
# nonzero element. It also takes the nonzero elements and collapses them into a
# vector. This vector is the DNA of the individual. The length of the DNA is
# the number of edges in the graph.
#
# @param adjMatrix The adjacency matrix returned by the MRPC algorithm. The
# adjacency matrix is a matrix of zeros and ones. The ones represent an edge
# and also indicates the direction of that edge.
#
# @return A matrix. The first row in the matrix holds the row locations of the
# nonzero elements in the adjacency matrix and the second row holds the column
# locations of the nonzero elements in the adjacency matrix.
#
coordinates <- function (adjMatrix) {

  # Get the number of ones in the adjacency matrix. This is a rough estimate of
  # the number of edges in the network.
  total <- sum(adjMatrix == 1)

  # Initialize the vectors to full length (and possibly over the actual number
  # of edges in the graph).
  Rows <- vector(mode = 'integer',
                 length = total)
  Columns <- vector(mode = 'integer',
                    length = total)

  i <- 1

  for (e in 1:nrow(adjMatrix)) {

    for (v in 1:ncol(adjMatrix)) {

      if (e < v && (adjMatrix[e, v] != 0 || adjMatrix[v, e] != 0)) {

        # Save the rows and columns of the nonzero entries of the upper
        # triangular adjacency matrix.
        Rows[[i]] <- e
        Columns[[i]] <- v

        i <- i + 1

      }

    }

  }

  # Remove the NAs, if there are any, at the end of the Rows and Columns
  # vectors.
  Rows <- Rows[Rows != 0]
  Columns <- Columns[Columns != 0]

  # Combine the Rows and Columns vectors into a matrix.
  return (rbind(Rows, Columns))

}
