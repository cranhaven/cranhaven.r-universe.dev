# cycleSaL
#
# The first step in identifying potential directed cycles in the graph is to
# find the nodes that could form a cycle. Since a node must be connected to at
# least two other nodes to be a part of a cycle we start by summing all of the
# rows of the undirected adjacency matrix and creating a new matrix that only
# has the rows of the undirected adjacency matrix with a sum of two or higher.
# The function cycleSaL creates the matrix of nodes that could form a directed
# cycle.
#
# Creates a stem and leaf matrix from the adjacency matrix with the nodes that
# could potentially form a cycle. The row names of the matrix created by the
# cycleSaL function are the stems and the 1s in the matrix are the leaves.
#
# @param adjMatrix The adjacency matrix of the graph.
#
# @param nCPh The number of clinical phenotypes in the graph.
#
# @param nGV The number of genetic variants in the graph.
#
# @param pmr Logical. If true the Metropolis-Hastings algorithm will use the
# Principle of Mendelian Randomization (PMR). This prevents the direction of an
# edge pointing from a gene expression node to a genetic variant node.
#
# @return  A binary matrix where the 1s represent an edge between the row node
# and the column node. If there are no cycles in the graph the function returns
# NULL.
#
cycleSaL <- function (adjMatrix,
                      nCPh,
                      nGV,
                      pmr) {

  # Add the adjacency matrix to its transpose to make all edges undirected.
  undirected <- adjMatrix + t(adjMatrix)

  # If a graph with undirected edges is passed to the removeCycles function then
  # the row sums could be 2 or greater even if that node is not in a potential
  # cycle because the cell in the undirected matrix for those nodes will be a 2.
  # To fix this problem we change any 2 in the undirected matrix to a 1.
  undirected[undirected == 2]  <- 1

  # Name the rows and columns of the undirected matrix from 1 to the number of
  # nodes in the adjacency matrix. These names will be used to remove the rows
  # of nodes that are not part of a cycle.
  rownames(undirected) <- c(1:dim(undirected)[2])
  colnames(undirected) <- c(1:dim(undirected)[2])

  # Calculate the number of nodes in the graph.
  nNodes <- dim(undirected)[2]

  # If pmr is false or if nGV or nCPh is equal to nNodes run the rmRows
  # function without making any changes to the undirected matrix.
  if (pmr == FALSE || nGV == nNodes || nCPh == nNodes) {

    # Remove rows with a sum of less than two.
    SaL <- rmRows(SaL = undirected)

    # Reduce the rows and columns of the adjacency matrix if pmr is true or if
    # there is at least one clinical phenotype present.
  } else if (pmr || nCPh >= 1) {

    # Determine the number of gv and ge nodes.
    nGVGE <- nNodes - nCPh

    # Set appropriate elements of the undirected matrix to 0 when using PMR and
    # there is at least 1 clinical phenotype.
    if (pmr && nCPh >= 1) {

      # Set elements in [1:nGV, (nGV + 1):nNodes] to 0.
      undirected[1:nGV, (nGV + 1):nNodes] <- 0
      # Set elements in [(nGV + 1):nNodes, 1:nGV] to 0.
      undirected[(nGV + 1):nNodes, 1:nGV] <- 0
      # Set elements in [(nGV + 1):nGVGE, (nGVGE + 1):nNodes] to 0.
      undirected[(nGV + 1):nGVGE, (nGVGE + 1):nNodes] <- 0
      # Set elements in [(nGVGE + 1):nNodes, (nGV + 1):nGVGE] to 0.
      undirected[(nGVGE + 1):nNodes, (nGV + 1):nGVGE] <- 0

      # Set appropriate elements of the undirected matrix to 0 when using PMR
      # and no clinical phenotypes are present.
    } else if (pmr && nCPh == 0) {

      # Set elements in [1:nGV, (nGV + 1):nNodes] to 0.
      undirected[1:nGV, (nGV + 1):nNodes] <- 0
      # Set elements in [(nGV + 1):nNodes, 1:nGV] to 0.
      undirected[(nGV + 1):nNodes, 1:nGV] <- 0

      # Set appropriate elements of the undirected matrix to 0 when clinical
      # phenotypes are present and PMR is FALSE.
    } else {

      # Set elements in [1:nGVGE, (nGVGE + 1):nNodes] to 0.
      undirected[1:nGVGE, (nGVGE + 1):nNodes] <- 0
      # Set elements in [(nGVGE + 1):nNodes, 1:nGVGE] to 0.
      undirected[(nGVGE + 1):nNodes, 1:nGVGE] <- 0

    }

    # Remove rows with a sum of less than two.
    SaL <- rmRows(SaL = undirected)

  }

  # If there are two or fewer rows in the SaL matrix there cannot be a cycle in
  # the graph.
  if (dim(SaL)[1] <= 2) {

    return (NULL)

  }

  # Remove the 1s in the SaL matrix that are not part of a cycle
  SaL <- rmOnes(SaL = SaL)

  # Remove any tails in the graph.
  SaL <- rmTail(SaL = SaL)

  # Check for directed cycles (i.e., there are three or more rows in SaL).
  if (is.null(SaL)) {

    return (NULL)

  }

  # Remove any columns that do not contain any 1s
  SaL <- SaL[, colSums(SaL) != 0]

  return (SaL)

}

# rmOnes
#
# Removes the elements that are a one in the SaL matrix if the column index the
# one occurs in is not in the vector of row names.
#
# @param SaL A binary matrix containing the nodes that could form a directed
# cycle.
#
# @return An updated SaL matrix.
#
rmOnes <- function (SaL) {

  # Turn the row and column names of the SaL matrix into a numeric vector. These
  # will be used later to determine which entries in the cycles matrix need to
  # remain a one and which need to be changed to zero. The cells that will be
  # changed to a zero are the column numbers that do not appear in the rows.
  rNames <- as.numeric(rownames(SaL))
  cNames <- as.numeric(colnames(SaL))

  # Go through each element in the SaL matrix and turn the ones to zeros if
  # the column number the one occurs in does not appear in the rownames vector.


  # Loop through each element in the SaL matrix.
  for (e in seq_along(rNames)) {

    for (v in seq_along(cNames)) {

      # Check if the column number where an element is a 1 appears in the vector
      # of row names.
      if (SaL[e, v] == 1) {

        # If the column number is not in the vector of row names change the one
        # to a zero in the SaL matrix.
        if (!any(rNames == cNames[[v]])) {

          SaL[e, v] <- 0

        }

      }

    }

  }

  return (SaL)

}

# rmRows
#
# Removes the rows in the SaL matrix that have a sum of less than two because
# the nodes that correspond to these rows cannot form a directed cycle.
#
# @param SaL A binary matrix containing the nodes that could form a directed
# cycle.
#
# @return An updated SaL matrix with rows summing to less than two removed.
#
rmRows <- function (SaL) {

  # Create a matrix whose rows are the nodes that could potentially form a
  # cycle. Nodes that could form a cycle have rows with a sum of 2 or greater.
  SaL <- SaL[which(rowSums(SaL) >= 2), , drop = FALSE]

  return (SaL)

}

# rmTail
#
# Removes nodes that are connected to at least two other nodes but are not part
# of a directed cycle. These are referred to as tails.
#
# @param SaL A binary matrix containing the nodes that could form a directed
# cycle.
#
# @return An updated SaL matrix with all tails removed.
#
rmTail <- function (SaL) {

  # Calculate the row sums after the nodes that cannot create a directed cycle
  # are removed from the SaL matrix.
  newSums <- rowSums(SaL)

  # The following while loop will continue to trim down the SaL matrix when
  # there is a string of nodes (a tail) that are connected to at least two other
  # nodes but they do not form a directed cycle.
  while (any(newSums <= 1)) {

    # Only keep the rows with a sum >= 2.
    SaL <- SaL[which(newSums >= 2), , drop = FALSE]

    # Get the new row names.
    rNames <- as.numeric(rownames(SaL))

    # If there are two or fewer rows with a row sum of at least two there cannot
    # be any directed cycles in the graph and cycleSaL returns NULL.
    if (length(rNames) <= 2) {

      return (NULL)

    } else {

      # Remove the 1s in the SaL matrix that are not part of a cycle
      SaL <- rmOnes(SaL)

      # Update the newSums vector after rows with a sum of two or less have been
      # removed from the SaL matrix.
      newSums <- rowSums(SaL)

    }

  }

  return (SaL)

}

# cycleB
#
# Turns the stem and leaf matrix returned by the cycleSaL function into a list
# of matrices. Each matrix contains all the cycles for each child of the root
# node. This function subsets the SaL matrix using row and column names - not by
# row and column indices.
#
# @param SaL A binary matrix containing the nodes that could potentially form a
# directed cycle.
#
# @return A matrix that contains all the cycles for the current graph. The
# cycles are displayed down the columns of the matrix.
#
#' @importFrom stats na.omit
#'
cycleB <- function (SaL) {

  # Start by getting the name of the root node.
  root <- rownames(SaL)[1]

  # Create a vector of the children of the root node.
  rChildren <- names(SaL[1, SaL[1, ] == 1])

  # Get the number of children of the root node.
  nChildren <- length(rChildren)

  # Create a list to hold the matrices of cycles. There will be one matrix for
  # each child of the root node.
  tree <- vector(mode = 'list',
                 length = nChildren)

  # This for loop will go through each child of the root node and build a matrix
  # that contains all of the nodes that could form a directed cycle down the
  # columns of the matrix.
  for (e in 1:nChildren) {

    # A matrix that holds the cycles of the eth child of the root node.
    tree[[e]] <- matrix(nrow = nrow(SaL) + 1,
                        ncol = 1)

    # The name in the first row/column will always be the name of the root node.
    tree[[e]][1, 1] <- root

    # The name in the second row first column will always be the name of the
    # eth child of the root node.
    tree[[e]][2, 1] <- rChildren[[e]]

    # Create a variable for the number of columns of the matrices in tree. This
    # variable will be updated at the end of the following repeat loop to
    # account for branches further down the tree.
    nCol <- ncol(tree[[e]])

    # An index to move across the columns of the tree[[e]] matrix
    v <- 1

    # The following loop creates branches on the tree as deep as possible for
    # all possible branches for each child of the root node. The loop breaks
    # when no new branches need to be added to the tree[[e]] matrix and the
    # last branch is completed.
    repeat {

      if (v == 1) {

        # Get the potential children of the eth child of the root node. They are
        # potential children because we will not keep all of the children of the
        # current node. One of the child nodes will be the root node.
        pChildren <- names(SaL[tree[[e]][2, 1],
                               SaL[tree[[e]][2, 1], ] == 1])

        # Eliminate the root node from the children of the current node. This is
        # done becuase it would lead to creating branches that would bounce back
        # and forth between two nodes.
        children <- pChildren[pChildren != tree[[e]][1, 1]]

        # If there is more than one child create additional columns to hold each
        # child and their descendants.
        if (length(children) > 1) {

          # This for loop creates new rows in the tree matrix for each child.
          for (i in 1:length(children)) {

            # Copy the current column and add it to the end of the tree[[e]]
            # matrix length(children) - 1 times.
            if (i != 1) {

              # Create a new column in the tree[[e]] matrix for an additional
              # branch in the tree.
              tree[[e]] <- cbind(tree[[e]], tree[[e]][, 1])

              # Add the children to their respective columns
              tree[[e]][3, i] <- children[[i]]

              # This is run the first time through the for loop, i = 1, because
              # the child node needs to be added to the first column of the
              # matrix and not one of the columns added for the additional
              # children of the current node.
            } else {

              # Add the children to their respective columns.
              tree[[e]][3, i] <- children[[i]]

            }

          }

          # This is run if the current node only has one child.
        } else {

          # Add the child node to the row beneath the parent node.
          tree[[e]][3, 1] <- children

        }

        # nRow is a counter for moving down the rows of tree[[e]]. It starts at
        # 3 because the first three rows have already been added to the matrix.
        # It is also used in the while loop to check if the current node has
        # appeared previously.
        nRow <- 3

        # This runs when v is not equal to 1, when it is not the first time
        # through the repeat loop.
      } else {

        # Get the number of elements in the current column to start filling in
        # the descendants of the current node.
        nRow <- length(na.omit(tree[[e]][, v]))

      }

      # Loops through the current column, v, of tree[[e]] and adds nodes until
      # the loop comes to a node that has already been added.
      while (! tree[[e]][nRow, v] %in% tree[[e]][1:(nRow - 1), v]) {

        # Increase the row index by one.
        nRow <- nRow + 1

        # Potential children of the current parent.
        pChildren <- names(SaL[tree[[e]][nRow - 1, v],
                               SaL[tree[[e]][nRow - 1, v], ] == 1])

        # Eliminate the child that is the same as the grandparent.
        children <- pChildren[pChildren != tree[[e]][nRow - 2, v]]

        # If there is more than one child create additional columns to hold each
        # child and their descendants.
        if (length(children) > 1) {

          # This for loop adds one column for each of the children after the
          # first child.
          for (i in 1:length(children)) {

            # Copy the current column and add it to the end of the tree[[e]]
            # matrix length(children) - 1 times.
            if (i != 1) {

              # Create a new column in the tree[[e]] matrix for the additional
              # branch in the tree.
              tree[[e]] <- cbind(tree[[e]], tree[[e]][, v])

              # Add the children to their respective columns
              tree[[e]][nRow, ncol(tree[[e]])] <- children[[i]]

              # This is run when i in the for loop that loops through the
              # children of the current parent is not equal to one.
            } else {

              # Add the children to their respective columns
              tree[[e]][nRow, v] <- children[[i]]

            }

          }

          # This runs when the current parent only has one child.
        } else {

          # Add the child node to the row beneath the parent node.
          tree[[e]][nRow, v] <- children

        }

      }

      # Get the number of columns currently in the matrix tree[[e]]
      nCol <- ncol(tree[[e]])

      # Exit the loop when the loop has filled in the last column of the
      # tree[[e]] matrix and no more columns need to be added.
      if (nCol == v) break

      # Increase the counter used to loop through the columns of the tree[[e]]
      # matrix.
      v <- v + 1

    }

  }

  # Combine all of the matrices into one matrix that contains all the branches.
  branches <- tree[[1]]

  for (b in 2:length(rChildren)) {

    branches <- cbind(branches, tree[[b]])

  }

  # Change the branches matrix from a character matrix to a numeric matrix.
  return (apply(branches, 2, as.numeric))

}

# cylceTB
#
# Takes the branches created by the cycleB function and trims them. The
# cycleB function creates a tree as deep as possible. Therefore, some of
# the branches may have nodes that are not part of a cycle.
#
# @param branches A matrix that holds all the branches of a tree.
#
# @return A list. The first element, nBranches, is an integer of the total
# number of branches in the tree. The second element, tBranches, is a list of
# matrices. Each matrix holds the trimmed branches for each cycle size.
#
#' @importFrom stats na.omit
#'
cycleTB <- function (branches) {

  # Get the number of branches in the tree.
  nBranches <- dim(branches)[2]

  # A list to hold each trimmed branch according to the number of nodes in the
  # cycle. A trimmed branch contains only the nodes that appear in a cycle. The
  # branches passed to this function may contain nodes that are connected to the
  # cycle but are not part of the cycle. The tBranches list can have a length of
  # up to 3:nNodes.
  tBranches <- vector(mode = 'list',
                      length = nBranches)

  # The following loop will take each branch, starting at the leaf, and move
  # toward the root along the nodes until it sees a node that matches the leaf.
  # These nodes will form a cycle and they will be stored in tBranches.
  for (e in 1:nBranches) {

    # Get the number of nodes in the eth column of the branches matrix excluding
    # any NAs.
    nNodes <- length(na.omit(branches[, e]))

    # A vector of the nodes that form a cycle. They will be in reverse order
    # from how they were listed in the branches matrix.
    branch <- numeric(length = nNodes)

    # The leaf or last node in the eth column of the branches matrix will be
    # used to stop the while loop.
    sNode <- branches[nNodes, e]

    # Add the leaf to the branch vector.
    branch[[1]] <- branches[nNodes, e]

    # Add the parent of the leaf to the branch vector.
    branch[[2]] <- branches[nNodes - 1, e]

    # The previous two lines are run in order to get into the while loop.

    # create a counter to fill in the branch vector.
    v <- 2

    # Continue adding nodes the the branch vector until we come to back to the
    # node we started with.
    while (branch[[v]] != branch[[1]]) {

      v <- v + 1

      # Continue adding nodes to the vector
      branch[[v]] <- branches[nNodes - (v - 1), e]

    }

    # Add the trimmed branch to the list
    tBranches[[e]] <- branch[branch != 0]

  }

  # Extract the unique cycle sizes.
  nCS <- unique(sapply(tBranches, length))

  # Create list for the trimmed branches sorted by cycle size.
  sTB <- vector(mode = 'list',
                length = length(nCS))

  # Check for multiple cycle sizes.
  if (length(nCS) == 1) {

    sTB[[1]] <- do.call(rbind, tBranches)

  } else {

    # Order the cycle sizes from smallest to largest.
    nCS <- nCS[order(nCS)]

    # Loop through each cycle size.
    for (a in seq_along(nCS)) {

      # Find all branches with a length matching the current cycle size.
      sTB[[a]] <- tBranches[which(sapply(tBranches,
                                         function(x) length(x) == nCS[[a]]))]

      # Collapse the lists of cycles to a matrix for each cycle size.
      sTB[[a]] <- do.call(rbind, sTB[[a]])

    }

  }

  return (list(nBranches = nBranches,
               tBranches = sTB))

}

# cycleCED
#
# Extracts the coordinates of the adjacency matrix for each edge that is part
# of a directed cycle and takes the names of the nodes in each cycle and
# creates a vector of edge directions that form a directed cycle.
#
# @param nBranches An integer. The number of branches in the tree.
#
# @param tBranches A list of cycles. Each element in the list is a vector of
# nodes that could form a directed cycle.
#
# @return A list with the coordinates of the adjacency matrix for each edge in
# each cycle, a vector of columns visited by the current cycles, and the edge
# directions for each cycle.
#
cycleCED <- function (nBranches,
                      tBranches) {

  # Extract the number of cycle sizes.
  ncs <- length(tBranches)

  # Create a list that will have a matrix for each cycle size. The rows of the
  # matrix will contain the adjacency matrix coordinates for each edge.
  cCoord <- vector(mode = 'list',
                   length = ncs)

  # A list that will hold the column indices of each column visited in the SaL
  # matrix by the current cycle.
  Columns <- vector(mode = 'list',
                    length = ncs)

  # The edgeDir list will hold the vectors of the edge directions that will form
  # a cycle in the graph.
  edgeDir <- vector(mode = 'list',
                    length = ncs)

  # Loop through each trimmed branch or cycle present in the
  # graph.
  for (e in 1:ncs) {

    # Extract the number of cycles for the current cycle size.
    nCycles <- dim(tBranches[[e]])[1]

    # Get the nubmer of edges for the current cycle size.
    nEdges <- dim(tBranches[[e]])[2] - 1

    # Create a list for each cycle of size n for the adjacency matrix
    # coordinates.
    cCoord[[e]] <- vector(mode = 'list',
                          length = nCycles)

    # Create a list for each cycle of size n for the columns visited.
    Columns[[e]] <- vector(mode = 'list',
                           length = nCycles)

    # Create a list for each cycle of size n for the edge directions.
    edgeDir[[e]] <- matrix(1,
                           nrow = nCycles,
                           ncol = nEdges)

    # Loop through each cycle for the current cycle size (number of nodes in the
    # cycle).
    for (v in 1:nCycles) {

      # A sublist that holds the column indices for each column visited in the
      # current cycle.
      Columns[[e]][[v]] <- vector(mode = 'integer',
                                  length = nEdges)

      # Create a matrix to hold the adjacency matrix coordinates in the rows.
      cCoord[[e]][[v]] <- matrix(nrow = nEdges,
                                 ncol = 2)

      # This for loop gets the row and column indices for each edge in the
      # cycle.
      for (a in 1:nEdges) {

        # The row index is for the parent node and the column index is for the
        # child node. These indices will be used to determine if a 0 or 1 should
        # be present in the adjacency matrix in order for the nodes to form a
        # directed cycle.
        cCoord[[e]][[v]][a, ] <- c(tBranches[[e]][v, a],     # Row index
                                   tBranches[[e]][v, a + 1]) # Column Index

        # Keep the columns visited by each edge in the current cycle. This will
        # be used to determine if there are disjoint cycles in the graph.
        Columns[[e]][[v]][[a]] <- tBranches[[e]][v, a + 1]

        # If the value of the first node is less than the value of the second
        # node the direction of the edge points from the node with a smaller
        # index to the node with a larger index.
        if (tBranches[[e]][v, a] <
            tBranches[[e]][v, a + 1]) {

          edgeDir[[e]][v, a] <- 0

          # If the value of the first node is greater than the value of the
          # second node the direction of the edge points from the node with a
          # larger index to the node with a smaller index.
        } else {

          edgeDir[[e]][v, a] <- 1

        }

      }

    }

  }

  # Get the unique column numbers for each column visited
  uColumns <- unique(unlist(Columns))

  return (list(cCoord = cCoord,
               columns = uColumns,
               edgeDir = edgeDir))

}

# cycleDJ
#
# Finds all disjoint cycles in the network.
#
# @param SaL A binary matrix containing the nodes that could potentially form a
# directed cycle.
#
# @param ced A list containing the coordinates in the adjacency matrix of the
# nodes that could form a cycle, the columns of the SaL matrix that have been
# visited by the current cycles, and the edge directions for each cycle.
#
# @return A list with the coordinates of the adjacency matrix for each edge in
# each cycle, a vector of columns visited by the current cycles, and the edge
# directions for each cycle.
#
cycleDJ <- function (SaL,
                     ced) {

  # Keep the row names of the original SaL matrix. This will be used to check if
  # every row in the matrix has been used.
  rowNamesSaL <- row.names(SaL)

  # Column names visited so far
  columnNamesSaL <- ced$columns

  # Check if all the cycles have been found. If TRUE all cycles have been found.
  # If FALSE there are disjoint cycles and the cycleSaL function needs to be run
  # again.
  disjoint <- setequal(rowNamesSaL, columnNamesSaL)

  # If there are not disjoint cycles return NULL.
  if (disjoint == TRUE) {

    return (NULL)

  }

  # Create a counter for the while loop.
  counter <- 1

  # Loop through the previous funcions, eliminating rows from the SaL matrix at
  # each iteration of the while loop, until all cycles are found.
  while (!disjoint) {

    # Remove the column names that have been visited previously from the SaL
    # matrix.
    SaL2 <- SaL[!rowNamesSaL %in% columnNamesSaL, , drop = FALSE]

    # Remove the 1s in the SaL matrix that are not part of a cycle.
    SaL2 <- rmOnes(SaL = SaL2)

    # Remove any tails in the graph.
    SaL2 <- rmTail(SaL = SaL2)

    # If there are less than three rows in SaL2 and it is the first time through
    # the while loop return NULL.
    if (is.null(SaL2) || (dim(SaL2)[1] < 3 && counter == 1)) {

      return (NULL)

      # If there are less than three rows in SaL2 and the counter is greater
      # than one then there was at least one disjoint cycle and the updated ced
      # object needs to be returned.
    } else if (dim(SaL2)[1] < 3 && counter > 1) {

      # Exit out of the while loop.
      break

    }

    # Create a tree as deep as possible with the remaining columns of the SaL
    # matrix.
    branches <- cycleB(SaL2)

    # Trim the branches starting at the leaf node to only include the nodes that
    # belong to the current cycle.
    trimmedB <- cycleTB(branches)

    # Extract the coordinates in the adjacency matrix for each edge in each
    # directed cycle and the edge states that create each directed cycle.
    ced2 <- cycleCED(nBranches = trimmedB$nBranches,
                     tBranches = trimmedB$tBranches)

    # Update the ced list.
    ced$cCoord <- append(ced$cCoord, ced2$cCoord)
    ced$columns <- append(ced$columns, ced2$columns)
    ced$edgeDir <- append(ced$edgeDir, ced2$edgeDir)

    # Update the columns that have been visited.
    columnNamesSaL <- append(columnNamesSaL, ced$columns)

    # Update the disjoint test.
    disjoint <- setequal(rowNamesSaL, columnNamesSaL)

    # Update the counter.
    counter <- counter + 1

  }

  # Return the ced list for the disjoint cycles.
  return (ced)

}

# combineCED
#
# Combines lists of the same cycle size for the cCoord and edgeDir lists.
#
# @param ced A list containing the coordinates in the adjacency matrix of the
# nodes that could form a cycle, the columns of the SaL matrix that have been
# visited by the current cycles, and the edge directions for each cycle.
#
# @return A list (the updated ced list) with the coordinates of the adjacency
# matrix for each edge in each cycle, a vector of columns visited by the current
# cycles, and the edge directions for each cycle.
#
combineCED <- function (ced) {

  # Create a counter to keep track of the number of lists in ced
  counter <- 1

  # Loop through each list in cCoord and edgeDir and combine the output of the
  # subsequent lists to the output of the previous lists if they have the same
  # cycle size.  For example, if the first list has output for cycles with three
  # nodes and the second list also hase output for cycles with three nodes
  # combine the first and second lists.
  repeat {

    # Compare the lists after counter to the list whose index match the counter.
    for (e in (counter + 1):length(ced$edgeDir)) {

      # If the cycle size is the same between the two lists combine them.
      if (dim(ced$edgeDir[[counter]])[2] == dim(ced$edgeDir[[e]])[2]) {

        # Append the eth cCoord list to the counter cCoord list.
        ced$cCoord[[counter]] <- append(ced$cCoord[[counter]],
                                        ced$cCoord[[e]])

        # Append the eth edgeDir list to the counter edgeDir list.
        ced$edgeDir[[counter]] <- rbind(ced$edgeDir[[counter]],
                                        ced$edgeDir[[e]])

        # Add e to a vector that will be used to remove lists from the cCoord
        # and edgeDir list.
        if (counter == 1) {

          rmList <- e

        } else {

          rmList <- append(rmList, e)

        }

      }

    }

    # Remove the eth list from cCoord becuase this was added to the list
    # with the counter index.
    ced$cCoord <- ced$cCoord[-rmList]

    # Remove the eth list from edgeDir becuase it was added to the list with
    # the counter index.
    ced$edgeDir <- ced$edgeDir[-rmList]

    # Update the counter
    counter <- counter + 1

    # Exit the loop if the remaining number of lists is the same as the counter.
    if (counter >= length(ced$edgeDir)) {

      break

    }

  }

  return (ced)

}

# cycleEID
#
# Extracts the edge index of each edge in the graph.
#
# @param cCoord A list containing the coordinates in the adjacency matrix of
# the nodes that could form a cycle.
#
# @param position A matrix of row and column indices for each edge in a cycle.
# This is the output from the coordiantes function.
#
# @return A list containing the edge indices for the edges that could form a
# directed cycle for each cycle in the graph.
#
cycleEID <- function (cCoord,
                      position) {

  # Store the number of edges in the graph.
  m <- dim(position)[2]

  # Number of cycle sizes.
  ncs <- length(cCoord)

  # Stores the index of each edge for each cycle in the tBranches list
  edgeID <- vector(mode = 'list',
                   length = ncs)

  # Loop through each of the cycle sizes.
  for (e in 1:ncs) {

    # Number of directed cycles in the graph.
    nCycles <- length(cCoord[[e]])

    # Get the number of edges for the current cycle size.
    nEdges <- dim(cCoord[[e]][[1]])[1]

    # Create a sublist of edgeID for each cycle in the current cycle size.
    edgeID[[e]] <- matrix(1,
                          nrow = nCycles,
                          ncol = nEdges)

    # Loop through the cycles in the current cycle size.
    for (v in 1:nCycles) {

      # Loop through each edge in the current cycle.
      for (a in 1:nEdges) {

        # This loops through all of the coordinates for each edge in the graph
        # and compares the coordinates of the current edge from the cycle to the
        # coordinates of each edge in the graph until it finds a match. When it
        # finds a match it stores the column number in the coordinates matrix
        # which is the edge number of the current edge in the graph.
        for (n in 1:m) {

          if (setequal(position[, n], cCoord[[e]][[v]][a, ])) {

            edgeID[[e]][v, a] <- n

            # Stop checking for the edge number once it is found.
            break

          }

        }

      }

    }

  }

  return (edgeID)

}

# cycleUES
#
# Extracts the unique edge states for each cycle.
#
# @param edgeDir A list, divided by the number of edges in the cycle, containing
# the edge directions of each edge in each cycle of the graph.
#
# @param edgeID A list, divided by the number of edges in the cycle, containing
# the edge indices of each edge in each cycle of the graph.
#
# @param nEdges An integer for the number of edges in the graph.
#
# @return A matrix of uinque cycles with the cycles down the rows and edge
# indices across the columns.
#
cycleUES <- function (edgeDir,
                      edgeID,
                      nEdges) {

  # Get the number of cycles (including repeated cycles).
  nCycles <- cycleLen(edgeID)

  # Start a counter that will be used to fill the rows of the cMatrix.
  counter <- 0

  # Create a matrix for all possible cycles. The cycles are listed down the rows
  # and the edges in the network are across the columns.
  cycles <- matrix(9,
                   nrow = nCycles,
                   ncol = nEdges)

  # Loop through each cycle size.
  for (e in 1:length(edgeDir)) {

    # Loop through each cycle in the current cycle size.
    for (v in 1:dim(edgeDir[[e]])[1]) {

      # Update the counter
      counter <- counter + 1

      # Fill in the edge directions for the edges in the current cycle.
      cycles[counter, edgeID[[e]][v, ]] <- edgeDir[[e]][v, ]

    }

  }

  # Return the unique rows.
  return (unique(cycles))

}

# cycleUID
#
# Extracts the edge indices for the unique cycles.
#
# @param unqCycle A matrix containing the edge states of each unique cycle.
#
# @return A list containing the edge indices for the unique cycles.
#
cycleUID <- function (uCycles) {

  # Create a list of unique decimal numbers.
  edgeID <- vector(mode = 'list',
                   length = dim(uCycles)[1])

  # Loop through each cycle.
  for (e in 1:dim(uCycles)[1]) {

    # Extract the edge indices for the edges that make up the cycle.
    edgeID[[e]] <- which(uCycles[e, ] != 9)

  }

  # Return the unique decimals and IDs.
  return (edgeID)

}

# cycleCG
#
# Takes the edge states from the current graph and fills a matrix for each
# potential directed cycle with them.
#
# @param currentES A vector of edge states for the current graph
#
# @param edgeID A list of edge indices for all possible directed cycles.
#
# @param nCycles An integer for the number of cycles in the graph.
#
# @param nEdges An integer for the number of edges in the graph.
#
# @return A matrix of current edge states across the rows and potential directed
# cycles down the columns.
#
cycleCG <- function (currentES,
                     edgeID,
                     nCycles,
                     nEdges) {

  # Create a matrix that will be filled with the current edge states for all
  # possible directed cycles.
  blight <- matrix(9,
                   nrow = nCycles,
                   ncol = nEdges)

  # Loop through each potential directed cycle.
  for (e in 1:nCycles){

    # Fill in the edges that could form a cycle with the current edge states.
    blight[e, edgeID[[e]]] <- currentES[edgeID[[e]]]

  }

  return (blight)

}

# cycleLen
#
# Calculates the number of cycles in the edgeID list.
#
# @param edgeID A list of all possible cycles sorted by the cycle size.
#
cycleLen <- function (edgeID) {

  # Get the number of cycle sizes.
  nSizes <- length(edgeID)

  # Create a vector to hold the length of each sub list.
  nSublists <- numeric(nSizes)

  # Loop through the cycle sizes.
  for (e in 1:nSizes) {

    # Extract the number of cycles in the eth cycle size.
    nSublists[[e]] <- dim(edgeID[[e]])[1]

  }

  return (sum(nSublists))

}

# cycleFC
#
# Takes a fully connected SaL matrix and finds all possible cycles with
# 3, 4, 5, ... nodes .
#
# @param SaL A binary matrix containing the nodes that could potentially form a
# directed cycle.
#
# @return A list of all possible cycles with 3, 4, 5, ... nodes
#
#' @importFrom utils combn
#'
cycleFC <- function (SaL) {

  # Calculate the number of cycle sizes.
  nCS <- length(3:dim(SaL)[1])

  # Create a vector to hold the number of cycles in each cycle size.
  nCycles <- vector(mode = 'numeric',
                    length = nCS)

  # Create a list to hold the matrix of node orders that form a directed cycle
  # for each cycles size.
  cycles <- vector(mode = 'list',
                   length = nCS)

  # Loop through the cycle sizes of the graph (starting with three nodes).
  for (e in 1:nCS) {

    # The first time through the for loop we will calculate the combinations for
    # all three node cycles. This can be done directly with the combn function.
    if(e == 1) {

      # Find all of the combinations for a cycle size of 3.
      cmbntns <- t(combn(as.numeric(rownames(SaL)), 3))

      # Permute the nodes in each row of the cmbntns matrix
      cycles[[e]] <- permutate(combs = cmbntns)

      # Extract the number of cycles for cycles with three nodes.
      nCycles[[e]] <- dim(cycles[[e]])[1]

    } else {

      # Find all of the combinations for the current cycle size.
      cmbntns <- t(combn(as.numeric(rownames(SaL)), (e + 2)))

      # Permute the nodes in each row of the cmbntns matrix
      cycles[[e]] <- permutate(combs = cmbntns)

      # Extract the number of cycles for the current cycle size.
      nCycles[[e]] <- dim(cycles[[e]])[1]

    }

  }

  return (list(nBranches = sum(unlist(nCycles)),
               tBranches = cycles))

}

# permutate
#
# permutes the node indices of the nodes that can form a cycle - after removing
# the first node in the vector.
#
# @param combs A matrix of combinations of nodes that can form a cycle.
#
# @return A matrix of permutations of nodes that can form a cycle.
#
#' @importFrom gtools permutations
#'
permutate <- function (combs) {

  # Store the number of rows and columns for the cmbntns matrix.
  nRows <- dim(combs)[1]
  nColumns <- dim(combs)[2]

  # Create a list for the permutations of each combination in the cmbtns
  # matrix.
  prmttns <- vector(mode = 'list',
                    length = nRows)

  # Go through each combination of nodes and choose the first node in the
  # row and permute the remaining nodes.
  for (v in 1:nRows) {

    # Fill in the prmttns matrix with the index of the node that appears
    # in the first column.
    prmttns[[v]] <- matrix(combs[v, 1],
                           nrow = factorial(nColumns - 1),
                           ncol = nColumns + 1)

    # Permute the remaining nodes in the cycle.
    prmttns[[v]][, 2:nColumns] <- permutations(n = nColumns - 1,
                                               r = nColumns - 1,
                                               v = combs[v, 2:nColumns])

  }

  # Combine all of the matrices into one matrix for the current cycle size.
  return (do.call(rbind, prmttns))

}

# cycleFndr
#
# Carries out all the steps to find directed cycles
#
# @param adjMatrix The adjacency matrix of the graph.
#
# @param nCPh The number of clinical phenotypes in the graph.
#
# @param nGV An integer. The number of genetic variants in the graph.
#
# @param nEdges An integer. The number of edges in the graph.
#
# @param pmr Logical. If TRUE the principle of Mendelian randomization will be
# used. This restrics any genetic variant node from being the child of a gene
# expression node (when pmr = TRUE).
#
# @param position A matrix of row and column indices for each edge in a cycle.
# This is the output from the coordiantes function.
#
# @return A list containing the unique decimal numbers for the cycles in the
# graph and the indices of each edge in every cycle. If there are no cycles
# in the graph the function returns NULL.
#
cycleFndr <- function (adjMatrix,
                       nCPh,
                       nGV,
                       nEdges,
                       pmr,
                       position) {

  # Check for potential cycles in the graph.
  SaL <- cycleSaL(adjMatrix = adjMatrix,
                  nCPh = nCPh,
                  nGV = nGV,
                  pmr = pmr)

  # If there aren't any cycles return NULL.
  if (is.null(SaL)) {

    return (list(cycles = NULL,
                 edgeID = NULL,
                 nCycles = NULL))

  }

  # Check if the SaL matrix is fully connected.
  if (sum(SaL) == dim(SaL)[1]^2 - dim(SaL)[1]) {

    # Find all possible cycles with three or more nodes.
    trimmedB <- cycleFC(SaL = SaL)

    # If the SaL matrix is not fully connected build a tree as deep as possible
    # from the SaL matrix.
  } else {

    # Creates a branch as deep as possible for each child of the parent node.
    branches <- cycleB(SaL)

    # Trim the branches starting at the leaf node to only include the nodes that
    # belong to the current cycle.
    trimmedB <- cycleTB(branches)

  }

  # Extract the coordinates in the adjacency matrix for each edge in each
  # directed cycle and the edge states that create each directed cycle.
  ced <- cycleCED(nBranches = trimmedB$nBranches,
                  tBranches = trimmedB$tBranches)

  # If there are disjoint cycles find them.
  ced_temp <- cycleDJ(SaL = SaL,
                      ced = ced)

  # check if ced_temp has any output. If it does then there were directed cycles
  # and ced will be replaced by ced_tmp.
  if (!is.null(ced_temp)) {

    # Update the ced list with ced_temp which contains the output for disjoint
    # cycles.
    ced <- combineCED(ced = ced_temp)

  }

  # Extract the indicies of the edges that form each cycle.
  edgeID <- cycleEID(cCoord = ced$cCoord,
                     position = position)

  # Convert the list of cycles to a matrix and remove repeated cycles.
  uCycles <- cycleUES(edgeDir = ced$edgeDir,
                      edgeID = edgeID,
                      nEdges = nEdges)

  # Match the edge directions to the unique cycles
  uEID <- cycleUID(uCycles = uCycles)

  # Return the unique edge state and edge index vectors.
  return (list(cycles = uCycles,
               edgeID = uEID,
               nCycles = dim(uCycles)[1]))

}

# cycleRmvr
#
# Changes the edge direction of the current graph until there are no directed
# cylces.
#
# @param cycles A matrix of edge states for each potential directed cycle.
#
# @param edgeID A list of edge indices for each edge in each potential directed
# cycle.
#
# @param edgeType A 0 or 1 indicating whether the edge is a gv-ge edge (1) or
# a gv-gv or ge-ge edge (0).
#
# @param currentES A vector of edge states for the current graph.
#
# @param nCPh The number of clinical phenotypes in the graph.
#
# @param nEdges An integer for the number of edges in the graph.
#
# @param pmr Logical. If true the Metropolis-Hastings algorithm will use the
# Principle of Mendelian Randomization, PMR. This prevents the direction of an
# edge pointing from a gene expression node to a genetic variant node.
#
# @param prior A vector containing the prior probability of seeing each edge
# direction.
#
# @return A vector of the DNA of the individual with the cycles removed.
#
cycleRmvr <- function (cycles,
                       edgeID,
                       edgeType,
                       currentES,
                       nCPh,
                       nCycles,
                       nEdges,
                       pmr,
                       prior) {

  # Get the edge states from the current graph for each potential cycle.
  currentC <- cycleCG(currentES = currentES,
                      edgeID = edgeID,
                      nCycles = nCycles,
                      nEdges = nEdges)

  # Create a vector that holds the row indices where the sum of the row is equal
  # to the number of edges in the graph. A sum of nEdges indicates a row in the
  # currentC matrix matches the corresponding row in the cycle matrix meaning
  # there is a directed cycle. The row number indicates which directed cycle is
  # present in the current graph.
  whichCycle <- which(rowSums(cycles == currentC) == nEdges)

  # If there is a cycle change the direction of an edge invovled in the cycle.
  while (length(whichCycle) != 0) {

    # Loop through each cycle and remove it.
    for (e in whichCycle) {

      # Get the edges involved in the eth cycle
      curEdges <- edgeID[[e]]

      # Choose one of these edges and change it.
      whichEdge <- sample(x = curEdges,
                          size = 1)

      # If there is a directed cycle the edge state is either a 0 or 1.
      if(currentES[[whichEdge]] == 0) {

        # The position in the prior vector of the prior probability for the two
        # edge states that the current edge can move to.
        prPos <- c(2, 3)

        # The states the current edge can move to.
        states <- c(1, 2)

      } else {

        # The position in the prior vector of the prior probability for the two
        # edge states that the current edge can move to.
        prPos <- c(1, 3)

        # The states the current edge can move to.
        states <- c(0, 2)

      }

      # Calculate the probability of moving to the two possible edge states.
      probability <- cPrior(prPos = prPos,
                            edgeType = edgeType[[whichEdge]],
                            nCPh = nCPh,
                            pmr = pmr,
                            prior = prior)

      # Select one of the two possible edge states according to the prior.
      currentES[[whichEdge]] <- sample(x = states,
                                       size = 1,
                                       prob = probability)

    }

    # Get the edge states from the current graph for each potential cycle.
    currentC <- cycleCG(currentES = currentES,
                        edgeID = edgeID,
                        nCycles = nCycles,
                        nEdges = nEdges)

    # Recalculate the row sums and determine if any are zero.
    whichCycle <- which(rowSums(cycles == currentC) == nEdges)

  }

  return (currentES)

}
