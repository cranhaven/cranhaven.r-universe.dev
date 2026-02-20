# Start supporting functions for operating characteristics functions
########################################################################
# This function converts a numeric vector to a character string,
#   where values are separated by commas with no spaces
# This function is to be used when converting a vector of individual
#   indices or group sizes in a group testing algorithm to a character
#   string to be displayed in a single column in a matrix/data frame

vec2string <- function(vec) {
  res.string <- vec[1]
  if (length(vec) > 1) {
    for (i in 2:length(vec)) {
      res.string <- paste(res.string, vec[i], sep = ",")
    }
  }
  res.string
}




# This function takes a matrix of individual accuracy measures,
#   and finds the indices for each unique row of results
# The output is a matrix of the unique individual accuracy measures
#   with an additional column specifying the indices for all individuals
#   with the same accuracy measure values

get.unique.index <- function(results, col.num, rowlabel = NULL) {

  if (is.null(dim(results))) {
    results <- matrix(data = results, nrow = 1, ncol = length(results),
                      dimnames = list(rowlabel, names(results)))
  }

  results <- as.data.frame(results)
  rows <- rownames(results)

  # account for individuals with same results as the first row
  if (nrow(results) == 1) {
    index <- rows
  } else {
    index <- rows[which(results[,col.num] == results[1,col.num])]
  }

  index.string <- vec2string(index)

  new <- cbind(results[index[1],], as.character(index.string))
  included <- as.character(index)

  # keep going, until all individuals are accounted for
  while (length(included) < nrow(results)) {
    new.start <- as.character(min(as.numeric(suppressWarnings(rows[rows != included]))))
    index <- rows[which(results[,col.num] == results[new.start,col.num])]
    index.string <- vec2string(index)
    new <- rbind(new, cbind(results[index[1],], as.character(index.string)))
    included <- as.character(sort(as.numeric(c(included, index))))
  }

  colnames(new)[length(colnames(new))] <- c("individuals")
  rownames(new) <- NULL
  new
}




# Brianna Hitt - 03.03.2020
#   Check whether all individual have equal accuracy measures
#   If not, list all the indices for each unique row
#   If yes, the individuals column will read "All"

check.all.equal <- function(results, col.num) {

  results <- as.data.frame(results)
  rows <- rownames(results)

  # account for individuals with same results as the first row
  index <- rows[which(results[,col.num] == results[1,col.num])]

  if (length(index) == nrow(results)) {
    index.string <- "All"
    new <- cbind(results[1,], as.character(index.string))
    colnames(new)[length(colnames(new))] <- c("individuals")
    rownames(new) <- NULL
    new
  }
}




# This function creates a vector of group sizes from a group membership matrix
get.pools <- function(group.nums){
  pools.table <- table(group.nums)
  pool.szs <- pools.table[[1]]
  for (i in 2:dim(pools.table)) {
    pool.szs <- c(pool.szs, pools.table[[i]])
  }
  pool.szs
}



# Start GroupMembershipMatrix
###############################################################################

# The purpose of this function is to construct a group membership matrix for a
#   two-, three-, four-, or five-stage hierarchical algorithm.

#' @title Construct a group membership matrix for hierarchical algorithms
#'
#' @description Construct a group membership matrix for two-, three-, or
#' four-stage hierarchical algorithms.
#'
#' @param stage1 the group size in stage one of testing. This also corresponds
#' to the number of individuals to be tested and will specify the number of
#' columns in the resulting group membership matrix.
#' @param stage2 a vector of group sizes in stage two of testing. The group sizes
#' specified here should sum to the number of individuals/group size specified
#' in \kbd{stage1}. If \kbd{NULL}, a group membership matrix will be constructed
#' for a two-stage hierarchical algorithm.
#' Further details are given under 'Details'.
#' @param stage3 a vector of group sizes in stage three of testing. The group sizes
#' specified here should sum to the number of individuals/group size specified
#' in \kbd{stage1}. If group sizes are provided in \kbd{stage2} and \kbd{stage3}
#' is \kbd{NULL}, a group membership matrix will be constructed for a
#' three-stage hierarchical algorithm. Further details are given under 'Details'.
#' @param stage4 a vector of group sizes in stage four of testing. The group sizes
#' specified here should sum to the number of individuals/group size specified
#' in \kbd{stage1}. If group sizes are provided in \kbd{stage3} and \kbd{stage4}
#' is \kbd{NULL}, a group membership matrix will be constructed for a four-stage
#' hierarchical algorithm. Further details are given under 'Details'.
#'
#' @details This function constructs a group membership matrix for two-, three-,
#' four-, or five-stage hierarchical algorithms. The resulting group membership
#' matrix has rows corresponding to the number of stages of testing and columns
#' corresponding to each individual to be tested. The value specified in
#' \kbd{stage1} corresponds to the number of individuals to be tested.
#'
#' For group membership matrices when only \kbd{stage1} is specified, a two-stage
#' hierarchical algorithm is used and the second stage will consist of individual
#' testing. For group membership matrices when \kbd{stage1} and \kbd{stage2} are
#' specified, a three-stage hierarchical algorithm is used and the third stage
#' will consist of individual testing. Group membership matrices for four- and
#' five-stage hierarchical algorithms follow a similar structure.
#' There should never be group sizes specified for later
#' stages of testing without also providing group sizes for all earlier stages
#' of testing (i.e., to provide group sizes for \kbd{stage3}, group sizes must
#' also be provided for \kbd{stage1} and \kbd{stage2}).
#'
#' @return A matrix specifying the group membership for each individual. The
#' rows of the matrix correspond to the stages of testing and the columns of
#' the matrix correspond to the individuals to be tested.
#'
#' @author Minh Nguyen and Christopher Bilder
#'
#' @family operating characteristic functions
#'
#' @examples
#' # Generate a group membership matrix for a two-stage
#' #   hierarchical algorithm, within the opChar1() function
#' #   and calculate operating characteristics
#' opChar1(algorithm = "D2", p = 0.0193, Se = 0.99, Sp = 0.99,
#'         hier.config = GroupMembershipMatrix(stage1 = 16),
#'         print.time = FALSE)
#'
#' # Generate a group membership matrix for a five-stage
#' #   hierarchical algorithm and calculate the
#' #   operating characteristics for a two-disease assay
#' config.mat <- GroupMembershipMatrix(stage1 = 16,
#'                                     stage2 = c(8,8),
#'                                     stage3 = c(4,4,4,4),
#'                                     stage4 = rep(2, times = 8))
#' Se <- matrix(data = rep(0.95, 10), nrow = 2, ncol = 5,
#'              dimnames = list(Infection = 1:2, Stage = 1:5))
#' Sp <- matrix(data = rep(0.99, 10), nrow = 2, ncol = 5,
#'              dimnames = list(Infection = 1:2, Stage = 1:5))
#' opChar2(algorithm = "D5", p.vec = c(0.92, 0.05, 0.02, 0.01),
#'         Se = Se, Sp = Sp, hier.config = config.mat)

# Brianna Hitt - 3 November 2023
#   Added checks to make sure all earlier stages are provided,
#   edited error messages and created documentation.


GroupMembershipMatrix <- function(stage1, stage2 = NULL,
                                  stage3 = NULL, stage4 = NULL) {

  # Check group sizes
  if (!is.null(stage2)) {
    if (sum(stage2) != stage1) {
      stop("The number of individuals in stage 2 is not the same as in stage 1.\n")
    }

    if (is.null(stage1)) {
      stop("Please provide an initial group size for stage 1 of testing.\n")
    }
  }

  if (!is.null(stage3)) {
    if (sum(stage3) != stage1) {
      stop("The number of individuals in stage 3 is not the same as in stage 1.\n")
    }

    if (is.null(stage1)) {
      stop("Please provide an initial group size for stage 1 of testing.\n")
    }

    if (is.null(stage2)) {
      stop("No group sizes were provided for stage 2 of testing. A group membership matrix will be constructed for a two-stage hierarchical algorithm.\n")
    }
  }

  if (!is.null(stage4)) {
    if (sum(stage4) != stage1) {
      stop("The number of individuals in stage 3 is not the same as in stage 1.\n")
    }

    if (is.null(stage1)) {
      stop("Please provide an initial group size for stage 1 of testing.\n")
    }

    if (is.null(stage2)) {
      stop("No group sizes were provided for stage 2 of testing. A group membership matrix will be constructed for a two-stage hierarchical algorithm.\n")
    }

    if (is.null(stage3)) {
      stop("No group sizes were provided for stage 3 of testing. A group membership matrix will be constructed for a three-stage hierarchical algorithm.\n")
    }

  }

  # Create group membership matrix for two-stage hierarchical testing
  if (is.null(stage2)) {
    # Set row1
    row1 <- rep(x = 1, time = stage1)

    # Set row2
    row2 <- 1:stage1

    # Set hier.config
    hier.config <- matrix(data = c(row1, row2), nrow = 2, byrow = TRUE)
    }

  # Create group membership matrix for three-stage hierarchical testing
  else if (is.null(stage3)) {
    # Set row1
    row1 <- rep(x = 1, time = stage1)

    # Set row2
    row2 <- NULL # Initiate row2
    for (i in 1:length(stage2)) {
      row2 <- c(row2, rep(x = i, time = stage2[i]))
      }

    # Set row3
    row3 <- 1:stage1

    # Set hier.config
    hier.config <- matrix(data = c(row1, row2, row3), nrow = 3, byrow = TRUE)
    }

  # Create group membership matrix for four-stage hierarchical testing
  else if (is.null(stage4)) {
    # Set row1
    row1 <- rep(x = 1, time = stage1)

    # Set row2
    row2 <- NULL # Initiate row2
    for (i in 1:length(stage2)) {
      row2 <- c(row2, rep(x = i, time = stage2[i]))
      }

    # Set row3
    row3 <- NULL # Initiate row3
    for (i in 1:length(stage3)) {
      row3 <- c(row3, rep(x = i, time = stage3[i]))
      }

    # Set row4
    row4 <- 1:stage1

    # Set hier.config
    hier.config <- matrix(data = c(row1, row2, row3, row4),
                          nrow = 4, byrow = TRUE)
    }

  else {
    # Set row1
    row1 <- rep(x = 1, time = stage1)

    # Set row2
    row2 <- NULL # Initiate row2
    for (i in 1:length(stage2)) {
      row2 <- c(row2, rep(x = i, time = stage2[i]))
      }

    # Set row3
    row3 <- NULL # Initiate row3
    for (i in 1:length(stage3)) {
      row3 <- c(row3, rep(x = i, time = stage3[i]))
      }

    # Set row4
    row4 <- NULL # Initiate row3
    for (i in 1:length(stage4)) {
      row4 <- c(row4, rep(x = i, time = stage4[i]))
      }

    # Set row5
    row5 <- 1:stage1

    # Set hier.config
    hier.config <- matrix(data = c(row1, row2, row3, row4, row5),
                          nrow = 5, byrow = TRUE)
    }

  hier.config
}


#
