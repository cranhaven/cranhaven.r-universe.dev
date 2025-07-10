#' Calculate the Consistency Proportion Index (CPP)
#'
#' This function calculates the Consistency Proportion Index (CPP), a measure of the consistency of clustering results.
#' The CPP is calculated by determining the most common cluster assignment for each group and then computing the proportion of cases that are assigned to these clusters.
#'
#' @param I A matrix where each row represents a case and each column represents a cluster assignment. The last column should indicate the group membership (1, 2, or 3).
#' @return A list containing:
#' \item{ICPP}{The Consistency Proportion Index.}
#' @export
#'
#' @examples
#' # Example usage
#' set.seed(123)
#' n <- 100
#' values1 <- sample(1:3, 30, replace = TRUE)
#' values2 <- sample(1:3, 30, replace = TRUE) + 1
#' values3 <- sample(1:3, 40, replace = TRUE) + 2
#' values <- c(values1, values2, values3)
#' categories <- c(rep(1, 30), rep(2, 30), rep(3, 40))
#' I <- cbind(1:n, values, categories)
#' CPP <- IndexCPP(I)
#' print(CPP)
#'
#' @keywords clustering consistency
IndexCPP <- function(I) {
  getmode <- function(v) {
    uniqv <- unique(v)
    uniqv[which.max(tabulate(match(v, uniqv)))]
  }
  n <- nrow(I); nc <- ncol(I)
  n1 <- sum(I[, nc] == 1)
  n2 <- sum(I[, nc] == 2)
  n3 <- sum(I[, nc] == 3)
  v1 <- I[1:n1, 2]
  v2 <- I[(n1 + 1):(n1 + n2), 2]
  v3 <- I[(n1 + n2 + 1):n, 2]
  n11 <- getmode(v1)
  n12 <- getmode(v2)
  n13 <- getmode(v3)
  if (n12 == n11) {
    v2 <- v2[-which(v2 == n11)]
  } else {
    v2 <- v2
  }
  n12 <- getmode(v2)
  v3 <- I[, 2]
  v3 <- v3[-which(v3 == n11)]
  v3 <- v3[-which(v3 == n12)]
  n13 <- getmode(v3)
  j11 <- sum(I[1:n1, 2] == n11)
  j12 <- sum(I[(n1 + 1):(n1 + n2), 2] == n12)
  j13 <- sum(I[(n1 + n2 + 1):n, 2] == n13)
  j1 <- j11 + j12 + j13
  ICPP <- j1 / n
  return(ICPP)
}
