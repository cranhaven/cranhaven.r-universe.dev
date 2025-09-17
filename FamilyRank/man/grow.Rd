\name{grow}
\alias{grow}
\title{Grow Families}
\description{Call to the C++ function that grows the families.}
\usage{
grow(n, f, d, graph, scores, feat_mat, score_mat, tol, weight_mat, selected)
}
\arguments{
  \item{n}{Number of features to rank.}
  \item{f}{Number of families to grow.}
  \item{d}{Damping factor}
  \item{graph}{A matrix or data frame representation of a graph object.}
  \item{scores}{A numeric vector of empirical feature scores.}
  \item{feat_mat}{Matrix to store selected features.}
  \item{score_mat}{Matrix to store weighted scores of selected features.}
  \item{tol}{Tolerance}
  \item{weight_mat}{A matrix to store the cumulative weighted scores of selected futures across all families.}
  \item{selected}{Vector indicating whether a feature has been selected yet.}
}
\details{This is the workhorse function for the Family Rank algorithm.}
\value{Returns a matrix with \code{1+2xn.families} columns and \code{n.rank} rows. The first column is the cumulative feature score for each of the ranked features \code{1:n.rank}. The row number corresponds to the re-indexed feature index. The next \code{n.families} columns contain the indices of selected features for each iteration of feature selection. The last \code{n.families} columns contain the weigthed scores of selected features for each iteration.}
\references{ADD REFERENCE}
\author{Michelle Saul}
\examples{
# Toy Example
scores <- c(.6, .2, .9)
graph <- cbind(c(1,1), c(2,3), c(.4, .8))

# initialize matrices 
n <- n.families <- length(scores)
feat.mat <- score.mat <- matrix(0, nrow = n, ncol = n.families)
feat.mat[1,] <- order(scores, decreasing = TRUE)
score.mat[1,] <- sort(scores, decreasing = TRUE)

# Grow families
mats <- grow(n = n, f = n.families, d = 0.5, graph = as.matrix(graph), 
             scores = scores, 
             feat_mat = feat.mat, score_mat = score.mat, tol = 0, 
             weight_mat = as.matrix(scores), selected = rep(1, n))
# Selected Feature Matrix
## columns represent familes
## rows represent iterations
## values indicate indices of selected features
feat.mat <- mats[, 2:(n.families+1)]
feat.mat
# Corresponding Score Matrix
## columns represent familes
## rows represent iterations
## values indicate max weighted score of selected features
score.mat <- mats[, (n.families+2):(1+2*n.families)]
score.mat
}