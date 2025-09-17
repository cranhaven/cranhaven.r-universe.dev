\name{familyRank}
\alias{familyRank}
\title{Feature Ranking with Family Rank}
\description{Ranks features by incorporating graphical knowledge to weight empirical feature scores. This is the main function of the FamilyRank package.}
\usage{
familyRank(scores, graph, d = 0.5, n.rank = min(length(scores), 1000), 
n.families = min(n.rank, 1000), tol = 0.001)
}
\arguments{
  \item{scores}{A numeric vector of empirical feature scores. Higher scores should indicate a more predictive feature.}
  \item{graph}{A matrix or data frame representation of a graph object.}
  \item{d}{Damping factor}
  \item{n.rank}{Number of features to rank.}
  \item{n.families}{Number of families to grow.}
  \item{tol}{Tolerance}
}
\details{
The \code{scores} vector should be generated using an existing statistical method. Higher scores should correspond to more predictive features. It is up to the user to adjust accordingly. For example, if the user wishes to use p-values as the empirical score, the user should first adjust the p-values, perhaps by subtracting all p-values from 1, so that a higher value corresponds to a more predictive feature.

The \code{graph} must be supplied in matrix form, where the first two columns represent graph nodes and the third column represents the edge weights between nodes. The graph nodes must be represented by the index of the feature that corresponds with the index in the \code{score} vector. For example, a node corresponding to the first value of the \code{score} vector should be indicated by a 1 in the \code{graph} object, the second by a 2, etc. It is not necessary that every feature in the \code{score} vector appear in the \code{graph}. Missing pairwise interactions will be considered to have interaction scores of 0.

The damping factor, \code{d}, represents the percentage of weight given to the interaction scores. The damping factor must be between 0 and 1. Higher values give more weight to the interaction score while lower values give more weight to the empirical score.

The value for \code{n.rank} must be less than or equal to the number of scored features. The algorithm will include only the top \code{n.rank} features in the ranking process (e.g. the \code{n.rank} features with the highest values in the \code{score} vector will be used to grow families). Higher values of \code{n.rank} require longer compute times.

The value for \code{n.families} must be less than or equal to the value of \code{n.rank}. This is the number of families the algorithm will grow. If \code{n.families} is less than \code{n.rank}, the algorithm will initate families using the \code{n.families} highest scoring features. Higher values of \code{n.families} require longer compute times.

The tolerance variable, \code{tol}, tells the algorithm when to stop growing a family. Features are added to families until the weighted score is less than the tolerance level, or until all features have been added.
}
\value{Returns a vector of the weighted feature scores.}
\references{ADD REFERENCE}
\author{Michelle Saul}

\examples{
# Toy Example
scores <- c(.6, .2, .9)
graph <- cbind(c(1,1), c(2,3), c(.4, .8))
familyRank(scores = scores, graph = graph, d = .5)

\donttest{# Simulate data set
# 100 samples
# 1000 features
# Features 1 through 15 perfectly define response
# All other features are random noise
simulatedData <- createData(n.case = 50, n.control = 50, mean.upper=13, mean.lower=5,
                            sd.upper=1, sd.lower=1, n.features = 10000,
                            subtype1.feats = 1:5, subtype2.feats = 6:10,
                            subtype3.feats = 11:15)
x <- simulatedData$x
y <- simulatedData$y
graph <- simulatedData$graph

# Score simulated features using absolute difference in group means
scores <- apply(x, 2, function(col){
  splt <- split(col, y)
  group.means <- unlist(lapply(splt, mean))
  score <- abs(diff(group.means))
  names(score) <- NULL
  return(score)
})

# Display top 15 features using emprical score
order(scores, decreasing = TRUE)[1:15]

# Rank scores using familyRank
scores.fr <- familyRank(scores = scores, graph = graph, d = .5)
# Display top 15 features using emprical scores with Family Rank
order(scores.fr, decreasing = TRUE)[1:15]
}}