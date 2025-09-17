\name{indexFeats}
\alias{indexFeats}
\title{Re-index features}
\description{Re-index features based on number to rank. Called by \code{\link{familyRank}}.}
\usage{
indexFeats(scores, graph, n.rank = NULL)
}
\arguments{
  \item{scores}{A numeric vector of empirical feature scores.}
  \item{graph}{A matrix or data frame representation of a graph object.}
  \item{n.rank}{Number of features to rank.}
}
\details{This function is used to re-index features for the Family Rank algorithm. The function takes in the scores for all features, and returns scores for the top \code{n.rank} features. It also takes in the full domain knowledge \code{graph} and returns the subgraph that only includes interactions between the top \code{n.rank} features. Finally, it re-indexes the top features in both the score vector and domain knowledge graph to 1:n.rank.}
\value{Returns a named list with re-indexed domain knowledge graph (\code{graph.w}), re-indexed scores  (\code{score.w}), a mapping between original and new indices (\code{loc.map}), and the number of features to rank (\code{n.rnak}).
}
\references{ADD REFERENCE}
\author{Michelle Saul}
\note{\code{indexFeats} is not meant to be called alone. It is designed as a helper function for \code{\link{familyRank}}.}
\seealso{\code{\link{familyRank}}}