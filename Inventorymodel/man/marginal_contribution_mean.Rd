\name{marginal_contribution_mean}
\alias{marginal_contribution_mean}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
fauxiliar}
\description{
Intermediate auxiliar function to calculate the mean of the marginal contributions for a set of permutations.
}
\usage{
marginal_contribution_mean(permute, costs)
}
\arguments{
  \item{permute}{
Matrix with \emph{n} columns. By rows, it contains a order.
}
  \item{costs}{
Vector with the associated costs to each posible coalition in a set of agents \emph{N}.
}
}
\value{
A vector with \emph{n} elements with component \emph{i} equal to the mean of the marginal contribution indicated by each order in \emph{permute} for agent \emph{i}.}