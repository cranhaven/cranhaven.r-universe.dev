\name{rRegMatch}
\alias{rRegMatch}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Regular matching with minimum-cost spanning subgraphs
}
\description{
This function matches each observation in X to r others so as to minimize the 
total distance across all matches. Optionally it computes the cross-count
statistic -- the number of matches associated with two observations from 
different classes.
}
\usage{
rRegMatch(X, r, y = NULL, dister = "daisy", dist.args = list(), keep.X = nrow(X) < 100, 
    keep.D = (dister == "treeClust.dist"), relax = (N >= 100), thresh = 1e-6)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{X}{
Matrix or data frame of data, or inter-point distances represented in an object inheriting from "dist"
}
  \item{r}{
Integer number of matches. The matching is "regular" in that every observation is matched to exactly
r others (or, if relax=TRUE, every observation is matched to others with weights in [0, 1] that add up to r).
}
  \item{y}{
Vector of class membership indices. This is used to compute the cross-count statistic. Optional.
}
  \item{dister}{
Function to compute inter-point distances. This must take as its first argument
a matrix of data argument name \code{x}. Default: \code{\link{daisy}}. 
If all the columns are numeric,
this produces unweighted Euclidean distance by default.
}
  \item{dist.args}{
List of argument to the \code{dister} function.
}
  \item{keep.X}{
If TRUE, and X was supplied, keep the X matrix in the output object. Default: 
TRUE if X was supplied and also nrow (X) < 100.
}
  \item{keep.D}{
If TRUE, keep the distance object in the output. Default: TRUE if the 
\code{\link{treeClust.dist}} function is being
used to compute the distances (since in that case the distances are random).
}
  \item{relax}{
If FALSE, solve the exact problem where each observation gets exactly r 
non-zero pairings, each with weight 1. If TRUE, solve the relaxed problem, 
where each observation has at least r non-zero pairings, 
each with its own weight between 0 and 1, the weights adding up to r. The
exact problem gets very slow with large samples.
}
  \item{thresh}{
Weights smaller than this are considered to be exactly zero. Default: 1e-6.
}
}
\details{
This function solves an optimization problem to extract the set of pairings which make the total weight
(distance) associated with all pairings a minimum, subject to the constraint that every observation is paired
to r others (or to enough others to have a total pair-weight of r).
}
\value{
A list of class AcrossTic, with elements:
\item{matches }{A two-column matrix, each row gving the indices of one matched pair.}
\item{total.dist }{total distance across all matches -- the optimal value from the optimization problem.}
\item{status }{Status of result -- if the optimum was found, a vector of length 1 with 
name "TM_OPTIMAL_SOLUTION_FOUND" and value 0.}
\item{time.required }{Time taken to run the optimization, as reported by \code{system.time()}.} 
\item{call }{The call made to the function, from \code{match.call}.}
\item{r }{The value of r, as supplied at the time of the call.}
\item{dister }{The value of dister, as supplied at the time of the call.}
\item{dist.args }{The value of dist.args, as supplied at the time of the call.}
\item{X.supplied }{Logical indicating whether X was supplied.}
\item{X }{X matrix, if it was available and asked to be kept}
\item{y }{y vector, as supplied}
\item{edge.weights}{vector, of length \code{nrow(matches)}, giving the distances for each match. For the exact problem (\code{relax = FALSE}), each value is
 equal to 0 or 1. For the relaxed problem (\code{relax = TRUE}), each value is
 between 0 and 1, with values summing to \code{(r * nrow(X) / 2)}.}
\item{cross.sum }{Sum of matcher.costs across all matches}
\item{cross.count }{Number of matches between two observations of different classes, possibly weighted}
\item{nrow.X, ncol.X }{dimension of X matrix}
}
\references{
David Ruth, "A new multivariate two-sample test using regular minimum-weight spanning subgraphs,"
J. Stat. Distributions and Applications (2014)
}
\author{
David Ruth and Sam Buttrey
}

\examples{
set.seed (123)
X <- matrix (rnorm (100), 50, 2) # Create data...
y <- rep (c (1, 2), each=25) # ...and class membership
rRegMatch (X, r = 3, y = y)
\dontrun{plot (rRegMatch (X, r = 3, y = y)) # to see picture}}


