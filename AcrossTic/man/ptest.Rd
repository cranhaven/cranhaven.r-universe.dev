\name{ptest}
\alias{ptest}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Permutation test for AcrossTic objects
}
\description{
This function permutes the "y" entries in an AcrossTic object and computes the
cross-count statistic for each permutation. This generates a null distribution
suitable for use in a permutation test.
}
\usage{
ptest(acobj, y, edge.weights, n = 1000)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{acobj}{
Object of class AcrossTic, output from \code{\link{rRegMatch}}.
}
  \item{y}{
Character, factor or logical indicating class membership for each observation.
Normally this will be found inside \code{acobj}.
}
  \item{edge.weights}{Vector of weights associated with each match. If omitted,
the default is a vector of 1's of the proper length, unless the "acobj" object
was computed with partial matching, in which case omitting edge.weights
produces an error.
}
  \item{n}{
Integer, number of simulations. Default, 1000.
}

}
\details{
This function permutes the y's \code{n} times and computes the cross-count-match statistic. 
If the observed value in the \code{acobj} is generally smaller than the permuted values,
we conclude the distributions of the classes are different.
}
\value{
A list with class AcrossTicPtest and three components: 
\item{sims }{Vector of \code{n} cross-count values computed under permutation}
\item{observed }{Observed cross-count statistic}
\item{p.value }{P-value for test}
}

\author{
Sam Buttrey and Dave Ruth
}
\examples{
set.seed (123)
X <- matrix (rnorm (100), 50, 2) # Create data...
y <- rep (c ("One", "Two"), each=25) # ...and class membership
\dontrun{ptest (rRegMatch (X, r = 3, y = y))} # p = .479
X[1:25,] <- X[1:25,] + 1
\dontrun{ptest (rRegMatch (X, r = 3, y = y))} # p = .037
}


%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{rRegMatch}}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ htest}

