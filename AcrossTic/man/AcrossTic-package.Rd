\name{AcrossTic-package}
\alias{AcrossTic-package}
\alias{AcrossTic}
\docType{package}
\title{
\packageTitle{AcrossTic}
}
\description{
\packageDescription{AcrossTic}
}
\details{

The DESCRIPTION file:
\packageDESCRIPTION{AcrossTic}
\packageIndices{AcrossTic}
This primarily provides rRegMatch, which for arguments X and r produces
a minimum-distance r-regular subgraph of the rows of X.
}
\author{
\packageAuthor{AcrossTic}

Maintainer: \packageMaintainer{AcrossTic}
}
\references{
David Ruth, "A new multivariate two-sample test using regular minimum-weight spanning subgraphs,"
J. Stat. Distributions and Applications (2014)
}

\examples{
set.seed (123)
X <- matrix (rnorm (100), 50, 2) # Create data...
y <- rep (c (1, 2), each=25) # ...and class membership
\dontrun{rRegMatch (X, r = 3, y = y)}
}

