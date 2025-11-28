\name{Gower.dist}
\alias{Gower.dist}
\title{Gower distance}
\description{Calculates Gower distance}

\usage{Gower.dist(data.x, data.y=data.x, rngs=NULL,
 KR.corr=TRUE, na.rm=FALSE)}

\arguments{

\item{data.x}{A matrix or a data frame containing variables that should
 be used in the computation of the distance.}

\item{data.y}{A numeric matrix or data frame with the same variables, of
 the same type, as those in 'data.x'}

\item{rngs}{A vector with the ranges to scale the variables. Its length
 must be equal to number of variables in 'data.x'}

\item{KR.corr}{When TRUE (default) the extension of the Gower's
 dissimilarity measure proposed by Kaufman and Rousseeuw (1990) is used.
 Otherwise, the original Gower's (1971) formula is considered.}

\item{na.rm}{Replace missing values with maximal distance?}

}

\details{

Gower.dist() code based on analogous function from 'StatMatch' package;
please see this package for the original code and full documentation.

This function computes the Gower's distance (dissimilarity) among units
in a dataset or among observations in two distinct datasets. Columns of
mode numeric will be considered as interval scaled variables; columns of
mode character or class factor will be considered as categorical nominal
variables; columns of class ordered will be considered as categorical
ordinal variables and, columns of mode logical will be considered as
binary asymmetric variables. Missing values (NA) are allowed. If only
data.x is supplied, the dissimilarities between _rows_ of data.x will be
computed.

For 'rngs', in correspondence of non-numeric variables, just put 1 or NA.
When rngs=NULL (default), the range of a numeric variable is estimated by
jointly considering the values for the variable in 'data.x' and those in
'data.y'.

When 'na.rm=TRUE', all missing values (NAs and NaNs) in the result will
be replaced with maximal distance. This is discussable but helps, e.g.,
to bootstrap hierarchical clustering in case if data is rich of NAs.

}

\value{

A distance object with distances among rows of 'data.x' and those of
'data.y'.

}

\references{

Gower J.C. 1971. A general coefficient of similarity and some of its
properties. Biometrics. 27: 623--637.

Kaufman L., Rousseeuw P.J. 1990. Finding Groups in Data: An Introduction
to Cluster Analysis. Wiley, New York.

}

\author{Alexey Shipunov}

\seealso{\code{\link{dist}}, cluster::\code{\link[cluster]{daisy}}}

\examples{

x1 <- as.logical(rbinom(10, 1, 0.5))
x2 <- sample(letters, 10, replace=TRUE)
x3 <- rnorm(10)
x4 <- ordered(cut(x3, -4:4, include.lowest=TRUE))
xx <- data.frame(x1, x2, x3, x4, stringsAsFactors=FALSE)

## matrix of distances among first obs. in xx and the remaining ones
Gower.dist(data.x=xx[1:6, ], data.y=xx[7:10, ])

## matrix of distances among observations in xx
row.names(xx) <- LETTERS[1:nrow(xx)]
dx <- Gower.dist(xx)
plot(hclust(dx))

}

\keyword{multivariate}
