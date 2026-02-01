\name{plot.nutriNetwork}
\alias{plot.nutriNetwork}

\title{ plot for S3 class "nutriNetwork" }

\description{
Plot the graph path which is the output of the \code{\link{nutriNetwork}}.
}

\usage{
\method{plot}{nutriNetwork}( x, n.memberships=NULL , ... )
}

\arguments{
	\item{x}{An object from "nutriNetwork" class.}
	\item{n.memberships}{A vector containing number of variables in each group. For example, the \code{vfit} dataset that is provided in the package contains 3 different groups, where the first 7 variables are general covariates (e.g. age, sex, BMI, and etc.), the next 17 variables belong to nutrient (e.g. vitamins B6, B12, C, D, and etc.), and the last  5 variables belong to physical performance and muscle strength. Thus, \code{n.memberships = c(7, 17, 5)}. If \code{n.memberships = NULL}, in the graph visualization all markers are represented same colour.}
	\item{\dots}{System reserved (No specific usage)}
}

\author{ 
  Pariya Behrouzi \cr
  Maintainer: Pariya Behrouzi \email{pariya.behrouzi@gmail.com}
}

\references{
Behrouzi, P., and Wit, E. C. (2017c). netgwas: An R Package for Network-Based Genome-Wide Association Studies. arXiv preprint, arXiv:1710.01236.
}

\examples{
\dontshow{ 
data(vfit)
out <- nutriNetwork(vfit, method = "npn")
plot(out)  
}
\donttest{
data(vfit)
out <- nutriNetwork(vfit, method = "gibbs")
plot(out)   
}
}