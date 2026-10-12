\name{DGobj.simul.regression}
\alias{DGobj.simul.regression}

\title{
Simulation of a DG object under a regression model
}
\description{
Simulation of a \link{DG object} under a regression model generating proportions of pathogen strains in each node of a square grid.
}
\usage{
DGobj.simul.regression(sqrtn, size1, size2, theta, alpha.function, sigma,
 plots = FALSE)
}

\arguments{
  \item{sqrtn}{
[Positive integer] Side size of the square grid over which the proportions are simulated. The inter-node distance in the grid is one in the horizontal and vertical directions. The total number of grid nodes is sqrtn^2. 
}
  \item{size1}{
[Positive integer] Number of grid nodes where pathogen isolates are collected (sampling sites).
}
  \item{size2}{
[Positive integer] Number of pathogen isolates sampled in each sampling site.
}
  \item{theta}{
[Vector of numerics] Regression coefficients representing the fitness of the strains. The length of this vector determines the number of strains.
}
  \item{alpha.function}{
[Function] Function whose value is a matrix of positive numerics with number of columns equal to the number of strains and the number of rows is the number of grid nodes. Each row of the matrix provides the parameters of the Dirichlet distribution used to draw the proportions of strains at each node. The argument of the function is a 2-column matrix of coordinates.
}
  \item{sigma}{
[Postive numeric] Standard deviation of the white noise.
}
  \item{plots}{
[Logical] If TRUE, plots are produced. The plots show the proportion of each strain in space.
}
}


\value{
An object from the \link{DG class}.
}
\references{
Soubeyrand S., Tollenaere C., Haon-Lasportes E. & Laine A.-L. (2014). Regression-based ranking of pathogen strains with respect to their contributions to natural epidemics. PLOS ONE 9(1): e86591.
}
\author{
Soubeyrand, S., Tollenaere, C., Haon-Lasportes, E. and Laine, A.-L. 
}
\note{
The function \code{DGobj.simul.regression} generates a growth variable (third column of the demographic slot of the returned DG object) satisfying:
\deqn{Z_i=\left(\sum_{s=1}^S p_i(s) \code{theta}[s])\right) + \eta_i,}
for each demographic sampling site \eqn{i}. In this expression, \eqn{(p_i(1),...,p_i(S))} are the proportions of the strains at sampling site \eqn{i}, where \eqn{S} is the number of different strains. These proportions are drawn in Dirichlet distributions. \code{theta}\eqn{[s]} denotes the \eqn{s}-th component of \code{theta}. \eqn{\eta_i} denotes a centered random normal variable (white noise) with standard deviation \code{sigma}.
}

\seealso{
	\link{DGobj-class}, \link{DGobj.rawdata}, \link{DGobj.simul.mechanistic}, \link{generation.alpha.3strains}
}
\examples{
## Simulation of a data set
DGreg=DGobj.simul.regression(sqrtn=10, size1=30, size2=10, theta=c(1.5,2,3), 
	alpha.function=generation.alpha.3strains, sigma=0.1)
summary(DGreg)

## Simulation of a data set and plots of the proportions in space the strains
DGreg=DGobj.simul.regression(sqrtn=10, size1=30, size2=10, theta=c(1.5,2,3), 
	alpha.function=generation.alpha.3strains, sigma=0.1,plots=TRUE)
summary(DGreg)
}

\keyword{ datagen }

