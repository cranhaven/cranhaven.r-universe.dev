\name{DGobj.simul.mechanistic}
\alias{DGobj.simul.mechanistic}

\title{
Simulation of a DG object under a mechanistic model
}
\description{
Simulation of a \link{DG object} under a mechanistic model generating a multi-strain epidemic with multiple introductions over a square grid.
}
\usage{
DGobj.simul.mechanistic(sqrtn, size1, size2, theta, beta, M, delta, 
 plots = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{sqrtn}{
[Positive integer] Side size of the square grid over which the epidemic is simulated. The inter-node distance in the grid is one in the horizontal and vertical directions. The total number of grid nodes is sqrtn^2. 
}
  \item{size1}{
[Positive integer] Maximum number of grid nodes where pathogen isolates are collected (sampling sites).
}
  \item{size2}{
[Positive integer] Maximum number of pathogen isolates sampled in each sampling site.
}
  \item{theta}{
[Vector of positive numerics] Fitness coefficients of the strains. The length of this vector determines the number of strains in the epidemic.
}
  \item{beta}{
[Vector of postive numerics of size 2] Immigration parameters. The first component is the expected number of immigration nodes for every strain. The second component is the expected number of pathogen units in each immigration node.
}
  \item{M}{
[Positive integer] Number of time steps of the epidemic.
}
  \item{delta}{
[Positive numeric] Dispersal parameter.
}
  \item{plots}{
[Logical] If TRUE, plots are produced. The plots show the curse of the epidemic for each strain and the proportion of each strain in space at the final time step.
}
}

\details{
The effective number of sampling sites is the maximum of \code{size1} and the number of sites occupied at the last time of the simulation.
	
In each sampling site, the effective number of sampled isolates is the maximum of \code{size2} and the number of pathogen isolates in the site.
	
The immigration time \eqn{T^{immigr}_s} at which the sub-epidemic due to strain \eqn{s}  is initiated is randomly drawn between 1 and \code{M} with probabilities \eqn{P(T^{immigr}_s=t)=(M-t)^2/\sum_{k=1}^M (M-k)^2}. 

The number of immigration nodes is drawn from the binomial distribution with size \code{sqrtn}\eqn{^2} and with expectation given by the first component of \code{beta}. The immigration nodes are uniformly drawn in the grid.

At time \eqn{T^{immigr}_s}, the numbers of pathogen units of strain \eqn{s}  at the immigration nodes are independently drawn under the Poisson distribution with mean equal to the second component of \code{beta}.
}

\value{
	An object from the \link{DG class}.
}

\note{
	Demographic measurements, say \eqn{Y_i(M-1)} and \eqn{Y_i(M)}, made at the grid nodes and at times \code{M}-1 and \code{M}, are transformed into the values 
\eqn{ Z_i=\log\left(\frac{1+Y_i(M-1)}{1+Y_i(M)}\right)} characterizing the temporal growth of the epidemic in space at the end of the epidemic. The growth variable \eqn{Z_i} is given in the thrid column of the demographic slot of the returned DG object.
}

\references{
Soubeyrand S., Tollenaere C., Haon-Lasportes E. & Laine A.-L. (2014). Regression-based ranking of pathogen strains with respect to their contributions to natural epidemics. PLOS ONE 9(1): e86591.
}
\author{
Soubeyrand, S., Tollenaere, C., Haon-Lasportes, E. and Laine, A.-L. 
}

\seealso{
	\link{DGobj-class}, \link{DGobj.rawdata}, \link{DGobj.simul.regression}
}
\examples{
## Simulation of a data set
DGmech=DGobj.simul.mechanistic(sqrtn=10, size1=30, size2=10, theta=c(1.5,2,3), 
	beta=c(5,5), M=7, delta=0.2)
summary(DGmech)

## Simulation of a data set and plots of the sub-epidemics for the strains and their
## proportions in space at the final time step
DGmech=DGobj.simul.mechanistic(sqrtn=10, size1=30, size2=10, theta=c(1.5,2,3), 
	beta=c(5,5), M=7, delta=0.2, plots=TRUE)
summary(DGmech)
}
\keyword{ datagen }
