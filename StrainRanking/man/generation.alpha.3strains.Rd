\name{generation.alpha.3strains}
\alias{generation.alpha.3strains}

\title{
Generation of parameters for the simulations under the regression model
}
\description{
Generation of parameters of the Dirichlet distribution used to draw the proportions of three strains at each site given in a matrix of coordinates.
}
\usage{
generation.alpha.3strains(x)
}

\arguments{
  \item{x}{
[2-column matrix] Coordinates where Dirichlet parameters are drawn. 
	}
}
\value{
Matrix of positive numerics with three columns corresponding to the number of strains that are considered and with number of rows equal to the number of sites given in \code{x}. Each row of the matrix provides the parameters of the Dirichlet distribution used to draw the proportions of three strains at each site given in \code{x}. 
}
\references{
Soubeyrand S., Tollenaere C., Haon-Lasportes E. & Laine A.-L. (2014). Regression-based ranking of pathogen strains with respect to their contributions to natural epidemics. PLOS ONE 9(1): e86591.
}
\author{
Soubeyrand, S., Tollenaere, C., Haon-Lasportes, E. and Laine, A.-L. 
}
\note{
	At each site \eqn{(x_{1,i},x_{2,i})} of \code{x}, the proportions of the three strains are defined by:
%\deqn{(p_i(1),p_i(2),p_i(3))\sim \text{Dirichlet}[100\{\cos(x_{2,i})+1.5,\sin(x_{1,i})+1.5,\sin(x_{2,i})+1.5\}].}
\deqn{(p_i(1),p_i(2),p_i(3)) \sim Dirichlet[100\{\cos(x_{2,i})+1.5,\sin(x_{1,i})+1.5,\sin(x_{2,i})+1.5\}].}
}

\seealso{
\link{DGobj.simul.regression}
}
\examples{
generation.alpha.3strains(expand.grid(1:10,1:10))
}
\keyword{ misc }

