\name{EPM_allocation}
\alias{EPM_allocation}
\title{
Equal Profit Method allocation}
\description{
This function gives the Equal Profit Method allocation described in Frisk et al. (2010).
}
\usage{
EPM_allocation(characteristic_function, r = NA, info = NA, game = c("profit", "cost"))
}
\arguments{
  \item{characteristic_function}{
A vector with the characteristic function \eqn{v(S)} (or \eqn{c(S)}, if we work with a cost game), according to the order of coalitions shown in \code{coalitions} function.
}
  \item{r}{
Incremental step for calculating the EPM-allocation. If no solution is found, we increase iteratively in \code{r} units the allowed excess to get an epsilon-core allocation. Procedure stops when a solution is found.
}
  \item{info}{
For knowing information about the resolution, make \code{info=1}
}
  \item{game}{
Characters to indicate if the game is a cost or profit game. The possibles values are \code{"cost"} or \code{"profit"}.
}
}
\value{
A vector with the allocation that EPM method proposes.
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
Frisk, M., Gothe-Lundgren, M.,Jornsten, K., Ronnqvist, M. (2010). Cost allocation in collaborative
forest transportation. European Journal of Operational Research, Vol. 205,
pp. 448-458.}
\author{
A. Saavedra-Nieves
}

\examples{
characteristic_function<-c(0,0.538, 0.761, 1.742, 0.554, 0.137, 0.293, 0.343)
EPM_allocation(characteristic_function,r=0.01,info=1,game="cost")
#[1] "EPM_allocation"
#[1] "The cost game has a non-empty core"
#     1     2     3
#  0.05 0.206 0.087
}

\keyword{EPM}% use one of  RShowDoc("KEYWORDS")
\keyword{EPM_allocation}% __ONLY ONE__ keyword per line
