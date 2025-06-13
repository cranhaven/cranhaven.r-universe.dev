\name{mod.lt}
\alias{mod.lt}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Calculate a Model Life Table
}
\description{
This function takes as arguments a value for child mortality, an indication of which child mortality indicator is supplied (defaults to 5q0), a possible value for 45q15, a possible target life expectancy to determine the level of mortality and the sex of the desired life table (defaults to "female"). The function selects the appropriate family (and level if a desired life expectancy is supplied or a value for alpha) and returns a life table. The resulting life table defaults to alpha=0.  
}
\usage{
mod.lt(child.value, child.mort=4, e0.target=NULL, adult.mort=NULL, 
	sex="female", alpha=0)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{child.value}{ A numeric value for one of four child mortality measures.
}
  \item{child.mort}{ An integer (1-4) to indicate which child mortality indicator is being supplied (1 - 1m0; 2 - 5m0; 3 - 1q0; 4 - 5q0). Defaults to 5q0. 
}
  \item{e0.target}{ The desired life expectancy for the life table. If no life expectancy is supplied the resulting life table defaults to alpha=0. 
}
  \item{sex}{ "male" or "female" indicates the sex for the desired life table. Defaults to "female".
}
  \item{adult.mort}{ Optional. If supplied, the function uses both the child mortality measure and 45q15 to select the family. Otherwise, just the child mortality indicator is used. 
}
  \item{alpha}{If e0.target is not supplied, alpha defaults to 0. This can be any number with negative values producing lower mortality and positive values producing higher mortality.  
  	}
}
%\details{
%%  ~~ If necessary, more details than the description above ~~
%}
\value{
\item{lt.out}{The life table output from the function \code{\link{lt.mx}} including \code{$e0}, life expectancy at birth, \code{$lt.5q0}, the probability of death between birth and age 5, \code{$lt.4q15}, the probability of death between age 15 and 60, and \code{$lt}, the complete, resulting life table with columns for age, nax, nmx, nqx, npx, ndx, lx, nLx, Tx,  and ex.}
\item{alpha}{The alpha value which produced the table. This value is either user supplied or calculated from the user supplied \code{e0.target}.}
\item{sex}{Sex of the life table.}
\item{family}{An integer (1-5) to indicate the life table family/pattern.}
}

\references{
S. Clark and D. Sharrow (2011) "Contemporary Model Life Tables for Developed Countries: An Application of Model-Based Clustering."  Paper presented at the annual meeting of the Population Association of America. \url{http://paa2011.princeton.edu/abstracts/111808}

\emph{Human Mortality Database} University of California, Berkeley (USA), and Max Planck Institute for Demographic Research (Germany). Available at www.mortality.org or www.humanmortality.de (data downloaded February 2015).
}

\author{
David Sharrow
}


\seealso{
\code{\link{hmd.DA}}, \code{\link{alpha.e0}}, \code{\link{mortmod}}, \code{\link{lt.mx}}
}
\examples{
## Example input data from USA females 1955-59 using a desired e0
mod.lt(child.value=0.02769, child.mort=4, sex="female", 
  adult.mort=0.13461, e0.target=72.5) 
  
## The user can also supply an alpha value directly
mod.lt(child.value=0.02769, child.mort=4, sex="female", 
  adult.mort=0.13461, alpha=0.233) 
}
\keyword{ models }
\keyword{ misc }% __ONLY ONE__ keyword per line
