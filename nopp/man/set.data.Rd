\name{set.data}
\alias{set.data}
\title{
Prepares data for Nash equilibrium
}
\description{
Prepares data for Nash equilibrium
}
\usage{
set.data(data, shape="wide", choice, varying, sep="_")
}
\arguments{
  \item{data}{the data set}
  \item{shape}{either \code{wide} or {long}. Default \code{wide}. See Details.}
  \item{choice}{the variable indicating the choice made: it can be either a logical vector, a numerical vector with 0 where the alternative is not chosen, a factor with level 'yes' when the alternative is chosen.}
  \item{varying}{the indexes of the variables that are alternative specific. See Details.}
  \item{sep}{the seperator of the variable name and the alternative name (only relevant for a wide data.frame). See Details.}
}
\details{
For general examples see the vignette. The arguments \code{shape}, \code{choice}, \code{varying} and \code{sep} as as in the \code{\link{mlogit.data}} function.
}
\value{A \code{mlogit.data} object, which is a \code{data.frame} in long format, i.e. one line for each alternative. It has a index attribute, which is a data.frame that contains the index of the choice made ('chid'), the index of the alternative ('alt') and, if any, the index of the individual ('id'). The choice variable is a boolean which indicates the choice made. This function use reshape if the data.frame is in wide format. It also has the attribute \code{call} for further data manipulation in the bootstrap task of \code{\link{equilibrium}}.
}
\author{
Luigi Curini, Stefano M. Iacus
}
\references{
Curini, L., and Iacus, S.M. (2017) Nash Optimal Party Positions: The nopp R Package, Journal of Statistical Software, 81(11), 1--25
}
\note{
See the vignette for detailed explanations and other working examples.
}


\examples{
\dontrun{
data(italy2006)

str(italy2006)
italy2006[1:2,1:14]

election <- set.data(italy2006 , shape="wide", choice="vote",
varying=c(5:14), sep="_")
str(election)

m <- mlogit(vote~prox+partyID | gov_perf+sex+age+education, 
election, reflevel = "UL")
summary(m)

true.pos <- list(FI=7.59, UL=3.50, RC=1.95, AN=8.08, UDC=5.66)
true.votes <- list(FI=.24, UL=.40, RC=.10, AN=.18, UDC=.08)
# model: comparison against true votes and party positions
nash.eq <- equilibrium(model=m, data=election, pos=true.pos, 
 votes=true.votes)
nash.eq
}
}

