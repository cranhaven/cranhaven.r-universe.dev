\name{aiStaHau}
\alias{aiStaHau}
\title{Conducting a Hausman Test on a Static AIDS Model}
\description{Conduct a Hausman test on a static AIDS model and report the result of likelihood ratio test.}
\usage{aiStaHau(x, instr, choice = FALSE,  
  exog = c("none", "all", "partial"), shift.new = NULL)}
\arguments{
  \item{x }{ an object of class \code{aiStaFit} from a static AIDS model.}
  \item{instr }{ a single time series data as instrument for the expenditure variable in AIDS model.}
  \item{choice }{ a logical value of whether to take a difference on the right-hand \code{price} and \code{instr} variables.}
  \item{exog }{ how the exogenous shift variables in the AIDS model should be included in the Hausman test; "none" for nothing; "all" for all the dummyr or seasonality variables; and "partial" for some.}
  \item{shift.new }{ when \code{exog} is "partial", this argument should contain the list of shift variables.}
}

\details{Conduct a Hausman test on a static AIDS model and report the result of likelihood ratio test. Note that logarithm is taken on every variable in the auxiliary regression. The dependant variable is the real total expenditure. The independant variables include the lagged value of the real total expenditure, the instrumental variable as supplied, the price variables, and some or all shift variables as included in the AIDS model. }
\value{Return a data frame object with the statistics and p values for the four tests by equation.
  \item{daHau }{ data used in estimating the Hausman test.}
  \item{formuHau }{ formula for estimating the Hausman test.}
  \item{regHau }{  regression for the Hausman test.}
  \item{daFit }{  revised data with the fitted value of expenditure included.}
  \item{aiBase }{  the base static AIDs model estimated.}
  \item{aiHaus }{  the reestimated static AIDS model using the fitted value of expenditure.}
  \item{ratio }{  result of the likelihood ration test for the Hausman test.}
}

\references{Wan, Y., C. Sun, and D.L. Grebner. 2010. Analysis of import demand for wooden beds in the United States. Journal of Agricultural and Applied Economics 42(4):643-658.}
\author{Changyou Sun (\email{cs258@msstate.edu})}
\seealso{\code{\link{aiStaFit}}; \code{\link{print.aiFit}}.}

\examples{
# see the examples for 'aiDynFit'.
}
\keyword{regression}