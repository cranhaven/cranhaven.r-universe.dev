\name{estSSfromVARX}
\alias{estSSfromVARX}

\title{Estimate a state space TSmodel using VAR estimation}
\description{
    Estimate a VAR TSmodel with (optionally) an exogenous input 
    and convert to state space.
}
\usage{
    estSSfromVARX(data, warn=TRUE, ...)}
\arguments{
    \item{data}{
    An object with the structure of an object of class TSdata (see TSdata).}
    \item{warn}{Logical indicating if warnings should be printed (TRUE) or
    suppressed (FALSE).}
    \item{...}{See arguements to estVARXls}}

\value{A state space model in an object of class TSestModel.}
\details{This function uses the functions estVARXls and toSS.}
\references{
    Gilbert, P. D. (1993) State space and ARMA models: An overview of
    the equivalence. Working paper 93-4, Bank of Canada. Available at
    \url{http://www.bankofcanada.ca/1993/03/publications/research/working-paper-199/}.

    Gilbert, P. D. (1995) Combining VAR Estimation and State Space 
    Model Reduction for Simple Good Predictions. \emph{J. of Forecasting: 
    Special Issue on VAR Modelling}. 14:229-250.
}
\seealso{
    \code{\link{toSS}}
    \code{\link{estSSMittnik}}
    \code{\link{bft}}
    \code{\link{estVARXls}}
    \code{\link{estMaxLik}}
}
\examples{
    data("eg1.DSE.data.diff", package="dse")
    model <-estSSfromVARX(eg1.DSE.data.diff)
}
\concept{DSE}
\keyword{ts}


