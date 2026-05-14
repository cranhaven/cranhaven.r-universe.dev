\name{LtTest}
\alias{LtTest}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Buning's LT Test}
\description{
\code{LtTest} performs LT test.
}

\usage{LtTest(formula, data, alpha = 0.05, na.rm = TRUE, verbose = TRUE)}

%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{formula}{a formula of the form \code{lhs ~ rhs} where \code{lhs} gives the sample values and \code{rhs} the corresponding groups.}
  \item{data}{a data frame containing the variables in the formula \code{formula}}
  \item{alpha}{the level of significance to assess the statistical difference. Default is
set to alpha = 0.05.}
  \item{na.rm}{a logical value indicating whether NA values should be stripped before the computation proceeds.}
  \item{verbose}{a logical for printing output to R console.}

}

%\details{
%%  ~~ If necessary, more details than the description above ~~
%}


\value{
A list with class "owt" containing the following components:

\item{statistic}{the LT test statistic.}
\item{mean}{the mean of the LT test statistic.}
\item{variance}{the variance of the LT test statistic.}
\item{Z}{the standardized test statistic.}
\item{p.value}{the p-value of the test.}
\item{alpha}{the level of significance.}
\item{method}{the character string "LT test ".}
\item{data}{a data frame containing the variables in which NA values (if exist) are removed.}
\item{formula}{a formula of the form \code{lhs ~ rhs} where \code{lhs} gives the sample values and \code{rhs} the corresponding groups.}


}

\references{

Buning, H.(1996). Adaptive tests for the c-sample location problem - the case of two-sided alternatives. \emph{Communications in Statistics-Theory and Methods}, \bold{25}, 1569-1582.

Beier, F., Buning, H.(1997). An adaptive test against ordered alternatives. \emph{Computational Statistics & Data Analysis}, \bold{25:4}, 441-452.

Jonckheere, A. R. (1954). A Distribution-Free k-Sample Test Against Ordered Alternatives. \emph{Biometrika}, \bold{41}, 133-145.


}


\author{
Bulent Altunkaynak}



\examples{

library(npordtests)

## Data from Jonckheere (1954)
data(jdata)
LtTest(Y~X,jdata)

## Data from Lehmann (1975)
data(lehmann)
LtTest(Values~Group,lehmann)

}

\keyword{functions}
