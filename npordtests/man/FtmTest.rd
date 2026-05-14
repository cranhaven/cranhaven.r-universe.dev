\name{FtmTest}
\alias{FtmTest}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Ferdhiana, Terpstra and Magel (FTM) Test}
\description{
\code{FtmTest} performs FTM test.
}

\usage{FtmTest(formula, data, alpha = 0.05, na.rm = TRUE, verbose = TRUE)}

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

\item{statistic}{the FTM test statistic.}
\item{mean}{the mean of the FTM test statistic.}
\item{variance}{the variance of the FTM test statistic.}
\item{Z}{the standardized test statistic.}
\item{p.value}{the p-value of the test.}
\item{alpha}{the level of significance.}
\item{method}{the character string "FTM test".}
\item{data}{a data frame containing the variables in which NA values (if exist) are removed.}
\item{formula}{a formula of the form \code{lhs ~ rhs} where \code{lhs} gives the sample values and \code{rhs} the corresponding groups.}


}

\references{

Ferdhiane, R., Terpstra, J., Magel, R.C. (2008). A nonparametric test for the ordered alternative based on Kendall's correlation coefficient. \emph{Communications in Statistics-Simulation and Computation}, \bold{37:6}, 1117-1128.

Jonckheere, A. R. (1954). A Distribution-Free k-Sample Test Against Ordered Alternatives. \emph{Biometrika}, \bold{41}, 133-145.


}


\author{
Bulent Altunkaynak}



\examples{

library(npordtests)

## Data from Jonckheere (1954)
data(jdata)
FtmTest(Y~X,jdata)

}

\keyword{functions}
