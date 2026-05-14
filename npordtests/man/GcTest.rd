\name{GcTest}
\alias{GcTest}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Gaur's Gc Test}
\description{
\code{GcTest} performs Gaur's Gc test.
}

\usage{GcTest(formula, data, alpha = 0.05, na.rm = TRUE, verbose = TRUE, c = 2)}

%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{formula}{a formula of the form \code{lhs ~ rhs} where \code{lhs} gives the sample values and \code{rhs} the corresponding groups.}
  \item{data}{a data frame containing the variables in the formula \code{formula}}
  \item{alpha}{the level of significance to assess the statistical difference. Default is set to alpha = 0.05.}
  \item{na.rm}{a logical value indicating whether NA values should be stripped before the computation proceeds.}
  \item{verbose}{a logical for printing output to R console.}
  \item{c}{a integer value chosen from \code{(1,..., min(n_i))} for subsample size. Default is set to c = 2.}

}

%\details{
%%  ~~ If necessary, more details than the description above ~~
%}


\value{
A list with class "owt" containing the following components:

\item{statistic}{the Gaur's Gc test statistic.}
\item{mean}{the mean of the Gaur's Gc test statistic.}
\item{variance}{the variance of the Gaur's Gc test statistic.}
\item{Z}{the standardized test statistic.}
\item{p.value}{the p-value of the test.}
\item{alpha}{the level of significance.}
\item{method}{the character string "Gaur's Gc test ".}
\item{data}{a data frame containing the variables in which NA values (if exist) are removed.}
\item{formula}{a formula of the form \code{lhs ~ rhs} where \code{lhs} gives the sample values and \code{rhs} the corresponding groups.}


}

\references{

Gaur, A., (2017). A class of k-sample distribution-free tests for location against ordered alternatives. \emph{Communications in Statistics-Theory and Methods}, \bold{46:5}, 2343-2353.

Jonckheere, A. R. (1954). A Distribution-Free k-Sample Test Against Ordered Alternatives. \emph{Biometrika}, \bold{41}, 133-145.


}


\author{
Bulent Altunkaynak}



\examples{

library(npordtests)

## Data from Jonckheere (1954)
data(jdata)
GcTest(Y~X,jdata)

## Data from Lehmann (1975)
data(lehmann)
GcTest(Values~Group,lehmann)

}

\keyword{functions}
