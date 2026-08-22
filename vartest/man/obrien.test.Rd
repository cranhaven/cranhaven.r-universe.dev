\name{obrien.test}
\alias{obrien.test}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
O'Brien Test
}
\description{
\code{obrien.test} performs O'Brien variance homogeneity test.
}

\usage{obrien.test(formula, data, center = "mean", trim.rate = 0.25, alpha = 0.05, 
  na.rm = TRUE, verbose = TRUE)}

%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{formula}{a formula of the form \code{lhs ~ rhs} where \code{lhs} gives the sample values and \code{rhs} the corresponding groups.}
  \item{data}{a tibble or data frame containing the variables in \code{formula}.} 
  \item{center}{a character string to define how to center. "mean": mean, "median": median, and "trim.mean": trimmed mean.} 
  \item{trim.rate}{the rate of observations trimmed from each tail of the distribution. Default is set to 0.25.} 
  \item{alpha}{the level of significance to assess variance homogeneity. Default is set to alpha = 0.05.}
  \item{na.rm}{a logical value indicating whether NA values should be stripped before the computation proceeds.}
  \item{verbose}{a logical for printing output to R console.}

}



\value{
A list with class "vht" containing the following components:
\item{statistic}{the corresponding test statistic.}
\item{parameter}{the parameter(s) of the approximate F distribution of the test statistic.}
\item{p.value}{the p-value of the test.}
\item{method}{the character string "O'Brien Test".}
\item{data}{a data frame containing the variables in which NA values (if exist) are removed.}
\item{formula}{a formula of the form \code{lhs ~ rhs} where \code{lhs} gives the sample values and \code{rhs} the corresponding groups.}

}



\references{

O'Brien, R.G. (1981). A Simple Test for Variance Effects in Experimental Designs. \emph{Psychological Bulletin}, \bold{89:3}, 570-574.

Gorbunova, A.A., Lemeshko, B.Y. (2012). Application of Parametric Homogeneity of Variances Tests under Violation of Classical Assumption. \emph{2nd Stochastic Modeling Techniques and Data Analysis International Conference}, \bold{5:8}, 253-260.

}



\author{
Gozde Cosar and Osman Dag
}





\examples{

library(vartest)

obrien.test(Sepal.Length ~ Species, data = iris, center = "mean")

out <- obrien.test(Sepal.Length ~ Species, data = iris, center = "mean")
summary(out)
}

\keyword{functions}
