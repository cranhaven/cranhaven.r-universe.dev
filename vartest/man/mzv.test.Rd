\name{mzv.test}
\alias{mzv.test}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Modified Z Variance Test
}
\description{
\code{mzv.test} performs Modified Z Variance variance homogeneity test.
}

\usage{mzv.test(formula, data, alpha = 0.05, na.rm = TRUE, verbose = TRUE)}

%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{formula}{a formula of the form \code{lhs ~ rhs} where \code{lhs} gives the sample values and \code{rhs} the corresponding groups.}
  \item{data}{a tibble or data frame containing the variables in \code{formula}.} 
  \item{alpha}{the level of significance to assess variance homogeneity. Default is set to alpha = 0.05.}
  \item{na.rm}{a logical value indicating whether NA values should be stripped before the computation proceeds.}
  \item{verbose}{a logical for printing output to R console.}

}



\value{
A list with class "vht" containing the following components:
\item{statistic}{the corresponding test statistic.}
\item{parameter}{the parameter(s) of the approximate F distribution of the test statistic.}
\item{p.value}{the p-value of the test.}
\item{method}{the character string "Modified Z Variance Test".}
\item{data}{a data frame containing the variables in which NA values (if exist) are removed.}
\item{formula}{a formula of the form \code{lhs ~ rhs} where \code{lhs} gives the sample values and \code{rhs} the corresponding groups.}

}



\references{

Wang, Y., Gil, P.R., Chen, Y.H., Kromrey, J.D., Kim, E.S., Pham, T., Nguyen, D., Romano, J.L. (2017). Comparing the Performance of Approaches for Testing the Homogeneity of Variance Assumption in One-Factor Anova Models. \emph{Educational and Psychological Measurement}, \bold{77:2}, 305-329.

Overall, J.E., Woodward, J.A. (1974). A Simple Test for Heterogeneity of Variance in Complex Factorial Designs. \emph{Psychometrika}, \bold{39:3}, 311-318.

}



\author{
Gozde Cosar and Osman Dag
}



\examples{

library(vartest)

mzv.test(Sepal.Length ~ Species, data = iris)

out <- mzv.test(Sepal.Length ~ Species, data = iris)
summary(out)
}

\keyword{functions}
