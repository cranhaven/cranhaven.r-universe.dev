\name{zv.test}
\alias{zv.test}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Z Variance Test
}
\description{
\code{zv.test} performs Z variance homogeneity test.
}

\usage{zv.test(formula, data, alpha = 0.05, na.rm = TRUE, verbose = TRUE)}

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
\item{method}{the character string "Z Variance Test".}
\item{data}{a data frame containing the variables in which NA values (if exist) are removed.}
\item{formula}{a formula of the form \code{lhs ~ rhs} where \code{lhs} gives the sample values and \code{rhs} the corresponding groups.}

}



\references{

Gorbunova, A.A., Lemeshko, B.Y. (2012). Application of Parametric Homogeneity of Variances Tests under Violation of Classical Assumption. \emph{2nd Stochastic Modeling Techniques and Data Analysis International Conference}, \bold{5:8}, 253-260.

Wang, Y., Gil, P.R., Chen, Y.H., Kromrey, J.D., Kim, E.S., Pham, T., Nguyen, D., Romano, J.L. (2017). Comparing the Performance of Approaches for Testing the Homogeneity of Variance Assumption in One-Factor Anova Models. \emph{Educational and Psychological Measurement}, \bold{77:2}, 305-329.

Overall, J.E., Woodward, J.A. (1974). A Simple Test for Heterogeneity of Variance in Complex Factorial Designs. \emph{Psychometrika}, \bold{39:3}, 311-318.

}



\author{
Gozde Cosar and Osman Dag
}



\examples{

library(vartest)

zv.test(Sepal.Length ~ Species, data = iris)

out <- zv.test(Sepal.Length ~ Species, data = iris, verbose = FALSE)
summary(out)

}

\keyword{functions}
