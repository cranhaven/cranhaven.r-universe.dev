\name{f.test}
\alias{f.test}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Fisher's Test
}
\description{
\code{f.test} performs Fisher's variance homogeneity test.
}

\usage{f.test(formula, data, alpha = 0.05, na.rm = TRUE, verbose = TRUE)}

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
\item{method}{the character string "Fisher's Test".}
\item{data}{a data frame containing the variables in which NA values (if exist) are removed.}
\item{formula}{a formula of the form \code{lhs ~ rhs} where \code{lhs} gives the sample values and \code{rhs} the corresponding groups.}

}



\references{

Gorbunova, A.A., Lemeshko, B.Y. (2012). Application of Parametric Homogeneity of Variances Tests under Violation of Classical Assumption. \emph{2nd Stochastic Modeling Techniques and Data Analysis International Conference}, \bold{5:8}, 253-260.

}



\author{
Gozde Cosar and Osman Dag
}





\examples{

library(vartest)

f.test(Sepal.Length ~ Species, data = iris)

out <- f.test(Sepal.Length ~ Species, data = iris)
summary(out)
}

\keyword{functions}
