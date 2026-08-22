\name{duran.test}
\alias{duran.test}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Duran Test
}
\description{
\code{duran.test} performs Duran variance homogeneity test.
}

\usage{duran.test(formula, data, alpha = 0.05, na.rm = TRUE, verbose = TRUE) }

%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{formula}{a formula of the form \code{lhs ~ rhs} where \code{lhs} gives the sample values and \code{rhs} the corresponding groups.}
  \item{data}{a tibble or data frame containing the variables in \code{formula}.} 
  \item{alpha}{the level of significance to assess variance homogeneity. Default is set to alpha = 0.05.}
  \item{na.rm}{a logical value indicating whether NA values should be stripped before the computation proceeds.}
  \item{verbose}{a logical for printing output to R console.}

}

\details{
Prior to testing for variance homogeneity, the data within each group are centered by their corresponding group median to eliminate the confounding effects of location parameters. This median-centered variant of the test is 
also referred to as the Conover-Iman test in literature (Conover and Iman, 1978). 
}


\value{
A list with class "vht" containing the following components:
\item{statistic}{the corresponding test statistic.}
\item{parameter}{the parameter(s) of the approximate chi-squared distribution of the test statistic.}
\item{p.value}{the p-value of the test.}
\item{method}{the character string "Duran Test".}
\item{data}{a data frame containing the variables in which NA values (if exist) are removed.}
\item{formula}{a formula of the form \code{lhs ~ rhs} where \code{lhs} gives the sample values and \code{rhs} the corresponding groups.}

}



\references{

Conover, W.J., Iman, R.L.(1978). Some Exact Tables For The Squared Ranks Test. \emph{Communications in Statistics - Simulation and Computation}, \bold{B7(5)}, 491-513.


}



\author{
Gozde Cosar and Osman Dag
}




\examples{

library(vartest)

duran.test(Sepal.Length ~ Species, data = iris)

out <- duran.test(Sepal.Length ~ Species, data = iris)
summary(out)
}

\keyword{functions}

