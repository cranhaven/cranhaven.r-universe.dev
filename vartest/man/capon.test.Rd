\name{capon.test}
\alias{capon.test}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Capon Test
}
\description{
\code{capon.test} performs Capon variance homogeneity test.
}

\usage{capon.test(formula, data, alpha = 0.05, na.rm = TRUE, verbose = TRUE)}

%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{formula}{a formula of the form \code{lhs ~ rhs} where \code{lhs} gives the sample values and \code{rhs} the corresponding groups.}
  \item{data}{a tibble or data frame containing the variables in \code{formula}.} 
  \item{alpha}{the level of significance to assess variance homogeneity. Default is set to alpha = 0.05.}
  \item{na.rm}{a logical value indicating whether NA values should be stripped before the computation proceeds.}
  \item{verbose}{a logical for printing output to R console.}

}

\details{
Prior to testing for variance homogeneity, the data within each group are centered by their corresponding group median to eliminate the confounding effects of location parameters (Conover et al., 1981).
}




\value{
A list with class "vht" containing the following components:
\item{statistic}{the corresponding test statistic.}
\item{parameter}{the parameter(s) of the approximate chi-squared distribution of the test statistic.}
\item{p.value}{the p-value of the test.}
\item{method}{the character string "Capon Test".}
\item{data}{a data frame containing the variables in which NA values (if exist) are removed.}
\item{formula}{a formula of the form \code{lhs ~ rhs} where \code{lhs} gives the sample values and \code{rhs} the corresponding groups.}

}



\references{

Capon, J. (1961). Asymptotic Efficiency of Certain Locally Most Powerful Rank Tests. \emph{The Annals of Mathematical Statistics}, \bold{32:1}, 88-100.

Conover, W.J., Johnson, M.E., Johnson, M.M. (1981). A Comparative Study of Tests for Homogeneity of Variances, with Applications to the Outer Continental Shelf Bidding Data. \emph{Technometrics}, \bold{23:4}, 351-361.

}



\author{
Gozde Cosar and Osman Dag
}




\examples{

library(vartest)

capon.test(Sepal.Length ~ Species, data = iris)

out <- capon.test(Sepal.Length ~ Species, data = iris)
summary(out)
}

\keyword{functions}
