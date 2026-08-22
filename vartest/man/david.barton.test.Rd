\name{david.barton.test}
\alias{david.barton.test}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
David Barton Test
}
\description{
\code{david.barton.test} performs David Barton variance homogeneity test.
}

\usage{david.barton.test(formula, data, alpha = 0.05, na.rm = TRUE, verbose = TRUE) }

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
\item{method}{the character string "David Barton Test".}
\item{data}{a data frame containing the variables in which NA values (if exist) are removed.}
\item{formula}{a formula of the form \code{lhs ~ rhs} where \code{lhs} gives the sample values and \code{rhs} the corresponding groups.}

}



\references{

Barton, D.E., David, F.N. (1958). A Test For Birth Order Effect. \emph{Annals of Human Genetics}, \bold{22:3}, 250-257.

Conover, W.J., Johnson, M.E., Johnson, M.M. (1981). A Comparative Study of Tests for Homogeneity of Variances, with Applications to the Outer Continental Shelf Bidding Data. \emph{Technometrics}, \bold{23:4}, 351-361.

Gibbons, J.D., Chakraborti, S. (2010). Nonparametric Statistical Inference. \emph{CRC Press}, \bold{5th Ed.}, 316-320.

}



\author{
Gozde Cosar and Osman Dag
}




\examples{

library(vartest)

david.barton.test(Sepal.Length ~ Species, data = iris)

out <- david.barton.test(Sepal.Length ~ Species, data = iris)
summary(out)
}

\keyword{functions}
