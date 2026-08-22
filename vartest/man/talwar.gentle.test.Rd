\name{talwar.gentle.test}
\alias{talwar.gentle.test}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Talwar and Gentle Test
}
\description{
\code{talwar.gentle.test} performs Talwar and Gentle variance homogeneity test.
}

\usage{talwar.gentle.test(formula, data, alpha = 0.05, na.rm = TRUE, verbose = TRUE) }

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
\item{method}{the character string "Talwar and Gentle Test".}
\item{data}{a data frame containing the variables in which NA values (if exist) are removed.}
\item{formula}{a formula of the form \code{lhs ~ rhs} where \code{lhs} gives the sample values and \code{rhs} the corresponding groups.}

}



\references{

Conover, W.J., Johnson, M.E., Johnson, M.M. (1981). A Comparative Study of Tests for Homogeneity of Variances, with Applications to the Outer Continental Shelf Bidding Data. \emph{Technometrics}, \bold{23:4}, 351-361.

Talwar, P.P., Gentle, J.E. (1976). A Robust Test for the Homogeneity of Scales. \emph{Communications in Statistics - Theory and Methods}, \bold{6:4}, 363-369.

}



\author{
Gozde Cosar and Osman Dag
}




\examples{

library(vartest)

talwar.gentle.test(Sepal.Length ~ Species, data = iris)

out <- talwar.gentle.test(Sepal.Length ~ Species, data = iris, verbose = FALSE)
summary(out)
}

\keyword{functions}
