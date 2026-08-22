\name{bartletts.test}
\alias{bartletts.test}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Bartlett's Test
}
\description{
\code{bartletts.test} performs Bartlett variance homogeneity test.
}

\usage{bartletts.test(formula, data, alpha = 0.05, na.rm = TRUE, verbose = TRUE)}

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
\item{parameter}{the parameter(s) of the approximate chi-squared distribution of the test statistic.}
\item{p.value}{the p-value of the test.}
\item{method}{the character string "Bartlett's Test".}
\item{data}{a data frame containing the variables in which NA values (if exist) are removed.}
\item{formula}{a formula of the form \code{lhs ~ rhs} where \code{lhs} gives the sample values and \code{rhs} the corresponding groups.}

}



\references{

Lee, H.B., Katz, G.S., Restori, A.F. (2010). A Monte Carlo Study of Seven Homogeneity of Variance Tests. \emph{Journal of Mathematics and Statistics}, \bold{6:3}, 359-366.

Conover, W.J., Johnson, M.E., Johnson, M.M. (1981). A Comparative Study of Tests for Homogeneity of Variances, with Applications to the Outer Continental Shelf Bidding Data. \emph{Technometrics}, \bold{23:4}, 351-361.

}



\author{
Gozde Cosar and Osman Dag
}

\seealso{
\code{\link[stats]{bartlett.test}}
}



\examples{

library(vartest)

bartletts.test(Sepal.Length ~ Species, data = iris)

out <- bartletts.test(Sepal.Length ~ Species, data = iris, verbose = FALSE)
summary(out)
}

\keyword{functions}
