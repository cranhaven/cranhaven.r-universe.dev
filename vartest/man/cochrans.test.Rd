\name{cochrans.test}
\alias{cochrans.test}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Cochran's C Test
}
\description{
\code{cochrans.test} performs Cochran's C variance homogeneity test.
}

\usage{cochrans.test(formula, data, alpha = 0.05, na.rm = TRUE, verbose = TRUE)}

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
\item{method}{the character string "Cochran's C Test".}
\item{data}{a data frame containing the variables in which NA values (if exist) are removed.}
\item{formula}{a formula of the form \code{lhs ~ rhs} where \code{lhs} gives the sample values and \code{rhs} the corresponding groups.}

}



\references{

Wang, Y., Gil, P.R., Chen, Y.H., Kromrey, J.D., Kim, E.S., Pham, T., Nguyen, D., Romano, J.L. (2017). Comparing the Performance of Approaches for Testing the Homogeneity of Variance Assumption in One-Factor Anova Models. \emph{Educational and Psychological Measurement}, \bold{77:2}, 305-329.

Cochran, W.G. (1941). The Distribution of The Largest of a set of Estimated Variances as a Fraction of Their Total. \emph{Annals of Eugenics}, \bold{11:1}, 47-52.

}



\author{
Gozde Cosar and Osman Dag
}

\seealso{
\code{\link[outliers]{cochran.test}}
}



\examples{

library(vartest)

cochrans.test(Sepal.Length ~ Species, data = iris)

out <- cochrans.test(Sepal.Length ~ Species, data = iris, verbose = FALSE)
summary(out)
}

\keyword{functions}
