\name{levene.test}
\alias{levene.test}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Levene's Test
}
\description{
\code{levene.test} performs Levene variance homogeneity test.
}

\usage{levene.test(formula, data, center = "mean", deviation = "absolute", 
  trim.rate = 0.25, alpha = 0.05, na.rm = TRUE, verbose = TRUE)}

%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{formula}{a formula of the form \code{lhs ~ rhs} where \code{lhs} gives the sample values and \code{rhs} the corresponding groups.}
  \item{data}{a tibble or data frame containing the variables in \code{formula}.} 
  \item{center}{a character string to define how to center. "mean": mean, "median": median, and "trim.mean": trimmed mean.} 
  \item{deviation}{a character string to define how to specify the deviation. "absolute": absolute deviation and "squared": squared deviation.} 
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
\item{method}{the character string "Levene's Test".}
\item{data}{a data frame containing the variables in which NA values (if exist) are removed.}
\item{formula}{a formula of the form \code{lhs ~ rhs} where \code{lhs} gives the sample values and \code{rhs} the corresponding groups.}

}



\references{

Wang, Y., Gil, P.R., Chen, Y.H., Kromrey, J.D., Kim, E.S., Pham, T., Nguyen, D., Romano, J.L. (2017). Comparing the Performance of Approaches for Testing the Homogeneity of Variance Assumption in One-Factor Anova Models. \emph{Educational and Psychological Measurement}, \bold{77:2}, 305-329.

Brown, M.B., Forsythe, A.B. (1974). Robust Tests for the Equality of Variances. \emph{Journal of the American Statistical Association}, \bold{69:346}, 364-367.

}



\author{
Gozde Cosar and Osman Dag
}

\seealso{
\code{\link[car]{leveneTest}}
}



\examples{

library(vartest)

levene.test(Sepal.Length ~ Species, data = iris, center="median", deviation="absolute")
levene.test(Sepal.Length ~ Species, data = iris, center="median", deviation="squared")
levene.test(Sepal.Length ~ Species, data = iris, center="mean", deviation="absolute")
levene.test(Sepal.Length ~ Species, data = iris, center="mean", deviation="squared")
levene.test(Sepal.Length ~ Species, data = iris, center="trim.mean", deviation="absolute")
levene.test(Sepal.Length ~ Species, data = iris, center="trim.mean", deviation="squared")

out <- levene.test(Sepal.Length~Species, data=iris, center="median", deviation="absolute")
summary(out)

}

\keyword{functions}
