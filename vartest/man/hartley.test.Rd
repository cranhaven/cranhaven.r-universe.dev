\name{hartley.test}
\alias{hartley.test}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Hartley's Maximum F-Ratio Test
}
\description{
\code{hartley.test} performs Hartley's Maximum F-Ratio variance homogeneity test.
}

\usage{hartley.test(formula, data, size = "mean", alpha = 0.05, na.rm = TRUE, verbose = TRUE)}

%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{formula}{a formula of the form \code{lhs ~ rhs} where \code{lhs} gives the sample values and \code{rhs} the corresponding groups.}
  \item{data}{a tibble or data frame containing the variables in \code{formula}.} 
  \item{size}{a character string to define how to number of group observation. "mean": mean, "harmonic": harmonic mean, "maxn": maximum n, and "minvar": minimum variance.} 
  \item{alpha}{the level of significance to assess variance homogeneity. Default is set to alpha = 0.05.}
  \item{na.rm}{a logical value indicating whether NA values should be stripped before the computation proceeds.}
  \item{verbose}{a logical for printing output to R console.}

}



\value{
A list with class "vht" containing the following components::
\item{statistic}{the corresponding test statistic.}
\item{parameter}{the parameter(s) of the maximum F-ratio distribution of the test statistic.}
\item{p.value}{the p-value of the test.}
\item{method}{the character string "Hartley's Maximum F-Ratio Test".}
\item{data}{a data frame containing the variables in which NA values (if exist) are removed.}
\item{formula}{a formula of the form \code{lhs ~ rhs} where \code{lhs} gives the sample values and \code{rhs} the corresponding groups.}

}



\references{

Gorbunova, A.A., Lemeshko, B.Y. (2012). Application of Parametric Homogeneity of Variances Tests under Violation of Classical Assumption. \emph{2nd Stochastic Modeling Techniques and Data Analysis International Conference}, \bold{5:8}, 253-260.

Bhandary, M., Dai, H. (2008). An Alternative Test for the Equality of Variances for Several Populations When the Underlying Distributions are Normal. \emph{Communications in Statistics-Simulation and Computation}, \bold{38:1}, 109-117.


}



\author{
Gozde Cosar and Osman Dag
}

\seealso{
\code{\link[PMCMRplus]{hartleyTest}}
}



\examples{

library(vartest)

hartley.test(Sepal.Length ~ Species, data = iris, size = "mean")

out <- hartley.test(Sepal.Length ~ Species, data = iris, size = "mean")
summary(out)
}

\keyword{functions}
