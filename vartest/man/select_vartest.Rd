\name{select_vartest}
\alias{select_vartest}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Data-Driven Selection of the Optimal Variance Homogeneity Test
}
\description{
\code{select_vartest} performs an automated, simulation-based evaluation to identify the optimal variance homogeneity test for a dataset.
}

\usage{select_vartest(formula, data, nrep = 1000, alpha = 0.05, na.rm = TRUE, verbose = TRUE) }

%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{formula}{a formula of the form \code{lhs ~ rhs} where \code{lhs} gives the sample values and \code{rhs} the corresponding groups.}
  \item{data}{a tibble or data frame containing the variables in \code{formula}.} 
  \item{nrep}{an integer specifying the number of Monte Carlo replications to perform for the simulation. The default is \code{1000}.}
  \item{alpha}{the level of significance to assess variance homogeneity. Default is set to alpha = 0.05.}
  \item{na.rm}{a logical value indicating whether NA values should be stripped before the computation proceeds.}
  \item{verbose}{a logical for printing output to R console.}

}

\details{
The function calculates the empirical moments of the input data and uses the Pearson distribution system to generate random datasets. By evaluating 28 distinct methodological modifications of variance homogeneity tests on these generated datasets, the function recommends the method that yields the highest adjusted power (with the lowest Type I error).
}


\value{
A data frame presenting empirical power, Type I error rate, and adjusted power for each variance homogeneity test.
}



%\references{

%}



\author{
Muhammed Ali Yilmaz and Osman Dag 
}




\examples{

library(vartest)
\donttest{
set.seed(1234)
select_vartest(Sepal.Length ~ Species, data = iris, nrep = 1000)
}
}

\keyword{functions}

