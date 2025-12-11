\name{BivariateAssoc}
\alias{BivariateAssoc}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Bivariate association measures for supervised learning tasks.
}
\description{
Computes bivariate association measures between a response and predictor variables (and, optionnaly, between every pairs of predictor variables.)
}
\usage{
BivariateAssoc(Y, X, xx = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Y}{the response variable}

  \item{X}{the predictor variables}

  \item{xx}{whether the association measures should be computed for couples of predictor variables (default) or not. With a lot of predictors, consider setting xx to FALSE (for reasons of computation time).}
}

\details{
For each pair of variable, a permutation test is computed, following the framework used in conditional inference trees to choose a splitting variable. This test produces a p-value, transformed as -log(1-p) for reasons of comparison stability. The function also computes a "standard" association measure : kenddal's tau correlation for pairs of numeric variables, Cramer's V for pairs of factors and eta-squared for pairs numeric-factor.}

\value{
A list of the following items :
\item{YX}{: a table with the association measures between the response and predictor variables}
\item{XX}{: a table with the association measures between every couples of predictor variables}
In each table :
\item{measure}{: name of the "standard" association measure}
\item{assoc}{: value of the "standard" association measure}
\item{p.value}{: p-value from the permutation test}
\item{criterion}{: p-value from the permutation test transformed as -log(1-p), which serves to sort rows}
}

\references{
Hothorn T, Hornik K, Van De Wiel MA, Zeileis A. "A lego system for conditional inference". \emph{The American Statistician}. 60:257–263, 2006.

Hothorn T, Hornik K, Zeileis A. "Unbiased Recursive Partitioning: A Conditional Inference Framework". \emph{Journal of Computational and Graphical Statistics}, 15(3):651-674, 2006.
}

\author{
Nicolas Robette
}

\note{
see also https://stats.stackexchange.com/questions/171301/interpreting-ctree-partykit-output-in-r
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{ctree}
}

\examples{
  data(iris)
  iris2 = iris
  iris2$Species = factor(iris$Species == "versicolor")
  BivariateAssoc(iris2$Species,iris2[,1:4])
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
