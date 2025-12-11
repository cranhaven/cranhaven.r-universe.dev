\name{FeatureSelection}
\alias{FeatureSelection}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Feature selection for conditional random forests.
}
\description{
Performs feature selection for a conditional random forest model. Four approaches are available : non-recursive feature elimination (NRFE), recursive feature elimination (RFE), permutation test approach with permuted response (Altmann et al, 2010), permutation test approach with permuted predictors (Hapfelmeier et Ulm, 2013).}

\usage{
FeatureSelection(Y, X, method = 'NRFE', ntree = 1000, measure = NULL,
                 nperm = 30, alpha = 0.05, distrib = 'approx',
                 parallel = FALSE, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Y}{response vector. Must be of class \code{factor} or \code{numeric}}

  \item{X}{matrix or data frame containing the predictors}

  \item{method}{method for feature selection. Should be 'NRFE' (non-recursive feature elimination, default), 'RFE' (recursive feature elimination), 'ALT' (permutation of response) or 'HAPF' (permutation of predictors)}

  \item{ntree}{number of trees contained in a forest}

  \item{measure}{the name of the measure of the \code{measures} package that should be used for error and variable importance calculations.}
  
  \item{nperm}{number of permutations. Only for 'ALT' and 'HAPF' methods.}
  
  \item{alpha}{alpha level for permutation tests. Only for 'ALT' and 'HAPF' methods.}
  
  \item{distrib}{the null distribution of the variable importance can be approximated by its asymptotic distribution (\code{"asympt"}) or via Monte Carlo resampling (\code{"approx"}, default). Only for 'ALT' and 'HAPF' methods.}
  
  \item{parallel}{Logical indicating whether or not to run \code{fastvarImp} in parallel using a backend provided by the \code{foreach} package. Default is \code{FALSE}.}
  
  \item{\dots}{Further arguments (like positive or negative class) that are needed by the measure.}
}

\details{
To be developed soon !
%%  ~~ If necessary, more details than the description above ~~
}
\value{
A list with the following elements :
\item{selection.0se}{selected variables with the 0 standard error rule}
\item{forest.0se}{forest corresponding the variables selected with the 0 standard error rule}
\item{oob.error.0se}{OOB error of the forest with 0 standard error rule}
\item{selection.1se}{selected variables with the 1 standard error rule}
\item{forest.1se}{forest corresponding the variables selected with the 1 standard error rule}
\item{oob.error.1se}{OOB error of the forest with 1 standard error rule}
}
\references{
B. Gregorutti, B. Michel, and P. Saint Pierre. "Correlation and variable importance in random forests". arXiv:1310.5726, 2017.

A. Hapfelmeier and K. Ulm. "A new variable selection approach using random forests". \emph{Computational Statistics and Data Analysis}, 60:50–69, 2013.

A. Altmann, L. Toloşi, O. Sander et T. Lengauer. "Permutation importance: a corrected feature importance measure". \emph{Bioinformatics}, 26(10):1340-1347, 2010.

}
\author{
Nicolas Robette
}
\note{
The code is adapted from Hapfelmeier & Ulm (2013).

Only works for regression and binary classification.
}


\examples{
  data(iris)
  iris2 = iris
  iris2$Species = factor(iris$Species == "versicolor")
  featsel <- FeatureSelection(iris2$Species, iris2[,1:4], measure='ACC', ntree=200)
  featsel$selection.0se
  featsel$selection.1se
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
