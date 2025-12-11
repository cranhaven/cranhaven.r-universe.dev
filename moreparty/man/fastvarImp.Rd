\name{fastvarImp}
\alias{fastvarImp}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Variable importance for conditional inference random forests
}
\description{
Parallelized version of \code{\link[varImp]{varImp}} function from \code{varImp} package, which computes the variable importance for arbitrary measures from the \code{measures} package.
}
\usage{
fastvarImp(object, mincriterion = 0, conditional = FALSE,
           threshold = 0.2, nperm = 1, OOB = TRUE,
           pre1.0_0 = conditional, measure = "multiclass.Brier",
           parallel = TRUE, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
\item{object}{
An object as returned by \code{\link[party]{cforest}} (or \code{\link{fastcforest}}).
}
\item{mincriterion}{
The value of the test statistic or 1 - p-value that must be exceeded in order to include a split in the computation of the importance. The default mincriterion = 0 guarantees that all splits are included.
}
\item{conditional}{
a logical determining whether unconditional or conditional computation of the importance is performed.
}
\item{threshold}{
The threshold value for (1 - p-value) of the association between the variable of interest and a covariate, which must be exceeded inorder to include the covariate in the conditioning scheme for the variable of interest (only relevant if conditional = TRUE). A threshold value of zero includes all covariates.
}
\item{nperm}{
The number of permutations performed.
}
\item{OOB}{
A logical determining whether the importance is computed from the out-of-bag sample or the learning sample (not suggested).
}
\item{pre1.0_0}{
Prior to party version 1.0-0, the actual data values were permuted according to the original permutation importance suggested by Breiman (2001). Now the assignments to child nodes of splits in the variable of interest are permuted as described by Hapfelmeier et al. (2012), which allows for missing values in the explanatory variables and is more efficient wrt memory consumption and computing time. This method does not apply to conditional variable importances.
}
\item{measure}{
The name of the measure of the \code{measures} package that should be used for the variable importance calculation.
}
\item{parallel}{
Logical indicating whether or not to run \code{fastvarImp} in parallel using a backend provided by the \code{foreach} package. Default is \code{FALSE}.
}
\item{\dots}{
Further arguments (like positive or negative class) that are needed by the measure.
}
}

\details{
The code is adapted from \code{\link[varImp]{varImp}} function in \code{varImp} package.
}
\value{
Vector with computed permutation importance for each variable.
}

\author{
Nicolas Robette
}

\seealso{
\code{\link[varImp]{varImp}}, \code{\link{fastvarImpAUC}}, \code{\link[party]{cforest}}, \code{\link{fastcforest}}
}
\examples{
  data(iris)
  iris2 = iris
  iris2$Species = factor(iris$Species == "versicolor")
  iris.cf = party::cforest(Species ~ ., data = iris2,
            control = party::cforest_unbiased(mtry = 2, ntree = 50))
  fastvarImp(object = iris.cf, measure='ACC', parallel=FALSE)          
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
