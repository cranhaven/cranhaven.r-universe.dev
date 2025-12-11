\name{fastvarImpAUC}
\alias{fastvarImpAUC}
\title{Variable importance (with AUC performance measure) for conditional inference random forests}

\usage{
  fastvarImpAUC(object, mincriterion = 0, conditional = FALSE,
            threshold = 0.2, nperm = 1, OOB = TRUE,
            pre1.0_0 = conditional,
            parallel = TRUE)
}
\arguments{
  \item{object}{An object as returned by \code{cforest} (or \code{fastcforest}).}

  \item{mincriterion}{The value of the test statistic or 1 - p-value that must be exceeded in order to include a
    split in the computation of the importance. The default mincriterion = 0 guarantees that all splits are included.}

  \item{conditional}{The value of the test statistic or 1 - p-value that must be exceeded in order to include a split
    in the computation of the importance. The default mincriterion = 0 guarantees that all splits are included.}

  \item{threshold}{The threshold value for (1 - p-value) of the association between the variable of interest and a
    covariate, which must be exceeded inorder to include the covariate in the conditioning scheme for the variable of
    interest (only relevant if conditional = TRUE). A threshold value of zero includes all covariates.}

  \item{nperm}{The number of permutations performed.}

  \item{OOB}{A logical determining whether the importance is computed from the out-of-bag sample or the learning
    sample (not suggested).}

  \item{pre1.0_0}{Prior to party version 1.0-0, the actual data values were permuted according to the original
    permutation importance suggested by Breiman (2001). Now the assignments to child nodes of splits in the variable
    of interest are permuted as described by Hapfelmeier et al. (2012), which allows for missing values in the
    explanatory variables and is more efficient wrt memory consumption and computing time. This method does not
    apply to conditional variable importances.}

  \item{parallel}{Logical indicating whether or not to run \code{fastvarImpAUC} in parallel using a backend provided by the \code{foreach} package. Default is \code{FALSE}.}
}

\value{
  Vector with computed permutation importance for each variable.
}
\description{
  Computes the variable importance regarding the AUC. Bindings are not taken into account in the AUC definition as they did
  not provide as good results as the version without bindings in the paper of Janitza \emph{et al.} (2013).
}
\details{
  For using the original AUC definition and multiclass AUC you can use the \code{fastvarImp} function and specify the particular measure.
  The code is adapted from \code{varImpAUC} function in \code{varImp} package.
}
\references{
Janitza, S., Strobl, C. & Boulesteix, A.-L. An AUC-based permutation variable
importance measure for random forests. \emph{BMC Bioinform.} 14, 119 (2013).
}
\author{
Nicolas Robette
}
\seealso{
\code{varImpAUC}, \code{fastvarImp}, \code{cforest}, \code{fastcforest}
}
\examples{
  data(iris)
  iris2 = iris
  iris2$Species = factor(iris$Species == "versicolor")
  iris.cf = party::cforest(Species ~ ., data = iris2,
            control = party::cforest_unbiased(mtry = 2, ntree = 50))
  fastvarImpAUC(object = iris.cf, parallel = FALSE)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
