\name{fastcforest}
\alias{fastcforest}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Parallelized conditional inference random forest
}
\description{
Parallelized version of \code{\link[party]{cforest}} function from \code{party} package, which is an implementation of the random forest and bagging ensemble algorithms utilizing conditional inference trees as base learners.
}
\usage{
fastcforest(formula, data = list(), subset = NULL, weights = NULL,
            controls = party::cforest_unbiased(),
            xtrafo = ptrafo, ytrafo = ptrafo, scores = NULL,
            parallel = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{formula}{
a symbolic description of the model to be fit. Note that symbols like \code{:} and \code{-} will not work and the tree will make use of all variables listed on the rhs of \code{formula}
}
  \item{data}{
a data frame containing the variables in the model
}
  \item{subset}{
an optional vector specifying a subset of observations to be used in the fitting process
}
  \item{weights}{
an optional vector of weights to be used in the fitting process. Non-negative integer valued weights are allowed as well as non-negative real weights. Observations are sampled (with or without replacement) according to probabilities \code{weights / sum(weights)}. The fraction of observations to be sampled (without replacement) is computed based on the sum of the weights if all weights are integer-valued and based on the number of weights greater zero else. Alternatively, \code{weights} can be a double matrix defining case weights for all \code{ncol(weights)} trees in the forest directly. This requires more storage but gives the user more control.
}
  \item{controls}{
an object of class \code{\link[party]{ForestControl-class}}, which can be obtained using \code{\link[party]{cforest_control}} (and its convenience interfaces \code{cforest_unbiased} and \code{cforest_classical}).
}
  \item{xtrafo}{
a function to be applied to all input variables. By default, the \code{\link[party]{ptrafo}} function is applied.
}
  \item{ytrafo}{
a function to be applied to all response variables. By default, the \code{\link[party]{ptrafo}} function is applied.
}
  \item{scores}{
an optional named list of scores to be attached to ordered factors
}
  \item{parallel}{
Logical indicating whether or not to run \code{fastcforest} in parallel using a backend provided by the \code{foreach} package. Default is \code{TRUE}.
}
}

\details{
See \code{\link[party]{cforest}} documentation for details.
The code for parallelization is inspired by \url{https://stackoverflow.com/questions/36272816/train-a-cforest-in-parallel}
}
\value{
 An object of class \code{\link[party]{RandomForest-class}}.
}
\references{
    Leo Breiman (2001). Random Forests. \emph{Machine Learning}, 45(1), 5--32.

    Torsten Hothorn, Berthold Lausen, Axel Benner and Martin Radespiel-Troeger
    (2004). Bagging Survival Trees. \emph{Statistics in Medicine}, \bold{23}(1), 77--91.

    Torsten Hothorn, Peter Buhlmann, Sandrine Dudoit, Annette Molinaro
    and Mark J. van der Laan (2006a). Survival Ensembles. \emph{Biostatistics},
    \bold{7}(3), 355--373.

    Torsten Hothorn, Kurt Hornik and Achim Zeileis (2006b). Unbiased
    Recursive Partitioning: A Conditional Inference Framework.
    \emph{Journal of Computational and Graphical Statistics}, \bold{15}(3),
    651--674.  Preprint available from
    \url{https://www.zeileis.org/papers/Hothorn+Hornik+Zeileis-2006.pdf}

    Carolin Strobl, Anne-Laure Boulesteix, Achim Zeileis and Torsten Hothorn (2007).
    Bias in Random Forest Variable Importance Measures: Illustrations, Sources and
    a Solution. \emph{BMC Bioinformatics}, \bold{8}, 25.
    \url{https://bmcbioinformatics.biomedcentral.com/articles/10.1186/1471-2105-8-25}

    Carolin Strobl, James Malley and Gerhard Tutz (2009).
    An Introduction to Recursive Partitioning: Rationale, Application, and Characteristics of
    Classification and Regression Trees, Bagging, and Random forests.
\emph{Psychological Methods}, \bold{14}(4), 323--348.
}

\author{
Nicolas Robette
}

\seealso{
\code{\link[party]{cforest}}, \code{\link{fastvarImp}}
}

\examples{
  ## classification
  data(iris)
  iris2 = iris
  iris2$Species = factor(iris$Species=="versicolor")
  iris.cf = fastcforest(Species~., data=iris2, parallel=FALSE)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{tree}% use one of  RShowDoc("KEYWORDS")
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
