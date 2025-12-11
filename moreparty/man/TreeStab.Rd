\name{TreeStab}
\alias{TreeStab}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Stability assessment of conditional inference trees
}
\description{
Assesses the stability of conditional inference trees through the partition of observations in the terminal nodes and the frequency of the variables used for splits.
}
\usage{
TreeStab(ct, B = 20)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{ct}{A tree of class \code{constparty} (as returned by \code{ctree} from \code{partykit} package).}
  \item{B}{Numerical value. The number of bootstrap replications. Default is 20.}
}

\value{
A list of two elements :
\item{partition}{average Jaccard index of concordance between the partition (terminal nodes) of ct and the partitions of bootstrap trees}
\item{variables}{a data frame with splitting variables in rows and two statistics in columns : their frequency of use in the tree vs in the bootstrap trees, and }
}

\details{
The study of splitting variables used in the original tree and in bootstrap trees in directly inspired from the approach implemented in \code{stablelearner} package.
The other side of this functions also uses bootstrap trees, this time to compute the Jaccard index of concordance between partitions, to assess the stability of the partition of observations in the terminal nodes of the tree.
}

\references{
Hothorn T, Hornik K, Van De Wiel MA, Zeileis A. "A lego system for conditional inference". \emph{The American Statistician}. 60:257–263, 2006.

Hothorn T, Hornik K, Zeileis A. "Unbiased Recursive Partitioning: A Conditional Inference Framework". \emph{Journal of Computational and Graphical Statistics}, 15(3):651-674, 2006.

Philipp M, Zeileis A, Strobl C (2016). "A Toolkit for Stability Assessment of Tree-Based Learners". In A. Colubi, A. Blanco, and C. Gatu (Eds.), \emph{Proceedings of COMPSTAT 2016 - 22nd International Conference on Computational Statistics} (pp. 315-325). The International Statistical Institute/International Association for Statistical Computing. Preprint available at https://EconPapers.RePEc.org/RePEc:inn:wpaper:2016-11
}

\author{
Nicolas Robette
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{ctree}
}
\examples{
  data(iris)
  iris2 = iris
  iris2$Species = factor(iris$Species == "versicolor")
  iris.ct = partykit::ctree(Species ~ ., data = iris2)
  TreeStab(iris.ct, B = 10)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{tree}% use one of  RShowDoc("KEYWORDS")
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
