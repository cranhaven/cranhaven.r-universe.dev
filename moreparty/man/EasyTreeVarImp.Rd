\name{EasyTreeVarImp}
\alias{EasyTreeVarImp}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Variable importance for conditional inference trees.
}
\description{
Variable importance for \code{partykit} conditional inference trees, using various performance measures.
}
\usage{
EasyTreeVarImp(ct, nsim = 1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{ct}{A tree of class \code{constparty} (as returned by \code{ctree} from \code{partykit} package).}
  \item{nsim}{Integer specifying the number of Monte Carlo replications to perform. Default is 1. If nsim > 1, the results from each replication are simply averaged together.}
}

\details{
If the response variable is a factor, AUC (if response is binary), accuracy, balanced accuracy and true predictions by class are used. If the response is numeric, r-squared and Kendall's tau are used.
}

\value{
A data frame of variable importances, with variables as rows and performance measures as columns.
}

\references{
Hothorn T, Hornik K, Van De Wiel MA, Zeileis A. "A lego system for conditional inference". \emph{The American Statistician}. 60:257–263, 2006.

Hothorn T, Hornik K, Zeileis A. "Unbiased Recursive Partitioning: A Conditional Inference Framework". \emph{Journal of Computational and Graphical Statistics}, 15(3):651-674, 2006.}

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
  EasyTreeVarImp(iris.ct, nsim = 1)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{tree}% use one of  RShowDoc("KEYWORDS")
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line