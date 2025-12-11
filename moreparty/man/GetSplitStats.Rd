\name{GetSplitStats}
\alias{GetSplitStats}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Permutation tests results for each split in a conditional tree.
}
\description{
This function displays the results of the variable selection process for each split of a conditional tree, i.e. the p-values from permutation tests of independence between every predictor and the dependent variable. This may help to assess the stability of the tree.
}
\usage{
GetSplitStats(ct)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{ct}{A tree of class \code{constparty} (as returned by \code{ctree} from \code{partykit} package).}
}

\value{
A list of two elements :
\item{details}{a list of data frames (one for each inner node), with one row per candidate variable, and test statistic and p-value of the permutation test of independence, criterion (equal to log(1-p)) and ratio (criterion/max(criterion) as columns. Variables are sorted by decreasing degree of association with the dependent variable.}
\item{summary}{a data frame with one row per inner node and 5 variables : the mode id, the splitting variable, the best candidate to split among the other variables, the ratio of the criterion of the splitting variable divided by the criterion of the best variable among the others.}
}

\details{
The ratio index represents the ratio between the association test result for the splitting variable and the association test result for another candidate variable for splitting. It is always greater than 1. The closer it is to 1, the tighter the competition for the splitting variable, and therefore the more potentially unstable the node concerned. Conversely, the higher the ratio, the more the splitting variable has dominated the competition, and the more stable the node is likely to be.
}

\references{
Hothorn T, Hornik K, Van De Wiel MA, Zeileis A. "A lego system for conditional inference". \emph{The American Statistician}. 60:257–263, 2006.

Hothorn T, Hornik K, Zeileis A. "Unbiased Recursive Partitioning: A Conditional Inference Framework". \emph{Journal of Computational and Graphical Statistics}, 15(3):651-674, 2006.}

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
  iris.ct = partykit::ctree(Species ~ ., data = iris2)
  GetSplitStats(iris.ct)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{tree}% use one of  RShowDoc("KEYWORDS")
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
