\name{GetCtree}
\alias{GetCtree}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Gets a tree from a conditional random forest
}
\description{
This function gets the ith tree from a conditional random forest as produced by \code{cforest}.
}
\usage{
GetCtree(object, k = 1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{An object as returned by \code{cforest} (or \code{fastcforest}).}
  
  \item{k}{The index of the tree to get from the forest. Default is 1.}
}

\value{
A tree of class \code{BinaryTree}, as returned by \code{ctree} from \code{party} package.
}

\note{
Code taken from https://stackoverflow.com/questions/19924402/cforest-prints-empty-tree
}

\examples{
  data(iris)
  iris2 = iris
  iris2$Species = factor(iris$Species == "versicolor")
  iris.cf = party::cforest(Species ~ ., data = iris2,
            control = party::cforest_unbiased(mtry = 2, ntree = 50))
  plot(GetCtree(iris.cf))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{tree}% use one of  RShowDoc("KEYWORDS")
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
