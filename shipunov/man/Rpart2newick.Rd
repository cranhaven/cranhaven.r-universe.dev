\name{Rpart2newick}
\alias{Rpart2newick}
\title{Converts 'rpart' object into Newick tree}

\description{Converts 'rpart' object into Newick tree}

\usage{
Rpart2newick(rpart.object)
}

\arguments{
  \item{rpart.object}{'rpart' object, output of rpart::rpart()}
}

\value{
 Newick tree (text string).
}

\details{
Inspired by 'shaunpwilkinson/rpart2dendro.R' gist.
}

% \references{
% }

% \seealso{
% }

\examples{
library(rpart)
(fit <- rpart(Kyphosis ~ Age + Number + Start, data=kyphosis))
plot(fit); text(fit, all=TRUE, xpd=TRUE)
library(ape)
tree1 <- read.tree(text=Rpart2newick(fit))
plot(tree1)
nodelabels(tree1$node.label, frame="none", bg="transparent", adj=-0.1)

(fit2 <- rpart(Species ~ ., data=iris))
plot(fit2); text(fit2, all=TRUE, xpd=TRUE)
tree2 <- read.tree(text=Rpart2newick(fit2))
plot(tree2)
nodelabels(tree2$node.label, frame="none", bg="transparent", adj=-0.1)
}

\keyword{multivariate}
