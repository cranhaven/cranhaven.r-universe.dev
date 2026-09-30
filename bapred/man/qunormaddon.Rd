\name{qunormaddon}
\alias{qunormaddon}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Addon quantile normalization using ``documentation by value'' (Kostka & Spang, 2008)
}
\description{
Performs addon quantile normalization by using documentation by value (Kostka & Spang, 2008).
}
\usage{
qunormaddon(params, x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{params}{
object of class \code{qunormtrain}. Contains parameters necessary for addon quantile normalization.
}
  \item{x}{
matrix. The covariate matrix of the new data. Observations in rows, variables in columns.
}
}
\details{
This function uses code from the off-CRAN package \code{docval}, version 1.0.
}
\value{
The covariate matrix of the test data after addon quantile normalization. Observations in rows, variables in columns.
}
\references{
Kostka, D., Spang, R. (2008). Microarray based diagnosis profits from better documentation of gene expression signatures. PLoS Computational Biology 4(2):e22, <\doi{10.1371/journal.pcbi.0040022}>.
}
\author{
Roman Hornung
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\examples{
data(autism)
Xtrain <- X[batch==1,]
Xtest <- X[batch==2,]

params <- qunormtrain(x=Xtrain)

Xtrainnorm <- params$xnorm

Xtestaddonnorm <- qunormaddon(params, x=Xtest)
}
