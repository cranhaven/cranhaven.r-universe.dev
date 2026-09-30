\name{qunormtrain}
\alias{qunormtrain}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Quantile normalization with ``documentation by value'' (Kostka & Spang, 2008)
}
\description{
Performs quantile normalization for a covariate matrix and returns the normalized dataset together with information necessary for addon quantile normalization (Kostka & Spang, 2008) using \code{\link{qunormaddon}}.
}
\usage{
qunormtrain(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
matrix. The covariate matrix. observations in rows, variables in columns.
}
}
\details{
This function uses code from the off-CRAN package \code{docval}, version 1.0.
}
\value{
\code{qunormtrain} returns an object of class \code{qunormtrain}.
An object of class "\code{qunormtrain}" is a list containing the following components: 
\item{xnorm}{ matrix of quantile normalized (training) data. Observations in rows, variables in columns. }
\item{mqnts}{ Vector of length ncol(xnorm). Averages of sorted values, with averages taken across observations. }
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

params <- qunormtrain(x=Xtrain)

Xtrainnorm <- params$xnorm
}
