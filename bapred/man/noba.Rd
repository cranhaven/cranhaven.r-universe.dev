\name{noba}
\alias{noba}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
No batch effect adjustment
}
\description{
This function is merely included for consistency. It returns the raw dataset not adjusted for batch effects.
}
\usage{
noba(x, batch)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
matrix. The covariate matrix. Observations in rows, variables in columns.
}
  \item{batch}{
factor. Batch variable. Each factor level (or 'category') corresponds to one of the batches. For example, if there are four batches, this variable would have four factor levels and observations with the same factor level would belong to the same batch.
}
}

\value{
\code{noba} returns an object of class \code{noba}.
An object of class "\code{noba}" is a list containing the following components: 
\item{xadj}{ matrix of (training) data }
\item{nbatches}{ number of batches }
\item{batch}{ batch variable }
}

\author{
Roman Hornung
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\examples{
data(autism)

Xadj <- noba(x=X, batch=batch)$adj

all(as.vector(Xadj)==as.vector(X))
}
