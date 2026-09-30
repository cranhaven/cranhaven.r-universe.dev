\name{standardize}
\alias{standardize}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Batch effect adjustment by standardization
}
\description{
Performs batch effect adjustment by standardizing the variables within batches to have zero mean and variance one.
}
\usage{
standardize(x, batch)
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
\code{standardize} returns an object of class \code{standardize}.
An object of class "\code{standardize}" is a list containing the following components: 
\item{xadj}{ matrix of adjusted (training) data }
\item{nbatches}{ number of batches }
\item{batch}{ batch variable }
}

\author{
Roman Hornung
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\examples{
data(autism)

params <- standardize(x=X, batch=batch)
}
