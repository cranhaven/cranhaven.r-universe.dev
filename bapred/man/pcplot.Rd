\name{pcplot}
\alias{pcplot}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Visualization of batch effects using Principal Component Analysis
}
\description{
This function performs principal component analysis on the covariate matrix and plots the first two principal components against each other. Different batches are distinguished by different colors and (optionally) the two classes of the target variable by different plot symbols.
}
\usage{
pcplot(x, batch, y, alpha = 0.35, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
matrix. The covariate matrix. Observations in rows, variables in columns.
}
  \item{batch}{
factor. Batch variable. Each factor level (or 'category') corresponds to one of the batches. For example, if there are four batches, this variable would have four factor levels and observations with the same factor level would belong to the same batch.
}
  \item{y}{
optional factor. Binary target variable. Has to have two factor levels, where each of them correponds to one of the two classes of the target variable.
}
  \item{alpha}{
optional numeric between 0 and 1. Alpha transparency of the contour lines of the batch-specific two-dimensional density estimates. Only applicable when default color scheme (\code{rainbow}) is used.
}
  \item{\dots}{
additional arguments to be passed to \code{plot}.
}
}
\details{
For the data corresponding to each batch a two-dimensional kernel density estimate is obtained using the function \code{kde2d()} from the \pkg{MASS}-package. These estimates are depicted through contour lines (using \code{contour}). 
}
\value{
\code{NULL}
}

\author{
Roman Hornung
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\examples{
data(autism)

par(mfrow=c(1,3))
pcplot(x=X, batch=batch, y=y, alpha=0.25, main="alpha = 0.25")
pcplot(x=X, batch=batch, y=y, alpha=0.75, main="alpha = 0.75")
pcplot(x=X, batch=batch, y=y, col=1:length(unique(batch)), 
  main="col = 1:length(unique(batch))")
par(mfrow=c(1,1))
}
