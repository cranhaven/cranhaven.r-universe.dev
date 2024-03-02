\name{plot.rocrisca}
\alias{plot.rocrisca}

\title{
Plot Method for 'rocrisca' Objects
}
\description{
A plot of ROC curves is produced.
}
\usage{
\method{plot}{rocrisca}(x, ..., information=TRUE)
}
\arguments{
  \item{x}{An object of class \code{rocrisca}, returned by the functions \code{roc.binary}, \code{roc.net}, \code{roc.summary}, and \code{roc.time}.}
  \item{...}{Additional arguments affecting the plot.}
  \item{information}{A logical value indicating whether the non-information line is plotted. The default values is TRUE.}
}

\value{No return value for this S3 method.}

\examples{
data(dataDIVAT3)

# A subgroup analysis to reduce the time needed for this example

dataDIVAT3 <- dataDIVAT3[1:400,]

# The time-dependent ROC curve to evaluate the
# capacities of the recipient age for the prognosis of post-kidney
# transplant mortality up to 2000 days.

# Compute the raw sensitivity and specificity
roc1 <- roc(times="death.time", failures="death", variable="ageR",
confounders=~1, data=dataDIVAT3, pro.time=2000,
precision=seq(0.1,0.9, by=0.2))

plot(roc1, type="b", col=1, pch=2, lty=2, xlab="1-specificity", ylab="sensibility")
}

\seealso{
\code{\link{plot.default}}
}
\keyword{Plotting}
