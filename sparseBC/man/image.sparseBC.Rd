\name{image.sparseBC}
\alias{image.sparseBC}
\alias{image.matrixBC}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Image plot of an object of class \code{sparseBC} or \code{matrixBC}
}
\description{
This function plots a \code{sparseBC} or \code{matrixBC} object --- the estimated bicluster mean matrix for the observations and features
}
\usage{
\method{image}{sparseBC}(x, labelx = TRUE, labely = TRUE, arrangex = FALSE, arrangey = FALSE, \dots) 
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
an object of class \code{sparseBC} or \code{matrixBC}
}

  \item{labelx}{
label the x-axis with the features number.  The default is labelx=TRUE
}
  \item{labely}{
label the y-axis with the observations number.  The default is labely=TRUE
}

  \item{arrangex}{
Rearrange the features/columns such that they are grouped into their respective clusters for the image plot.  The default is arrangex=FALSE
}
  \item{arrangey}{
Rearrange the observations/rows such that they are grouped into their respective clusters for the image plot.  The default is arrangey=FALSE.
}
  \item{\dots}{
additional parameters to be passed to \code{\link{image}}
}
}
\details{
This function plots the estimated mean matrix from  \code{\link{sparseBC}} or \code{\link{matrixBC}}.  The columns are the features and the rows are  the observations.
}
\references{
	#
}
\author{
Kean Ming Tan and Daniela Witten
}

\seealso{
\code{\link{sparseBC}}
\code{\link{matrixBC}}
\code{\link{summary.sparseBC}}
\code{\link{sparseBC.BIC}}
\code{\link{sparseBC.choosekr}}
}
\examples{
# See example in sparseBC
}

