\name{empty.plot}
\alias{empty.plot}
\title{
Creates an empty scatter plot}
\description{
Creates an empty scatter plot that is equal to the function call:

\code{plot(1,lwd=0,axes=F,xlab="",ylab="",...)}
}
\usage{
empty.plot(..., xlab = "", ylab = "", new=T, add=!new, n=1, axes = F)
}
\arguments{

 \item{...}{
 other arguments of the generic x-y plotting fucntion \link[graphics]{plot}.
}

 \item{xlab, ylab}{
label for the x- and y-axis of the plot (default is empty).
}

 \item{new, add}{
whether to show add plot to a current plot device or to start a new figure (default is: new=\code{TRUE} and add=\code{FALSE}).
}

 \item{n}{
 number of figures to be plotted (default is 1)
}

 \item{axes}{
whether to show plot axes (default is \code{FALSE}).
}


}

\author{
Robert K. Bauer
}


\examples{
empty.plot()
title("empty plot")
box()
axis(1)
axis(2)
}