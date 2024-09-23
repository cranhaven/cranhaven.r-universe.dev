\name{hist.CAMAN.object}
\alias{hist.CAMAN.object}

\title{Histograms for finite mixture models}

\description{
Graphical display of the data and the mixture. Intended for larger data sets.
}

\usage{
\method{hist}{CAMAN.object}(x, nbreaks=NULL, mixdens=TRUE, mixdens.col="red", 
                return.mixdens=FALSE, data.plot=NULL, singleDistr=TRUE, 
                main="", xlab="", plotlegend=TRUE, ...)
}
\arguments{
\item{x}{A \code{CAMAN.object}.}
\item{nbreaks}{Number of breaks in histogram. Defaults to \code{NULL}. In this case it is set to \code{min(60,max(30,x@num.obs))}. Passed on to \code{\link{hist}}.}
\item{mixdens}{logical; plot mixture density?}
\item{mixdens.col}{color of mixture density.}
\item{return.mixdens}{logical; return the mixture density?}
\item{data.plot}{data to plot. Defaults to \code{NULL}. In this case it is taken from the data of the mixture density.}
\item{singleDistr}{logical; should the distributions of the single components be plotted?}
\item{main}{character; heading of the plot.}
\item{xlab}{character; label of x-axis.}
\item{plotlegend}{logical; should a legend be plotted.}
\item{...}{arguments to be passed on to \code{\link{hist}}.}
}



\value{
    The function plots a histogram with the mixture density and its components overlaid. If \code{return.mixdens} is set to \code{TRUE}, the density at the breaks will also be returned.
}

\references{
Schlattmann, P. (2009). \emph{Medical Applications of Finite Mixture Models}. Berlin: Springer.
}


\author{
Peter Schlattmann and Johannes Hoehne   
}

\keyword{hplot}
\concept{histogram}