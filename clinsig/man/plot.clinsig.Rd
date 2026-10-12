\name{plot.clinsig}
\alias{plot.clinsig}
\title{Plot a clinsig list}
\description{
 Displays a scatterplot of the result of the \samp{clinsig} function.
}
\usage{
 \method{plot}{clinsig}(x,main="Clinical significance plot",
  xlab="Pre-intervention score",ylab="Post-intervention score",
  xlim=NA,ylim=NA,pch=par("pch"),point.id=NA,...)
}
\arguments{
 \item{x}{a clinsig table produced by \link{clinsig}}
 \item{main}{The title for the plot.}
 \item{xlab,ylab}{The labels for the axes.}
 \item{xlim,ylim}{The limits for the plot.}
 \item{pch}{The symbols to be used in plotting the score pairs.}
 \item{point.id}{Optional text to be used instead of symbols for the points.}
 \item{...}{additional arguments passed to \samp{print}.}
}
\details{
 \samp{plot.clinsig} displays a scatterplot and other information in the object
  returned by \samp{clinsig}.
}
\value{nil}
\author{Jim Lemon}
\seealso{\link{clinsig}}
\keyword{misc}
