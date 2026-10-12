\name{hist.clinsig}
\alias{hist.clinsig}
\title{Plot a histogram of a clinsig list}
\description{
 Displays a histogram of the result of the \samp{clinsig} function.
}
\usage{
 \method{hist}{clinsig}(x,breaks=NA,main="",xlab="Score",ylab="Frequency",
  xlim=NA,ylim=NA,col=2:3,border=par("fg"),only.pairs=FALSE,...)
}
\arguments{
 \item{x}{a clinsig table produced by \link{clinsig}}
 \item{breaks}{The breaks to be used in categorizing scores.}
 \item{main}{The title for the plot.}
 \item{xlab,ylab}{The labels for the axes.}
 \item{xlim,ylim}{The limits for the plot.}
 \item{col}{The colors of the paired bars, pre- first, post- second.}
 \item{border}{The border color for the bars.}
 \item{only.pairs}{Whether to display counts only for clients with both pre and
  post scores.}
 \item{...}{additional arguments passed to \samp{plot}.}
}
\details{
 \samp{hist.clinsig} displays a histogram of the counts of scores in the categories
 defined by \samp{breaks} and other information in the object returned by
 \samp{clinsig}.

 The default is to display all pre and post scores. This may lead to plots that
 look wrong because the measures of central tendency were calculated for pairs
 rather than all scores. Setting \samp{only.pairs} will display only the pairs of
 scores.
}
\value{nil}
\author{Jim Lemon}
\seealso{\link{clinsig}}
\keyword{misc}
