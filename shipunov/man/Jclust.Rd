\name{Jclust}
\alias{Jclust}
\alias{plot.Jclust}
\alias{print.Jclust}
\title{Simple bootstrap and jackknife clustering}

\description{Simple bootstrap and jackknife clustering}

\usage{

Jclust(data, n.cl, iter=1000, method.d="euclidean", method.c="ward.D",
 bootstrap=TRUE, monitor=TRUE)

\method{print}{Jclust}(x, ...)

\method{plot}{Jclust}(x, main="", xlab=NULL, rect.lty=3, rect.col=1,
 rect.xpd=TRUE, top=FALSE, lab.pos=3, lab.offset=0.5, lab.col=par("col"),
 lab.font=par("font"), ...)

}

\arguments{

\item{data}{Data}

\item{n.cl}{Number of desired clusters}

\item{iter}{Number of iterations, default 1000}

\item{method.d}{Distance method}

\item{method.c}{Hierarchical clustering method}

\item{bootstrap}{Bootstrap or jackknife?}

\item{monitor}{If TRUE (default), prints a dot for each replicate}

\item{x}{Object of the class 'Jclust'}

\item{main}{Plot title}

\item{xlab}{Horizontal axis label}

\item{rect.lty}{Line type for the rectangles}

\item{rect.col}{Color of rectangles}

\item{rect.xpd}{Plot rectangle sides if they go outside the plotting
region?}

\item{top}{Plot values on top?}

\item{lab.pos}{Position specifier for the values text labels}

\item{lab.offset}{Distance of the text labels in fractions of a character width}

\item{lab.col}{Color of the text labels}

\item{lab.font}{Font of the text labels}

\item{...}{Additional arguments to the print() or plot.hclust()}

}

\details{

Simple method to bootstrap and jackknife cluster memberships, and plot
consensus membership tree. Requires the desired number of clusters.

The default clustering method is the variance-minimizing "ward.D" (which
works better with Euclidean distances); to make it coherent with hclust()
default, specify 'method.c="complete"'.

Note that Jclust() is fast indirect bootstrap, it boostrap the consensus
(not the original) tree and narrows results with the desired number of
clusters. Please consider also Bclust() which is the direct method, and
phylogeny-based BootA().

}

\value{

Returns 'Jclust' object which is a list with components "meth" (bootstrap
or jacknife), "mat" (matrix of results, consensus matrix), "hclust"
(consensus tree as 'hclust' object), "gr" (groups), "supp" (support
values), "iter" (number of iterations) and "n.cl" (number of cluters
used.)

}

% \references{}

\author{Alexey Shipunov}

\seealso{

\code{\link{Bclust}},

\code{\link{BootA}},

\code{\link{Fence}}

}

\examples{

## 'moldino' data, 1000 iterations
(mo.j <- Jclust(t(moldino), n.cl=3, iter=1000))
plot(mo.j)

## adjust locations of value labels
data.jb <- Jclust(t(atmospheres), method.c="complete", n.cl=3)
plot(data.jb, top=TRUE, lab.pos=1, lab.offset=1, lab.col=2, lab.font=2)

## plot together with Fence()
iris.jb <- Jclust(iris[, -5], n.cl=3)
plot(iris.jb, labels=FALSE)
Fence(iris.jb$hclust, iris$Species)
legend("topright", legend=levels(iris$Species), col=1:3, lwd=2.5, bty="n")

\donttest{
## This is how one can bootstrap _all_ reliable cluster numbers:
for (i in 2:(nrow(t(moldino)) - 1)) print(Jclust(t(moldino), i, iter=1000, boot=TRUE))
}
}

\keyword{cluster}
