\name{BootA}
\alias{BootA}
\title{Bootstrap clustering}
\description{How to bootstrap clustering with 'ape'}

\usage{BootA(dat, FUN=function(.x) ape::nj(dist(.x)), iter=1000, mc.cores=1, tresh=50,
 cons=TRUE, prop=0.5)}

\arguments{
\item{dat}{data}
\item{FUN}{how to bootstrap (see examples)}
\item{iter}{number of iterations, default 1000}
\item{mc.cores}{how many cores to employ (system-dependent)}
\item{tresh}{Threshold for printing bootstrap values}
\item{cons}{Calculate consensus tree?}
\item{prop}{0.5 is majority-rule consensus (default), 1 is strict consensus}
}

\details{
This is how to bootstrap clustering with 'ape::boot.phylo()'.
}

% \value{}

% \references{}

\author{Alexey Shipunov}

\seealso{
 \code{\link{Bclust}}, \code{\link{BootA}}, ape::\code{\link[ape]{boot.phylo}}
}

\examples{
dat <- iris[, -5]
row.names(dat) <- abbreviate(make.names(iris[, 5], unique=TRUE))
iris.BA1 <- BootA(dat, iter=100)
plot(iris.BA1$boot.tree, show.node.label=TRUE)
plot(iris.BA1$cons.tree)
iris.BA2 <- BootA(dat, FUN=function(.x) ape::as.phylo(hclust(dist(.x))), iter=100)
\dontrun{
## change (or remove) 'mc.cores=...' in accordance with your system features
iris.BA3 <- BootA(dat, FUN=function(.x) phangorn::NJ(dist(.x)), iter=100,
 mc.cores=4)
}
}

\keyword{cluster}
