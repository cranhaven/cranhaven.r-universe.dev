\name{BootRF}
\alias{BootRF}
\title{Bootstrap with 'randomForest()'}
\description{How to bootstrap with 'randomForest()'}

\usage{BootRF(data, classes, sub="none", nsam=4, nboot=1000, misclass=TRUE, ...)}

\arguments{
\item{data}{Data frame to classify}
\item{classes}{Character vector of class names}
\item{sub}{Subsample to use (see example)}
\item{nsam}{Number of training items from each level of grouping factor, default 4}
\item{nboot}{Number of iterations}
\item{misclass}{Calculate misclassification table?}
\item{...}{Further options to randomForest()}
}

\details{
Note that as randomForest::randomForest() is based on sampling, BootRF()
is the kind of second-level bootstrap.

BootRF() is very simple and does not interact with Random Forest
algorithms. It is stratified, i.e. samples equal numbers ('nsam') of
training items from the \emph{each level} of grouping factor.

Also, it allows to use the \emph{sub}set of data which will be in turn
used for sub-sampling of training data.

}

\value{Returns all predictions as character matrix, each boot is a column}

% \references{}

\author{Alexey Shipunov}

\seealso{\code{randomForest::\link[randomForest]{randomForest}}}

\examples{
iris.sub <- 1:nrow(iris) \%in\% seq(1, nrow(iris), 5)
\donttest{
## could be slow
iris.bootrf <- BootRF(iris[, -5], iris[, 5], sub=iris.sub)
iris.bootrf <- BootRF(iris[, -5], iris[, 5]) # naturally, lower
## calculate and plot stability
st <- apply(iris.bootrf, 1, function(.x) var(as.numeric(as.factor(.x))))
plot(prcomp(iris[, -5])$x, col=iris$Species, pch=ifelse(st == 0, 19, 1))
}
}

\keyword{multivariate}
