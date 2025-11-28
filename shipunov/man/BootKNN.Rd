\name{BootKNN}
\alias{BootKNN}
\title{Bootstrap with kNN}
\description{How to bootstrap with kNN (and DNN)}

\usage{BootKNN(data, classes, sub="none", nsam=4, nboot=1000, misclass=TRUE, method="knn", ...)}

\arguments{
\item{data}{Data frame to classify}
\item{classes}{Character vector of class names}
\item{sub}{Subsample to use (see example)}
\item{nsam}{Number of training items from each level of grouping factor, default 4}
\item{nboot}{Number of iterations}
\item{misclass}{Calculate misclassification table?}
\item{method}{Either "knn" (class::knn()) or "dnn" (shipunov::Dnn())}
\item{...}{Further arguments to method functions}
}

\details{
This function samples equal numbers ('nsam') of training items from
\emph{each level} of grouping factor.

It also allows to use \emph{sub}set of data which will be used for
sub-sampling of training data.
}

\value{Returns all predictions as character matrix, each boot is a column}

% \references{}

\author{Alexey Shipunov}

\seealso{\code{class::\link[class]{knn}}, \code{\link{Dnn}}}

\examples{
iris.sub <- 1:nrow(iris) \%in\% seq(1, nrow(iris), 5)
iris.bootknn <- BootKNN(iris[, -5], iris[, 5], sub=iris.sub)
## calculate and plot stability
st <- apply(iris.bootknn, 1, function(.x) var(as.numeric(as.factor(.x))))
plot(prcomp(iris[, -5])$x, col=iris$Species, pch=ifelse(st == 0, 19, 1))
## boot Dnn
BootKNN(iris[, -5], iris[, 5], nboot=50, method="dnn",
 k=1, FUN=function(.x) Gower.dist(.x))
}

\keyword{multivariate}
