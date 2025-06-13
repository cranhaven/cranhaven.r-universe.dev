\name{threshold}
\alias{threshold}

\title{Threshold indices
}
\description{All indices below a treshold are set to be zero.
}
\usage{
threshold(graphlist, delta, scaled = TRUE, robust = FALSE)
}

\arguments{
  \item{graphlist}{an object of class \code{graphlist} as obtained from \code{\link{estimateGraph}}
}
  \item{delta}{numeric threshold, between 0 and 1 if \code{scaled = TRUE}
}
  \item{scaled}{optional boolean, if TRUE, indices are normalized by the overall variance before for threshold cut, defaults to TRUE
}
  \item{robust}{optional boolean, if TRUE, upper confidence intervals limits are used for the threshold cut instead of indices themselves, confidence intervals must be provided in \code{graphlist}, defaults to FALSE
}
}
\value{
 an object of class \code{graphlist} where the indices are thresholded the clique structure is updated respectively, see \code{\link{estimateGraph}} for a detailed description
}

\section{Warning}{
The threshold cut is by default performed on scaled indices. For a cut on the original unscaled indices set \code{scaled = FALSE}.
}


\references{
Muehlenstaedt, T.; Roustant, O.; Carraro, L.; Kuhnt, S. (2011) Data-driven Kriging models based on FANOVA-decomposition, \emph{Statistics and Computing}.
}
\author{
J. Fruth, T. Muehlenstaedt, O. Roustant
}
\examples{
# Kriging model prediction
x <- matrix(runif(100*3,-pi,pi),100,3)
KM <- km(~1, design = data.frame(x), response = ishigami.fun(x))
krigingMean <- function(Xnew) predict(object = KM, newdata = Xnew, 
    type = "UK", se.compute = FALSE, checkNames = FALSE)$mean
    
# full graph estimation
g <- estimateGraph(krigingMean, d=3, n.tot=10000, q.arg=list(min=-pi, max=pi))
print(g[c(2,6)])
# threshold graph
g.cut <- threshold(g, delta = 0.1)
print(g.cut[c(2,6)])
}
