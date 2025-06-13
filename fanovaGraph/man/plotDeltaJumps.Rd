\name{plotDeltaJumps}
\alias{plotDeltaJumps}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Delta Jump Plot
}
\description{Threshold discision plot. \code{plotDeltaJumps} plots the threshold steps (the values of delta at which the graph changes) equidistant against the number of cliques and the values of delta on the real axis. The indices are assumed to be scaled for the threshold cuts.
}
\usage{
plotDeltaJumps(graphlist, interval = c(0, 1), mean.clique.size = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{graphlist}{an object of class \code{graphlist} as obtained from \code{\link{estimateGraph}}
}
\item{interval}{an optional vector of size 2, range for the values of delta to be shown in the plot, defaults to c(0,1)
}
  \item{mean.clique.size}{logical, if TRUE (default) an additional line is drawn representing 
        the mean of the number of vertices in the cliques
}
}
\details{
The plots shall give help in the choice for the treshold. In the first plot a small number of cliques might be preferable in order to have less parameters to estimate. If several values result in the same number of cliques the ones with higher mean clique size are possibly preferable. 

In the second plot a high jump indicates a point of big distance between two successive edge indices and thus a clear change in the graph structure. The intervals with notable jumps are highlighted in green, the higher the jump the darker the colour. Those highlighted intervals together with a small number of cliques are probably good choices for the threshold.

Use \code{\link{plotGraphChange}} to visualize the graph structure for possible threshold values.
}

\author{
J. Fruth, O. Roustant
}

\seealso{
\code{\link{thresholdIdentification}}, \code{\link{plotGraphChange}}
}
\examples{
tii <- matrix(c(0.0018, 0.0265, 0.0017, 0.0277, 0.0018, 0.001, 0.028, 0.0013, 
  0.0212, 0.002, 0.0372, 0.0024, 0.0022, 0.0157, 0.003))
g <- list(d = 6, 
tii = tii,
i1 = matrix(c(0.0901, 0.1288, 0.0683, 0.0979, 0.0882, 0.1572)),
V = 0.8,
tii.scaled = tii/0.8,
cliques = list(1:6))

### Delta Jump Plot (jump between 0.0038 and 0.0196)
plotDeltaJumps(g)


}
