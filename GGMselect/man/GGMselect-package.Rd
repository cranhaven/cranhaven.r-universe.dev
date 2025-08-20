\name{GGMselect-package}
\alias{GGMselect-package}
\alias{GGMselect}
\docType{package}
\title{Gaussian Graphs Models selection}
\description{GGMselect is a package dedicated to graph estimation in Gaussian
Graphical Models. The main functions return the adjacency matrix of an
undirected graph estimated from a data matrix.

This package is developed in the Applied Mathematics and Informatics
(\url{https://maiage.inrae.fr/}) Lab of INRA - Jouy-en-Josas, France.

To cite GGMselect, please use \code{citation("GGMselect")}.}
\details{\tabular{ll}{
Package: \tab GGMselect\cr
URL: \tab \url{https://CRAN.R-project.org/package=GGMselect}\cr
}}
\author{Annie Bouvier, Christophe Giraud, Sylvie Huet, Verzelen Nicolas

  Maintainer: Benjamin Auder <benjamin.auder@universite-paris-saclay.fr>
  
  More details are available on \url{../doc/Notice.pdf}\cr
}
\references{Please use \code{citation("GGMselect")}.}
\seealso{ \code{\link{selectFast}}, \code{\link{selectQE}},
 \code{\link{selectMyFam}},\code{\link{convertGraph}},
  \code{\link{simulateGraph}}, \code{\link{penalty}}}
\examples{
p=30
n=30
# simulate graph
eta=0.11
Gr <- simulateGraph(p,eta)
# simulate data
X <- rmvnorm(n, mean=rep(0,p), sigma=Gr$C)
# estimate graph
\dontrun{GRest <- selectFast(X)}

# plot result
\dontrun{library(network)}
\dontrun{par(mfrow=c(1,2))}
\dontrun{gV <- network(Gr$G)}
\dontrun{plot(gV,jitter=TRUE, usearrows = FALSE, label=1:p,displaylabels=TRUE)}
\dontrun{g <- network(GRest$EW$G)}
\dontrun{plot(g, jitter=TRUE, usearrows = FALSE,  label=1:p,displaylabels=TRUE)}
}
\keyword{ package }
\keyword{ multivariate }
\keyword{ graphs }
\keyword{ models }
