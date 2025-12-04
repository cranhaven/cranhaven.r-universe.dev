\name{WAIC}
\alias{WAIC}
\title{
Extract the Watanabe-Akaike Information Criterion (WAIC)
}
\description{
Extracts the Watanabe-Akaike Information Criterion from objects of class \code{Bwiqid} which have a WAIC attribute.
}
\usage{
WAIC(object, ...)
}

\arguments{
  \item{object}{
a fitted model object with an attribute giving the WAIC and pD for the fitted model.
}
  \item{\dots}{
optionally more fitted model objects.
}
}
\value{
If just one object is provided, the corresponding WAIC.

If multiple objects are provided, a data frame with rows corresponding to the objects and columns representing the number of parameters in the model (pD) and the WAIC.
}
\references{
Burnham, K P; D R Anderson 2002. \emph{Model selection and multimodel inference: a practical information-theoretic approach}. Springer-Verlag. 
}
\author{
Mike Meredith. 
}
\note{
The code to produce WAIC is new and not fully tested: it probably harbours bugs!
}

\examples{
## TODO
}