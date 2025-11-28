\name{Mag}
\alias{Mag}
\title{Interpreter for effect sizes}

\description{Interprets R^2-related effect sizes}

\usage{Mag(x, squared=TRUE)}

\arguments{
\item{x}{Value}
\item{squared}{Is value squared?}
}

\details{Interpreter for R^2-related effect sizes (see example).}

% \value{}

% \references{}

\author{Alexey Shipunov}

% \seealso{}

\examples{
aa <- apply(cor(trees), 1:2, function(.x) Mag(.x, squared=FALSE))
aa[upper.tri(aa, diag=TRUE)] <- "-"
noquote(aa)
}

\keyword{univar}
