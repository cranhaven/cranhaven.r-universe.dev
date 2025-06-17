\name{create.N.matrix}
\alias{create.N.matrix}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Function to create spp x spp matrix of potential co-occurring sites (N) from a potential spp x site matrix.
}
\description{
Returns a spp x spp matrix of potential co-occurring sites from spp x site matrix of possible species occupancy.
}
\usage{
create.N.matrix(mat)
}
\arguments{
  \item{mat}{
spp x site matrix where 1 = potential occupancy and 0 = species does not occur.}
}
\value{
Returns a spp x spp matrix where the upper triangle contains N for each species pair.
}

\author{
Daniel M Griffith}


\seealso{
\code{\link{pair}}, \code{\link{pair.attributes}}
}
\examples{
#data(finches)
#N_matrix <- matrix(data = rbinom(n = nrow(finches)*ncol(finches),1,prob = 0.75),
#                        nrow = nrow(finches),
#                        ncol = ncol(finches)
#                        ,byrow = T)
#create.N.matrix(N_matrix)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{co-occurrence}
