\name{f.influence.total}
\alias{f.influence.total}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Field of Influence (Total)
}
\description{Calculates the total field of influence for the input-output system using \code{\link{f.influence}}
}
\usage{
f.influence.total(io)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{io}{An \code{InputOutput} class object from \code{\link{as.inputoutput}}}
}
\details{
The total field of influence calculates the sum of all first order field of influences:
\deqn{F^{total} = \sum_i \sum_j F_{i,j}}
where
\deqn{F_{i, j} = L_{.j} L_{i.}}
such that \eqn{L_{.j}} is the jth column of the Leontief inverse and \eqn{L_{i.}} is the ith row of the Leontief inverse.
}

\value{
Returns a matrix of the total field of influence.
}
\author{
John J. P. Wade
}
\note{
If the input-output system is large, then the computation can become cumbersome. Consequently, a progress bar will be printed if the algorithm determines it to be relevant.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{\code{\link{f.influence}}}

\examples{
data(toy.IO)
class(toy.IO)

fit = f.influence.total(toy.IO)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
