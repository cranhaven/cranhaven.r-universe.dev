\name{Phyllotaxis}
\alias{Phyllotaxis}
\alias{Fibonacci}

\title{Plant phyllotaxis}
\description{Outputs the plant phyllotaxis formula or angle of divergence}

\usage{
Phyllotaxis(n, angle=FALSE)
Fibonacci(x)
}

\arguments{
\item{n}{non-negative integer}
\item{angle}{if TRUE, output angle of divergence}
\item{x}{non-negative integer}
}

\details{
'Fibonacci(x)' calculates the n's Fibonacci's number, it is the rare case that is not exercise but really used for work.

'Phyllotaxis(n)' uses 'Fibonacci(x)' to output the phyllotaxis formula (see examples) or (if 'angle=TRUE') the angle of divergence.
}

\value{Number or character vector of length one.}

% \references{}

\author{Alexey Shipunov}

% \seealso{}

\examples{
sapply(1:10, Fibonacci)
sapply(1:10, Phyllotaxis)
sapply(1:10, Phyllotaxis, angle=TRUE)
}

\keyword{math}
