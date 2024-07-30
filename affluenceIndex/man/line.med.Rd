\name{line.med}
\alias{line.med}

\title{
Medeiros's affluence line}

\usage{
line.med(x, weight, k)
}

\description{Computes the Medeiros's affluence line.
}


\arguments{
\item{x}{the income vector}
\item{weight}{vector of weights}
\item{k}{poverty line as a multiple of the median income}
}

\details{
The Medeiros's affluence line is based on the concept of poverty gap related to a given poverty line (in the package this line is set as a defined by the user 
multiple of the median income). Based on the determined poverty gap, there is calculated the point where the income of the richest should be reduced in order 
to make possible enough transfers to cover this gap and eliminate poverty. The calculated point of income may be also presented as the multiple of the median income.}


\value{
\item{median_inc}{the median income}
\item{Gp}{the poverty gap}
\item{rho_medeiros}{Medeiros's affluence line}
\item{median_multiple}{Medeiros's affluence line as a multiple of the median}

}
\references{
Medeiros M. (2006) The rich and the poor: The construction of an affluence line from the poverty line. \emph{Social Indicators Research}, 78(1), pp. 1-18.
}
\author{
Anna Saczewska-Piotrowska, Alicja Wolny-Dominiak}

\examples{
data(affluence)
line.med(affluence$income, affluence$hs_size, 0.6)
}
\keyword{line}