\name{r.fgt.sub}
\alias{r.fgt.sub}
\title{Convex measure of affluence in subpopulation
}
\description{Computes the measure of affluence in subpopulation analogous to the convex version of Foster, Greer and Thorbecke (1984) family of poverty indices.
}

\usage{
r.fgt.sub(x.sub, x, weight.sub, weight, k, alpha)
}

\arguments{
\item{x}{income vector of subpopulation}
\item{x.sub}{income vector of population}
\item{weight.sub}{weight vector of subpopulation}
\item{weight}{weight vector of population}
\item{k}{multiple of the median income}
\item{alpha}{parameter of the index: \code{alpha} > 1}
}

\value{
\item{r}{values of the sum in the index formula}
\item{r.fgt}{the value of index}
}

\references{
1. Foster J.E., Greer J., Thorbecke E. (1984) A class of decomposable poverty measures. \emph{Econometrica}, 52, pp. 761-766. \cr
2. Peichl A., Schaefer T., Scheicher C. (2008) Measuring richness and poverty - A micro data application to Europe and Germany. IZA Discussion Paper No. 3790, Institute for the Study of Labor (IZA).
}
\author{Alicja Wolny-Dominiak, Anna Saczewska-Piotrowska}

\seealso{\code{\link{r.fgt}}
}

\examples{
data(affluence)
affluence$weight <- rep(1, nrow(affluence))
aff.sub <- subset(affluence, education == 2)
r.fgt.sub(aff.sub$income, affluence$income, aff.sub$weight, affluence$weight, 2, 1)
}
\keyword{ind}
