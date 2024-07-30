\name{r.cha.sub}
\alias{r.cha.sub}
\title{Concave measure of affluence in subpopulation
}

\description{Computes the measure of affluence in subpopulation analogous to the poverty index of Chakravarty(1983).
}

\usage{
r.cha.sub(x.sub, x, weight.sub, weight, k, beta)
}

\arguments{
\item{x}{income vector of subpopulation}
\item{x.sub}{income vector of population}
\item{weight.sub}{weight vector of subpopulation}
\item{weight}{weight vector of population}
\item{k}{multiple of the median income}
\item{beta}{parameter of the index: \code{beta} > 0}
}

\value{
\item{r}{elements of the sum in the index formula}
\item{r.cha}{the value of index}
}

\references{
1. Chakravarty S.R. (1983) A new index of poverty. \emph{Mathematical Social Sciences}, 6, pp. 307-313. \cr
2. Peichl A., Schaefer T., Scheicher C. (2008) Measuring richness and poverty - A micro data application to Europe and Germany. IZA Discussion Paper No. 3790, Institute for the Study of Labor (IZA).
}

\author{Alicja Wolny-Dominiak, Anna Saczewska-Piotrowska}

\seealso{\code{\link{r.cha}}
}

\examples{
data(affluence)
r.cha(affluence$income, weight = NULL, 2, 2)
}
\keyword{ind}