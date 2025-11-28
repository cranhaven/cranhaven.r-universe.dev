\name{Cor}
\alias{Cor}
\alias{Cor2}
\title{Correlation matrix with p-values}
\description{Correlation matrix with p-values}

\usage{
Cor(X, stars=TRUE, dec=4, p.level=0.05, ...)
Cor2(X, dec=4, p.level=0.05)
}

\arguments{
\item{X}{Matrix or data frame with values}
\item{stars}{Replaces p-values with stars if it not greater than 'p.level'}
\item{dec}{Decimal point}
\item{p.level}{P-level}
\item{...}{Arguments to 'cor.test()'}
}

\details{
'Cor()' calculates correlation matrix with p-values.

'Cor2()' is another (faster) variant of correlation matrix with p-values based on F-statistic.
Shows significances in the upper triagle. Uses Pearson correlation only but much faster than 'Cor()'.
}

% \value{}

% \references{}

\author{Alexey Shipunov}

% \seealso{}

\examples{
Cor(longley, dec=2)
Cor2(longley, dec=2)
}

\keyword{correlation}
