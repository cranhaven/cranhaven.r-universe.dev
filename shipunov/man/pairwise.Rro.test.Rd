\name{pairwise.Rro.test}
\alias{pairwise.Rro.test}
\title{Robust rank order test post hoc derivative}

\description{Robust rank order test post hoc derivative}

\usage{pairwise.Rro.test(x, g, p.adjust.method="BH")}

\arguments{
\item{x}{Values}
\item{g}{Groups}
\item{p.adjust.method}{See '?p.adjust'}
}

\details{'pairwise.Rro.test()' is the Robust rank order test post hoc derivative.}

\value{List with test outputs}

% \references{}

\author{Alexey Shipunov}

\seealso{\code{\link{Rro.test}}}

\examples{
pairwise.Rro.test(airquality$Ozone, airquality$Month)
}

\keyword{htest}
