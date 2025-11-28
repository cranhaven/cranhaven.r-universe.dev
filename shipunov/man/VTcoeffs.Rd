\name{VTcoeffs}
\alias{VTcoeffs}
\title{Effect sizes of association between categorical variables}

\description{Effect sizes of association between categorical variables}

\usage{VTcoeffs(table, correct=FALSE, ...)}

\arguments{
\item{table}{Contingency table}
\item{correct}{Perform continuity correction in underlying chi-square test?}
\item{...}{Additional arguments to underlying chisq.test()}
}

\details{
Association between categorical variables.

Calculates Cramer's V and Tschuprow's, original and corrected (Bergsma, 2013)

Alternative: vcd::assocstats()

Includes magnitude interpretation for original Cramer's V (for df < 6).
}

\value{Data frame with coefficients, values and tables.}

% \references{}

\author{Alexey Shipunov}

% \seealso{}

\examples{
x <- margin.table(Titanic, 1:2)
VTcoeffs(x)
VTcoeffs(x)[2, ] # most practical
}

\keyword{univar}
