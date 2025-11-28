\name{pairwise.Table2.test}
\alias{pairwise.Table2.test}
\title{Pairwise Chi-squared or Fisher test for 2-dimensional tables}

\description{Pairwise Chi-squared or Fisher test for 2-dimensional tables}

\usage{pairwise.Table2.test(tbl, names=rownames(tbl), p.adjust.method="BH", exact=FALSE, ...)}

\arguments{
\item{tbl}{Contingency table}
\item{names}{Level names}
\item{p.adjust.method}{See '?p.adjust'}
\item{exact}{Run exact test?}
\item{...}{Arguments to test function}
}

\details{

Pairwise Chi-squared or Fisher test for 2-dimensional tables.

Alternatives: NCStats::chisqPostHoc() and fifer::chisq.post.hoc(). Both
of them are not CRAN packages.

}

\value{List with test outputs.}

% \references{}

\author{Alexey Shipunov}

% \seealso{}

\examples{
titanic <- margin.table(Titanic, c(1, 4))
chisq.test(titanic)
pairwise.Table2.test(titanic)
}

\keyword{htest}
