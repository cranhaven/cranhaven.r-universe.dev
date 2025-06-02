\docType{package}
\name{cta-package}
\alias{cta-package}
\title{
cta: Contingency Table Analysis Based on ML Fitting of MPH Models
}
\description{
Contingency table analysis is performed based on maximum likelihood (ML) fitting of multinomial-Poisson homogeneous (MPH) models (Lang, 2004) and homogeneous linear predictor (HLP) models (Lang, 2005). Objects computed include model goodness-of-fit statistics; likelihood-based (cell- and link-specific) residuals; and cell probability and expected count estimates along with standard errors. This package can also compute test-inversion--e.g. Wald, profile likelihood, score, power-divergence--confidence intervals for contingency table estimands, when table probabilities are potentially subject to equality constraints. See Lang (2008) and Zhu (2020) for test-inversion intervals.
}
\details{
Please call the following two \R functions in this \pkg{cta} package.

\code{\link{mph.fit}}: Computes maximum likelihood estimates and fit statistics for MPH and HLP models for contingency tables.

\code{\link{ci.table}}: Constructs test-inversion approximate confidence intervals for estimands in contingency tables with or without equality constraints.
}
\references{
Lang, J. B. (2004) Multinomial-Poisson homogeneous models for contingency tables, \emph{Annals of Statistics}, \bold{32}, 340--383.

Lang, J. B. (2005) Homogeneous linear predictor models for contingency tables, \emph{Journal of the American Statistical Association}, \bold{100}, 121--134.

Lang, J. B. (2008) Score and profile likelihood confidence intervals for contingency table parameters, \emph{Statistics in Medicine}, \bold{27}, 5975--5990.

Zhu, Q. (2020) "On improved confidence intervals for parameters of discrete distributions." PhD dissertation, University of Iowa.
}
\author{Joseph B. Lang, Qiansheng Zhu}
