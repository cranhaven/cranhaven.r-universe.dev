\name{InDisc-package}
\alias{InDisc-package}
\title{
A unified approach for obtaining and estimating unidimensional Item Response Theory (IRT) Dual Models (DMs)
}
\description{
Package consisting on a main function (\code{\link{InDisc}}) that calls all the sub-functions that implement the procedures described in Ferrando (2019) for fitting binary, graded, and continuous response DMs. Estimation is based on a two stage (calibration and scoring) random-regressors approach (McDonald, 1982). Item calibration at the first stage is the same as in the corresponding standard IRT models, is based on a factor-analytic Underlying-Variables approach, and uses an unweighted least squares, (ULS) minimum-residual criterion as implemented in the \code{\link{psych}} R package (Revelle, 2018). Individual trait scores and individual discriminations are obtained at the second stage using Expected a Posteriori (EAP) Bayes estimation. Overall, the combined ULS-EAP estimation procedure is simple, robust, and can handle large datasets, both in terms of sample size and test length.
}
\value{
\item{\code{\link{InDisc}}}{Performs the procedure proposed in Ferrando (2019).}
}
\references{
Ferrando, P. J. (2019). A Comprehensive IRT Approach for Modeling Binary, Graded, and Continuous Responses With Error in Persons and Items. Applied Psychological Measurement, 43(5), 339-359. \doi{10.1177/0146621618817779}

McDonald, R. P. (1982). Linear versus models in item response theory. Applied Psychological Measurement, 6, 379-396. \doi{10.1177/014662168200600402}

Revelle, W. (2018) psych: P Procedures for Personality and Psychological Research, Northwestern University, Evanston, Illinois, USA, \url{https://CRAN.R-project.org/package=psych} Version = 1.8.12.

}
\author{
Pere Joan Ferrando

David Navarro-Gonzalez
}
\examples{
## For speeding purposes, the number of observations and quadrature nodes have been
## reduced. For a proper use of InDisc, use the full dataset and the default quadrature
## nodes, and consider using the graded model.

InDisc(CTAC35[1:40,], nquad = 10, model = "linear", approp = FALSE, display = TRUE)

}
\keyword{ package }
