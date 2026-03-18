\name{Snoring}
\alias{Snoring}
\docType{data}
\title{ Data on the incidence of hypertension and three indicator variables. }
\description{
  A total of 433 persons were tested for hypertension and checked for
  whether they were smokers, obese, or snored. The data are in Altman(1991).
}
\usage{data(Snoring)}
\format{
  A data frame with 8 observations on the following 5 variables:
  \describe{
    \item{\code{smoking}}{did the person smoke (1) or not (0)?}
    \item{\code{obese}}{was the person obese (1) or not (0)?}
    \item{\code{snoring}}{did the person snore (1) or not (0)?}
    \item{\code{n}}{the number of persons observed with these covariates}
    \item{\code{hypert}}{did the person suffer from hypertension (1) or not (0)?}
  }
}
\source{
  Altman, D.G. 1991. Practical Statistics for Medical Research. Chapman \&
  Hall, London.
  }
\references{
  van Hulst, R. 2018. Evaluating Scientific Evidence. ms.
}
\examples{
data(Snoring)
fit <- glm(cbind(hypert, n - hypert) ~ smoking + obese + snoring,
  family=binomial, data=Snoring)
summary(fit)
}
\keyword{datasets}