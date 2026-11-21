\name{cimehr_example}
\alias{cimehr_example}
\docType{data}
\title{Simulated Participant and Biomarker Visit Records}
\description{
Simulated data generated from the CIMEHR shared-latent-variable model.
They illustrate binary and continuous baseline covariates, unrecorded
biomarker values at visits, and participants with no visits.
The records do not represent real people or clinical measurements.
}
\usage{data(cimehr_example)}
\format{
A list with three components:
\describe{
  \item{participants}{A data frame with 1,000 rows and columns \code{id}
    (integer participant identifier), \code{Z} (a binary baseline covariate),
    \code{X} (a continuous baseline covariate), and \code{C} (end of
    follow-up, 60 months for every participant).}
  \item{visits}{A data frame with one row per actual visit. Columns are
    \code{id}, \code{time} (months since baseline), \code{Y} (the
    biomarker value), and \code{R} (the recording indicator).
    The recording indicator is 1 when a biomarker value is observed and
    0 otherwise. Unrecorded values are \code{NA}. Participants with no
    visits have no rows in this component.}
  \item{truth}{The generating parameters and seed. It contains no
    participant-specific latent variables or unobserved biomarker values.}
}
}
\details{
The data use seed 200000013. Baseline covariates are independent
\eqn{Z_i\sim\mathrm{Bernoulli}(0.5)} and \eqn{X_i\sim N(0,1)}.
For independent \eqn{U_i\sim N(0,1)}, visiting frailty is
\deqn{\eta_i = \exp(0.6U_i-0.6^2/2).}
Conditional on the covariates and frailty, visits follow a Poisson process
with rate \eqn{\exp(-1.6+0.4Z_i+0.2X_i)\eta_i} over 60 months.
Both observation and outcome random-effect designs are \eqn{(1,Z_i)}.

The observation coefficients satisfy
\eqn{q_i=(0.2,0.8)^\top U_i+e_{qi}}, with independent normal residual
coefficients, each with variance 0.2. The probit observation predictor is
\deqn{0.2-0.4Z_i+q_{0i}+q_{Zi}Z_i.}
The coefficient of \eqn{X_i} in the observation model is zero.
The outcome coefficients satisfy
\eqn{b_i=(0.5,0.2)^\top U_i+e_{bi}}, where the normal residual
coefficients have covariance
\deqn{\operatorname{Var}(e_{bi})=
\left(\begin{array}{rr}0.75&-0.10\\-0.10&1.96\end{array}\right).}
Thus the marginal variances of \eqn{b_{0i}} and \eqn{b_{Zi}} are 1 and 2,
and their marginal covariance is zero. The outcome is
\deqn{Y_{ij}=-2+0.1t_{ij}-0.5Z_i+0.5X_i+b_{0i}+b_{Zi}Z_i+\varepsilon_{ij},}
where measurement errors are independent standard normal draws.
The residual observation and outcome vectors are independent of each
other, \eqn{U_i}, and the baseline covariates. Measurement errors are
independent of these quantities. Conditional on \eqn{U_i}, the covariates,
and \eqn{q_i}, recording indicators are independent Bernoulli draws with
the stated probit probabilities and are independent of the outcome
residuals and measurement errors. The simulated biomarker is a continuous
variable in arbitrary units on the modeled scale; no log transformation
is required.

Retain every participant when merging the two components. A participant
without visits then contributes a placeholder row with missing
\code{time}, \code{Y}, and \code{R}. These rows preserve the risk-set
information used in visiting-process estimation. A missing biomarker
value at a real visit instead has a finite visit time and \code{R = 0}.

The reproducible generator is installed at
\code{system.file("examples", "simulate_example.R", package = "CIMEHR")}.
It uses only base R and the \pkg{stats} package.
}
\examples{
data(cimehr_example)
participants <- cimehr_example$participants
visits <- cimehr_example$visits
analysis_data <- merge(participants, visits, by = "id", all.x = TRUE)
analysis_data <- analysis_data[order(analysis_data$id, analysis_data$time), ]
sum(is.na(analysis_data$time))
table(visits$R)
}
\keyword{datasets}
