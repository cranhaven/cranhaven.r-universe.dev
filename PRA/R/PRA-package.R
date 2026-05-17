#' PRA: Project Risk Analysis
#'
#' @description
#' Data analysis for Project Risk Management via the Second Moment Method,
#' Monte Carlo Simulation, Contingency Analysis, Sensitivity Analysis,
#' Earned Value Management, Learning Curves, Bayesian Methods, and more.
#'
#' @section Key modules:
#' \describe{
#'   \item{**Monte Carlo Simulation** (`mcs()`)}{
#'     Propagates schedule or cost uncertainty through a triangular distribution
#'     model using `mc2d`. Returns an S3 object with a `print` method.}
#'   \item{**Second Moment Method** (`smm()`)}{
#'     Analytical first- and second-moment propagation of uncertainty without
#'     simulation. Returns an S3 object with a `print` method.}
#'   \item{**Earned Value Management** (`pv()`, `ev()`, `ac()`, `sv()`, `cv()`,
#'     `spi()`, `cpi()`, `eac()`, `etc()`, `tcpi()`, `vac()`)}{
#'     Full suite of EVM performance metrics and forecasting functions.}
#'   \item{**Contingency Analysis** (`contingency()`)}{
#'     Derives cost or schedule contingency from simulation output at a
#'     user-specified confidence level.}
#'   \item{**Sensitivity Analysis** (`sensitivity()`)}{
#'     Ranks risk drivers by their contribution to overall project uncertainty.}
#'   \item{**Learning Curves** (`fit_sigmoidal()`, `predict_sigmoidal()`,
#'     `plot_sigmoidal()`)}{
#'     Fits Pearl, Gompertz, or Logistic sigmoidal growth models to
#'     cumulative cost or progress data using nonlinear least squares
#'     (`minpack.lm`).}
#'   \item{**Bayesian Risk Inference** (`risk_prob()`, `risk_post_prob()`)}{
#'     Beta-Binomial conjugate updating for risk event probabilities.}
#'   \item{**Correlation Matrices** (`cor_matrix()`)}{
#'     Constructs valid positive-definite correlation matrices for use in
#'     multivariate simulations.}
#'   \item{**Design Structure Matrix** (`parent_dsm()`, `grandparent_dsm()`)}{
#'     Derives direct (parent) and indirect (grandparent) dependency structure
#'     matrices from a binary adjacency matrix.}
#' }
#'
#' @section References:
#' Damnjanovic, Ivan, and Kenneth Reinschmidt. Data analytics for engineering and
#' construction project risk management. No. 172534. Cham, Switzerland: Springer, 2020.
#'
#' @srrstats {G1.2} *Life Cycle Statement: This package is in active
#'   development. The API is considered stable for all exported functions.
#'   Future development will focus on additional risk analysis methods and
#'   performance improvements. Users can expect backward compatibility for
#'   existing functions.*
#'
#' @srrstats {G1.3} *All statistical terminology is defined in individual
#'   function documentation. Key terms include: Monte Carlo Simulation,
#'   Earned Value Management (EVM), Bayesian inference, sensitivity analysis,
#'   and sigmoidal growth models (Pearl, Gompertz, Logistic).*
#'
#' @author Paul Govan \email{paul.govan2@@gmail.com}
#'   (ORCID: \href{https://orcid.org/0000-0002-1821-8492}{0000-0002-1821-8492})
#'
#' @keywords internal
"_PACKAGE"

## usethis namespace: start
## usethis namespace: end
NULL
