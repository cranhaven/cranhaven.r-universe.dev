\name{BFactor_zoo_example}
\alias{BFactor_zoo_example}
\docType{data}
\title{
    Simulated Example Dataset \bold{'BFactor_zoo_example'}
}
\description{
    A simulated dataset used in Figure 1 of \insertCite{bryzgalova2023bayesian;textual}{BayesianFactorZoo}.
}
\usage{data("BFactor_zoo_example")}
\format{
  A list consisting of the following variables:
  \describe{
    \item{HML}{High-minus-low value factor, from Ken French Website}
    \item{lambda_ols}{Hypothetical true risk prices of factors in simulations}
    \item{R2.ols.true}{Hypothetical true OLS R-squared in simulations}
    \item{sim_f}{Simulated strong factor}
    \item{sim_R}{Simulated test asset returns}
    \item{uf}{Simulated weak/unspanned factor}
    \item{W_ols}{Weighting matrix used in GMM OLS estimations}
  }
}
\source{
    Section III in \insertCite{bryzgalova2023bayesian;textual}{BayesianFactorZoo}.
}
\references{
    \insertRef{bryzgalova2023bayesian}{BayesianFactorZoo}
}
\examples{
data(BFactor_zoo_example)
HML <- BFactor_zoo_example$HML
lambda_ols <- BFactor_zoo_example$lambda_ols
R2.ols.true <- BFactor_zoo_example$R2.ols.true
sim_f <- BFactor_zoo_example$sim_f
sim_R <- BFactor_zoo_example$sim_R
uf <- BFactor_zoo_example$uf
W_ols <- BFactor_zoo_example$W_ols

cat("Load the simulated example \n")
cat("Cross-section: Fama-French 25 size and value portfolios \n")
cat("True pricing factor in simulations: HML \n")
cat("Misspecified model with pseudo-true R-squared:", R2.ols.true, "\n")
cat("Pseudo-true (monthly) risk price:", lambda_ols[2], "\n")
}
\keyword{datasets}
