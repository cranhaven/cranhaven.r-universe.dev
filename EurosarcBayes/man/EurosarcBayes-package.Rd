\name{EurosarcBayes-package}
\alias{EurosarcBayes-package}
\alias{EurosarcBayes}
\docType{package}
\title{Bayesian sample size calculation software}
\description{
Bayesian sample size calculation software and examples for Eurosarc clinical trials which utilise Bayesian methodology for binary endpoints (response/no-response). These trials rely on binomial based endpoints so the majority of programs found here relate to this sort of endpoint. Interim analyses are permitted for most designs. Developed as part of the EuroSARC FP7 grant.
}
\details{
\tabular{ll}{
Package: \tab EurosarcBayes\cr
Type: \tab Package\cr
Version: \tab 1.0\cr
Date: \tab 2015-11-18\cr
License: \tab None\cr
}
This package contains functions and some corresponding shiny versions of them for a user interface approach to sample size calculation and some examples.

There are both frequentist and Bayesian sample size optimisation programs contained here. Both versions are capable of computing frequentist and Bayesian properties of the given approach. This should allow for easy comparison between approaches.

\bold{List of user friendly shiny apps:}

\code{\link{shiny_binom_single_onestage}}\cr
\code{\link{shiny_binom_single_twostage}}\cr
\code{\link{shiny_LINES_posterior}}

\bold{function naming convention:}

Functions are named in the following way:\cr
\itemize{
\item freq_ or bayes_ denoting a frequentist or Bayesian designs.
\item binom_ indicating a binomial endpoint.
\item one_ or two_ indicating one or two endpoints.
\item methodname_ indicating the approach used.
\item onestage, twostage or nstage. Program for the number of stages. If the program is designed for any number of stages this has been ommited.
}
For example freq_binom_one_simons_twostage is a function for designing a frequentist single endpoint binomial trial using Simons two stage design.

\bold{One endpoint designs:}\cr
\code{\link{freq_binom_one_onestage}}: Finds the smallest sample size for a frequentist trial given the design parameters.

\code{\link{freq_binom_one_simons_twostage}}: Returns Simon's two stage designs with frequentist and bayesian properties of the designs. Options to return both the optimal and minmax designs.

\code{\link{freq_binom_one_landemets}}: Returns designs based on Lan-DeMets   alpha spending approach using the O'Brien-Fleming alpha spending function (Lan and DeMets 1995, O'Brien and Fleming 1979).

\code{\link{bayes_binom_one_postprob_onestage}}: Finds the smallest sample size of a Bayesian trial given the design parameters.

\code{\link{bayes_binom_one_postprob_nstage}}: Computes frequentist and Bayesian properties for a trial with given sample sizes at each interim analysis. The posterior probability is used to determine the stopping critical values at interim.

\code{\link{bayes_binom_one_postlike_nstage}}: Computes frequentist and Bayesian properties for a trial with given sample sizes at each interim analysis. The posterior predictive probabilities are used to determine the stopping critical values at interim.

\bold{Two endpoint designs:}\cr
The two endpoint designs assume that two endpoints are independent.

\code{\link{freq_binom_two_singlestage}}: Finds the smallest sample size for a frequentist trial with two binary endpoints given the design parameters. Exact errors are computed so there is no issue of multiplicity.

\code{\link{freq_binom_two_bryantday_twostage}}: Returns Bryant and Day's two-stage designs with frequentist and bayesian properties of the designs. Options to return both the optimal and minmax designs (Bryant and Day 1995).

\code{\link{bayes_binom_two_postprob}}: Computes frequentist and Bayesian properties for a trial with two binary endpoints and given sample sizes at each interim analysis. Posterior probabilities are used to determine the stopping critical values at interim.

\code{\link{bayes_binom_two_postlike}}: Computes frequentist and Bayesian properties for a trial with two binary endpoints and given sample sizes at each interim analysis. Posterior predictive probabilities are used to determine the stopping critical values at interim.

\code{\link{bayes_binom_two_loss}}: Computes frequentist and Bayesian properties for a trial with two binary endpoints and given sample sizes at each interim analysis. A Bayesian loss function approach is used to determine the stopping critical values at all analyses (Chen 2009).

}
\author{
Peter Dutton

Maintainer: Peter Dutton <dutton.peter@gmail.com>
}
\references{
Simon R. Optimal two-stage designs for phase II clinical trials. Control Clin Trials 1989; 10: 1-10.

Bryant J, Day R. Incorporating toxicity considerations into the design of two-stage phase II clinical trials. Biometrics 1995; 51: 1372-1383.

DeMets, D. L. and G. Lan (1995). The alpha spending function approach to interim data analyses. Cancer Treat Res 75: 1-27.

O'Brien, P. C. and T. R. Fleming (1979). A Multiple Testing Procedure for Clinical Trials. Biometrics 35(3): 549-556.

Chen Y, Smith BJ. Adaptive group sequential design for phase II clinical trials: a Bayesian decision theoretic approach. Stat Med 2009; 28: 3347-3362.
}
\keyword{ package }

