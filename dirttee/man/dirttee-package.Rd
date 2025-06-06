\name{dirttee-package}
\alias{dirttee-package}
\alias{dirttee}
\docType{package}
\title{
DIstributional Regression for Times To EvEnt
}
\description{
This package includes regession methods for right-censored response variables.
It allows for the estimation of distributional regression methods with semiparametric predictors,
including, for example, nonlinear, spatial or random effects.
The distribution of the response can be estimated with expectiles, quantiles and mode regression.
Censored observations can be included with accelerated failure time models or inverse probability of censoring weights.
}
\author{
Alexander Seipp, Fabian Otto-Sobotka \cr
Carl von Ossietzky University Oldenburg \cr
\url{https://uol.de/eub} \cr

Maintainer: Alexander Seipp <alexander.seipp@uni-oldenburg.de>

Special thanks for their help go to Lisa Eilers and Florian Berger!

Partially funded by the German Research Foundation (DFG) grant SO1313/1-1, project 'Distributional Regression for Time-to-Event Data'.
}
\references{
Seipp A, Uslar V, Weyhe D, Timmer A, Otto-Sobotka F. Weighted expectile regression for right-censored data. \emph{Statistics in Medicine}. 2021;40(25):5501–5520. doi: 10.1002/sim.9137


Seipp A, Uslar V, Weyhe D, Timmer A, Otto-Sobotka F. Flexible Semiparametric Mode Regression for Time-to-Event Data. \emph{Statistical Methods in Medical Research}. 2022;31(12):2352-2367. doi: 10.1177/09622802221122406
}

\keyword{ package }
\keyword{ nonparametric }
\keyword{ smooth }% __ONLY ONE__ keyword per line
\keyword{ multivariate }
\keyword{ regression }
\keyword{ nonlinear }
\keyword{ models }
\keyword{ survival }

\examples{

\donttest{
data(colcancer)
c100 <- colcancer[1:100,]

#mode regression
reg <- modreg(Surv(logfollowup, death) ~ sex + LNE, data = c100)

#expectile regression
fit_exp <- expectreg.aft(Surv(logfollowup, death) ~ LNE, data = c100,smooth="f")
fit_expipc <- expectreg.ipc(Surv(logfollowup, death) ~ sex + LNE, data = c100)

#quantile regression
qu1 <- qureg.aft(Surv(logfollowup, death) ~ sex + LNE, data=c100, smooth="fixed")
}



}
