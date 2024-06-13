\name{OBsMD-package}
\alias{OBsMD-package}
\alias{OBsMD}
\docType{package}
\title{
Objective Bayesian Model Discrimination in Follow-Up Designs
}
\description{
Implements the objective Bayesian methodology proposed in Consonni and Deldossi in order to choose the optimal experiment that better discriminate between competing models.
}
\details{
\tabular{ll}{
Package: \tab OBsMD\cr
Type: \tab Package\cr
Version: \tab 11.1\cr
Date: \tab 2023-11-14\cr
License: \tab GPL version 3 or later\cr
}
The packages allows you to perform the calculations and analyses described
in Consonni and Deldossi paper in TEST (2016), Objective Bayesian model discrimination
in follow-up experimental designs.
}
\author{
Author: Laura Deldossi and Marta Nai Ruscone based on Daniel Meyer's code.\\
Maintainer: Marta Nai Ruscone <marta.nairuscone@unige.it>
}
\references{
Deldossi, L., Nai Ruscone, M. (2020)
R Package OBsMD for Follow-up Designs in an
Objective Bayesian Framework.
  \emph{Journal of Statistical Software} \bold{94}(2), 1--37.
  \doi{10.18637/jss.v094.i02}.
%  \url{https://www.jstatsoft.org}.

  Consonni, G. and Deldossi, L. (2016)
  Objective Bayesian Model Discrimination in Follow-up design.,
  \emph{Test} \bold{25}(3), 397--412.
  \doi{10.1007/s11749-015-0461-3}.

  Box, G. E. P. and Meyer R. D. (1986)
  An Analysis of Unreplicated Fractional Factorials.,
  \emph{Technometrics} \bold{28}(1), 11--18.
  \doi{10.1080/00401706.1986.10488093}.

  Box, G. E. P. and Meyer R. D. (1993)
  Finding the Active Factors in Fractionated Screening Experiments.,
  \emph{Journal of Quality Technology} \bold{25}(2), 94--105.
  \doi{10.1080/00224065.1993.11979432}.

  Meyer, R. D., Steinberg, D. M. and Box, G. E. P. (1996)
  Follow-Up Designs to Resolve Confounding in Multifactor Experiments (with discussion).,
  \emph{Technometrics} \bold{38}(4), 303--332.
  \doi{10.2307/1271297}.

}
\keyword{ package }
\examples{
    data(BM86.data)
}
