\name{haldport}
\alias{haldport}
\docType{data}
\title{Portland Cement data of Hald(1952)}
\description{Heat evolved during setting of 13 cement mixtures of four (or five) ingredients.
 The first four ingredient percentages appear to be "rounded down" to a full integer. The fifth
 integer percentage of "other" material assures that the five percentages sum to exactly 100\%.
 However, the "centered" X-matrix resulting from inclusion of all five ingredients would then be
 Singular (rank=4). In other words, regressing any y-Outcome on only the first four X-variables
 yields an "ill-conditioned" model that, while having numerical "full rank"=4, actually suffers
 a effective "rank deficiency" of at least mcal = 1. 
}
\usage{data(haldport)}
\details{The (RXshrink) haldport data are identical to the (MASS) cement data except for
 variable names and inclusion of the "other" X-variable.
}
\format{
  A data frame with 13 observations on the following 6 variables.
  \describe{
    \item{\code{p3ca}}{Positive Integer percentage of 3CaO.Al2O3 [tricalcium aluminate] in the mixture.}
    \item{\code{p3cs}}{Positive Integer percentage of 3CaO.SiO2 [tricalcium silicate] in the mixture.}
    \item{\code{p4caf}}{Positive Integer percentage of 4CaO.Al2O3.Fe2O3 [tetracalcium aluminoferrite] in the mixture.}
    \item{\code{p2cs}}{Positive Integer percentage of 2CaO.SiO2 [dicalcium silicate] in the mixture.}
    \item{\code{other}}{Positive Integer percentage of other ingredients in the mixture.} 	
    \item{\code{heat}}{Heat (cals/gm) evolved in setting, recorded to nearest tenth.}
  }
}
\source{
  Woods H, Steinour HH, Starke HR. "Effect of composition of Portland cement on heat
  evolved during hardening. Industrial Engineering and Chemistry 1932; 24: 1207-1214.
}
\references{
  Hald A. Statistical Theory with Engineering Applications. 1952 (page 647.) New York; Wiley.
  
  Obenchain RL. (2022) Efficient Generalized Ridge Regression. \emph{Open Statistics}
  \bold{3}: 1-18. \doi{10.1515/stat-2022-0108}
}
\keyword{datasets}