\name{informedSen-package}
\alias{informedSen-package}
\alias{informedSen}
\concept{Sensitivity analysis}
\concept{Observational study}
\concept{Placebo test}
\concept{Test for bias}
\concept{Causal inference}
\concept{Control outcome}
\concept{Unaffected outcome}
\concept{Known effect}
\docType{package}
\title{
\packageTitle{informedSen}
}
\description{
\packageDescription{informedSen}
}
\details{

The DESCRIPTION file:
\packageDESCRIPTION{informedSen}
\packageIndices{informedSen}
The package performs a sensitivity analysis within a confidence set provided by a test for unmeasured bias.  The method is a substantial computational and practical enhancement of a concept introduced in Rosenbaum (1992) Detecting bias with confidence in observational studies. Biometrika, 79(2), 367-374.  <doi:10.1093/biomet/79.2.367>  The main function in the package is informedsen.  The package uses the optimization software gurobi obtainable from <https://www.gurobi.com/>, together with its associated R package, also called gurobi; see: <https://www.gurobi.com/documentation/7.0/refman/installing_the_r_package.html>.  See the example in informedsen for discussion about obtaining gurobi and its associated local R-package.
}
\author{
\packageAuthor{informedSen}

Maintainer: \packageMaintainer{informedSen}
}
\references{
Rosenbaum, P. R. (1984). From association to causation in observational studies: The role of tests of strongly ignorable treatment assignment. Journal of the American Statistical Association 79, 41-48.
<doi:10.1080/01621459.1984.10477060>

Rosenbaum, P. R. (1989a). On permutation tests for hidden biases in observational studies. The Annals of Statistics 17, 643-653.
<doi:10.1214/aos/1176347131>

Rosenbaum, P. R. (1989b). The role of known effects in observational studies. Biometrics 45, 557-569. <doi:10.2307/2531497>

Rosenbaum, P. R. (1992). Detecting bias with confidence in observational studies. Biometrika, 79(2), 367-374. <doi:10.1093/biomet/79.2.367>

Rosenbaum, P. R. (2007) Sensitivity analysis for m-estimates, tests and confidence intervals in matched observational studies. Biometrics, 2007, 63, 456-464. <doi:10.1111/j.1541-0420.2006.00717.x>

Rosenbaum, P. R. (2021). Sensitivity analyses informed by tests for bias in observational studies.  Manuscript.  This manuscript describes and illustrates the new computational tools that make feasible the method in Rosenbaum (1992).  The example in the package is the example in this manuscript.
}

\keyword{ package }

\examples{
\dontrun{
# To run these examples, you MUST have gurobi installed.
# The makers of gurobi provide free access to academics.
# Additionally, you must install the local R package gurobi
# that is provided by installing gurobi.
# The examples are from Rosenbaum (2021)
data(HDL)
shdl<-senmscores(HDL$hdl,HDL$z,HDL$mset)
smmerc<-senmscores(HDL$mmercury,HDL$z,HDL$mset)
sc<-cbind(shdl,smmerc)
informedsen(3.5, sc, HDL$z, HDL$mset, alpha = 0.05)
informedsen(3.4, sc, HDL$z, HDL$mset, alpha = c(0.04,0.01))
}
}
