\encoding{utf8} % \encoding{latin1}
\name{rvif-package}
\alias{rvif-package}
\alias{rvif}
\docType{package}
\title{
Detecting multicollinearity using RVIF and graphical methods.
}
\description{
Detecting troubling near-multicollinearity in multiple linear regression models is a classical econometric problem. 
The purpose of this package is to detect it by using the Redefined Variance Inflation Factor 
(RVIF) and the scatterplot between the Variance Inflation Factor (VIF) and the Coefficient of Variation (CV).

In addition, the RVIF is used to determine whether the statistical analysis of the model is affected by the degree 
of multicollinearity in the model.
}
\details{
This package contains four functions. The first two, \code{cv_vif} and \code{cv_vif_plot}, respectively return the values of the Variance Inflation Factor (VIF) 
and the Coefficient of Variation (CV), as well as their representation in a scatterplot. It should be noted that the 
VIF is useful for detecting essential multicollinearity, while the CV is useful for detecting non-essential multicollinearity.
Thus, the scatterplot of both measures can provide interesting information for determining whether there is a troubling degree 
of multicollinearity and identifying the type of multicollinearity present and the variables causing it.

On the other hand, the funcion \code{rvif} calculates the redefined VIF and the percentage of approximate multicollinearity due to each 
independent variable.

Finally, \code{multicollinearity} determines whether the degree of multicollinearity in the regression model affects the statistical 
analysis of the model, i.e., whether the non-rejection of the null hypothesis in the individual significance tests 
is due to the linear relationships between the independent variables of the model. 
}
\author{
  Román Salmerón Gómez (University of Granada) and Catalina B. García García (University of Granada).

  Maintainer: Román Salmerón Gómez (romansg@ugr.es)
}
\references{
Salmerón, R., García, C.B. and García, J. (2018). Variance inflation factor and condition number in multiple linear
regression. Journal of Statistical Computation and Simulation, 88:2365-2384, doi: https://doi.org/10.1080/00949655.2018.1463376.

Salmerón, R., Rodríguez, A. and García, C.B. (2020). Diagnosis and quantification of 
the non-essential collinearity. Computational Statistics, 35(2), 647-666, doi: https://doi.org/10.1007/s00180-019-00922-x.

Salmerón, R., García, C.B., Rodríguez, A. and García, C. (2022). Limitations in detecting multicollinearity due to scaling issues in the 
mcvis package. R Journal, 14(4), 264-279, doi: https://doi.org/10.32614/RJ-2023-010.

Salmerón, R., García, C.B. and García, J. (2025). A redefined Variance Inflation Factor: overcoming the limitations of the Variance 
Inflation Factor. Computational Economics, 65, 337-363, doi: https://doi.org/10.1007/s10614-024-10575-8.

Overcoming the inconsistences of the variance inflation factor: a redefined VIF and a test to detect statistical troubling multicollinearity 
by Salmerón, R., García, C.B and García, J. (working paper, https://arxiv.org/pdf/2005.02245).
}

\keyword{Redefined Variation Inflation Factor}
\keyword{Variation Inflation Factor}
\keyword{Coefficient of Variation}

