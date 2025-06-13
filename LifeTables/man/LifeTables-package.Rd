\name{LifeTables-package}
\alias{LifeTables-package}
\alias{LifeTables}
\docType{package}
\title{
HMD Model Life Tables
}
\description{
Provides functions for using a model life table system generated from the Human Mortality Database
}
\details{
\tabular{ll}{
Package: \tab LifeTables\cr
Type: \tab Package\cr
Version: \tab 1.0\cr
Date: \tab 2015-08-07\cr
License: \tab GPL (>= 2) \cr
LazyLoad: \tab yes\cr
}
\code{\link{mortmod}} is the mortality model that is the basis for the system. The user can supply the two input parameters - family and level - and a complete mortality rate schedule can be generated. If the user has some measure of child mortality (or a partial schedule) and optionally 45q15, the function \code{\link{hmd.DA}} can be used to find the appropriate family. Then \code{\link{alpha.e0}} can calculate an alpha value to generate a life table with a desired life expectancy. The function \code{\link{mod.lt}} combines the steps outlined above. It will find the family and level based on the child (and possibly adult) mortality indicator and a desired life expectancy and return a complete life table generated with the appropriate input parameter values.

There is a graphical user interface implemented in the package that allows for generating model life tables, exporting them and plotting various measures of the life table. It can be invoked using the function \code{modelLT.gui()}. 

Note: In version 1.0, the calibration data set has been updated. Most recent HMD calibration data downloaded February 2015.
}
\author{
David J. Sharrow, GUI by Hana Sevcikova

Maintainer: David Sharrow <dsharrow@uw.edu>

}
\references{
S. Clark and D. Sharrow (2011) "Contemporary Model Life Tables for Developed Countries: An Application of Model-Based Clustering."  Paper presented at the annual meeting of the Population Association of America. \url{http://paa2011.princeton.edu/abstracts/111808}

C. Fraley and A. E. Raftery (2002) "Model-based clustering, discriminant analysis, and density estimation." \emph{Journal of the American Statistical Association} \bold{97}:611-631.

C. Fraley, A. E. Raftery, T. B. Murphy, L. Scrucca (2012) "mclust Version 4 for R: Normal Mixture Modeling for Model-Based Clustering, Classification, and Density Estimation", Technical Report no. 597, Department of Statistics, University of Washington

\emph{Human Mortality Database} University of California, Berkeley (USA), and Max Planck Institute for Demographic Research (Germany). Available at www.mortality.org or www.humanmortality.de (data downloaded February 2015).
}

\keyword{ package }
%\seealso{
%~~ Optional links to other man pages, e.g. ~~
%~~ \code{\link[<pkg>:<pkg>-package]{<pkg>}} ~~
%}
\examples{

# first get the appropriate family #
hmd.DA(x=.09, sex="male", child.mort=4, adult.mort=.28)$classification

# then get the appropriate level (alpha) #
alpha.e0(pattern=1, e0.target=59, sex="male")

# put in the family and alpha to model to produce complete schedule #
mortmod(pattern=1, alpha=.034, sex="male")
}
