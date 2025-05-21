#' NSAE : Nonstationary Small Area Estimation
#'
#' Executes nonstationary Fay-Herriot model and nonstationary generalized linear mixed model for small area estimation. It produces empirical best linear unbiased predictor (EBLUP) and empirical best predictor (EBP) under stationary and nonstationary Fay-Herriot models. Functions give EBLUP and EBP estimators along with their mean squared error (MSE) estimator for each model. The nonstationary Fay-Herriot model was developed by Hukum  Chandra, Nicola Salvati and Ray Chambers (2015) <doi:10.1093/jssam/smu026> and the nonstationary generalized linear mixed model was developed by Hukum  Chandra, Nicola Salvati and Ray Chambers (2017) <doi:10.1016/j.spasta.2017.01.004>.
#
#' @section Author(s):
#' Hukum  Chandra, Nicola Salvati, Ray Chambers, Saurav Guha
#'
#' {Maintainer}: Saurav Guha \email{saurav.iasri@gmail.com}
#'
#' @section Functions:
#' \describe{
#'   \item{\code{\link{eblupFH1}}}{Provides the EBLUPs and MSE under stationary Fay-Herriot model for sample area}
#'   \item{\code{\link{eblupFH2}}}{Provides the EBLUPs and MSE under stationary Fay-Herriot model for sample and non-sample area}
#'   \item{\code{\link{eblupNSFH1}}}{Provides the EBLUPs and MSE under nonstationary Fay-Herriot model for sample area}
#'   \item{\code{\link{eblupNSFH2}}}{Provides the EBLUPs and MSE under nonstationary Fay-Herriot model for sample and non-sample area}
#'   \item{\code{\link{NS.test}}}{Provides a p-value for testing spatial nonstationarity in the data under Fay-Herriot model.}
#'   \item{\code{\link{ebp}}}{Provides the EBPs and MSE under stationary generalized linear mixed model.}
#'   \item{\code{\link{ebpNS}}}{Provides the EBPs and MSE under nonstationary generalized linear mixed model.}
#'   \item{\code{\link{ebpSP}}}{Provides the EBPs and MSE under a spatially correlated generalized linear mixed model.}
#'   \item{\code{\link{ebpNP}}}{Provides the EBPs and MSE under nonparametric generalized linear mixed model.}
#'   \item{\code{\link{NSglm.test}}}{Provides a p-value for testing spatial nonstationarity in the data under generalized linear mixed model.}
#' }
#'
#' @section Reference:
#' \itemize{
#'   \item{Chandra, H., Salvati, N., & Chambers, R. (2015). A spatially nonstationary fay-herriot model for small area estimation. Journal of survey statistics and methodology. 3. 109-135. DOI:10.1093/jssam/smu026.}
#'   \item{Chandra, H., Salvati, N., & Chambers, R. (2017). Small area prediction of counts under a non-stationary spatial model. Spatial Statistics. 20. 30-56. DOI:10.1016/j.spasta.2017.01.004.}
#'   \item{Chandra, H., Salvati, N., & Chambers, R. (2018). Small area estimation under a spatially non-linear model. Computational Statistics and Data Analysis. 126. 19-38. DOI:10.1016/j.csda.2018.04.002.}
#'   \item{Fay, R. E. & Herriot, R. A. (1979). Estimates of Income for Small Places: An Application of James-Stein Procedures to Census Data. Journal of the American Statistical Association. 74. 269-277. DOI:10.2307/2286322.}
#'   \item{Rao, J.N.K & Molina. (2015). Small Area Estimation 2nd Edition. New York: John Wiley and Sons, Inc.}
#'   }
#'
#' @docType package
#' @name NSAE
#' @import rlist, cluster, MASS, lattice, Matrix, numDeriv, nlme, spgwr, SemiPar
NULL
