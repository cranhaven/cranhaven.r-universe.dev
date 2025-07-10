#' LongMemoryTS: Long Memory Time Series
#'
#' The LongMemoryTS package is a collection of functions for estimation,
#' simulation and testing of long memory processes, spurious long memory processes, 
#' and fractionally cointegrated systems.
#'
#' @docType package
#' @seealso \code{\link{ARRLS.sim}}, \code{\link{ELW}}, \code{\link{ELW2S}}, 
#' \code{\link{F.hat}}, \code{\link{FCI_CH03}}, \code{\link{FCI_CH06}}, 
#' \code{\link{FCI_MV04}}, \code{\link{FCI_N10}}, \code{\link{FCI_NS07}},
#' \code{\link{FCI_R08}}, \code{\link{FCI_SRFB18}}, \code{\link{FCI_WWC15}},
#' \code{\link{FCI_ZRY18}}, \code{\link{FDLS}}, \code{\link{FI.sim}}, 
#' \code{\link{FMNBLS}}, \code{\link{G.hat}}, \code{\link{GSE}}, 
#' \code{\link{GSE_coint}}, \code{\link{Hou.Perron}}, \code{\link{LPWN}},
#' \code{\link{MLWS}}, \code{\link{McC.Perron}}, \code{\link{Peri}}, 
#' \code{\link{Qu.test}}, \code{\link{T.rho}}, \code{\link{T0stat}}, 
#' \code{\link{VARFIMA.est}}, \code{\link{VARFIMA.sim}}, \code{\link{W_multi}},
#' \code{\link{cross.Peri}}, \code{\link{ddiffw}}, \code{\link{fBM}}, \code{\link{fdiff}}, 
#' \code{\link{gph}}, \code{\link{ll.VARFIMA}}, \code{\link{local.W}},
#' \code{\link{partition.X}}, \code{\link{pre.White}}, \code{\link{rank.est}}
#' 
#' @author Christian Hendrik Leschinski <christian_leschinski@gmx.de>, Michelle Voges, Kai Wenger
#' @name LongMemoryTS
#' @references Bai, J. and Perron, P. (1998): Estimating and Testing Linear Models With Multiple Structural Changes. Econometrica, Vol. 66, No. 1, pp. 47 - 78.
#'
#' Bai, J. and Perron, P. (2003): Computation and Analysis of Multiple Structural Change Models. Journal of Applied Econometrics, Vol. 18, pp. 1-22.
#'
#' Bardet, J.-M. et al. (2003): Generators of long-range dependent processes: a survey. 
#' Theory and applications of long-range dependence, pp. 579 - 623, Birkhauser Boston.
#' 
#' Chen, W. W. and Hurvich, C. M. (2003): Semiparametric estimation of multivariate fractional cointegration. Journal of the American Statistical Association, Vol. 98, No. 463, pp. 629 - 642.
#' 
#' Chen, W. W. and Hurvich, C. M. (2006): Semiparametric estimation of fractional cointegrating subspaces. The Annals of Statistics, Vol. 34, No. 6, pp. 2939 - 2979.
#' 
#' Christensen, B. J. and Nielsen, M. O. (2006): Asymptotic normality of narrow-band least squares in the stationary fractional cointegration model and volatility forecasting. Journal of Econometrics, 133, pp. 343-371.
#'
#' Davidson, J., Hashimzade, N. (2009). Type I and type II fractional Brownian motions: A reconsideration. 
#' Computational Statistics & Data Analysis, No. 53, Vol. 6, pp. 2089 - 2106. 
#'
#' Frederiksen, P., Nielsen, F. S., and Nielsen, M. O. (2012): Local polynomial Whittle estimation of perturbed fractional processes. Journal of Econometrics, Vol. 167, No.2, pp. 426-447.
#'
#' Geweke, J. and Porter-Hudak, S. (1983): The estimation and application of long memory time series models. Journal of Time Series Analysis, 4, 221-238.
#'
#' Hou, J., Perron, P. (2014): Modified local Whittle estimator for long memory processes in the presence of low frequency (and other) contaminations. Journal of Econometrics,  Vol. 182, No. 2, pp. 309 - 328.
#'
#' Hualde, J. (2013): A simple test for the equality of integration orders. Economics Letters, Vol. 119, No. 3, pp. 233 - 237.
#'
#' Hurvich, C. M., and Chen, W. W. (2000): An Efficient Taper for Potentially Overdifferenced Long-Memory Time Series. Journal of Time Series Analysis, Vol. 21, No. 2, pp. 155-180.
#' 
#' Jensen, A. N. and Nielsen, M. O. (2014): A fast fractional difference algorithm. Journal of Time Series Analysis 35(5), pp. 428-436.
#' 
#' Lavielle, M. and Moulines, E. (2000): Least Squares Estimation of an Unknown Number of Shifts in a Time Series. Journal of Time Series Analysis, Vol. 21, No. 1, pp. 33 - 59. 
#' 
#' Lutkepohl, H. (2007): New introduction to multiple time series analysis. Springer.
#' 
#' Marinucci, D., Robinson, P. M. (1999). Alternative forms of fractional Brownian motion. 
#' Journal of Statistical Planning and Inference, Vol. 80 No. 1-2, pp. 111 - 122.
#' 
#' Marmol, F. and Velasco, C. (2004): Consistent testing of cointegrating relationships. Econometrica, Vol. 72, No. 6, pp. 1809 - 1844.
#' 
#' McCloskey, A. and Perron, P. (2013): Memory parameter estimation in the presence of level shifts and deterministic trends. Econometric Theory, 29, pp. 1196-1237.
#' 
#' Nielsen, M. O. (2010): Nonparametric cointegration analysis of fractional systems with unknown integration orders. Journal of Econometrics, Vol. 155, No. 2, pp. 170 - 187.
#' 
#' Nielsen, M. O. and Frederiksen (2011): Fully modified narrow-band least squares estimation of weak fractional cointegration. The Econometrics Journal, 14, pp. 77-120.
#' 
#' Nielsen, M. O. and Shimotsu, K. (2007): Determining the coinegrating rank in nonstationary fractional systems by the exact local Whittle approach. Journal of Econometrics, 141, pp. 574-59.
#'
#' Qu, Z. (2011): A Test Against Spurious Long Memory. Journal of Business and Economic Statistics, Vol. 29, No. 3, pp. 423 - 438.
#'
#' Robinson, P. M., (1994): Semiparametric analysis of long-memory time series. Annals of Statistics, 22, pp. 515-539.
#' 
#' Robinson, P. M. (1995): Log-periodogram regression of time series with long range dependence. The Annals of Statistics, Vol. 23, No. 5, pp. 1048 - 1072.
#' 
#' Robinson, P. M. (1995): Gaussian Semiparametric Estimation of Long Range Dependence. The Annals of Statistics, Vol. 23, No. 5, pp. 1630 - 1661.
#' 
#' Robinson, P. (2008): Diagnostic testing for cointegration. Journal of Econometrics, Vol. 143, No. 1, pp. 206 - 225.
#' 
#' Robinson, P. M. and Marinucci, D. (2003): Semiparametric frequency domain analysis of fractional cointegration. In: Robinson, P. M. (Ed.), Time Series with Long Memory, Oxford University Press, Oxford, pp. 334-373.
#'
#' Robinson, P. M. and Yajima, Y. (2002): Determination of cointegrating rank in fractional systems. Journal of Econometrics, Vol. 106, No.2, pp. 217-241.
#'
#' Shimotsu, K. (2007): Gaussian semiparametric estimation of multivariate fractionally integrated processes. Journal of Econometrics, Vol. 137, No. 2, pp. 277 - 310.
#' 
#' Shimotsu, K. (2010): Exact Local Whittle Estimation Of Fractional Integration with Unknown Mean and Time Trend. Econometric Theory, Vol. 26, pp. 501 - 540.
#' 
#' Shimotsu, K. and Phillips, P. C. B. (2005): Exact Local Whittle Estimation Of Fractional Integration. The Annals of Statistics, Vol. 33, No. 4, pp. 1890-1933.
#' 
#' Sibbertsen, P., Leschinski, C. H., Holzhausen, M., (2018): A Multivariate Test Against Spurious Long Memory. Journal of Econometrics, Vol. 203, No. 1, pp. 33 - 49.
#'
#' Souza, I. V. M., Reisen, V. A., Franco, G. d. C. and Bondon, P. (2018): The estimation and testing of the cointegration order based on the frequency domain. Journal of Business & Economic Statistics, Vol. 36, No. 4, pp. 695 - 704.
#' 
#' Velasco, C. (1999): Gaussian Semiparametric Estimation for Non-Stationary Time Series. Journal of Time Series Analysis, Vol. 20, No. 1, pp. 87-126. 
#' 
#' Wang, B., Wang, M. and Chan, N. H. (2015): Residual-based test for fractional cointegration. Economics Letters, Vol. 126, pp. 43 - 46.
#' 
#' Xu, J. and Perron, P. (2014): Forecasting return volatility: Level shifts with varying jump probability and mean reversion. International Journal of Forecasting, 30, pp. 449-463.
#' 
#' Zhang, R., Robinson, P. and Yao, Q. (2018): Identifying cointegration by eigenanalysis. Journal of the American Statistical Association (forthcoming).
NULL