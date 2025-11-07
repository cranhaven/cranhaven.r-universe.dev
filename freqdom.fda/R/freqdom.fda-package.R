#' Functional time series: dynamic FPCA
#'
#' Implementation of dynamic functional principle component analysis (FDPCA),
#' simulation of functional AR and functional MA processes and frequency domain tools for funcional data.
#' The package is a wrapper for functionality of the multivariate package \pkg{freqdom}
#' for applying frequency domain on objects from \pkg{fda}.
#' Compared to \pkg{freqdom} some new visualization methods are added --
#' adequate only if data has functional structure.
#'
#' \pkg{fda.ts} package allows you to analyse functional time series objects
#' in both time and frequency domain. The main feature is dynamic functional principal component analysis.
#' This method allows to transform a stationary functional time series into a vector process with 
#' mutually uncorrelated component processes.
#'
#' There are two key differnces between classical PCA and dynamic PCA:
#' * Component processes returned by the dynamic procedure are mutually uncorrelated,
#' * The mapping maximizes the long run variance of compoments, which, in case of stationary functional time series, means
#' that the process reconstructed from and \eqn{d > 0} first dynamic principal components
#' better approximates the original functional time series process than the first \eqn{d} classic principal components.
#' 
#' For functional data one can conveniently visualize properties of the
#' filters, covariances or the spectral density operator.
#'
#' For details we refer to the literature below and to help pages of functions \code{\link{fts.dpca}}
#' for estimating the components, \code{\link{fts.dpca.scores}} for estimating scores and
#' \code{\link{fts.dpca.KLexpansion}} for retrieving the signal from components.
#'
#' The package \pkg{fda.ts} require the package \pkg{freqdom} provides the analogue multivariate toolset.
#'
#' @references Hormann Siegfried, Kidzinski Lukasz and Hallin Marc.
#' \emph{Dynamic functional principal components.} Journal of the Royal
#' Statistical Society: Series B (Statistical Methodology) 77.2 (2015): 319-348.
#' @references Hormann Siegfried, Kidzinski Lukasz and Kokoszka Piotr.
#' \emph{Estimation in functional lagged regression.}
#' Journal of Time Series Analysis 36.4 (2015): 541-561.
#' @references Hormann Siegfried and Kidzinski Lukasz.
#' \emph{A note on estimation in Hilbertian linear models.}
#' Scandinavian journal of statistics 42.1 (2015): 43-62.
#' @aliases fda.ts
"_PACKAGE"
