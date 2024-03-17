#' @title Feature selection for multivariate time series
#'
#' @description
#' \code{fsMTS} implements algorithms for feature selection in multivariate time series
#'
#' @details
#' The function implements selection of potential relationships between multivariate time series' components and their lags.
#'
#' @param mts an matrix object with values of the multivariate time series (MTS)
#' MTS components are by \emph{k} columns, observations are by rows
#' @param max.lag the maximal lag value
#' @param method a feature selection algorithm.
#' Implemented algorithms:
#' \itemize{
#'  \item{\strong{"ownlags"}}{ - only own (autoregressive) lags. The method constructs the matrix of features that
#'  represents independent AR(max.lag) processes for every MTS component.
#'  \item{\strong{"distance"}}{ - distance-based feature selection.
#'  The method uses directed distances \emph{shortest} between every pair of time series components (origin and destination).
#'  The lag \emph{l} is selected as a potential relationship (feature) if the destination component is reachable
#'  from the origin component within (\emph{l*step}) time steps, rounded to integer value.
#'  All previous and next lags are \strong{not} included into the resulting structure.}
#'  \item{\strong{"CCF"}}{ - cross-correlation-based. The method returns values of Pearson's correlation coefficient between every MTS component
#'  and all other MTS components and their lags. See Yang et al.(2005) as an example of application.}
#'  Only own lags of every MTS component are included as selected features.}
#'  \item{\strong{"MI"}}{ - mutual information-based. The method returns values of mutual information between every component
#' of the multivariate time series and all other components and their lags.
#' The method is localized - mutual information is independently estimated for every MTS component and
#' lags (1:\emph{max.lag}) of all MTS components. See Liu et al. (2016) as an example of application.}
#'  \item{\strong{"RF"}}{ - random forest estimation of \emph{k} linear regression models. The method returns increase of mean square error (%IncMSE) metric values for every component
#'  of the multivariate time series and all other components and their lags.
#' The method is localized - the linear regression is independently estimated by the random forest algorithm for every MTS
#' component as a dependent variable and lags (1:\emph{max.lag}) of all MTS components as explanatory variables.
#' See Pavlyuk (2020) for more details}
#'  \item{\strong{"GLASSO"}}{ - feature selection using graphical LASSSO regularisation of the inverse covariance matrix.
#'  The method returns values from inverse correlation matrix between every MTS component and all other components and their lags.
#'  The method is localized - the sparse inverse correlation matrix is independently estimated for every time series
#'  component and lags (1:_max.lag_) of all other components.}
#'  \item{\strong{"LARS"}}{ - feature selection using least angle regression.
#'  The method returns values of beta proportions from the least angle regression, estimated for every MTS component
#'  and all other components and their lags.
#'  The method is localized - the least angle regression is independently estimated for every MTS
#'  component and lags (1:_max.lag_) of all other components.}
#'  \item{\strong{"PSC"}}{ - feature selection using partial spectral coherence of MTS components.
#'  The method returns maximal values of the partial spectral coherence function for all MTS lags}
#' }
#' @param localized the logical parameter to executed localized (component-wise) feature selection
#' if the selected method supports this ("MI", "GLASSO", "RF"). Localized versions of algorithms are based on selection
#' of features for independently for every MTS component from all lagged components. Non-localised versions include
#' simulteneous feature selection for all components, including potential instantaneous effects
#' (relationships between feature within the same lag). Leter, non-localised algortihms ignore instantaneous effects and return only
#' lagged features.
#'
#'   By default is TRUE
#' @param show.progress the logical parameter to print progress of calculation.  By default is FALSE.
#' @param ... method-specific parameters:
#' \itemize{
#'  \item{\strong{"shortest"}}{
#'   ("distance" algorithm) matrix of externally provided shortest distances between every pair of time series' components.
#'  }
#'  \item{\strong{"step"}}{
#'   ("distance" algorithm) distance that covered by the process during one time step of the time series. By default is 1.
#'  }
#'  \item{\strong{"rho"}}{
#'   ("GLASSO" algorithm) non-negative regularization parameter for lasso. rho=0 means no regularization.
#'  }
#'  }
#'
#' @return returns a real-valued or binary (depends on the algorithm) feature matrix of \emph{k*max.lag} rows and \emph{k} columns,
#' where \emph{k} is number of time series components (number of columns in the \emph{mts} parameter).
#' Columns correpond to components of the time series; rows correspond to lags (from 1 to \emph{max.lag}).
#'
#' @export
#' @references
#'
#' \emph{Distance-based feature selection for MTS}
#'
#' Pfeifer, P. E., & Deutsch, S. J. 1980. A Three-Stage Iterative Procedure for Space-Time Modeling. Technometrics, 22(1), 35.
#'
#' \emph{Cross-corelation-based feature selection for MTS}
#'
#' Netoff I., Caroll T.L., Pecora L.M., Sciff S.J. 2006. Detecting coupling in the presence of noise and nonlinearity.
#' In: Schelter B, Winterhalder W, Timmer J, editors. Handbook of time series analysis.
#'
#' \emph{Mutual information-based feature selection for MTS}
#'
#' Liu, T., Wei, H., Zhang, K., Guo, W., 2016. Mutual information based feature selection for multivariate time series forecasting, in: 35th Chinese Control Conference (CCC). Presented at the 2016 35th Chinese Control Conference (CCC), IEEE, Chengdu, China, pp. 7110–7114.
#'
#' \emph{Random forest-based feature selection for MTS}
#'
#' Pavlyuk, D., 2020. Random Forest Variable Selection for Sparse Vector Autoregressive Models,
#' in: Valenzuela, O., Rojas, F., Pomares, H., Rojas, I. (Eds.),
#' Theory and Applications of Time Series Analysis. Selected Contributions from ITISE 2019., Contributions to Statistics.
#'
#' \emph{Graphical LASSO-based feature selection for MTS}
#'
#' Haworth, J., Cheng, T., 2014. Graphical LASSO for local spatio-temporal neighbourhood selection, in: Proceedings the GIS Research UK 22nd Annual Conference. Presented at the GIS Research UK 22nd Annual Conference, Leicester, UK, pp. 425–433.
#'
#' \emph{Least angle regression for feature selection for MTS}
#'
#' Gelper S. and Croux C., 2008. Least angle regression for time series forecasting with many predictors, Leuven, Belgium, p.37.
#'
#' \emph{Partial spectral coherence for feature selection for MTS}
#'
#' Davis, R.A., Zang, P., Zheng, T., 2016. Sparse Vector Autoregressive Modeling. Journal of Computational and Graphical Statistics 25, 1077–1096.
#'
#'
#' @examples
#'
#' # Load traffic data
#' data(traffic.mini)
#'
#' # Scaling is sometimes useful for feature selection
#' # Exclude the first column - it contains timestamps
#' data <- scale(traffic.mini$data[,-1])
#'
#' mIndep<-fsMTS(data, max.lag=3, method="ownlags")
#' mCCF<-fsMTS(data, max.lag=3, method="CCF")
#' mDistance<-fsMTS(data, max.lag=3, method="distance", shortest = traffic.mini$shortest, step = 5)
#' mGLASSO<-fsMTS(data, max.lag=3,method="GLASSO", rho = 0.05)
#' mLARS<-fsMTS(data, max.lag=3,method="LARS")
#' mRF<-fsMTS(data, max.lag=3,method="RF")
#' mMI<-fsMTS(data, max.lag=3,method="MI")
#' mlist <- list(Independent = mIndep,
#'               Distance = mDistance,
#'               CCF = mCCF,
#'               GLASSO = mGLASSO,
#'               LARS = mLARS,
#'               RF = mRF,
#'               MI = mMI)
#'
#' th<-0.30
#' (msimilarity <- fsSimilarityMatrix(mlist,threshold = th, method="Kuncheva"))
#'
fsMTS <- function(mts, max.lag, method=c("ownlags", "distance", "CCF","MI", "RF", "GLASSO", "LARS", "PSC"),
                  show.progress = F, localized = F,...) {
  method <- match.arg(method)
  if (is.null(mts) || !is.matrix(mts))
    stop('Incorrect mts value - a matrix with multivariate time series is required')
  if (is.null(max.lag))
    stop('max.lag is required')
  if (max.lag!=as.integer(max.lag) || max.lag<=0)
    stop('max.lag should be a positive integer')
  opts <- list(...)
  res <- switch(method,
         ownlags=fsIndependent(mts, max.lag),
         distance={
           if (is.null(opts$shortest))
             stop('shortest is required for distance-based feature selection')
           if (!is.matrix(opts$shortest) || ncol(opts$shortest)!=ncol(mts) || nrow(opts$shortest)!=ncol(mts))
             stop('shortest shoould be a k by k square matrix, where k is number of columns in mts time series')
           if (is.null(opts$step))
             stop('step is required for distance-based feature selection')
           if (!is.numeric(opts$step)||opts$step<=0)
             stop('step should be positive for distance-based feature selection')
           fsDistance(mts, max.lag, shortest=opts$shortest, step=opts$step)
         },
         CCF=fsCCF(mts, max.lag, show.progress=show.progress),
         MI=fsMI(mts, max.lag, show.progress=show.progress,localized=localized),
         RF=fsRF(mts, max.lag, show.progress=show.progress,localized=localized),
         PSC=fsPSC(mts, max.lag, show.progress=show.progress),
         GLASSO={
           if (is.null(opts$rho))
             stop('rho value is required for graphical LASSO')
           if (!is.numeric(opts$rho)||opts$rho<0||opts$rho>1)
             stop('Incorrect rho value for graphical LASSO - value from the [0, 1] interval is required')
           fsGLASSO(mts, max.lag,rho=opts$rho, show.progress=show.progress, localized=localized)
         },
         LARS=fsLARS(mts, max.lag, show.progress=show.progress),
         stop('Unknown feature selection method')
  )
  return(res)
}
