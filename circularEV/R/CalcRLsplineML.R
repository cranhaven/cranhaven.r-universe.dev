
#' Calculate T-year levels for spline ML model
#' @param Data Response variable
#' @param drc Directional covariate
#' @param xiBoot Bootstrap estimates for EVI
#' @param sigBoot Bootstrap estimates for shape
#'
#' @inheritParams LocalEstim
#' @return List including bootstrap estimates of T-year levels.
#' @seealso \code{\link{SplineML}} for examples.
#' @examples
#' ## See also examples in vignettes:
#' # vignette("localMethods", package = "circularEV")
#' # vignette("splineML", package = "circularEV")
#' @export
CalcRLsplineML <- function(Data, drc, h,
                   xiBoot, sigBoot, TTs=c(100, 10000), thetaGrid=1:360, timeRange, thr){

  n <- length(Data)
  taufun <- Calc_taufun(Data=Data, drc=drc, h=h, thetaGrid=thetaGrid, thr)
  taufun <- taufun[-1]

  nBoot <- ncol(xiBoot)

  RL_listBoot <- list()
  for(i_boot in 1:nBoot){

    xi <- xiBoot[,i_boot]
    xi <- xi[-1]
    sig <- sigBoot[,i_boot]
    sig <- sig[-1]

    RL_mat <- matrix(NA, length(thetaGrid), length(TTs))

    for(TT_idx in seq_along(TTs)){
      TT <- TTs[TT_idx]
      ny <- n/timeRange
      period <- TT*ny
      for(theta_i in seq_along(thetaGrid)){
        RL_mat[theta_i,TT_idx] <- ret_level_gpd(sigma = sig[theta_i], xi = xi[theta_i],
                                                tau = taufun[theta_i], thresh = thr[theta_i],
                                                period = period)
      }
    }

    RL_listBoot[[i_boot]] <- RL_mat
  }

  return(RL_listBoot)
}
