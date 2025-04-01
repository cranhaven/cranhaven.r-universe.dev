#' Parameter Transfer Function for Weber et al.(2019) model.
#' 
#' @description Predicts \insertCite{Weber.2019}{spsh} model parameters in the van Genuchten-Mualem variant \emph{01110FM}, from given van Genuchten-Mualem parameters 
#' for the constrained van Genuchten-Mualem model.
#' @param x vector of 6 van Genuchten-Model parameters. The order is sensitive see for \code{\link[spsh]{shypFun.01110}}
#'   \tabular{lll}{
#'       \code{thr}\tab{Residual water content (-), alway equal to zero}\cr
#'       \code{ths}\tab{Saturated water content (-)}\cr
#'       \code{alf1}\tab{Shape parameter (cm^-1)}\cr
#'       \code{n1}\tab{Shape parameter (-)}\cr
#'       \code{Ks}\tab{Hydraulic conductivity at 0 potential (cm/day)}\cr
#'       \code{tau}\tab{Shape parameter (-)}\cr
#'    }
#' @details Pedotransfer function returns the van Genuchten - Mualem model \code{\link[spsh]{shypFun.01110}} parameters in the Brunswick-Model variant \emph{01110FM}, based on previously determined van Genuchten-Mualem parameters. The transfer function is based on an ordinary linear regression between the i-th \emph{01110} and \emph{01110FM}. The paraemeters
#' were based on model parameter estimation to a dataset of >400 samples with retention and conductivity measurements.
#' @note 
#' The parameter transfer function was derived by weighted linear regression with weights in \code{x} and \code{y}, 
#' by regressing the estimated the Brunswick model parameters in the Genuchten Mualem variant \insertCite{Weber.2019}{spsh} 
#' inferred from matching measured soil water retention and hydraulic conductivity data against the constrained van Genuchten model parameters. 
#' The regression of \code{alf1} and \code{Ks}, and \code{(n-1)} was done in the \code{log[10])}-transformed space,
#' and \code{Kncs} is a fixed value of 10^-1.72.
#'  
#' @return
#' \tabular{lll}{
#'      \code{thsnc}\tab{Saturated water content of the non-capillary part.}\cr
#'      \code{thsc}\tab{Saturated water content of the capillary part.}\cr
#'      \code{alf}\tab{Shape parameter (cm^-1)}\cr
#'      \code{n}\tab{Shape parameter (-)}\cr
#'      \code{Ksc}\tab{Saturated hydraulic conductivity of the capillary part [cm day-1]}\cr
#'      \code{tau}\tab{Shape parameter (-)}\cr
#'      \code{Ksnc}\tab{Saturated hydraulic conductivity of the non-capillary part [cm day-1]}\cr
#'      \code{a}\tab{slope of the non-capillary unsaturated conductivity [ - ]}\cr
#'      \code{h0}\tab{anker point at which the water content is 0}
#'      }
#' @author Efstathios Diamantopoulos , \email{ed@plen.ku.dk}
#' @author Tobias KD Weber , \email{tobias.weber@uni-hohenheim.de}
#' @seealso \code{\link[spsh]{shypFun}}
#' @examples
#' p      <- c(0.08, 0.42, 0.01, 1.5, 100, 0.5)
#' result <- ptf.vG2BW(p)
#' 
#' @export
#' 
ptf.vG2BW <- function(x) {
      stopifnot(is.vector(x))
      #
      ## Purpose parameter transfer function from van Genuchten Muaelem to Weber et al.
      #
      #
      
      #
      ## Arguments
      #
      #     x     num         vector with numerical parameter values
      #           1. theta r
      #           2. theta_s
      #           3. alf1
      #           4. n1
      #           5. Ks
      #           6. tau
      
      
      # Table 3 in Weber et al. 2020, submitted
      M.t <- rbind("ptf.thr"   = c(1.285,  -1.58E-03	),
                   "ptf.ths"   = c( 0.993,  1.89E-03  ), 
                   "log10alf1" = c(0.986,  -2.06E-02	), 
                   "log10n-1"  = c(0.933,  6.42E-02	 ),
                   "log10Ks"   = c(1.060,  1.16E-01	 ),
                   "tau"       = c(1.833,  2.95E-02	 ))
      
      # assign column names, defining order of input
      colnames(M.t) <- c("slope", "intercept")
      # transfer functions
      ptransfit <- c(function(x) x, function(x) x, log10, function(x) log10(x -1),
                     log10, function(x) x)
      pretransfit <- c(function(x) x, function(x) x, function(x) 10^x, 
                       function(x) 10^x + 1, function(x) 10^x, function(x) x)
      # transform 
      x_trans <- mapply(function(f, x) f(x), ptransfit, x)
      # apply PTF
      x.FM_trans <- c(M.t[, 1] * x_trans + M.t[, 2])
      # backtransform
      x.FM <- mapply(function(f, x) f(x), pretransfit, x.FM_trans)
      # add Ksnc
      Ksnc_med = 10^-1.72
      # brind results together
      x.FM.return <- c(thnsc = x.FM[1], thsc = (x.FM[2] - x.FM[1]), 
                       alf = x.FM[3], n = x.FM[4], Ksc = x.FM[5], 
                       tau = x.FM[6], Ksnc =  Ksnc_med, a = 1.5, h0 = 6.8)
      # assign tau < 0 as 0
      if(x.FM.return["tau"]<0){x.FM.return["tau"]=0}
      if(x.FM.return["thnsc"]<0){x.FM.return["thnsc"]=0}
      #return
      return(x.FM.return)
      
      
      # Slope and Intercept
      # old version 1.0.4
      # M.t <- rbind("ptf.thr" = c(1.0024,0.03780),
      # "ptf.ths" = c(0.9697, 0.00564),
      # "log10Ks" = c(1.0280,-0.12968),
      # "log10alf1" = c(0.8557,-0.29466),
      # "log10n-1" = c(0.7178,0.18336),
      # "tau" = c(0.8418, 0.61881))
      # colnames(M.t) <- c("slope", "intercept")
      # 
      # ptransfit    <- c(function(x)x, function(x)x,    log10,        function(x)log10(x-1),      log10     , function(x)x)
      # pretransfit  <- c(function(x)x, function(x)x, function(x)10^x, function(x)10^x+1    , function(x)10^x, function(x)x)
      # 
      # x_trans <- mapply(function(f, x) f(x), ptransfit, x)
      # 
      # x.FM_trans <- c(M.t[,1]*x_trans + M.t[,2])
      # 
      # x.FM <- mapply(function(f, x) f(x), pretransfit, x.FM_trans)
      # 
      # w = .01
      # 
      # x.FM.return <- c("thr" = x.FM[1],
      #                  "ths" = (x.FM[2]-x.FM[1]),
      #                  "alf1" = x.FM[3], 
      #                  "n1" = x.FM[4],
      #                  "Kcs" = x.FM[5]*(1-w),
      #                  "tau" = x.FM[6],
      #                  "Kncs" = x.FM[5]* w,
      #                  "a" = 1.5,
      #                  "h0" = 6.8)
      # 
      # return(x.FM.return)      
}
