#' Goodness-of-fit and Information Criteria
#' 
#' @description Calculates goodness-of-fit criteria and the likelihood-based Akaike 
#' and Bayesian Information Criterion based on a given parameter set,
#' typically from the estimation procedure.
#' 
#' 
#' @param phat Vector of non-transformed (back-transformed) model parameters after estimation, i.e. the best fit or maximum likelihood estimate.
#' @param shpmodel Character specifying the soil hydraulic property model.
#' @param retdata Dataframe or matrix with 2 columns. The first with pressure head values in \ifelse{html}{\out{log<sub>10</sub>}}{\eqn{log_10}} [cm], i.e. pF values, and the second with volumetric water contents in [cm cm-3].
#' @param condata Dataframe or matrix with 2 columns. The first with pressure head values in \ifelse{html}{\out{log<sub>10</sub>}}{\eqn{log_10}} [cm], i.e. pF values, and the second with hydraulic conductivity values \ifelse{html}{\out{log<sub>10</sub>}}{\eqn{log_10}} [cm d-1].
#' @param weight List of the model residual weights used or estimated by the parameter estimation scheme, to calculate the weighted statistical analyses.
#' @param psel Vector specifying the selected parameters for the parameter estimation from \code{parL}.
#' @param ivap.query  Specification of \emph{ivap} method, if FALSE selected, no isothermal vapour conductivity is consideredSee \code{Details} 
#' @param hclip.query Implemented purely for future compatability. Currently no use. See \code{Details}
#'
#' @details Output for data groups.
#'  \tabular{llll}{
#'     \code{th}\tab      {\code{list} with goodness of fit statistics for the retention curve \emph{see below}}\cr
#'     \code{logKh}\tab   {\code{list}with output same as \code{th} but for the \ifelse{html}{\out{log<sub>10</sub>}}{\eqn{log_10}} fitted conductivity curve}\cr
#'     \code{combined}\tab{\code{list} with \code{AIC}, \code{AICc}, and \code{BIC} calculated for the multi-objective function if arguments \code{retdata} and \code{condata} are both \code{!NULL}}\cr
#'    }
#' Statistical analyses of the inverse modelling results.
#'   \tabular{lll}{
#'     \code{me}\tab{mean (weighted) error}\cr
#'     \code{mae}\tab{mean absolute (weighted) error}\cr
#'     \code{mse}\tab{mean squared (weighted) error}\cr
#'     \code{rss}\tab{sum of squared (weighted) errors}\cr
#'     \code{rmse}\tab{root mean squared (weighted) error}\cr
#'     \code{AIC}\tab{Akaike Information Criteria}\cr
#'     \code{AICc}\tab{corrected Akaike Information Criteria}\cr
#'     \code{BIC}\tab{Bayesian Information Criteria}\cr
#'     \code{m}\tab{number of observations}\cr
#'             }
#' @references 
#' \insertRef{Hoege.2018}{spsh}
#' 
#' @author Tobias KD Weber , \email{tobias.weber@uni-hohenheim.de}
#'
#' @examples
#' data("shpdata1")
#' retdata <- shpdata1$TS1$wrc
#' condata <- shpdata1$TS1$hcc
#' condata <- condata[!is.na(condata[,1]),]
#' # Parameter list
#' parL <- list("p" = c("thr"= 0.05, "ths" = 0.45, "alf1" = 0.01, "n" = 2, "Ks" = 100, "tau" = .5),
#'              "psel" = c(1, 1, 0, 1, 1, 1),
#'              "plo" = c(0.001 , 0.2, 0.001, 1.1, 1, -2),
#'              "pup" = c(0.3, 0.95, 1, 10, 1e4, 10)
#'              )
#' # Calulation of the goodness of fit.
#' gofL <-gofFun(parL$p, shpmodel = "01110", retdata = retdata, condata = condata, 
#'               weight = weightFun(weightmethod = "fix1"), parL$psel, 
#'               ivap.query = NULL, hclip.query = FALSE)
#' @importFrom Rdpack reprompt

#' @export
gofFun <- function(phat, shpmodel = "01110", retdata = NULL, condata = NULL,
                                        weight, psel, ivap.query = NULL, hclip.query = FALSE){
          
          # initialise two lists
          th <- vector("list")
          logKh <- vector("list")
          combined <- vector("list")                    # multi-objective goodness-of-fit
          
          #  - check for trimodal soil hydraulic property model. Included for future functionality
          ifelse(shpmodel%in%c("01310"), trim.query <- TRUE, trim.query <- FALSE)
          
          # calculate vectors of residuals
          res.L <- resFun(phat, shpmodel, retdata, condata, 
                          pretrans = NULL, weight, method = "res", 
                          trim.query, ivap.query, hclip.query) # residuals of THETA and log10Kh
          
          n <- sum(psel)                             # number of estimated SHP model parameters
          
          # 1. calculate for th
          if(!is.null(condata)){
                    th$res <- res.L[1:dim(retdata)[1]]       # Residuals of THETA
                    r <- th$res
                    m = length(r);
                    th$me   = mean(r);                       # mean (weighted) error
                    th$mae  = mean(abs(r));                  # mean average (weighted) error
                    th$mse  = sum(r^2)/m                     # mean squared (weighted) error
                    th$rss  = t(r)%*%r;                      # sum of squared (weighted) residuals
                    th$rmse = sqrt(th$rss/m);                # root mean squared (weighted) error
                    th$logLik = log(th$rss/m)
                    th$AIC  = -2 * th$logLik + 2 * n;
                    th$AICc = ifelse((m-n-1) <= 0, NaN, th$AIC + 2*n*(n+1)/(m-n-1));
                    th$BIC  = -2 * th$logLik+n*log(m);
                    th$m    = m;                             # Number of THETA-H-pairs
          }
          
          # 2. calculate for Kh
          if(!is.null(condata)){
                    logKh$res <- res.L[(dim(retdata)[1]+1) : (dim(retdata)[1]+dim(condata)[1]) ]             # Residuals of log10Kh
                    r = logKh$res
                    m = length(r);
                    logKh$me   = mean(r);                    # mean (weighted) error
                    logKh$mae  = mean(abs(r));               # mean average (weighted) error
                    logKh$mse  = sum(r^2)/m;                 # mean squared (weighted) error
                    logKh$rss  = t(r)%*%r;                   # sum of squared (weighted) residuals
                    logKh$rmse = sqrt(logKh$rss/m);          # root mean squared (weighted) error
                    logKh$logLik = log(logKh$rss/m)
                    logKh$AIC  = -2*logKh$logLik + 2*n;
                    logKh$AICc = ifelse((m-n-1) <= 0, NaN, logKh$AIC + 2*n*(n+1)/(m-n-1));
                    logKh$BIC  = -2 * logKh$logLik + n*log(m);
                    logKh$m    = m;                          # Number of LOG10K-H-pairs
          }
          
          
          # returns
          if(!is.null(retdata) & !is.null(condata)){
                    combined$AIC  <- -2 * (log(th$rss/m) + log(logKh$rss/m))  + 2 * n;
                    combined$AICc <- ifelse((m-n-1)<=0,NaN,combined$AIC + 2 * n * (n+1) / (m-n-1));
                    combined$BIC  <- -2 * (log(th$rss/m) + log(logKh$rss/m)) + n * log(m)
                    
                    return(list("th" = th, "log10Kh" = logKh, "combined" = combined))
          }
          
          if(!is.null(retdata) & is.null(condata)){
                    return(list("th" = th, "log10Kh" = NULL))
          }
          
          if(is.null(retdata) & !is.null(condata)){
                    return(list("th" = NULL, "log10Kh" = logKh))
          }
}



