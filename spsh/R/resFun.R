#' Calculation of the Objective Function Value
#' @description Contains the objective functions to calculate (weighted) sum of squared residuals or likelihoods.
#' The assumption made is that the residuals are identically, independantly and normally distributed.
#' The normal distribution of the model residuals is standardly given with mean = 0, and variance = 1.if weighting is not considered.
#' There are three methods to consider weights: a) fixed skalar values for each data type, b) a vector of weights for each 
#' data type. The vector has to have the same length as the vector of the data type.
#' c) It is possible to estimate the 
#' @param p Vector of model parameters handed used to calculate the soil hydraulic property model values in \link[spsh]{shypFun}.
#' Depends on \code{shpmodel} and the pressure head values specified in \code{retdata} and \code{condata} 
#' @param shpmodel Character identifying the soil hydraulic property model. See \link[spsh]{shypFun}.
#' @param retdata A dataframe or matrix with 2 columns. The first with pressure head values in [cm] and the second with volumetric water contents in [cm cm-3].
#' @param condata A dataframe or matrix with 2 columns. The first with pressure head values in [cm] and the second with hydraulic conductivity values in log10[cm d-1].
#' @param pretrans A vector to back transform the parameters before the soil hydraulic property function values calculated.
#' @param weight Specification of weight method. See \link[spsh]{weightFun}.
#' @param method 
#'  \tabular{lll}{
#'     \code{rss}\tab{Default for the optimisation algortihm DEoptim. resFun returns skalar sum of squared (weighted) residuals.} \cr
#'     \code{res}\tab{Weighted residuals are computed and returnd. Required for post hoc analyses.} \cr
#'     \code{-2logLik}\tab{-2 * log-likelihood is returned.} \cr
#' }
#' @param trim.query Default \code{FALSE}. If a trimodal soil hydraulic property function is used, this has to be specified by setting the argument to (\emph{TRUE}) which ensures the sum of modal \code{weights == 1}.
#' @param ivap.query Default is \code{FALSE}, and no \emph{ivap} method is specified. See \link[spsh]{KvapFun}
#' @param hclip.query Implemented purely for future compatability.
#' @param parL Defaults to \code{NULL}, only inserted for compatbility with \code{modMCMC} used in \link[spsh]{shypEstFun}. \code{modMCMC}, only
#' handled parameters which are estimated, other model parameters need to be passed through parL. See Details of \link[spsh]{shypEstFun}.
#' 
#' @details  Model errors may be specified or estimated as nuisance parameters weighting the data classes. In case the model error !=1, the output statistics are weighted accordingly. 
#' 
#' @return Returns scalar of sum of squared (weighted) residuals or vector of weighted residuals, as specified by 
#'  \tabular{lll}{
#'     \code{user}\tab{user defined weights}\cr
#'     \code{none}\tab{no weights are considered, i.e. no measurement error assumed}\cr
#'     \code{range}\tab{rescaling (normalization of observations to the intervall [0,1]}\cr
#'     \code{fix1}\tab{fixed scalar weight for THETA is 1/0.05^2 and weight for log10K is 1 }\cr
#'     \code{est1}\tab{Two scalar model weights 1/sigma_i^2 are treated as free parameters to be estimated by inversion, one for \code{THETA} and one for \code{log10K}. Only simultaneously estimateable}\cr
#'     } 

#' @examples
#' # load data
#' data("shpdata1")
#' 
#' # observations
#' retdata <- shpdata1$LFH1$wrc[!is.na(shpdata1$LFH1$wrc[,1]),]
#' condata <- shpdata1$LFH1$hcc
#' 
#' # 7 - resFun ------------------------------------------------------------
#' # soil hydraulic property model parameters, van Genuchten-Mualem
#' p <- c("thr" = 0.16, "ths" = 0.46, "alf1" = 0.03, "n1" = 1.42, "Ks" = 26, "tau" = .5)
#' 
#' # calculate weighted residuals
#' wres <- resFun(p, retdata = retdata, condata = condata, pretrans = NULL,
#'                weight = list("wth" = 0.0025, "wKh" = 1), method = "res", trim = FALSE)
#' 
#' ## residuals of the soil water retention curve [-]
#' theta.wres <- wres[1:dim(retdata)[1]]
#' 
#' ## residuals of the log10 hydraulic conductivity curve [cm/d]
#' log10K.wres <- wres[(dim(retdata)[1]+1) : length(wres)]
#' 
#' @export
#'
#'
resFun <- function(p, shpmodel = "01110", retdata = NULL, condata = NULL, pretrans = NULL, 
                   weight = NULL, method = "rss", 
                   trim.query = FALSE, ivap.query = NULL, hclip.query = FALSE, parL = NULL) {
          #
          ####      ARGUMENTS
          #
          #         p         - vector of num transformed soil hydraulic model parameters
          #         retdata   - data.frame of num observations of retention data first column: pressure heads, second column: volumetric water content [-]
          #         condata   - data.frame of num observations of hydraulic conductivity data, data.frame, first column: pressure heads, second column: log10K [cm/d]
          #         pretrans  - list of functions with same length as parameter vector to back-transform the parameters.
          #         weight    - List of 2 vectors with weights (inverse of variance, describing e.g. the measurement erro)  
          #         method    - chr character specifying the adopted fitting method.
          #         trim      - trimodal query
          
          # assigning fixed paraemeters to the parameter vector
          
          if(!is.null(pretrans)){ 
                    p <- transFun(p, pretrans)
          }
          
          if(!is.null(parL)){ 
                    parL$p[which(parL$psel == 1)] <- p
                    p <- parL$p
          }
          
          # assigning the weights if they are estimated scalars and remove the parameters from the vector
          if(is.null(weight)){
                    np <- length(p)
                    sdth <- p[np-1];
                    sdKh <- p[np];
                    p <- p[-c(np-1, np)]
          }
          
          # query if trimodal function is used: if TRUE, the sum of weights is calculated, to verify sum == 1
          if(trim.query == TRUE){
                    w1 <- p[5]
                    w2 <- p[8]
                    if((1-w1-w2) <= 0) {
                              
                              if(method == "res") {
                                        return(rep(1000, dim(retdata)[1] + dim(condata)[1]))
                              } 
                              
                              if(method == "rss") {
                                        return(1e32)
                              }
                              
                              if(method == "-2logLik")
                                        return(1e32)
                    }
          }      
          
          # calculation of weights if they are fix
          if(!is.null(weight) & method == "rss" ) {
                    sdth <- 1/weight[[1]];
                    sdKh <- 1/weight[[2]];
          }
          
          if(!is.null(weight) & method == "res" ) {
                    sdth <- 1/weight[[1]];
                    sdKh <- 1/weight[[2]];
          }
          
          if(!is.null(weight) & method == "-2logLik" ) {
                    sdth <- weight[[1]];
                    sdKh <- weight[[2]];
          }
          
          # CALCULATE RESIDUALS
          yth <- retdata[,2]
          yhatth <- shypFun(p, h = 10^retdata[,1], shpmodel, ivap.query)[['theta']] 
          
          yKh <- condata[,2] 
          yhatKh <- log10(shypFun(p, h = 10^condata[,1], shpmodel, ivap.query)[['Kh']])
          
          # if(!isFALSE(ivap.query)) {
          #           if(!isTRUE(ivap.query)) {ivap.query <- "MQ61"}
          #           ifelse(ivap.query%in%c("MC", "TPM","TPEM"), retFun.query <- shypFun, retFun.query <- NA)
          #           yhatKh <- log10(10^yhatKh + KvapFun(p, por = p[2], retFun = retFun.query, theta = yhatth, model = ivap.query, Temp = 20, m = 3, pF = 10^condata[,1], output = "nolog10"))
          # }
          
          # OUTPUT
          if(method == "rss") {
                    res_th <- sdth * (yth - yhatth)
                    res_Kh <- sdKh * (yKh - yhatKh)
                    
                    rss <- drop(crossprod(res_th) + crossprod(res_Kh))
                    if(is.finite(rss) == TRUE) return(rss)
                    else(return(10^15))
          }
          
          if(method == "-2logLik"){
                    
                    # normal distribution
                    loglikth <- logLikFun.norm(yth, yhatth, sdth)
                    loglikKh <- logLikFun.norm(yKh, yhatKh, sdKh)
                    
                    if(sum(is.finite(c(loglikth, loglikKh))) == 2){
                              return(-2 * (loglikth + loglikKh))
                    }
                    if(sum(is.finite(c(loglikth, loglikKh))) != 2){
                              return(10^10)
                    }
          }
          
          if(method == "res") {
                    res_th <- sdth * (yth - yhatth)
                    res_Kh <- sdKh * (yKh - yhatKh)
                    res <- c(res_th,res_Kh)
                    if(sum(!is.finite(res)) == 0) return(res)
                    else(return(rep(10^6, length(res))))
          }
} 
