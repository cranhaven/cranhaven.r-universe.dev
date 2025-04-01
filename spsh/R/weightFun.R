#' Specification of Weights for the Data Groups Retention Data and Conductivity Data.
#' @description Weights can be fixed to suggested standards, fixed by the user, or estimated as additional nuisance parameters.
#' @param weightmethod Character specifying the method of selecting model weights, cd Details.
#' @param retdata A dataframe or matrix with 2 columns. The first with pressure head values in [cm] and the second with volumetric water contents in [cm cm-3].
#' @param condata A dataframe or matrix with 2 columns. The first with pressure head values in [cm] and the second with hydraulic conductivity values log10[cm d-1].
#' @param parL Defaults to \code{NA} has to be provided if \code{weightmethod == "est1"} . See Details of (\link[=shypEstFun]{shypEstFun} for explanation of \code{parL}).
#' @details Character specifying \code{weightmethod}
#' \tabular{lll}{
#'       \code{user}\tab{user defined weights}\cr
#'       \code{none}\tab{no weights are considered, i.e. no measurement error assumed}\cr
#'       \code{range}\tab{rescaling (normalization of observations to the intervall [0,1]}\cr
#'       \code{fix1}\tab{fixed scalar weight for THETA is 0.05^2 and weight for log10K is 1 }\cr
#'       \code{fix2}\tab{vector with the length of number of observations as given in \code{retdata} and \code{condata} are given, fixed to weight for THETA is 0.05^2 and weight for log10K is 1 }\cr
#'       \code{est1}\tab{Two scalar model weights (sigma^-2) are treated as free parameters to be estimated by inversion, one for \code{THETA} and one for \code{log10K}}\cr
#' }
#' If \code{weightmethod} is set to \code{est1} and \code{parL} is given as an extra argument, the function returns a list wich is concatenated to the \code{parL} used in \code{shypEstFun}
#' providing extra information on the nuisance parameters. Alternatively, \code{parL} can be passed as an argument to \code{shypEstFun} directly, accounting for the two additional nuisance parameters at the end of the respective vectors.
#' @return The function returns a \code{list} of \code{weights} as specified through \code{weightmethod}.
#' @export
#'
#' @examples
#' # Example 1 | fixed weights
#' weight.fix.L <- weightFun("fix1") 
#' 
#' # Example 2 | range of measure data
#' data(shpdata1)
#' 
#' wrc <- shpdata1$TS1$wrc
#' hcc <- shpdata1$TS1$hcc
#' # Remove NAs
#' hcc <- shpdata1$TS1$hcc[!is.na(shpdata1$TS1$hcc[,1] ),]
#' weight.fix.L <- weightFun("range", wrc, hcc) 
#' 
weightFun <- function(weightmethod = "fix1", retdata, condata, parL = NA){
         # 
      if(weightmethod == "est1"){
            {stopifnot(!is.na(parL))}
      }
      
          if(weightmethod == "range"){
                    weight <- list("wth" = max(retdata[,2]) - min(retdata[,2]), "wKh" = max(condata[,2]) - min(condata[,2]))
          }
          
          # NO WEIGHTING
          if(weightmethod == "none"){
                    weight <- list("wth" = 1, "wKh" = 1)
          }
          
          # MEASUREMENTS PREDEFINED DUE TO A FIXED MEASUREMENT UNCERTAINTY Peters and Durner (2008)
          if(weightmethod == "fix1"){
                    weight <- list("wth" = 0.0025, "wKh" = 1)
          }
          
          # MEASUREMENTS PREDEFINED DUE TO A FIXED MEASUREMENT UNCERTAINTY
          if(weightmethod == "fix2"){
                    weight <- list("wth" = c(rep(0.05, dim(retdata)[1])), "wKh" = c(rep(1,dim(condata)[1])))
          }
          
          # PARAMETERS ESTIMATED
          if(weightmethod == "est1"){
                    
                    p <- parL[[1]]
                    psel <- parL[[2]]
                    plo <- parL[[3]]
                    pup <- parL[[4]]
                    
                    weight <- NULL
                    # add 
                    pstat    <- c(sdth = 0.1 , sdKh = 0.1)
                    pstatsel <- c(   1       ,    1   )
                    pstatlo  <- c(   1e-3    ,    1e-3   )
                    pstatup  <- c(   1e0     ,    1e1    )
                    
                    p    <- c(p  , pstat   )
                    psel <- c(psel, pstatsel)
                    
                    plo <- c(plo, pstatlo)
                    pup <- c(pup, pstatup) 
                    
                    plo[which(psel == 0)] <- p[which(psel == 0)]
                    pup[which(psel == 0)] <- p[which(psel == 0)]
                    
                    # ptrans   <- c(ptrans, function(x)x, function(x)x)
                    # pretrans <- ptrans  
                    # 
                    # ptransfit   <- c(ptrans, log10, log10)
                    # pretransfit <- c(pretrans, function(x)10^x, function(x)10^x)
                    # 
                    # ptrans[which(psel == 1)]   <- ptransfit[which(psel == 1)] 
                    # pretrans[which(psel == 1)] <- pretransfit[which(psel == 1)] 
                    
          }

          if(weightmethod != "est1"){ 
                    return(weight)   
          } else {
                    return(list("parL" = list("p" = p, "psel" = psel, "plo" = plo, "pup" = pup)))
                    }
}


