#' Creates Parameter Transformation and Backtransformation Rules for the Estimation Procedure
#' @details This function is intended for the function \code{shypEstFun} so that lists with set rules for the transformation and back-transformation of the soil hydraulic model
#' parameters are enabled. In general, the following rules apply \code{log10} transformation for the model parameters \eqn{\alpha_i}, \code{n_i-1}, \code{K_s}, \code{K_sc}, \code{K_snc}.
#'
#' @param parL a list with 4 numeric vectors specifying:
#' \tabular{lll}{
#'     \code{p}\tab{Vector of model parameters, has to coincide with the chosen soil hydraulic property model. If \code{weightmethod == est1} then two additional nuisance parameters, need to be specified and concatenated to the vector of soil hydraulic property model parameters, a first, for THETA and a second for log10K)}\cr
#'     \code{psel}\tab{vector identifying which parameters are to be estimatedmodel parameters, has to coincide with the chosen soil hydraulic property model}\cr
#'     \code{plo}\tab{vector of lower bounds (non-transformed parameter boundaries)}\cr
#'     \code{pup}\tab{vector of upper bounds (non-transformed parameters boundaries)}\cr
#' }
#' @param shpmodel A string specifying the selected soil hydraulic property model.
#' @param weightmethod A string specifying the selected weigthing method, if weightmethod == "est1" is TRUE, then \code{parL} is modified to account for nuisance parameters).
#' @details The function is meant for internal use in \code{shypEstFun}.
#' @return a list of two lists. One of the sub-lists is \code{parL} but with transformed parameters, and the second, \code{transL} with model specific transformation and back-transformation rules.
#' @author Tobias KD Weber , \email{tobias.weber@uni-hohenheim.de}
#' @export
#'
#' @examples
#' 
# List of model paramaters
#'parL <- list("p" = c("thr"= 0.05, "ths" = 0.45, "alf1" = 0.01, "n" = 2, "Ks" = 100, "tau" = .5),
#'             "psel" = c(1, 1, 0, 1, 1, 1),
#'             "plo" = c(0.001 , 0.2, 0.001, 1.1, 1, -2),
#'             "pup" = c(0.3, 0.95, 1, 10, 1e4, 10))
#'
#'# transformation and back-transformation of parameter vectors
#'for(k in c("p", "plo", "pup")){
#'      for (j in c("none")){
#'            parL.trans <- transBoundFun(parL, shpmodel = "01110", weightmethod = j)
#'            
#'            p_trans <- transFun(parL[[k]], parL.trans$transL$ptrans)
#'            p_retrans <- transFun(p_trans, parL.trans$transL$pretrans)
#'            
#'            stopifnot(sum(p_retrans != parL[[k]])==0)
#'      }
#'}
#'
#'
transBoundFun <- function(parL, shpmodel, weightmethod){
          
          
          #### AUXHILLARY VARIABLES 
          p    <- parL[[1]]           
          psel <- parL[[2]]
          plo  <- parL[[3]]
          pup  <- parL[[4]]
          
          pretransbase <- ptransbase <- rep(list(function(x)x), length(p))
          
          switch(gsub("FM", '',shpmodel),
                 "01110" = { 
                           # TRANFORMATION RULES OF PARAMETERS FOR van Genuchten-Mualem m = 1-1/n constrained MODEL, shpmodel = 01110
                           ptransfit    <- c(function(x)x, function(x)x,    log10,        function(x)log10(x-1),      log10     , function(x)x)
                           pretransfit  <- c(function(x)x, function(x)x, function(x)10^x, function(x)10^x+1    , function(x)10^x, function(x)x)

                 },
                 "01210" = {
                           ptransfit    <- c(function(x)x, function(x)x,    log10,        function(x)log10(x-1), function(x)x,    log10,        function(x)log10(x-1),      log10     , function(x)x)
                           pretransfit  <- c(function(x)x, function(x)x, function(x)10^x, function(x)10^x+1    , function(x)x, function(x)10^x, function(x)10^x+1    ,function(x)10^x, function(x)x)
                 },
                 "01310" = {  ptransfit    <- c(function(x)x, function(x)x,    log10,        function(x)log10(x-1),function(x)x,    log10,        function(x)log10(x-1), function(x)x,    log10,        function(x)log10(x-1),      log10     , function(x)x)
                              pretransfit  <- c(function(x)x, function(x)x, function(x)10^x, function(x)10^x+1    ,function(x)x, function(x)10^x, function(x)10^x+1    , function(x)x, function(x)10^x, function(x)10^x+1    ,function(x)10^x, function(x)x)
                 },
                 
                 "02110" = {
                           # TRANFORMATION RULES OF PARAMETERS FOR Kosugi-Mualem m = 1-1/n constrained MODEL, shpmodel = 01110
                           ptransfit    <- c(function(x)x, function(x)x,    log10,           log10           ,      log10     , function(x)x)
                           pretransfit  <- c(function(x)x, function(x)x, function(x)10^x, function(x)10^x    , function(x)10^x, function(x)x)
                           
                 },
                 "03110" = { 
                           # TRANFORMATION RULES OF PARAMETERS FOR Fredlund-Xing-Mualem m = 1-1/n constrained MODEL, shpmodel = 01110
                           ptransfit    <- c(function(x)x, function(x)x,    log10,        function(x)log10(x-1),      log10     , function(x)x)
                           pretransfit  <- c(function(x)x, function(x)x, function(x)10^x, function(x)10^x+1    , function(x)10^x, function(x)x)
                           
                 },
                 stop("Enter a valid code for a soil hydraulic property model")
          )
          
          
          #
          # query if FM selected and add extra parameters
          if(grepl("FM", shpmodel, fixed = TRUE)){
                    
                    ptransfit <- c(ptransfit, log10, function(x)x ,function(x)x)
                    pretransfit <- c(pretransfit,  function(x)10^x, function(x)x ,function(x)x)
          }


          if(!is.list(weightmethod)) {
                    
                    
                    if(weightmethod == "est1"){
                              
                              ptransfit   <-  c(ptransfit, log10, log10)
                              pretransfit <-  c(pretransfit, function(x)10^x, function(x)10^x)
                              
                              # check if user has provided additional parameters and bounds for the estimated standard deviation
                              # stopifnot(length(p) == ptransfit & length(psel) == ptransfit & length(plo) == ptransfit & length(pup) == ptransfit )
                              
                              # stopifnot(sum(sapply(parL, length))/length(parL) == length(ptransfit))
                              
                    }
          }
          
          ptransbase[which(psel == 1)]     <- ptransfit[which(psel == 1)] 
          pretransbase[which(psel == 1)]   <- pretransfit[which(psel == 1)] 
          
          # Modify the boundaries to exakt value for non selected parameters, only necessary if assignment in resFun exact to psel parameters
          plo[which(psel == 0)] <- unlist(p)[which(psel == 0)]
          pup[which(psel == 0)] <- unlist(p)[which(psel == 0)]
          
          return(list("parL" = list("p" = p, "psel" = psel, "plo" = plo, "pup" = pup), "transL" = list("ptrans" = ptransbase, "pretrans" = pretransbase )))
          
}
