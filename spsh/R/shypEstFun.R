#' Wrapper function for the Estimation of Soil Hydrologic Property Model Parameters
#' @description Estimates model parameters of implemented soil hydraulic property functions. 
#' This function sets up the parameter estimation, given a set of arguments, and enables minimisation of (weighted) 
#' sum of squared residuals, assuming independent and identically distributed model residuals.
#' More information on the options is given in the \emph{Details}
#' @param shpmodel 
#'  \tabular{lll}{Character specifying the soil hydraulic property model. Currently valid models as documented in \link[spsh]{shypFun} and are:\cr
#'       \code{01110} constrained unimodal van Genuchten-Mualem.\cr 
#'       \code{01210} constrained bimodal van Genuchten-Mualem.\cr
#'       \code{01310} constrained trimodal van Genuchten-Mualem.\cr
#'       \code{02110} unimodel Kosugi 2 parametric-Mualem model (Kosugi, 1996)\cr
#'       \code{03110} unimodel Fredlund-Xing-Mualem model, with the contraint of m = 1-1/n (Fredlund D.G., and A. Xing, 1994)\cr
#'       }
#' @param parL \code{list} of 4 vectors with named vectors, the order in the list is sensitive.
#' \tabular{lll}{
#'       \code{p}\tab{vector with length \code{l} of model specific initial parameters, has to coincide with the chosen soil hydraulic property model.}\cr
#'       \code{psel}\tab{vector with length \code{l} identifying which parameters are to be estimated}\cr
#'       \code{plo}\tab{vector of lower bounds (non-transformed parameter boundaries)}\cr
#'       \code{pup}\tab{vector of upper bounds (non-transformed parameters boundaries)}\cr
#' }
#' @param retdata A dataframe or matrix with 2 columns. The first with log10 values of pressure head values in [cm] and the second with volumetric water contents in [cm cm-3].
#' @param condata A dataframe or matrix with 2 columns. The first with log10 values of pressure head values in [cm] and the second with hydraulic conductivity values log10[cm d-1].
#' @param ivap Specification if isothermal vapour conductivity after Saito et al. (2006) is accounted, defaults to \code{NULL} and no isothermal vapour conducitvity is considered. See Details.
#' @param hclip Implemented for future development reasons and is not yet functional. Specification if the hydraulic conductivity model should be 'clipped', i.e. constrained to a maxium pore diamater as introduced by Iden et al. (2015), defaults to \code{FALSE}.
#' @param weightmethod Specification of weight method. The implemented methods are
#'   \tabular{lll}{
#'       \code{none}\tab{no weights are considered, i.e. no measurement error assumed}\cr
#'       \code{range}\tab{normalization of observations to the intervall [0,1]}\cr
#'       \code{fix1}\tab{fixed scalar weight for THETA is 0.05^2 and weight for log10K is 1 }\cr
#'       \code{est1}\tab{Two scalar model weights (\$1/sigma^2) are treated as free parameters to be estimated by inversion, one for \code{THETA} and one for \code{log10K}}\cr
#'       }
#' Alternatively, a list of vectors can be provided specifying the user given model weights (\$1/sigma^2). Either as skalar for each data class, or a vector with the same length as the number of data points given for each of the measurements in the respective data class.
#' The length of the list has to coincide with the data groups.
#' @param LikModel Specification of inverse modelling type. Has to be specified but implemented for future compatability)
#' \tabular{lll}{
#'     \code{rss}\tab{Default for the optimisation algorithm \code{DEoptim}. \code{resFun} returns skalar sum of squared (weighted) residuals}\cr
#'     \code{-2loglik}\tab{Specified if \code{ALG == -2*log-likelihood} value, which is minimised assuming Gaussian and i.i.d (weighted) residuals}\cr
#' }
#' 
#' @param ALG Select global optimisation algorithm or a Markov chain Monte Carlos (MCMC) sampler.
#' \tabular{lll}{
#'    \code{DE}\tab{Default for the optimisation algortihm DEoptim. resFun returns a skalar sum of squared (weighted) residuals if \code{LikModel == "rss"}. }\cr
#'    \code{modMCMC}\tab{Default for the DRAM (Delayed Rejection Adaption Metropolis) algrothim implemented in \code{modMCMC} of the FME package. 
#'          resFun returns a skalar -2loglik and \code{LikModel = "-2logLik"} has to be specified.}\cr
#'    }
#' @param set.itermax Integer specifying the maximum number of iterations \code{default = 200}.
#' 
#' @param ALGoptions A list with named entries setting the algorithm options. Each list element name is required to be identical with the names
#' as documented in the respective algortihm help \link[DEoptim]{DEoptim.control} and \link[FME]{modMCMC}. \cr
#' \code{set.itermax} overrides the maximum iterations argument. 
#' 
#' @param lhs.query default \code{FALSE}, \code{TRUE} will produce a Latin Hypercube Sample for the initial population when using \code{DEoptim}.
#' 
#' @details Several in-built methods for weighting the (multi-) objective function residuals are available, they may be specified, or estimated as nuisance parameters for the two data groups. More details see \code{weightFun}.
#'          Weights are the inverse of the squared standard deviation of the residuals (variance).
#'          
#'          Generally, soil hydraulic property model parameters are estimated as transformed parameters: log10 for alpha_i, Ks, and log10 for n_i-1, Kc, Knc
#'          
#'          For model codes in \emph{ivap} please refer to \link[spsh]{KvapFun}.
#'          
#'          Parallel computing for package \code{DEoptim} is not supported. And the optional arguments in \code{modMCMC} are not supported.
#' @return \code{list} returns the result of the optimisation algrorithm or MCMC sampler and all settings.
#' 
#' \item{settings}{a \code{list} with output of the optimisation and summary of settings:
#'    \tabular{lll}{
#'       \code{weigth}\tab{the \code{list} with weights for the retention and conductivity data. }\cr
#'       \code{parL}\tab {the \code{list} of initial and selected model parameters, and upper and lower bounds.}\cr
#'       \code{transL}\tab{list of parameter transformation rules used}\cr
#'       \code{shpmodel}\tab{the used soil hydraulic property model}\cr
#'       \code{ivap}\tab{isothermal vapour conductivity model}\cr
#'       \code{hclip}\tab{for future compatability}\cr
#'       \code{LikModel}\tab{the adopted method to calculate the objective function value}\cr
#'       \code{data}\tab {a \code{list} of 2 objects with a) retention data and b) conductivity data used for the parameter estimation.}
#' }
#' }
#' \item{out}{result of algorithm function \code{DEoptim} or \code{modMCMC}}
#' 
#' @importFrom FME modMCMC
#' @importFrom DEoptim DEoptim DEoptim.control
#' @export
#'
#' @examples
#' \dontrun{
#' data("shpdata1")
#'retdata <- shpdata1$TS1$wrc
#'condata <- shpdata1$TS1$hcc
#'condata <- condata[!is.na(condata[,1]),]
#'
#'weightmethod <- "range"
#'ivap         <- NULL
#'set.itermax  <- 1
#'LikModel     <- "rss" # ALTERNATIVE OPTION: LikModel = "-2logLik"
#'ALG          <- "DE"       # ALTERNATIVE OPTION: ALG = "modMCMC"
#'
#'parL<-list("p"=c("thr"=0.05,"ths"=0.45,"alf1"=0.01,"n"=2,"Ks"=100,"tau"=.5),
#'           "psel" = c(1, 1, 1, 1, 1, 1),
#'           "plo"= c(0.001 , 0.2 , 0.001 , 1.1, 1, -2),
#'           "pup"= c(0.3 , 0.8 , .1, 11 , 1e4, 10))
#'
#'out <- shypEstFun(shpmodel = "01110", 
#'                  parL = parL, 
#'                  retdata = retdata, condata = condata, 
#'                  ivap = ivap, 
#'                  hclip = FALSE, 
#'                  weightmethod = weightmethod,
#'                  LikModel = LikModel, 
#'                  ALG = ALG, 
#'                  set.itermax = set.itermax,
#'                  lhs.query = FALSE)
#'\dontshow{
#'}
#'\donttest{
#'data("shpdata1")
#'retdata <- ret <- shpdata1$TS1$wrc
#'condata <- con <- shpdata1$TS1$hcc
#'condata <- condata[!is.na(condata[,1]),]
#'
#'---
#'      
#' #  1 SET VARIABLES --------------------
#'#  VARIABLES FOR PLOTTING
#'{pF <- seq(-3, 6.8, length = 201)
#'h <- 10^pF
#'ticksatmin <- -2
#'tcllen <- 0.4
#'ticksat <- seq(ticksatmin,5,1)
#'pow <- ticksatmin:6
#'
#'#  VARIABLES FOR THE FITTING ALGORITHM
#'weightmethod = "range"
#'ivap = NULL
#'set.itermax = 3e1
#'LikModel = "rss" # ALTERNATIVE OPTION: LikModel = "-2logLik"
#'ALG = "DE"       # ALTERNATIVE OPTION: ALG = "modMCMC"
#'shpmodel.v <- c("01110", "01110FM") 
#'
#'plot.query = FALSE
#'no.shps <- length(shpmodel.v)
#'
#'#  initialising lists
#'out.L <- vector("list", no.shps)
#'gof.L <- vector("list", no.shps)
#'}
#'# Run comparison
#'for (i in 1:2) {
#'      shpmodel = shpmodel.v[i]
#'      # INITIAL PARAMETERS, BOUNDS, and SELECTED PARAMETERS FOR FITTING
#'      switch(shpmodel,
#'     "01110" = {
#'           
#'           # van Genuchten-Mualem Model parameters
#'           parL<-list("p"=c("thr"=0.05,"ths"=0.45,"alf1"=0.01,"n"=2,"Ks"=100,"tau"=.5),
#'                      "psel" = c(1, 1, 1, 1, 1, 1),
#'                      "plo"= c(0.001 , 0.2 , 0.001 , 1.1, 1, -2),
#'                      "pup"= c(0.3 , 0.8 , .1, 11 , 1e4, 10)
#'           )
#'     },
#'     
#'     "01110FM" = {
#'           
#'           # van Genuchten-Mualem Model parameters + BRUNSWICK MODEL
#'           parL<-list("p"=c("thr"=0.05,"ths"=0.45,"alf1"=0.01,"n"=2,"Ksc"=100,
#'                            "tau"=.5,"Ksnc"=1e-4,"a"=1.5,"h0"=6.8),
#'                      "psel" = c( 1,1, 1 ,1 , 1,1,1, 0, 0),
#'                      "plo"= c(0.001 , 0.1 , 0.001 , 1.1, 1,0,1e-6 , 1, 6.5),
#'                      "pup"= c(0.35, 0.7 , .1, 11 , 1e4,10 ,1e0, 2, 6.9)
#'           )
#'     },
#'     stop("Enter a meaningful shpmodel")
#'      )
#'      
#'      out <- shypEstFun(shpmodel = shpmodel, 
#'                 parL = parL, 
#'                 retdata = retdata, condata = condata, 
#'                 ivap = ivap, 
#'                 hclip = FALSE, 
#'                 weightmethod = weightmethod,
#'                 LikModel = LikModel, 
#'                 ALG = ALG, 
#'                 set.itermax = set.itermax
#'                 ,lhs.query = FALSE)
#'      
#'      out$model <- shpmodel.v[i]
#'      out.L[[i]] <- out
#'      
#'      
#'      #  Calculate the soil hydraulic properties for the plot
#'      if(ALG == "DE"){
#'            p <- out$out$optim$phattrans
#'      }
#'      
#'      if(ALG == "modMCMC"){
#'            p <- out$out$bestpartrans
#'      }
#'      
#'      if(weightmethod =="est1"){
#'            np <- length(p)
#'            p <- p[-c(np-1, np)]
#'            if(ALG =="modMCMC"){
#'                  parL$p[which(parL$psel==1)] <- p
#'                  p <- parL$p
#'            }
#'      }
#'      
#'      if(plot.query==TRUE){
#'            
#'            shyp.L<-shypFun(p,h,shpmodel=shpmodel.v[i],ivap.query=ivap)
#'            
#'            if(shpmodel == c("01110")){
#'                  
#'                  wrc<-shyp.L$theta
#'                  hcc<-log10(shyp.L$Kh)
#'                  
#'                  # PLOT THE WATER RETENTION CURVE
#'                  par(mfrow=c(1,2),tcl=tcllen)
#'                  plot(retdata,ylim=c(.0,.50),xlim=c(0,6.8),ylab="",xlab="",
#'                       col="darkgrey",axes=FALSE,main="WaterRetentionCurve",cex=2)
#'                  lines(log10(abs(h)),wrc,col="darkblue",lwd=2)
#'                  legend("topright",c("observed","simulated"),pch=c(1,NA),
#'                         lty=c(NA,1),lwd=2,bty="n",cex=1.3,col=c("darkgrey","darkblue"))
#'                  axis(1,at=pow,labels=parse(text=paste('10^',(pow),sep="")),tcl=tcllen)
#'                  axis(2,tcl=tcllen)
#'                  axis(4,labels=NA)
#'                  axis(3,labels=NA)
#'                  mtext("pressurehead|h|[cm]",1,cex=1.2,line=2.8)
#'                  mtext("vol.watercontent[-]",2,cex=1.2,line=2.8)
#'                  box()
#'                  
#'                  # PLOT THE MEASURED HYDRAULIC CONDUCTIVITY CURVE
#'                  plot(condata,ylim=c(-8,2),xlim=c(0,6.8),ylab="",xlab="",col="darkgrey",
#'                       axes=FALSE,main="HydraulicConductivityCurve",cex=2)
#'                  lines(log10(abs(h)),hcc,col="darkblue",lwd=2)
#'                  legend("topright",c("observed","simulated"),pch=c(1,NA),
#'                         lty=c(NA,1),lwd=2,bty="n",cex=1.3,col=c("darkgrey","darkblue"))
#'                  axis(1,at=pow,labels=parse(text=paste('10^',(pow),sep="")),tcl=tcllen)
#'                  axis(2)
#'                  axis(4,labels=NA)
#'                  axis(3,labels=NA)
#'                  mtext("log10K[cm/d]",2,cex=1.2,line=2.8)
#'                  mtext("pressurehead|h|[cm]",1,cex=1.2,line=2.8)
#'                  box()
#'                  par(mfrow=c(1,1))
#'                  
#'            }
#'            
#'            if(shpmodel == "01110FM"){
#'                  
#'                  wrc<-shyp.L$theta
#'                  wrccap<-shyp.L$thetacap
#'                  wrcnc<-shyp.L$thetanc
#'                  
#'                  hcc<-log10(shyp.L$Kh)
#'                  hcccap<-log10(shyp.L$Kcap)
#'                  hccnc<-log10(shyp.L$Knc)
#'                  hcvap<-log10(shyp.L$Kvap)
#'                  
#'                  par(mfrow=c(1,2),tcl=tcllen)
#'                  plot(retdata,ylim=c(.0,.50),xlim=c(0,6.8),ylab="",xlab="",
#'                       col="darkgrey",axes=FALSE,main="WaterRetentionCurve",cex=2)
#'                  lines(log10(h),wrccap,col="brown",lwd=2)
#'                  lines(log10(h),wrcnc,col="brown",lwd=2,lty=2)
#'                  lines(log10(h),wrc,col="darkblue",lwd=2)
#'                  
#'                  legend("topright",c("observed","simulated"),pch=c(1,NA),
#'                         lty=c(NA,1),lwd=2,bty="n",cex=1.3,col=c("darkgrey","darkblue"))
#'                  axis(1,at=pow,labels=parse(text=paste('10^',(pow),sep="")),tcl=tcllen)
#'                  axis(2,tcl=tcllen)
#'                  axis(4,labels=NA)
#'                  axis(3,labels=NA)
#'                  mtext("pressurehead|h|[cm]",1,cex=1.2,line=2.8)
#'                  mtext("vol.watercontent[-]",2,cex=1.2,line=2.8)
#'                  box()
#'                  
#'                  #  PLOT THE HYDRAULIC CONDUCTIVITY CURVE
#'                  plot(condata,ylim=c(-8,max(max(condata[,1]),max(hcc)))
#'                       ,xlim=c(0,6.8),ylab="",xlab="",col="darkgrey",
#'                       axes=FALSE,main="HydraulicConductivityCurve",cex=2)
#'                  lines(log10(h),hcccap,col="brown",lwd=2)
#'                  lines(log10(h),hccnc,col="brown",lwd=2,lty=2)
#'                  lines(log10(h),hcc,col="darkblue",lwd=2)
#'                  lines(log10(h),hcvap,col="darkblue",lwd=2)
#'                  legend("topright",c("observed","simulated"),pch=c(1,NA),
#'                         lty=c(NA,1),lwd=2,bty="n",cex=1.3,col=c("darkgrey","darkblue"))
#'                  axis(1,at=pow,labels=parse(text=paste('10^',(pow),sep="")),tcl=tcllen)
#'                  axis(2)
#'                  axis(4,labels=NA)
#'                  axis(3,labels=NA)
#'                  mtext("log10K[cm/d]",2,cex=1.2,line=2.8)
#'                  mtext("pressurehead|h|[cm]",1,cex=1.2,line=2.8)
#'                  box()
#'                  par(mfrow=c(1,1))
#'            }
#'      }
#'      phattrans.m <- out$out$optim$phattrans
#'      gof.L[[i]]<-gofFun(phattrans.m,shpmodel=shpmodel.v[i],retdata=retdata,condata=condata,
#'                         out.L[[i]]$settings$weight,parL$psel,ivap.query=NULL,hclip.query=FALSE)
#'}
#'
#'statstab3 <- cbind("th_rmse" = unlist(lapply(lapply(gof.L, `[[`, "th"), '[[', "rmse")),
#'                   "log10Kh_rmse" = unlist(lapply(lapply(gof.L, `[[`, "log10Kh"), '[[', "rmse"))
#')
#'}
#'} 

shypEstFun <- function(shpmodel = "01110", parL, retdata, condata, 
                       ivap = NULL, hclip = FALSE, 
                       weightmethod = "none", LikModel = "rss",  ALG = "DE", 
                       set.itermax = 200, ALGoptions = NULL, lhs.query = FALSE){
      
      # shypEstFun <- function(likmod ="rss", shpmodel = "01110", retdata, condata, parL, weightmethod, ALG = "DE", set.itermax = 200){
      
      
      #
      ####      ARGUMENTS
      #
      #         shpmodel            chr       Soil hydrailic property model, defaults to "01110" van Genuchten-Mualem constrained 1-1/m
      #
      #         parL                list      of 4 vectors with same length
      #                                                 1) p    num        vector of initial parameter estimates | the value for the fixed parameters
      #                                                 2) psel num        vector with 0s and 1s specifiying estimated parameters | 0 = no estimation, 1 = estimation
      #                                                 3) plo  num        vector of lower parameter boundaries
      #                                                 4) pup  num        vector of upper parameter boundaries
      #
      #         retdata             data.frame       observations of retention data, data.frame, 
      #                                                 first column     num   pressure heads in pF scale [cm], 
      #                                                 second column    num   volumetric water content [-]
      #
      #         condata             data.frame       observations of hydraulic conductivity data, 
      #                                                 first column     num   pressure heads [cm], 
      #                                                 second column    num   conductivity values, log10 transfromed | log10K [cm/d]
      #
      #         ivap                logical   FALSE (default) isothermal vapour conductivity (ivap) is not considered.  
      #                                       TRUE ivap is considered, defaults to Millington-Quirk (1961)
      #                             chr       isothermal vapour conductivity as described in Saito et al. (2006) is considered with a given specified model 
      #
      #         hclip               logical   Select of hclip correction method for the conductivity data after Iden et al. (2015) will be done, defaults to FALSE 
      #                                       (for future functionality, only)
      #
      #         weightmethod        chr       Selection of weighting method for the inverse modelling, defaults to none
      #                                                 range     chr       the data range of data values is considered (cf. Peters ad Durner, 2008)
      #                                                 fix1      chr       
      #                                                 fit       chr       the variance of the selected data classes is estimated, assuming homoscedastic variance.
      #                                                 none      chr       no weighting is considered.
      #                                                 est 1     chr       the standard deviation is estimated as a nuisance parameter
      #                             list
      #
      #         LikModel            chr       likelihood model, defaults to "rss"
      #                                                 rss       chr       minimisation of the  sum of squared (weighted) residuals
      #                                                 loglik    chr       minimisation of the log-likelihood
      #
      #                                                 res       chr       returns vector of (weighted) residuals                                  
      #         ALG              chr       Specification of ptimisation/sampling algorithm (for future functionality).
      #                                                 DE        chr       Differential Evolution | DEoptim package
      #
      #         set.itermax         num       number of maximum iterations            list      later: fit.control control parameters; see DEoptim.control.          
      #         
      #         lhs.query           chr       FALSE or TRUE: asks if a Latin Hypercube sample should be drawn for initial population for DEoptim
      
      #
      ####      INPUT CHECKS
      #                   Stop function execution if the following checks are negative
      
      
      #  - Is shpmodel ill-specified?
      stopifnot(shpmodel%in%c("01110","01110FM",
                              "01210",
                              "01310",  
                              "02110", "02110FM",
                              "03110", "03110FM"))   
      
      
      #  - Is input data not of class data.frame?
      stopifnot(is.data.frame(retdata), is.data.frame(retdata))
      
      #  - Is likmod ill-specified?
      stopifnot(LikModel%in%c("rss", "res", "-2logLik"))
      
      #  - Is likmod ill-specified?
      stopifnot(ALG%in%c("DE", "modMCMC"))
      
      
      #  - Is weightmethod ill-specified?
      stopifnot(weightmethod%in%c("range", "fix1", "fix2", "est1", "none")| is.list(weightmethod) )
      
      #  - Is parL ill-specified? 
      stopifnot(is.list(parL))
      
      #  - Does parL contain vectors of different lengths?
      stopifnot(sum(sapply(parL, length))/length(parL) == length(parL[[1]]))
      
      #  - parL is not a list of four 
      stopifnot(length(parL) == 4)
      
      #  - check for trimodal soil hydraulic property model. Included for future functionality
      ifelse(shpmodel%in%c("01310"), trim.query <- TRUE, trim.query <- FALSE)
      
      #  - check if ALGoptions exists and if not create it
      if(is.null(ALGoptions)){ALGoptions <- list()}
      
      #  - assigning some options
      ALGoptions$itermax <- set.itermax
      if(!"storepopfrom" %in% names(ALGoptions)){ALGoptions$storepopfrom <- ALGoptions$itermax+1}
      
      #
      ####       ASSIGN CORRECT FUNCTION FOR resFun to operate properly
      #
      
      #
      ####       INVERSE MODELLING ----
      #
      
      
      #
      ###        WEIGHTING METHOD   
      #
      
      if(is.list(weightmethod)){            # USER DEFINED WEIGHTS
            
            weight = weightmethod
            
            # Ensure that weight is either a skalar, or given as the same length as number of observations in
            #  retdata
            stopifnot(length(weight[[1]]) == 1 | length(weight[[1]]) == dim(retdata)[1]) 
            #  condata
            stopifnot(length(weight[[2]]) == 1 | length(weight[[2]]) == dim(condata)[1]) 
            
      } else {
            
            if(weightmethod != "est1"){ # FIXED SKALAR WEIGHTS
                  weight <- weightFun(weightmethod, retdata, condata)
                  
            } else {  
                  weight <- NULL    # FITTED SKALAR SD of THETA and SD of log10K
                  parL <- weightFun(weightmethod, retdata, condata, parL = parL)$parL
            }  
      }
      
      #
      #####      TRANSFORM BOUNDARIES AND GENERATE INITIAL POPULATION, HYDRAULIC PROPERTY MODEL SPECIFIC
      #
      
      # get transform parameters.
      returnL <- transBoundFun(parL, shpmodel, weightmethod)
      parL <- returnL$parL
      transL <- returnL$transL
      
      # get transform parameters.
      p <- parL$p 
      plo_bd <- parL$plo 
      pup_bd <- parL$pup 
      psel <- parL$psel
      
      
      # initial population on transformed parameters.
      if(ALG == "DE"){
            if(lhs.query == TRUE) {
                  ALGoptions$initialpop <- inipopFun(p = p, psel = psel, plo = plo_bd , pup = pup_bd, trans.L = transL$ptrans, Npop = NA)          
            }else{
                  if(!"initialpop"%in%names(ALGoptions)) ALGoptions$initialpop <- NULL}
      }
      
      #
      ###       FITTING 
      #
      
      switch(ALG,
             DE = {out <- DEoptim(resFun, 
                                  # model specific
                                  lower = transFun(plo_bd, transL$ptrans),
                                  upper = transFun(pup_bd, transL$ptrans),
                                  shpmodel = shpmodel,
                                  retdata = retdata, 
                                  condata = condata, 
                                  pretrans = returnL$transL$pretrans, 
                                  method = LikModel,
                                  weight = weight,
                                  trim.query = trim.query,
                                  ivap.query = ivap,
 
                                  
                                  DEoptim.control(
                                        VTR      = ifelse("VTR" %in% names(ALGoptions), ALGoptions$VTR, -Inf)
                                        , strategy = ifelse("strategy" %in% names(ALGoptions), ALGoptions$strategy, 2)
                                        , bs       = ifelse("bs" %in% names(ALGoptions), ALGoptions$bs, FALSE)
                                        , NP       = ifelse("NP" %in% names(ALGoptions), ALGoptions$NP, NA)
                                        , itermax  = ALGoptions$itermax
                                        , CR       = ifelse("CR" %in% names(ALGoptions), ALGoptions$CR, .5)
                                        , F        = ifelse("F" %in% names(ALGoptions), ALGoptions$F, .8)
                                        , trace    = ifelse("trace" %in% names(ALGoptions), ALGoptions$trace, TRUE)
                                        , initialpop = if("initialpop" %in% names(ALGoptions)){ALGoptions$initialpop}
                                        , storepopfrom = ALGoptions$storepopfrom
                                        , storepopfreq = ifelse("storepopfreq" %in% names(ALGoptions), ALGoptions$storepopfreq, 1)
                                        , p = ifelse("p" %in% names(ALGoptions), ALGoptions$p, .2)
                                        , c = ifelse("c" %in% names(ALGoptions), ALGoptions$c, 0)
                                        , reltol = ifelse("reltol" %in% names(ALGoptions), ALGoptions$reltol, sqrt(.Machine$double.eps))
                                        # steptol missing, 
                                        # all parallel options missing
                                  )
             )
             
             
             out$optim$phat <- out$optim$bestmem
             out$optim$phattrans <- transFun(out$optim$bestmem, 
                                             trans.L = returnL$transL$pretrans)
             
             
             },
             modMCMC = {
                   
                   sel <- which(parL$psel == 1)
                   
                   par.lo <- transFun(plo_bd, transL$ptrans)[sel]
                   par.up <- transFun(pup_bd, transL$ptrans)[sel]
                   p.ini <- par.lo + abs(par.lo-par.up)/2
                   
                   if(!"jump"%in%names(ALGoptions))         {ALGoptions$jump  <- NULL }
                   if(!"prior"%in%names(ALGoptions))        {ALGoptions$prior <- NULL }
                   if(!"var0"%in%names(ALGoptions))         {ALGoptions$var0  <- NULL }
                   if(!"wvar0"%in%names(ALGoptions))        {ALGoptions$wvar0 <- NULL }
                   if(!"n0"%in%names(ALGoptions))           {ALGoptions$n0    <- NULL }
                   if(!"burninlength"%in%names(ALGoptions)) {ALGoptions$burninlength <- 0.7*set.itermax }
                   if(!"covscale"%in%names(ALGoptions))     {ALGoptions$covscale <- 2.4^2/length(p.ini) }
                   if(!"ntrydr"%in%names(ALGoptions))       {ALGoptions$ntrydr   <- 1 }
                   if(!"verbose"%in%names(ALGoptions))      {ALGoptions$verbose  <- TRUE }
                   if(!"drscale"%in%names(ALGoptions))      {ALGoptions$drscale  <- NULL }
                   if(!"updatecov"%in%names(ALGoptions))    {ALGoptions$updatecov<- 100 }
                   
                   out <- modMCMC(
                         f = resFun, 
                         # model specific
                         p = p.ini,
                         lower = par.lo,
                         upper = par.up,
                         shpmodel = shpmodel,
                         retdata = retdata, 
                         condata = condata, 
                         pretrans = returnL$transL$pretrans[sel], 
                         method = LikModel,
                         weight = weight, 
                         trim.query = trim.query,
                         ivap.query = ivap, 
                         parL = parL
                         # algorithm specific
                         , prior = ALGoptions$prior
                         , var0 = ALGoptions$var0 
                         , wvar0 = ALGoptions$wvar0
                         , n0 = ALGoptions$n0
                         , niter = ALGoptions$itermax
                         , outputlength = ALGoptions$itermax
                         , burninlength = ALGoptions$burninlength
                         , covscale = ALGoptions$covscale
                         , ntrydr = 1
                         , drscale = ALGoptions$drscale
                         , verbose = TRUE
                         , updatecov = ALGoptions$updatecov
                   )
                   
                   out$optim$phat <- out$bestpar
                   out$optim$phattrans <- transFun(out$bestpar, 
                                                   returnL$transL$pretrans[sel] )
                   
             },
             stop("Enter a valid code for ALG")
      )
      
      # add the set up to
      
      settings              <- vector("list")
      settings$data$retdata <- retdata
      settings$data$condata <- condata
      settings$transL       <- transL
      settings$ALGoptions   <- ALGoptions
      settings$weight       <- weight
      settings$parL         <- as.list(parL)
      settings$shpmodel     <- shpmodel
      settings$ivap         <- ivap
      settings$hclip        <- hclip
      settings$LikModel     <- LikModel
      

      return(list("settings" = settings, "out" = out))
  
}
