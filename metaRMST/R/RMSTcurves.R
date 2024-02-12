#' prepare data for plot of RMSTD over time
#'
#'
#'
#'
#' @param trialdata IPD trial data
#' @param time_horizons specified vector of time horizons for the meta-analysis
#' @param nboot the number of bootstrap iterations, if using the MVMA with bootstrap covariance matrix; default=500
#' @param tmax maximum value for RMSTD to be calculated in each trial
#' @param tstep increment for calculation of RMSTD over time interval from 0 to tmax; default=0.25
#' @param MA_mvma TRUE or FALSE indicates whether to include combined effect by this method
#' @param MA_mvma_boot TRUE or FALSE indicates whether to include combined effect by this method
#' @param MA_uni TRUE or FALSE indicates whether to include combined effect by this method
#' @param MA_uni_flex TRUE or FALSE indicates whether to include combined effect by this method
#' @return an object to be plotted with \code{\link{RMSTplot}}
#' @import mvmeta meta survival survRM2
#' @importFrom rstpm2 stpm2 predict 
#' @export
#' @description Prepare the data for use with \code{\link{RMSTplot}}. This function computes RMSTD over 
#' specified time horizons and also fits a flexible parametric model to each trial.
#' It calls the \code{\link{metaRMSTD}} function to compute the estimated combined effects for each of the 4 methods.
#' @references Royston, P. and Parmar, MK. Flexible parametric proportional-hazards and proportional-odds models for censored
#' survival data, with application to prognostic modelling and estimation of treatment effects.
#' Stat. Med. 2002.

RMSTcurves <- function(trialdata, time_horizons, tmax=max(time_horizons), tstep=0.25, nboot=500,
                       MA_mvma=TRUE, MA_mvma_boot=TRUE, MA_uni=TRUE, MA_uni_flex=TRUE){

  J <- max(trialdata$trialID)

  t <- seq(tstep, tmax, by=tstep)
  RMSTcurveRes <- matrix(NA, length(t), J+1)

  for (j in 1:J){
    dat <- trialdata[which(trialdata$trialID==j),]
    index <- 0
    FU <- min(max(dat[which(dat$Arm==1),]$Time), max(dat[which(dat$Arm==0),]$Time))

    for (tau in t){
      index <- index+1
      if(FU>=tau){obj<-rmst2(dat$Time, dat$Event, dat$Arm, tau=tau)}else{obj <- NA}
      if(FU>=tau){RMSTcurveRes[index,j+1] <-  obj$unadjusted.result[1]}else{RMSTcurveRes[index,j+1] <- NA}
      RMSTcurveRes[index,1] <- tau
    }
  }
  # fit RP flex parametric model in each trial
  if(MA_uni_flex){
  RPres <- matrix(NA, length(t), J+1)
  for (j in 1:J){
    dat <- trialdata[which(trialdata$trialID==j),]
    index <- 0
    MC <- stpm2(Surv(Time, Event==1)~Arm, data=dat, smooth.formula=~ns(log(Time),df=3)+log(Time):Arm)

    for (tau in t){
      index <- index+1
      RPres[index, j+1]	<- predict(MC, newdata=data.frame(Arm=1, Time=tau), type="rmst",se.fit=TRUE)$Estimate	- predict(MC, newdata=data.frame(Arm=0, Time=tau), type="rmst",se.fit=TRUE)$Estimate
      RPres[index,1] <- tau
    }
  }
  }else{RPres <- matrix(NA, length(t), J+1)}
  # prepare MA results for RMST curves+RP curves plot
  if(MA_mvma){
    mvma_res      <- metaRMSTD(trialdata, time_horizons=time_horizons, MA_method="mvma")
  }else {mvma_res <- c()}

  if(MA_mvma_boot){
    mvma_boot_res <- metaRMSTD(trialdata, time_horizons=time_horizons, MA_method="mvma_boot", nboot=nboot)
  }else {mvma_boot_res <- c()}

  if(MA_uni_flex){
    RP_res <- metaRMSTD(trialdata, time_horizons=time_horizons, MA_method="uni_flex")
  }else {RP_res <- c()}

  if(MA_uni){
    UNI_res <- metaRMSTD(trialdata, time_horizons=time_horizons, MA_method="uni")
  }else {UNI_res <- c()}
  MA_results <- rbind(mvma_res$REresult, mvma_boot_res$REresult, RP_res$result[,1:9], UNI_res$result[,1:9])

  return(list(RMST=RMSTcurveRes, RMST_est=RPres, MA_results=MA_results))
}


