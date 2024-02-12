#' Meta-analysis of RMSTD at multiple time horizons
#' @description Perform a meta-analysis with RMSTD using individual patient data.
#' Methods include:
#' \enumerate{
#' \item \code{"mvma"} a multivariate meta-analysis borrowing strength across time-points with within-trial covariance matrix derived analytically
#' \item \code{"mvma_boot"} a multivariate meta-analysis borrowing strength across time-points with within-trial covariance matrix derived by bootstrap
#' \item \code{"uni"} a univariate meta-analysis for combined effect at each time-point using only available data
#' \item \code{"uni_flex"} a univariate meta-analysis for combined effect at each time-point using estimates based on flexible parametric models as described by Wei et al (Stat Med 2015). 
#' }
#'
#' @param trialdata IPD trial data, see details for specifications
#' @param time_horizons specified vector of time horizons for the meta-analysis
#' @param MA_method the desired meta-analysis method; options are: "mvma", "mvma_boot", "uni", "uni_flex"
#' @param nboot the number of bootstrap iterations, if using the MVMA with bootstrap covariance matrix; default=500
#' @return The \code{metaRMSTD} function returns a list object containing the random-effects model results,
#' the RMSTD and SE values for each trial at each available time horizon, and the estimated within-trial covariance matrix for each RCT.
#' @note RMSTD is estimable if time horizon > minimum of last observed times 
#' across the two groups. We implement the method-of-moments estimator for MVMA 
#' (Chen et al. Biometrics 2012, Jackson et al. Biometrical Journ 2013) and 
#' Dersimonian and Laird for univariate MA.
#' @details Specify the time horizons at which to calculate the meta-analytic results.
#' The \code{trialdata} must be formatted as a dataframe containing the IPD for each single trial.
#' Variable names must include Trial ID ("trialID"), Time ("Time"), Event status ("Event"), and randomization group ("Arm").
#' @examples 
#' 
#' # read in built-in dataset 
#' data(AorticStenosisTrials)
#' 
#' \donttest{
#' # meta-analysis to obtain combined effect by multivariate model (method="mvma")
#' result <- metaRMSTD(AorticStenosisTrials, time_horizons=c(12,24,36), MA_method="mvma")
#' 
#' # generate figure: 
#' obj <- RMSTcurves(AorticStenosisTrials, time_horizons=c(12,24,36), tmax=40, nboot=500)
#' RMSTplot(obj, xlim=c(0,40), ylim=c(-0.25,2.75), yby=0.5, ylab="RMSTD (mos)", xlab="Time (mos)")
#' }
#' 
#' \dontshow{
#' set.seed(10)
#' sample_rows <- sample(1:nrow(AorticStenosisTrials),500, replace=FALSE)
#' result_hidden <- metaRMSTD(AorticStenosisTrials[sample_rows,], time_horizons=c(12,24,36), MA_method="uni")
#' obj_hidden <- RMSTcurves(AorticStenosisTrials[sample_rows,], time_horizons=c(12,24,36), tmax=40, MA_mvma_boot=FALSE, MA_uni_flex=FALSE, MA_mvma=FALSE, tstep=1)
#' RMSTplot(obj_hidden, xlim=c(0,40), ylim=c(-0.25,2.75), yby=0.5, ylab="RMSTD (mos)", xlab="Time (mos)")
#' }
#' 
#' @import mvmeta meta survival survRM2
#' @importFrom rstpm2 stpm2 predict
#' @references
#' Wei, Y, Royston, P, Tierney, JF and Parmar, MKB. (2015). Meta-analysis of time-to-event outcomes 
#' from randomized trials using restricted mean survival time: application to 
#' individual participant data. Stat Med 34(21), 2881-2898.
#' 
#' Chen, Han, Alisa K. Manning, and Josée Dupuis. "A method of moments estimator for 
#' random effect multivariate meta-analysis." Biometrics 68.4 (2012): 1278-1284.
#' 
#' Jackson, Dan, Ian R. White, and Richard D. Riley. "A matrix-based method of moments for 
#' fitting the multivariate random effects model for meta-analysis and meta-regression."
#' Biometrical Journal 55.2 (2013): 231-245.
#' @export

metaRMSTD <- function(trialdata, time_horizons, MA_method, nboot=500){

# function to determine largest tau value less than the FU time in each trial:
largest_less_than<-function(x,y){
  max(x[x <= y])
  }

# determine number of trials in meta analysis
J <- max(trialdata$trialID)

  # calculation of difference in RMST at each time horizon
  # storing the values in 2 formats to be used with different MA methods
rmstDsL <- rep(list(data.frame(tau=numeric(length(time_horizons)), RMSTD=numeric(length(time_horizons)),SE=numeric(length(time_horizons)))), J)
rmstd_table <- matrix(NA, J, length(time_horizons))
se_table <- matrix(NA, J, length(time_horizons))
var_table <- matrix(NA, J, length(time_horizons))

  for (j in 1:J){
    #jdata <- trial_list[[j]]
    jdata <- trialdata[which(trialdata$trialID==j),]
    index <- 0
    FU <- min(max(jdata[which(jdata$Arm==1),]$Time), max(jdata[which(jdata$Arm==0),]$Time)) # minimum of max observed followup times across groups

    for (tau in time_horizons){
      index <- index+1

      if(FU>=tau){obj<-rmst2(jdata$Time, jdata$Event, jdata$Arm, tau=tau)}else{obj <- NA}
      if(FU>=tau){RMSTd <- obj$unadjusted.result[1]}else{RMSTd <- NA}
      if(FU>=tau){SERMSTd <-  (obj$unadjusted.result[1,2]-obj$unadjusted.result[1])/-qnorm(0.975)}else{SERMSTd <- NA}
      if(FU>=tau){varRMSTd <-  (obj$RMST.arm1$result[1,2])^2+(obj$RMST.arm0$result[1,2])^2}else{varRMSTd <- NA}

      rmstDsL[[j]]$tau[index] <- tau
      rmstDsL[[j]]$SE[index] <- SERMSTd

      if(FU>=tau){rmstDsL[[j]]$RMSTD[index] <- RMSTd}else{rmstDsL[[j]]$RMSTD[index] <- NA}
      if(FU>=tau){rmstDsL[[j]]$SE[index] <- SERMSTd}else{rmstDsL[[j]]$SE[index] <- NA}

      rmstd_table[j,] <- rmstDsL[[j]]$RMSTD
      se_table[j,]    <- rmstDsL[[j]]$SE
      var_table[j,index]   <- varRMSTd

    }
  }

colnames(rmstd_table) <- paste0("RMSTD_at_", time_horizons)
rownames(rmstd_table) <- paste0("study_", 1:J)

colnames(se_table) <- paste0("se_RMSTD_at_", time_horizons)
rownames(se_table) <- paste0("study_", 1:J)

if (MA_method=="uni"){
# univariate meta-analysis at each available tau
uni <- data.frame(method=numeric(length(time_horizons)),
                  time_horizon=numeric(length(time_horizons)),
                  ntrials=numeric(length(time_horizons)),
                  Estimate=numeric(length(time_horizons)),
                  SE=numeric(length(time_horizons)),
                  lower=numeric(length(time_horizons)),
                  upper=numeric(length(time_horizons)),
                  Z=numeric(length(time_horizons)),
                  pval=numeric(length(time_horizons)),
                  tausq=numeric(length(time_horizons)))

for (i in 1:length(time_horizons)){

	temp <- metagen(rmstd_table[,i], se_table[,i], method.tau="DL")

	uni$method[i]       <- "univariate"
	uni$time_horizon[i]	<- time_horizons[i]
	uni$Estimate[i]	<- temp$TE.random
	uni$lower[i]  <- temp$lower.random
	uni$upper[i] <- temp$upper.random
	uni$SE[i]	<- temp$seTE.random
	uni$Z[i] <- temp$zval.random
	uni$pval[i]	<- temp$pval.random
	uni$ntrials[i] <- length(which(!is.na(rmstd_table[,i])))
	uni$tausq[i] <- (temp$tau)^2

}
return(list(result=uni, rmstd=rmstd_table, se_rmstd=se_table))
}

if (MA_method=="uni_flex"){
# royston parmar flexible parametric model
RP_rmstd_table <- matrix(NA, J, length(time_horizons))
RP_se_table <- matrix(NA, J, length(time_horizons))

RP <- data.frame(method=numeric(length(time_horizons)),
                  time_horizon=numeric(length(time_horizons)),
                  ntrials=numeric(length(time_horizons)),
                  Estimate=numeric(length(time_horizons)),
                  SE=numeric(length(time_horizons)),
                  lower=numeric(length(time_horizons)),
                  upper=numeric(length(time_horizons)),
                  Z=numeric(length(time_horizons)),
                  pval=numeric(length(time_horizons)),
                 tausq=numeric(length(time_horizons)))

for (j in 1:J){

  jdata <- trialdata[which(trialdata$trialID==j),]
  index <- 0
  FU <- min(max(jdata[which(jdata$Arm==1),]$Time), max(jdata[which(jdata$Arm==0),]$Time)) # minimum of max observed followup times across groups

  MC <- stpm2(Surv(Time, Event==1)~Arm, data=jdata, smooth.formula=~ns(log(Time),df=3)+log(Time):Arm)

  for (tau in time_horizons){
    index                   <- index+1
    exp				              <- predict(MC, newdata=data.frame(Arm=1, Time=tau), type="rmst",se.fit=TRUE)
    cont			            	<- predict(MC, newdata=data.frame(Arm=0, Time=tau), type="rmst",se.fit=TRUE)
    RP_rmstd_table[j,index] <- exp$Estimate-cont$Estimate
    SE_exp 			            <- (exp$upper-exp$Estimate)/qnorm(0.975)
    SE_cont 			          <- (cont$upper-cont$Estimate)/qnorm(0.975)
    RP_se_table[j,index]    <- sqrt(SE_exp^2 + SE_cont^2)

  }
}

colnames(RP_rmstd_table) <- paste0("RMSTD_est_at_", time_horizons)
rownames(RP_rmstd_table) <- paste0("study_", 1:J)

colnames(RP_se_table) <- paste0("se_RMSTD_est_at_", time_horizons)
rownames(RP_se_table) <- paste0("study_", 1:J)


for (i in 1:length(time_horizons)){
  temp <- metagen(RP_rmstd_table[,i], RP_se_table[,i], method.tau="DL")

  RP$method[i]        <- "Univariate with model estimates"
  RP$time_horizon[i]	<- time_horizons[i]
  RP$Estimate[i]	    <- temp$TE.random
  RP$lower[i]         <- temp$lower.random
  RP$upper[i]         <- temp$upper.random
  RP$SE[i]	          <- temp$seTE.random
  RP$Z[i]             <- temp$zval.random
  RP$pval[i]	        <- temp$pval.random
  RP$ntrials[i] <- length(which(!is.na(RP_rmstd_table[,i])))
  RP$tausq[i] <- (temp$tau)^2


}
return(list(result=RP, rmstd_est=RP_rmstd_table, se_rmstd_est=RP_se_table))
}


# MVMA
if (MA_method=="mvma"){
MVMA_RE <- data.frame(method=numeric(length(time_horizons)),
                time_horizon=numeric(length(time_horizons)),
                ntrials=numeric(length(time_horizons)),
                Estimate=numeric(length(time_horizons)),
                 SE=numeric(length(time_horizons)),
                 lower=numeric(length(time_horizons)),
                 upper=numeric(length(time_horizons)),
                 Z=numeric(length(time_horizons)),
                 pval=numeric(length(time_horizons)))

# can eliminate this if we just square the se_table from step 1 and then edit the vartocov function to accept new format
var_table <- se_table^2
var_list  <- split(var_table, row(var_table))

# make matrix of variances
S <- lapply(var_list, function(x) vartocov(x))

# MVMA random effect model
REmod <- mvmeta(rmstd_table, S, method="mm")

for (i in 1:length(time_horizons)){
  MVMA_RE$method[i]       <- "Random Effect MVMA"
  MVMA_RE$time_horizon[i]	<- time_horizons[i]
  MVMA_RE$Estimate[i]	    <- summary(REmod)$coefficients[i,1]
  MVMA_RE$lower[i]	        <- summary(REmod)$coefficients[i,5]
  MVMA_RE$upper[i]         <- summary(REmod)$coefficients[i,6]
  MVMA_RE$SE[i]	          <- summary(REmod)$coefficients[i,2]
  MVMA_RE$Z[i]             <- summary(REmod)$coefficients[i,3]
  MVMA_RE$pval[i]	        <- summary(REmod)$coefficients[i,4]
  MVMA_RE$ntrials[i]	        <- length(which(!is.na(rmstd_table[,i])))
}

return(list(REresult=MVMA_RE, between_study_vcov=REmod$Psi, rmstd=rmstd_table, se_rmstd=se_table, within_study_vcov=S))
}


# mvma with bootstrap derived covariance matrix
if(MA_method=="mvma_boot"){

  MVMAboot_RE <- data.frame(method=numeric(length(time_horizons)),
                     time_horizon=numeric(length(time_horizons)),
                     ntrials=numeric(length(time_horizons)),
                     Estimate=numeric(length(time_horizons)),
                     SE=numeric(length(time_horizons)),
                     lower=numeric(length(time_horizons)),
                     upper=numeric(length(time_horizons)),
                     Z=numeric(length(time_horizons)),
                     pval=numeric(length(time_horizons)))

  BSres <- rep(list(matrix(NA,nrow=nboot, ncol=length(time_horizons))),J)
  #availableRMSTd <- matrix(NA, J, length(time_horizons))

  for (j in 1:J){
    #jdata <- trial_list[[j]]
    jdata <- trialdata[which(trialdata$trialID==j),]

    FU <- min(max(jdata[which(jdata$Arm==1),]$Time), max(jdata[which(jdata$Arm==0),]$Time)) # minimum of max observed followup times across groups
    critical_time <- largest_less_than(time_horizons, FU)

    b <- 0

    while (b < nboot){
      index <- 0
      b <- b+1

      # create bootstrap dataset, b

      dse<-jdata[jdata$Arm==1,]
      iboot.e <- sample(1:(nrow(dse)), replace = TRUE)
      boot.e <- dse[c(iboot.e),]

      dsc<-jdata[jdata$Arm==0,]
      iboot.c <- sample(1:(nrow(dsc)), replace = TRUE)
      boot.c <- dsc[c(iboot.c),]

      bootds<-rbind(boot.e,boot.c)

      min.time.bootds <- min(max(bootds[which(bootds$Arm==1),]$Time), max(bootds[which(bootds$Arm==0),]$Time))

      # check that the minimum of the max times between the two groups exceeds the followup time
      if(min.time.bootds >= critical_time){
        for (tau in time_horizons){
          index <- index+1
          if(tau <= FU){BSobj<-rmst2(bootds$Time, bootds$Event, bootds$Arm, tau=tau)}else{BSobj<- NA}
          BSres[[j]][b, index]<- ifelse(tau <= FU, BSobj$unadjusted.result[1] ,NA)
        }
      }else{
        #print(paste0("fail_",j,"_", b, "_", min.time.bootds))
        b <- b-1}
    }
  }


  beta.star <- rep(list(matrix(NA,nrow=1, ncol=length(time_horizons))),5)
  BSmat <- rep(list(list()), J )
  Sboot <- vector("list", J)

  for (j in 1:J){
    beta.star[[j]] <- apply(BSres[[j]], 2, mean, na.rm=TRUE)

    for (boot in 1:nboot){
      BSmat[[j]][[boot]] <- (BSres[[j]][boot,]-beta.star[[j]]) %*% t((BSres[[j]][boot,]-beta.star[[j]]))
    }

    Sboot[[j]] <- Reduce('+', BSmat[[j]])/(nboot-1)

  }


  # MVMA random effect model
  REmod <- mvmeta(rmstd_table, Sboot, method="mm")

  for (i in 1:length(time_horizons)){

    MVMAboot_RE$method[i]       <- "Random Effect MVMA boot"
    MVMAboot_RE$time_horizon[i]	<- time_horizons[i]
    MVMAboot_RE$Estimate[i]	    <- summary(REmod)$coefficients[i,1]
    MVMAboot_RE$lower[i]	        <- summary(REmod)$coefficients[i,5]
    MVMAboot_RE$upper[i]         <- summary(REmod)$coefficients[i,6]
    MVMAboot_RE$SE[i]	          <- summary(REmod)$coefficients[i,2]
    MVMAboot_RE$Z[i]             <- summary(REmod)$coefficients[i,3]
    MVMAboot_RE$pval[i]	        <- summary(REmod)$coefficients[i,4]
    MVMAboot_RE$ntrials[i]	        <- length(which(!is.na(rmstd_table[,i])))

  }


return(list(REresult=MVMAboot_RE, between_study_vcov=REmod$Psi, rmstd=rmstd_table, se_rmstd=se_table, within_study_vcov=Sboot))
}

if(!MA_method %in% c("uni", "mvma", "mvma_boot", "uni_flex")){print("error: MA_method must be one of uni, mvma, mvma_boot, or uni_flex")}

}



