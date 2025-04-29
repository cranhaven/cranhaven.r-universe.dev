prsim.weather <- function(data_p, data_t, station_id_p="Precip",station_id_t="Temp", number_sim=1, win_h_length=15, 
                          n_wave=100,verbose=TRUE,t_margin='sep',p_margin='egpd',...){  
  
  ### function for backtransformation of continuous wavelet transform
  ### inverse wavelet transform
  ### x is the input matrix
  fun_icwt<-function(x){
    wt.r<-Re(x)
    
    ### define number of scales
    J<-length(x[1,])
    # Reconstruct as in formula (11):
    dial<-2*2^(0:J*.125)
    rec<-rep(NA,(length(x[,1])))
    for(l in 1:(length(x[,1]))){
      rec[l]<-0.2144548*sum(wt.r[l,]/sqrt(dial)[1:length(wt.r[l,])])
    }
    return(rec)
  }
  
  ## start preparing arguments.
  ### input data needs to be of the format year (four digits), month (two digits), day (one digit), input discharge time series
  
  ### prepare marginal distributions
  ### precipitation
  p_margin <- p_margin[1]    # take only the first element
  if (!(p_margin %in% c("egpd"))) {   # check if distributions exist
    if (!is.character(p_margin)) stop("'p_margin' should be a character string.")
    rCDF_p <- get(paste0("r",p_margin), mode = "function")
    CDF_fit_p <- get(paste0(p_margin,"_fit"), mode = "function")
    # if (GoFtest=="AD")	  pCDF <- get(paste0("p",p_margin), mode = "function")
  }
  ### temperature
  t_margin <- t_margin[1]    # take only the first element
  if (!(t_margin %in% c("sep"))) {   # check if distributions exist
    if (!is.character(t_margin)) stop("'p_margin' should be a character string.")
    rCDF_t <- get(paste0("r",t_margin), mode = "function")
    CDF_fit_t <- get(paste0(t_margin,"_fit"), mode = "function")
    # if (GoFtest=="AD")	  pCDF <- get(paste0("p",p_margin), mode = "function")
  }
  op <- options("warn")$warn
  ### input data needs to be of the format year (four digits), month (two digits), day (one digit), input discharge time series
  
  
  
  # ### check for correct input data labels and length
  # ### run through all stations: list
  for(l in 1:length(data_p)){
    ### precipitation
      if (nrow(data_p[[l]])[1]<730) stop("At least one year of data required.")
      if (is.numeric(station_id_p)){
        station_id_p <- colnames(data_p[[l]])[station_id_p]
      }
      if (is.na(station_id_p)||!("Precip" %in% colnames(data_p[[l]]))) stop("Wrong column (name) for observations selected.")

      # test for proper format:
      if (any(class(data_p[[l]][,1])%in%c("POSIXct","POSIXt"))){
        data <- data.frame(YYYY=as.integer(format(data_p[[l]][,1],'%Y')),
                           MM=as.integer(format(data_p[[l]][,1],'%m')),
                           DD=as.integer(format(data_p[[l]][,1],'%d')),
                           Precip=data_p[[l]][,station_id_p],
                           timestamp=data_p[[l]][,1])
      } else {
        if(!all(c("YYYY","MM","DD") %in% colnames(data_p[[l]]))) stop("Wrong time column names")

        data_p[[l]] <- data_p[[l]][,c("YYYY","MM","DD", station_id_p)]
        tmp <- paste(data_p[[l]]$YYYY,data_p[[l]]$MM,data_p[[l]]$DD,sep=" ")
        names(data_p[[l]]) <- c("YYYY","MM","DD","Precip")
        data_p[[l]]$timestamp <- as.POSIXct(strptime(tmp, format="%Y %m %d", tz="GMT"))
      }
    ### temperature
    if (nrow(data_t[[l]])[1]<730) stop("At least one year of data required.")
    if (is.numeric(station_id_t)){
      station_id_t <- colnames(data_t[[l]])[station_id_t]
    }
    if (is.na(station_id_t)||!("Temp" %in% colnames(data_t[[l]]))) stop("Wrong column (name) for observations selected.")
    
    # test for proper format:
    if (any(class(data_t[[l]][,1])%in%c("POSIXct","POSIXt"))){
      data <- data.frame(YYYY=as.integer(format(data_t[[l]][,1],'%Y')),
                         MM=as.integer(format(data_t[[l]][,1],'%m')),
                         DD=as.integer(format(data_t[[l]][,1],'%d')),
                         Temp=data_t[[l]][,station_id_t],
                         timestamp=data_t[[l]][,1])
    } else {
      if(!all(c("YYYY","MM","DD") %in% colnames(data_t[[l]]))) stop("Wrong time column names")
      
      data_t[[l]] <- data_t[[l]][,c("YYYY","MM","DD", station_id_t)]
      tmp <- paste(data_t[[l]]$YYYY,data_t[[l]]$MM,data_t[[l]]$DD,sep=" ")
      names(data_t[[l]]) <- c("YYYY","MM","DD","Temp")
      data_t[[l]]$timestamp <- as.POSIXct(strptime(tmp, format="%Y %m %d", tz="GMT"))
    }
    
    ### remove February 29
    data_p[[l]] <- data_p[[l]][format(data_p[[l]]$timestamp, "%m %d") != "02 29",]
    data_t[[l]] <- data_t[[l]][format(data_t[[l]]$timestamp, "%m %d") != "02 29",]
    
    ### remove incomplete years
    if(which(format(data_p[[l]]$timestamp,format='%j')=='001')[1]>1){
      data_p[[l]] <- data_p[[l]][-c(1:(which(format(data_p[[l]]$timestamp,format='%j')=='001')[1]-1)),]
    }
    if ((nrow(data_p[[l]]) %% 365)>0) stop("No missing values allowed. Some days are missing.")
    if(which(format(data_t[[l]]$timestamp,format='%j')=='001')[1]>1){
      data_t[[l]] <- data_t[[l]][-c(1:(which(format(data_t[[l]]$timestamp,format='%j')=='001')[1]-1)),]
    }
    if ((nrow(data_t[[l]]) %% 365)>0) stop("No missing values allowed. Some days are missing.")
    
    ### replace missing data by mean values
    if(length(which(is.na(data_p[[l]]$timestamp)))>0){
      ### replace days with missing data
      data_p[[l]][which(is.na(data_p[[l]]$timestamp)),]$Precip <- mean(data_p[[l]]$Precip,na.rm=T)
    }
    if(length(which(is.na(data_t[[l]]$timestamp)))>0){
      ### replace days with missing data
      data_t[[l]][which(is.na(data_t[[l]]$timestamp)),]$Temp <- mean(data_p[[l]]$Temp,na.rm=T)
    }
    ### generate a day index
    # data[[l]]$index <- rep(c(1:365), times=length(unique(data[[l]]$YYYY)))
    data_p[[l]]$index <- as.numeric(format(data_p[[l]]$timestamp,format='%j'))
    data_t[[l]]$index <- as.numeric(format(data_t[[l]]$timestamp,format='%j'))
    
    ### replace empty index positions
    if(length(which(is.na(data_p[[l]]$index))>0)){
      data_p[[l]]$index[which(is.na(data_p[[l]]$index))] <- rep(c(1:365), times=length(unique(data_p[[l]]$YYYY)))[which(is.na(data_p[[l]]$index))]
    }
    if(length(which(is.na(data_t[[l]]$index))>0)){
      data_t[[l]]$index[which(is.na(data_t[[l]]$index))] <- rep(c(1:365), times=length(unique(data_t[[l]]$YYYY)))[which(is.na(data_t[[l]]$index))]
    }
  }
  
  if (verbose) cat(paste0("Detrending with (half-)length ",win_h_length,"...\n"))  
  
  ### (1) Generation of white noise for random phases generation
  ### generate random sample of indices for each simulation run
  ### use same phases for all variables and stations
  
  # set.seed(10)
  noise_mat_r <- list()
  for (r in 1:number_sim){
    ### first, randomly sample a station from which to sample years
    station_rand <- sample(size=1,1:length(data_t))
   
    ### use something close to the observations
    years <- unique(data_t[[station_rand]]$YYYY)
    year_samp <- sample(years)
    data_samp <- data_t[[station_rand]][which(data_t[[station_rand]]$YYYY==year_samp[1]),]
    for(i in 2:length(year_samp)){
      data_year <- data_t[[station_rand]][which(data_t[[station_rand]]$YYYY==year_samp[i]),]
      data_samp <- rbind(data_samp,data_year)
    }
    ts_wn <- data_samp$Temp
    ### determine scale range
    scale.range = deltat(data_p[[l]]$Precip) * c(1, length(data_p[[l]]$Precip))
    ### sampling interval
    # sampling.interval <- 1
    sampling.interval <- 0.1
    
    ### determine octave
    octave <- logb(scale.range, 2)
    ### determine wavelet scales
    scale <- ifelse1(n_wave > 1, 2^c(octave[1] + seq(0, n_wave -
                                                       2) * diff(octave)/(floor(n_wave) - 1), octave[2]), scale.range[1])
    scale <- unique(round(scale/sampling.interval) * sampling.interval)
    wt_morlet <- cwt_wst(signal=ts_wn,scales=scale,wname='MORLET',makefigure=FALSE,dt=1,powerscales=FALSE,wparam=5)
    noise_mat_r[[r]] <- as.matrix(wt_morlet$coefs)
  }
  
  ### monthly distribution fitting
  # if(fit=='monthly'){
    p_fit <-p_val_p <- t_fit <- p_val_t<- rep(list(rep(list(NA),times=12)),times=length(data_p))
    ### loop through all stations
    ### Precipitation
    if(p_margin=='egpd'){
        for(i in 1:length(data_p)){
        ### loop through all months
        for(m in 1:12){
          ### extract monthly information
          pos_month <- as.numeric(data_p[[i]]$MM)%in%m
          ### fit E-GPD distribution to non-zero values
          data_month <- data_p[[i]]$Precip[which(as.numeric(data_p[[i]]$MM)==m)]
          ### only non-zero values
          data_month <- data_month[which(data_month>0)]
          ### distribution fitting
          # p_fit[[i]][[m]] <- fit.extgp(data=data_month,method='pwm',init= c(0.9, gp.fit(data_month, 0)$est),model=1)
          p_fit[[i]][[m]] <- fit.extgp(data=data_month,method='pwm',init= c(0.9, 4,0.1),model=1)
          ### look at p-values of E-GPD distribution using KS-test
          # sample <- rextgp(n=length(data_month),kappa=p_fit[[i]][[m]]$fit$pwm[1],sigma=p_fit[[i]][[m]]$fit$pwm[2], xi=p_fit[[i]][[m]]$fit$pwm[3])
          # p_val_p[[i]][[m]] <- ks.test(data_month,sample)$p.value ### kappa distribution not rejected at alpha=0.05
        }
      }
    }
    
    ### use another pre-defined distribution or define own function
    if(p_margin!='egpd'){
      for(i in 1:length(data_p)){
        ### loop through all months
        for(m in 1:12){
          ### extract monthly information
          pos_month <- as.numeric(data_p[[i]]$MM)%in%m
          ### fit E-GPD distribution to non-zero values
          data_month <- data_p[[i]]$Precip[which(as.numeric(data_p[[i]]$MM)==m)]
          ### only non-zero values
          data_month <- data_month[which(data_month>0)]
          ### distribution fitting
          p_fit[[i]][[m]] <- CDF_fit_p(xdat=data_month,...)
        }
      }
    }
      
    ### temperature
    ### normal
    # if(t_margin=='normal'){
    #   ### temperature
    #   for(i in 1:length(data_p)){
    #     ### loop through all months
    #     for(m in 1:12){
    #       data_month <- data_t[[i]]$Temp[which(as.numeric(data_t[[i]]$MM)==m)]
    #       ### fit normal distribution
    #       t_fit[[i]][[m]] <- fitdistr(data_month,'normal')
    #       sample <- rnorm(n=length(data_month),mean=t_fit[[i]][[m]]$estimate[1],sd=t_fit[[i]][[m]]$estimate[2])
    #       # hist(data_month,breaks=20)
    #       # hist(sample,add=T,col='red',breaks=20)
    #       p_val_t[[i]][[m]]<-ks.test(data_month, sample)$p.value
    #     }
    #   }
    # }
    ### skewed exponential power
    if(t_margin=='sep'){
      for(i in 1:length(data_p)){
        ### loop through all months
        for(m in 1:12){
          data_month <- data_t[[i]]$Temp[which(as.numeric(data_t[[i]]$MM)==m)]
          l_moments <- lmoms(data_month)
          ### fit skewed exponential power distribution
          aep_par <- lmom2par(lmom=l_moments, type='aep4')
          # sample <- rlmomco(n=length(data_month),aep_par)
          # hist(data_month,breaks=20)
          # hist(sample,add=T,col='red',breaks=20)
          t_fit[[i]][[m]] <- aep_par
          # p_val_t[[i]][[m]]<-ks.test(data_month, sample)$p.value
        }
      }
    }
    
    ### use another distribution than sep
    if(t_margin!='sep'){
      for(i in 1:length(data_t)){
        ### loop through all months
        for(m in 1:12){
          ### extract monthly information
          pos_month <- as.numeric(data_t[[i]]$MM)%in%m
          ### fit E-GPD distribution to non-zero values
          data_month <- data_t[[i]]$Temp[which(as.numeric(data_t[[i]]$MM)==m)]
          ### only non-zero values
          data_month <- data_month[which(data_month>0)]
          ### distribution fitting
          t_fit[[i]][[m]] <- CDF_fit_t(xdat=data_month,...)
        }
      }
    }
    
  # }
  
  ### repeat stochastic simulation several times
  
  if(verbose) cat(paste0("Starting ",number_sim," simulations:\n"))
  
  ### run through all stations
  out_list<-list()
  for(l in 1:length(data_p)){
    ### list for storing results
    data_sim_t <- data_sim_p <- list()
    ### simulate n series
    for (r in c(1:number_sim)){
      ### determine scale range
      # scale.range = deltat(data_p[[l]]$norm) * c(1, length(data_p[[l]]$norm))
      scale.range = deltat(data_p[[l]]$Precip) * c(1, length(data_p[[l]]$Precip))
      
      ### sampling interval
      # sampling.interval <- 1 ### too little variability in resulting signal
      sampling.interval <- 0.1 
      
      ### determine octave
      octave <- logb(scale.range, 2)
      ### determine wavelet scales
      scale <- ifelse1(n_wave > 1, 2^c(octave[1] + seq(0, n_wave -
                                                         2) * diff(octave)/(floor(n_wave) - 1), octave[2]), scale.range[1])
      scale <- unique(round(scale/sampling.interval) * sampling.interval)
      ### these scales correspond to the scales originally used in wavCWT()
      
      ### apply continuous wavelet transform: use package wavScalogram
      # wt_morlet_p <- cwt_wst(signal=data_p[[l]]$norm,scales=scale,wname='MORLET',
      #                      powerscales=FALSE,makefigure=FALSE,dt=1,wparam=5)
      # wt_morlet_t <- cwt_wst(signal=data_t[[l]]$norm,scales=scale,wname='MORLET',
      #                        powerscales=FALSE,makefigure=FALSE,dt=1,wparam=5)
      wt_morlet_p <- cwt_wst(signal=data_p[[l]]$Precip,scales=scale,wname='MORLET',
                             powerscales=FALSE,makefigure=FALSE,dt=1,wparam=5)
      wt_morlet_t <- cwt_wst(signal=data_t[[l]]$Temp,scales=scale,wname='MORLET',
                             powerscales=FALSE,makefigure=FALSE,dt=1,wparam=5)
      ### return CWT coefficients as a complex matrix with rows and columns representing times and scales, respectively.
      morlet_mat_p <- as.matrix(wt_morlet_p$coefs)
      morlet_mat_t <- as.matrix(wt_morlet_t$coefs)
      
      
      ### something is wrong with the scale of modulus
      
      ### derive modulus of complex numbers (radius)
      modulus_p <- Mod(morlet_mat_p)
      modulus_t <- Mod(morlet_mat_t)
      
      ### extract phases (argument)
      phases_p <- Arg(morlet_mat_p)
      phases_t <- Arg(morlet_mat_t)
      
      ### use the noise matrix corresponding to this run
      noise_mat <- noise_mat_r[[r]]
      phases_random <- Arg(noise_mat)
      # hist(phases_random[,1])
      # hist(phases_p[,1])
      # hist(phases_t[,1])
      # plot(phases_random[,1][1:365],type='l')
      # plot(phases_t[,1][1:365],type='l')
      # plot(phases_random[,2][1:365],type='l')
      # plot(phases_t[,2][1:365],type='l')
      # plot(phases_t[,1][1:365],phases_random[,1][1:365])
      
      # plot(phases_random[,3][1:365],type='l')
      # plot(phases_random[,6][1:365],type='l')
      
      ### does not seem to affect results
      # fix<-1
      # if(!is.na(fix)){
      #   for(f in fix){
      #     ### replace randomized phases with original ones
      #     phases_random[,f] <- phases_t[,f]
      #   }
      # }
      
      ### iv) combine this randomised phase and the WT modulus of the original signal to obtain a surrogate time-frequency distribution
      ### create a new matrix
      ### combine modulus of original series to randomised phase: create new matrix of complex values
      mat_new_p <- matrix(complex(modulus=modulus_p,argument=phases_random),ncol=ncol(phases_random))
      mat_new_t <- matrix(complex(modulus=modulus_t,argument=phases_random),ncol=ncol(phases_random))
      
      ### v) inverse wavelet transform
      ### apply inversion to CWT of original data
      ### line OK
      rec_orig_p = fun_icwt(x=morlet_mat_p)+mean(data_p[[l]]$Precip)
      rec_orig_t = fun_icwt(x=morlet_mat_t)+mean(data_t[[l]]$Temp)
      
      ### apply wavelet reconstruction to randomized signal
      rec_p<- fun_icwt(x=mat_new_p)  
      rec_t<- fun_icwt(x=mat_new_t) 
      # plot(rec_orig_t[1:365],type='l')
      # plot(rec_t[1:365],type='l')
      # plot(rec_orig_t[1:1000],type='l')
      # plot(rec_t[1:1000],type='l')
      # 
      # plot(rec_orig_p[1:365],type='l')
      # plot(rec_p[1:365],type='l')
      ### add mean
      # rec_random_p<-rec_p+mean(data_p[[l]]$Precip)
      # rec_random_t<-rec_t+mean(data_t[[l]]$Temp)
      rec_random_p<-rec_p
      rec_random_t<-rec_t
      
      ### look at reconstructed signal
      # plot(data_p[[l]]$Precip[1:100],type="l")
      # lines(rec_random_p[1:100],col=2)
      # lines(rec_orig_p[1:100],col='green')
      # plot(data_t[[l]]$Temp[1:100],type="l")
      # lines(rec_orig_t[1:100],col='green3')
      
      ### look at acf to figure out when change in acf happens
      # par(mfrow=c(2,2))
      # acf(data_t[[l]]$Temp)
      # acf(data_t[[l]]$Temp-mean(data_t[[l]]$Temp,na.rm=T))
      # acf(rec_orig_t)
      # acf(rec_t)
      # acf_rand <- acf(rec_random_t,plot=F)
      # lines(acf_rand$acf)
      
      # plot(rec_random)
      # par(mfrow=c(1,2))
      # hist(rec_orig)
      # # hist(rec_random)
      # hist(abs(rec_random))
      
      ### create new data frame
      data_new <- data.frame("random_p"=rec_random_p,'random_t'=rec_random_t)
      
      ### add months and years
      data_new$MM <- data_p[[l]]$MM
      data_new$DD <- data_p[[l]]$DD
      data_new$YYYY <- data_p[[l]]$YYYY
      data_new$index <- data_p[[l]]$index
      
      ### use transformed data directly
      data_new$seasonal_p <- data_new$random_p
      data_new$seasonal_t <- data_new$random_t
      # acf(data_t[[l]]$Temp)
      # acf(data_new$random_t) ### too little autocorrelation at longer lags
      # plot(data_new$seasonal_p,data_new$seasonal_t)
      
      # ### derive the ranks of the data
      data_new$rank_p <- rank(data_new$seasonal_p)
      data_new$rank_t <- rank(data_new$seasonal_t)
      # ranks_test <- rank(data_new$seasonal_t,ties.method='random')
      # plot(data_new$rank_t,ranks_test)
      ### vi) rescale the surrogate to the distribution of the original time series 
      ### apply daily backtransformation: ensures smoothness of regimes
      d<-1
      data_new$simulated_p <- NA
      data_new$simulated_t <- NA
      
      
      # if(fit=='monthly'){
        ### generate sample from bivariate empirical copula
        # emp_cop_samp <- cbind(pobs(-prec[which(data_p[[l]]$MM==m)]),pobs(temp[which(data_t[[l]]$MM==m)]))[sample(size=length(temp[which(data_t[[l]]$MM==m)]),x=c(1:length(temp[which(data_t[[l]]$MM==m)]))),]
        ### compute empirical bivariate distribution to be used for ranking
        
        ### temperature
        for(m in c(1:12)){ 
          data_month <- data_t[[l]][which(as.numeric(data_t[[l]]$MM)%in%c(m)),]
          ### add noise here?
          # noise <- rnorm(n=length(data_month$Temp),0,4)
          # data_month$Temp <- data_month$Temp + noise ### no, changes distribution
          
          ### skewed exponential power
          if(t_margin=='sep'){
            sample <- rlmomco(n=length(data_month$Temp),t_fit[[l]][[m]])
            ### add random noise
            # noise <- rnorm(n=length(sample),0,4)
            # sample <- sample + noise ### no, changes distribution
          }
          ### normal
          # if(t_margin=='normal'){
          #   sample <- rnorm(n=length(data_month$Temp),mean=t_fit[[l]][[m]]$estimate[1],sd=t_fit[[l]][[m]]$estimate[2])
          # }
          # ### or use empirical distribution
          # if(t_margin=='empirical'){
          #   sample <- data_month$Temp
          # }
          ### if other distribution
          if(t_margin!='sep'){
            sample <- rCDF_t(n=length(data_month$Temp),t_fit[[l]][[m]])
          }
          ### test effect of marginal distribution on acf
          ### rank-backtransformation changes acf!!!
          # sample <- rgev(n=length(data_month$Temp),loc=mean(data_month$Temp),scale=2,shape=0.5)
          # hist(data_month$Temp,breaks=20)
          # hist(sample,add=T,col="blue",breaks=20)
          # plot(data_month$Temp[order(rank(data_month$Temp))],sample[order(rank(sample))])
          ### rank order data
          ranks <- rank(sample,ties.method='first')
          # ranks <- rank(round(sample,1))
          # data_new$rank_t[which(as.numeric(data_t[[l]]$MM)%in%c(m))] <- rank(data_new[which(as.numeric(data_t[[l]]$MM)%in%c(m)),]$seasonal_t)
          data_new$rank_t[which(as.numeric(data_t[[l]]$MM)%in%c(m))] <- rank(round(data_new[which(as.numeric(data_t[[l]]$MM)%in%c(m)),]$seasonal_t),1)
          
          ### identify value corresponding to rank in the precipitation time series
          data_ordered <- sample[order(ranks)]
          data_new$simulated_t[which(as.numeric(data_t[[l]]$MM)%in%c(m))] <- data_ordered[data_new$rank_t[which(as.numeric(data_t[[l]]$MM)%in%c(m))]]
        }
        ### look at temperature
        # plot(data_t[[l]]$Temp[1:365],type='l',ylim=c(-10,35))
        # lines(data_new$simulated_t[1:365],type='l',ylim=c(-10,35))
        # # plot(data_new$seasonal_t[1:365],type='l')
        # plot(data_t[[l]]$Temp[1:700],type='l',ylim=c(-10,35))
        # plot(data_new$simulated_t[1:700],type='l',ylim=c(-10,35))
        # plot(data_t[[l]]$Temp[5000:6000],type='l',ylim=c(-10,35))
        # plot(data_new$simulated_t[5000:6000],type='l',ylim=c(-10,35))
        # plot(data_t[[l]]$Temp[1:2000],type='l')
        # plot(data_new$simulated_t[1:2000],type='l')
        # plot(data_t[[l]]$Temp,type='l')
        # plot(data_new$simulated_t,type='l')
        
        ### precipitation
        for(m in c(1:12)){    
          #   ### use empirical distribution for backtransformation
          data_month <- data_p[[l]][which(as.numeric(data_p[[l]]$MM)%in%c(m)),]
          
          ### generate as many values from the E-GPD as there are positive values in the observations
          # sample <- rextgp(n=length(which(outs=='1')),kappa=p_fit[[l]][[m]]$fit$pwm[1],sigma=p_fit[[l]][[m]]$fit$pwm[2], xi=p_fit[[l]][[m]]$fit$pwm[3])
          if(p_margin=='egpd'){
            sample <- rextgp(n=length(which(data_month$Precip>0)),kappa=p_fit[[l]][[m]]$fit$pwm[1],sigma=p_fit[[l]][[m]]$fit$pwm[2], xi=p_fit[[l]][[m]]$fit$pwm[3])
          }
          if(p_margin!='egpd'){
            sample <- rCDF_t(n=length(data_month$Precip),p_fit[[l]][[m]])
          }
          ### In addition, use as many 0 values as in the observations
          ### mix zeros with E-GPD.
          zeros <- rep(0,length(which(data_month$Precip==0)))
          outs <- c(sample,zeros)
          ### combine with markov series
          ### replace states of 1 with actual values
          # outs[outs=='1'] <- sample ### rain use value from gamma distribution
          # outs <- as.numeric(outs)
          # plot(data_month$Precip[1:100],type='l')
          # plot(outs[1:100],type='l')
          # hist(data_month$Precip,breaks=20)
          # hist(outs,add=T,col=adjustcolor("blue",0.5),breaks=20)
          
          ### rank order data
          # ranks <- rank(outs)
          ## have to address ties problem because of many 0s
          ranks <- rank(outs,ties.method='first')
          data_new$rank_p[which(as.numeric(data_p[[l]]$MM)%in%c(m))] <- rank(data_new[which(as.numeric(data_p[[l]]$MM)%in%c(m)),]$seasonal_p)
          ### identify value corresponding to rank in the precipitation time series
          data_ordered <- outs[order(ranks)]
          data_new$simulated_p[which(as.numeric(data_p[[l]]$MM)%in%c(m))] <- data_ordered[data_new$rank_p[which(as.numeric(data_p[[l]]$MM)%in%c(m))]]
        }        
        ### look at precipitation
        # plot(data_p[[l]]$Precip[1:365],type='l')
        # lines(data_new$simulated_p[1:365],type='l')
        # plot(data_p[[l]]$Precip[1:2000],type='l')
        # plot(data_new$simulated_p[1:2000],type='l')
        # plot(data_p[[l]]$Precip,type='l',ylim=c(0,30))
        # plot(data_new$simulated_p,type='l',ylim=c(0,30))
        
        # if(fit_t=='annual'){
        #     data <- data_t[[l]]
        #     ### add noise here?
        #     # noise <- rnorm(n=length(data_month$Temp),0,4)
        #     # data_month$Temp <- data_month$Temp + noise ### no, changes distribution
        #     
        #     ### skewed exponential power
        #     if(t_margin=='sep'){
        #       sample <- rlmomco(n=length(data$Temp),t_fit[[l]])
        #       ### add random noise?
        #       # noise <- rnorm(n=length(sample),0,4)
        #       # sample <- sample + noise ### no, changes distribution
        #     }
        #     ### normal
        #     # if(t_margin=='normal'){
        #     #   sample <- rnorm(n=length(data$Temp),mean=t_fit[[l]]$estimate[1],sd=t_fit[[l]][[m]]$estimate[2])
        #     # }
        #     ### or use empirical distribution
        #     if(t_margin=='empirical'){
        #       sample <- data$Temp
        #     }
        #     ### test effect of marginal distribution on acf
        #     ### rank-backtransformation changes acf!!!
        #     # sample <- rgev(n=length(data_month$Temp),loc=mean(data_month$Temp),scale=2,shape=0.5)
        #     hist(data$Temp,breaks=20)
        #     hist(sample,add=T,col="blue",breaks=20)
        #     plot(data$Temp[order(rank(data$Temp))],sample[order(rank(sample))])
        #     ### rank order data
        #     ranks <- rank(sample)
        #     # ranks <- rank(round(sample,1))
        #     data_new$rank_t <- rank(data_new$seasonal_t)
        #     # data_new$rank_t <- rank(round(data_new$seasonal_t),1)
        #     
        #     ### identify value corresponding to rank in the precipitation time series
        #     data_ordered <- sample[order(ranks)]
        #     data_new$simulated_t <- data_ordered[data_new$rank_t]
        # }
        
        ### temperature is too unequally spred across years
        ### try to simulate and rank transform for each year individually
        # for(y in c(1:length(unique(data_t[[l]]$YYYY)))){
        #   years <- unique(data_t[[l]]$YYYY)
        #   data_t_year <- data_t[[l]][which(as.numeric(data_t[[l]]$YYYY)%in%c(years[y])),]
        #   for(m in c(1:12)){ 
        #     data_month <- data_t_year[which(as.numeric(data_t_year$MM)%in%c(m)),]
        #     ### skewed exponential power
        #     if(t_margin=='sep'){
        #       sample <- rlmomco(n=length(data_month$Temp),t_fit[[l]][[m]])
        #       ### add random noise?
        #       # noise <- rnorm(n=length(sample),0,4)
        #       # sample <- sample + noise ### no, changes distribution
        #     }
        #     
        #     ### test effect of marginal distribution on acf
        #     ### rank-backtransformation changes acf!!!
        #     # sample <- rgev(n=length(data_month$Temp),loc=mean(data_month$Temp),scale=2,shape=0.5)
        #     hist(data_month$Temp,breaks=20)
        #     hist(sample,add=T,col="blue",breaks=20)
        #     ### rank order data
        #     ranks <- rank(sample,ties.method='first')
        #     data_year <- data_new[which(as.numeric(data_new$YYYY)%in%c(years[y])),]
        #     ### rank within year instead of over all years
        #     data_year$rank_t <- rank(data_year$seasonal_t)
        #     data_year$rank_t[which(as.numeric(data_t_year$MM)%in%c(m))] <- rank(data_new[which(as.numeric(data_t_year$MM)%in%c(m)),]$seasonal_t)
        #     ### identify value corresponding to rank in the precipitation time series
        #     data_ordered <- sample[order(ranks)]
        #     data_year$simulated_t[which(as.numeric(data_t_year$MM)%in%c(m))] <- data_ordered[data_year$rank_t[which(as.numeric(data_t_year$MM)%in%c(m))]]
        #   }
        #     plot(data_t_year$Temp,type='l')
        #     plot(data_year$simulated_t,type='l')
        #     ### append for each year
        #     data_new$simulated_t[which(as.numeric(data_new$YYYY)%in%c(years[y]))] <- data_year$simulated_t
        #   }
        #   ### look at temperature
        #   plot(data_t[[l]]$Temp[1:365],type='l')
        #   plot(data_new$simulated_t[1:365],type='l')
        #   plot(data_t[[l]]$Temp[1:2000],type='l')
        #   plot(data_new$simulated_t[1:2000],type='l')
        #   plot(data_t[[l]]$Temp,type='l')
        #   plot(data_new$simulated_t,type='l')
      # }
      
      ### try to add random noise in order to reduce long-range dependence, which is overestimated
      # noise <- rnorm(n=length(data_new$simulated_p),0,1.5)
      data_sim_p[[r]] <- data_new$simulated_p
      data_sim_t[[r]] <- data_new$simulated_t
   
      if(verbose) cat(".")
      ### next simulation run
    }
    if(verbose) cat("\nFinished.\n")
    ### put observed and simulated data into a data frame
    data_sim_p <- as.data.frame(data_sim_p)
    names(data_sim_p) <- paste("r",seq(1:number_sim),sep="")
    data_stoch_p <- data.frame(data_p[[l]][,c("YYYY", "MM", "DD", "timestamp", "Precip")],
                               data_sim_p)
    data_sim_t <- as.data.frame(data_sim_t)
    names(data_sim_t) <- paste("r",seq(1:number_sim),sep="")
    data_stoch_t <- data.frame(data_t[[l]][,c("YYYY", "MM", "DD", "timestamp", "Temp")],
                               data_sim_t)
    
    # if (GoFtest=="NULL") {  
    #   p_vals <- NULL 
    # }
    
  ### store values in list
  out_list[[l]] <- list(data_stoch_t,data_stoch_p)
    
  ### to to next station
  }
  # if(is.na(out_dir)){
  # return(list(data_stoch_p,data_stoch_t))
  # }
  return(out_list)
}
