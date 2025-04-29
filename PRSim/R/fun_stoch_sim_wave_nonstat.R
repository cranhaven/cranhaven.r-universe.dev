# data<- runoff_multi_site_T
# library('PRSim')
# library(wavScalogram)
# library(splus2R)
# library(Lmoments) 
# station_id="Qobs"
# number_sim=1
# win_h_length=15
# marginal<- 'kappa'
# n_par <- 4
# n_wave <- 100
# marginalpar=TRUE
# GoFtest=NULL
# verbose=TRUE
# suppWarn=FALSE
# cov_name <- 'T'

prsim.wave.nonstat <- function(data, station_id="Qobs", number_sim=1, win_h_length=15,
                  marginal=c("kappa","empirical"), n_par=4, n_wave=100, cov_name='T', marginalpar=TRUE,
                  GoFtest=NULL, verbose=TRUE, suppWarn=FALSE, warming_level, ...){

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
  if (!is.null(GoFtest)) {
    GoFtest <- toupper(GoFtest)[1]
    if (!(GoFtest %in% c("AD","KS"))) stop("'GoFtest' should be either 'NULL', 'AD' or 'KS'.")
  } else  GoFtest <- "NULL"


  marginal <- marginal[1]    # take only the first element
  if (!(marginal %in% c("kappa","empirical"))) {   # check if distributions exist
    if (!is.character(marginal)) stop("'marginal' should be a character string.")
    rCDF <- get(paste0("r",marginal), mode = "function")
    CDF_fit <- get(paste0(marginal,"_fit"), mode = "function")
    if (GoFtest=="AD")	  pCDF <- get(paste0("p",marginal), mode = "function")
  }

  op <- options("warn")$warn
  ### input data needs to be of the format year (four digits), month (two digits), day (one digit), input discharge time series

  ### check for correct input data labels and length
  ### run through all stations: list
  for(l in 1:length(data)){
    if (nrow(data[[l]])[1]<730) stop("At least one year of data required.")
    if (is.numeric(station_id)){
      station_id <- colnames(data[[l]])[station_id]
    }
    if (is.na(station_id)||!("Qobs" %in% colnames(data[[l]]))) stop("Wrong column (name) for observations selected.")

    # test for proper format:
    if (any(class(data[[l]][,1])%in%c("POSIXct","POSIXt"))){
      data <- data.frame(YYYY=as.integer(format(data[[l]][,1],'%Y')),
                         MM=as.integer(format(data[[l]][,1],'%m')),
                         DD=as.integer(format(data[[l]][,1],'%d')),
                         Qobs=data[[l]][,station_id],
                         T=data[[l]][,cov_name],
                         timestamp=data[[l]][,1])
    } else {
      if(!all(c("YYYY","MM","DD") %in% colnames(data[[l]]))) stop("Wrong time column names")

      data[[l]] <- data[[l]][,c("YYYY","MM","DD", station_id,'T')]
      tmp <- paste(data[[l]]$YYYY,data[[l]]$MM,data[[l]]$DD,sep=" ")
      names(data[[l]]) <- c("YYYY","MM","DD","Qobs",cov_name)
      data[[l]]$timestamp <- as.POSIXct(strptime(tmp, format="%Y %m %d", tz="GMT"))
    }

    ### remove February 29
    data[[l]] <- data[[l]][format(data[[l]]$timestamp, "%m %d") != "02 29",]
    ### remove incomplete years
    if(which(format(data[[l]]$timestamp,format='%j')=='001')[1]>1){
      data[[l]] <- data[[l]][-c(1:(which(format(data[[l]]$timestamp,format='%j')=='001')[1]-1)),]
    }
    if ((nrow(data[[l]]) %% 365)>0) stop("No missing values allowed. Some days are missing.")

    ### replace missing data by mean values
    if(length(which(is.na(data[[l]]$timestamp)))>0){
      ### replace days with missing data
      data[[l]][which(is.na(data[[l]]$timestamp)),]$Qobs <- mean(data[[l]]$Qobs,na.rm=T)
    }

    ### generate a day index
    # data[[l]]$index <- rep(c(1:365), times=length(unique(data[[l]]$YYYY)))
    data[[l]]$index <- as.numeric(format(data[[l]]$timestamp,format='%j'))
    ### replace empty index positions
    if(length(which(is.na(data[[l]]$index))>0)){
      data[[l]]$index[which(is.na(data[[l]]$index))] <- rep(c(1:365), times=length(unique(data[[l]]$YYYY)))[which(is.na(data[[l]]$index))]
    }
    ### replace NA values by seasonal cycle as simulating series in the case of many NA values produces strange results
    if(length(which(is.na(data[[l]]$Qobs)))>0){
      cycle <- aggregate(data[[l]]$Qobs,by=list(data[[l]]$index),FUN=mean,na.rm=TRUE)
      data_missing <- data[[l]][which(is.na(data[[l]]$Qobs)),]
      data_missing$Qobs <- cycle$x[as.numeric(data_missing$index)]
      data[[l]][which(is.na(data[[l]]$Qobs)),]$Qobs <- data_missing$Qobs
    }
  }

  if (verbose) cat(paste0("Detrending with (half-)length ",win_h_length,"...\n"))

  ### (1) Generation of white noise for random phases generation
  ### generate random sample of indices for each simulation run
  # set.seed(10)
  noise_mat_r <- list()
  for (r in 1:number_sim){
    ts_wn <- rnorm(n=length(data[[1]]$Qobs), mean = 0, sd = 1) ### iid time seris
    # wt_noise <- wavCWT(x=ts_wn,wavelet="morlet",n.scale=n_wave) ### needs to be replaced
    # noise_mat_r[[r]] <- as.matrix(wt_noise)
    ### test new potential functions
    # wt_noise <- WaveletTransform(ts_wn,dt=1,dj=1/10)
    ### determine scale range
    scale.range = deltat(data[[l]]$Qobs) * c(1, length(data[[l]]$Qobs))
    ### sampling interval
    sampling.interval <- 1
    ### determine octave
    octave <- logb(scale.range, 2)
    ### determine wavelet scales
    scale <- ifelse1(n_wave > 1, 2^c(octave[1] + seq(0, n_wave -
                                                        2) * diff(octave)/(floor(n_wave) - 1), octave[2]), scale.range[1])
    scale <- unique(round(scale/sampling.interval) * sampling.interval)

    wt_morlet <- cwt_wst(signal=ts_wn,scales=scale,wname='MORLET',makefigure=FALSE,dt=1,powerscales=FALSE)
    noise_mat_r[[r]] <- as.matrix(wt_morlet$coefs)
  }

  ### fitting of kappa distribution to all stations for which simulations are to be derived
  par_day_list <- marginal_list<- list()
  for(l in 1:length(data)){
    ### daily fitting of Kappa distribution
    ### fit the parameters of the Kappa distribution for each day separately.
    ### To enable a sufficient sample size by using daily values in moving window around day i (i.e., reduce uncertainty due to fitting)

    if(marginal=='empirical'){
      marginal_list[[l]]<-'empirical'
    }
    if(marginal=="kappa"){
      marginal_list[[l]] <- 'kappa'
      p_vals <- numeric(365)
      par_day <- matrix(0, nrow=365, ncol=4)
      # density_kap <- list()
      ### define window length
      win_length <- c(1:win_h_length)
      for(d in c(1:365)){
        ### define start and end of window
        before <- data[[l]]$index[d+365-win_length]
        after <- data[[l]]$index[d+365+win_length-1]
        ### define days within window
        ids <- c(before, after)
        ### determine values in window around day i
        data_window <- data[[l]][which(data[[l]]$index%in%ids),]

        # par.kappa(data_monthly)
        ll<- Lmoments(data_window$Qobs)


          ###===============================###===============================###
          ### Step 1: estimate mu (mean flow) using some sort of regression model (can be linear or more flexible)
          ### using temperature as a covariate
          ###===============================###===============================###
          ### simplest case: linear model (maybe go for more sophisticated option later on)
          # plot(data_window$timestamp,data_window$Qobs,type='h',ylab='Discharge (m3/s)',xlab='Date (day)',main=paste('Day =',d,sep=''))
          lm_T <- lm(Qobs~T,data=data_window)
          Q_pred <- predict(lm_T,data_window)
          # lines(data_window$timestamp,Q_pred,col='grey')
          ### use some sort of mean temperature as covariate, e.g. annual mean temperature
          ### I want to predict overall Q trend not daily variations
          # # annual_mean <- aggregate(data_window,by=list(format(data_window$Date,format='%Y')), FUN='mean')
          # # plot(annual_mean$Qobs,annual_mean$P)
          # # plot(annual_mean$T,annual_mean$Qobs,xlab='Temperature',ylab='Discharge')
          # # abline(lm(annual_mean$Qobs~annual_mean$T)) ### increasing T -> increasing Q
          #
          # ### linear model based on annual averages
          # lm_T <- lm(Qobs~T,data=annual_mean)
          # summary(lm_T)
          # ### add P as additional covariate?
          # lm_T_P <- lm(Qobs~T+P,data=annual_mean)
          # summary(lm_T_P) ### much better fit
          # Q_pred <- predict(lm_T_P,annual_mean)
          # plot(annual_mean$Qobs,Q_pred)
          # abline(0,1)
          # ### residuals: look iid, good
          # plot(annual_mean$Qobs-Q_pred)
          ### now, use same model to predict daily values
          # Q_pred <- predict(lm_T,newdata=data_window)
          # # Q_pred <- predict(lm_T_P,newdata=data_window)
          # plot(data_window$Date,data_window$Qobs,type='h',ylab='Discharge (m3/s)',xlab='Date (day)')
          # lines(data_window$Date,Q_pred,col='grey')

          ### compute residuals: obs-pred
          residuals <- data_window$Q-Q_pred
          # plot(residuals,type='l')
          # hist(residuals)

          ### Step 2: fit kappa model to residuals, set mean to 0, because already removed
          ###===============================###===============================###
          ### fit kappa distribution and assess goodness-of-fit
          ll<- Lmoments(na.omit(residuals))

          ### test whether Kappa distribution can be fit
          if (suppWarn) {
            suppressWarnings( test <- try(par.kappa(ll[1],ll[2],ll[4],ll[5]), silent = TRUE) )
          } else {
            test <- try(par.kappa(ll[1],ll[2],ll[4],ll[5]), silent = TRUE)
          }

          ### fit non-stationary kappa distribution
          if(length(test)>1){
          ### determine parameter of kappa distribution
          kap_par <- par.kappa(0,ll[2],ll[4],ll[5])

          ### define vector of quantiles
          quant <- sort(residuals)
          thresh <- kap_par$xi + kap_par$alfa*(1 - kap_par$h^(-kap_par$k))/kap_par$k
          if(!is.na(thresh)|!is.nan(thresh)){
            min(quant)>thresh
            ### only use quantiles larger than threshold (as in f.kappa function documentation)
            quant <- quant[which(quant>thresh)]
          }
          # kappa_density <- f.kappa(x=quant,xi=kap_par$xi,alfa=kap_par$alfa,k=kap_par$k,h=kap_par$h)
          # plot(kappa_density)
          data_kap <- rand.kappa(length(na.omit(residuals)),xi=kap_par$xi,alfa=kap_par$alfa,k=kap_par$k,h=kap_par$h)
          density_kap <- density(data_kap)
          # hist(residuals,prob=TRUE)
          # lines(density_kap,col="red")
          p_val <- ks.test(residuals,data_kap)$p.value ### kappa distribution not rejected at alpha=0.05

          ### QQ-plot
          # plot(sort(residuals),sort(data_kap),xlab='Residuals',ylab='Residuals simulated with Kappa')
          # abline(0,1)

          ### Step 3: simulate for period with changed t, by using the mu parameter estimated with model from Step 1, and combining it with distribution parameters from Step 2.
          ###===============================###===============================###
          ### predict mu using regression model from Step 1.
          ### 1.5? warmer world
          data_new <- data.frame('T'=c(mean(data_window$T)+warming_level[l]))
          ### predict mu
          pred_Q_new <- predict(lm_T,newdata=data_new)
          ### use this predicted mu in kappa distribution
          kap_par_ns <- par.kappa(pred_Q_new,ll[2],ll[4],ll[5])
          data_kap_ns <- rand.kappa(length(data_window$Qobs),xi=kap_par$xi,alfa=kap_par$alfa,k=kap_par$k,h=kap_par$h)
          density_kap <- density(data_kap_ns)
          # hist(data_window$Qobs,prob=TRUE)
          # lines(density_kap,col="red")
          # hist(data_kap)
          ### set negative values to 0
          data_kap[which(data_kap<0)] <- 0
          ### QQ-plot
          # plot(sort(data_window$Q),sort(data_kap),xlab='Observed discharge',ylab='Discharge simulated with non-stat Kappa')
          # abline(0,1)

          ### store parameters of the non-stationary kappa distribution
          kap_par <- kap_par_ns
          par_day[d,] <- unlist(kap_par)

          if (tolower(GoFtest)=="ks")
            p_vals[d] <- ks_test(data_window, data_kap) ### kappa distribution not rejected at alpha=0.05
  #        p_vals[d] <- ks.test(data_window, data_kap)$p.value ### kappa distribution not rejected at alpha=0.05
          if (tolower(GoFtest)=="ad") {

            try_ad_test <- try(ad.test(data_window,F.kappa,xi=kap_par$xi,alfa=kap_par$alfa,k=kap_par$k,h=kap_par$h), silent=TRUE)
            if(length(try_ad_test)==1){
              p_vals[d]  <- NA
            }else{
              p_vals[d]  <- try_ad_test$p.value
            }
          }
        } else{
          if(d==1){
            p_vals[d] <- NA
            par_day[d,] <- NA
          }else{
            p_vals[d] <- p_vals[d-1]
            par_day[d,] <- par_day[d-1,]
          }
        }
      }


      ### Treatment for the case when Kappa distribution can not be fitted
      ### a) parameters can't be fitted for any of the days
      if(length(which(is.na(par_day[,1])))==365){
        ### use empirical distribution instead
        marginal_list[[l]]<-'empirical'
      } else{
        ### b) parameters can be fitted for some days
        ### replace NA entries by values estimated for subsequent day
        if(length(which(is.na(par_day[,1])))>0){
          indices <- rev(which(is.na(par_day[,1])))
          for(i in 1:length(indices)){
            par_day[indices[i],] <- par_day[indices[i]+1,]
          }
        }
      }
      par_day_list[[l]] <- par_day
    }

      ### use either a predefined distribution in R or define own function
      if(marginal!="kappa" & marginal!="empirical"){
        marginal_list[[l]] <- marginal
        p_vals <- numeric(365)
        par_day <- matrix(0, nrow=365, ncol=n_par)
        for(d in c(1:365)){
          ### define window length
          win_length <- seq(1:15)
          ### define start and end of window
          before <- data[[l]]$index[d+365-win_length]
          after <- data[[l]]$index[d+365+win_length-1]
          ### define days within window
          ids <- c(before,after)
          ### determine values in window around day i
          data_window <- data[[l]]$Qobs[which(data[[l]]$index%in%ids)]
          theta <-  CDF_fit(xdat=data_window, ...)

          ### goodness of fit test
          data_random <- rCDF(n=length(data_window), theta)

          # density_gengam[[d]] <- density(data_gengam)
          # hist(data_window)
          # hist(data_random,add=T,col="red")
          if (tolower(GoFtest)=="ks"){
            p_vals[d] <- ks_test(data_window,data_random)
#            p_vals[d] <- ks.test(data_window,data_random)$p.value
          }
          if (tolower(GoFtest)=="ad"){
            p_vals[d] <-  ad.test(data_window,pCDF,theta)$p.value
          }
          ### store parameters
          par_day[d,] <- theta
        }
        par_day_list[[l]] <- par_day
      }
  }


   ### replace NA values by mean if necessary: otherwise, problems with transformation
  for(l in 1:length(data)){
    # if(length(which(is.na(data[[l]]$Qobs)))>0){
    #   data[[l]]$Qobs[which(is.na(data[[l]]$Qobs))] <- mean(data[[l]]$Qobs,na.rm=T)
    # }

    ### center_data: substract mean from values
    data[[l]]$norm <- data[[l]]$Qobs-mean(data[[l]]$Qobs,na.rm=T)
  }

  ### repeat stochastic simulation several times

  if(verbose) cat(paste0("Starting ",number_sim," simulations:\n"))

  ### run through all stations
  out_list<-list()
  for(l in 1:length(data)){
    ### list for storing results
    data_sim <- list()
    ### simulate n series
    for (r in c(1:number_sim)){
      ###===============================###===============================###
      ### use the R-package wmtsa, which relates to the book by Percival and Walden
      ### on wavelet methods for time series analysis
      ### allows for a flexible range of different wavelet filters: Morlet, Daubechies, Gaussian,...
      ### A) Produce surrogates using phase randomization as in Chavez and Cazelles 2019
      ###===============================###===============================###
      ### A) Produce surrogates using phase randomization as in Chavez and Cazelles 2019
      ### Requirement: choose a complex values filter: Morlet
      ### later on test alternatives: e.g. Gaussian filter
      ### i) use continuous wavelet transform (CWT) to wavelet transform the data
      ### then, follow the randomization procedure proposed by Chavez and Cazelles 2019
      ### ii) generate a Gaussian white noise time series to match the original data length
      ### iii) derive the wavelet transform of this noise to extract the phase
      ### iv) combine this randomised phase and the WT modulus of the original signal to obtain a surrogate time-frequency distribution
      ### v) inverse wavelet transform
      ### vi) rescale the surrogate to the distribution of the original time series by sorting the data (after a wavelet filtering in the frequency band of interest) according to the ranking of values of the wavelet-based surrogate

      #Extract the real part for the reconstruction: (see Torrence and Campo equation 11)

      ###===============================###===============================###
      ### i) use continuous wavelet transform (CWT) to wavelet transform the data
      ### needs to be replaced as wmtsa was orphaned
      # wt_morlet_old <- wavCWT(x=data[[l]]$norm,wavelet="morlet",n.scale=n_wave)
      #
      # ### return CWT coefficients as a complex matrix with rows and columns representing times and scales, respectively.
      # morlet_mat_old <- as.matrix(wt_morlet_old)
      # ### derive modulus of complex numbers (radius)
      # modulus <- Mod(morlet_mat_old)
      # ### extract phases (argument)
      # phases <- Arg(morlet_mat_old)
      #
      # ### use the noise matrix corresponding to this run
      # noise_mat <- noise_mat_r[[r]]
      # phases_random <- Arg(noise_mat)

      ### use alternative R-package instead
      # wt_morlet <- WaveletTransform(x=data[[l]]$norm,dt=1,dj=1/8)
      ### determine scale range
      scale.range = deltat(data[[l]]$norm) * c(1, length(data[[l]]$norm))
      ### sampling interval
      sampling.interval <- 1
      ### determine octave
      octave <- logb(scale.range, 2)
      ### determine wavelet scales
      scale <- ifelse1(n_wave > 1, 2^c(octave[1] + seq(0, n_wave -
                                                          2) * diff(octave)/(floor(n_wave) - 1), octave[2]), scale.range[1])
      scale <- unique(round(scale/sampling.interval) * sampling.interval)
      ### these scales correspond to the scales originall used in wavCWT()

      ### apply continuous wavelet transform: use package wavScalogram
      wt_morlet <- cwt_wst(signal=data[[l]]$norm,scales=scale,wname='MORLET',
                           powerscales=FALSE,makefigure=FALSE,dt=1,wparam=5)
      ### return CWT coefficients as a complex matrix with rows and columns representing times and scales, respectively.
      morlet_mat <- as.matrix(wt_morlet$coefs)

      ### something is wrong with the scale of modulus

      ### derive modulus of complex numbers (radius)
      modulus <- Mod(morlet_mat)
      ### extract phases (argument)
      phases <- Arg(morlet_mat)

      ### use the noise matrix corresponding to this run
      noise_mat <- noise_mat_r[[r]]
      phases_random <- Arg(noise_mat)

      # ### fix all phases at a specified level
      # ### the phases at scale one are not uniformly distributed, fix these
      # fix<-1
      # if(!is.na(fix)){
      #   for(f in fix){
      #     ### replace randomized phases with original ones
      #     phases_random[,f] <- phases[,f]
      #   }
      # }

      ### iv) combine this randomised phase and the WT modulus of the original signal to obtain a surrogate time-frequency distribution
      ### create a new matrix
      ### combine modulus of original series to randomised phase: create new matrix of complex values
      mat_new <- matrix(complex(modulus=modulus,argument=phases_random),ncol=ncol(phases_random))
      ### plug into the original time-frequency object
      ### wmtsa package does not allow for the inverser transform of a CWT object

      ### v) inverse wavelet transform
      ### apply inversion to CWT of original data
        rec_orig = fun_icwt(x=morlet_mat)+mean(data[[l]]$Qobs)
        ### apply wavelet reconstruction to randomized signal
        rec<- fun_icwt(x=mat_new)
        ### add mean
        rec_random<-rec+mean(data[[l]]$Qobs)

        # plot(data[[l]]$Qobs[1:2000],type="l")
        # lines(rec_random[1:1000],col=2)
        # lines(rec_orig[1:1000],col='green')

        # plot(rec_random)
        # par(mfrow=c(1,2))
        # hist(rec_orig)
        # # hist(rec_random)
        # hist(abs(rec_random))

        ### create new data frame
        data_new <- data.frame("random"=rec_random)

        ### add months and years
        data_new$MM <- data[[l]]$MM
        data_new$DD <- data[[l]]$DD
        data_new$YYYY <- data[[l]]$YYYY
        data_new$index <- data[[l]]$index

        ### use transformed data directly
        data_new$seasonal <- data_new$random
        # ### derive the ranks of the data
        data_new$rank <- rank(data_new$seasonal)

        ### vi) rescale the surrogate to the distribution of the original time series
        ### apply daily backtransformation: ensures smoothness of regimes
        d<-1
        data_new$simulated_seasonal <- NA


      for(d in c(1:365)){
        data_day <- data[[l]][which(data[[l]]$index%in%c(d)),]

        ### use kappa distribution for backtransformation
        if(marginal_list[[l]]=="kappa"){
          colnames(par_day_list[[l]]) <- names(kap_par)

          ### use monthly Kappa distribution for backtransformation
          ### simulate random sample of size n from Kappa disribution
          data_day$kappa <- rand.kappa(length(data_day$Qobs),
                                       xi=par_day_list[[l]][d,"xi"],alfa=par_day_list[[l]][d,"alfa"],
                                       k=par_day_list[[l]][d,"k"],h=par_day_list[[l]][d,"h"])


          data_day$rank <- rank(data_day$kappa)

          data_new$rank <- rank(data_new$seasonal)
          data_new$rank[ which(data[[l]]$index%in%c(d)) ] <- rank(data_new[which(data[[l]]$index%in%c(d)), ]$seasonal)
          ### derive corresponding values from the kappa distribution
          ### identify value corresponding to rank in the kappa time series
          data_ordered <- data_day[order(data_day$rank),]
          data_new$simulated_seasonal[which(data_new$index%in%c(d))] <- data_ordered$kappa[data_new$rank[which(data[[l]]$index%in%c(d))]]
          ### if error was applied, replace negative values by 0 values
          ### in any case, replace negative values by 0. Corresponds to a bounded Kappa distribution

          if(length(which(data_new$simulated_seasonal<0))>0){
            ### do not use 0 as a replacement value directly
            # data_new$simulated_seasonal[which(data_new$simulated_seasonal<0)] <- 0
            ### sample value from a uniform distribution limited by 0 and the minimum observed value
            ### determine replacement value
            rep_value <- runif(n=1,min=0,max=min(data_day$Qobs))
            data_new$simulated_seasonal[which(data_new$simulated_seasonal<0)]<-rep_value
          }
        }
        ### use empirical distribution for backtransformation
        if(marginal_list[[l]]=="empirical"){
          data_day$rank <- rank(data_day$Qobs)
          data_new$rank <- rank(data_new$seasonal)
          data_new$rank[which(data[[l]]$index%in%c(d))] <- rank(data_new[which(data[[l]]$index%in%c(d)),]$seasonal)
          ### derive corresponding values from the empirical distribution
          ### identify value corresponding to rank in the original time series
          data_ordered <- data_day[order(data_day$rank),]
          data_new$simulated_seasonal[which(data_new$index%in%c(d))] <- data_ordered$Qobs[data_new$rank[which(data[[l]]$index%in%c(d))]]
          # }
        }

        ### use any predefined distribution for backtransformation
        if(marginal_list[[l]]!="kappa" & marginal_list[[l]]!="empirical"){
          ### use monthly distribution for backtransformation
          ### simulate random sample of size n from disribution
          data_day$cdf <-   rCDF(n=length(data_day$Qobs), par_day_list[[l]][d,])
          data_day$rank <- rank(data_day$cdf)

          data_new$rank <- rank(data_new$seasonal)

          # hist(data_day$Qobs)
          # hist(data_day$cdf,add=T,col="blue")
          # data_day$rank <- rank(data_day$cdf)
          data_new$rank[which(data[[l]]$index%in%c(d))] <- rank(data_new[which(data[[l]]$index%in%c(d)),]$seasonal)
          ### derive corresponding values from the kappa distribution
          ### identify value corresponding to rank in the kappa time series
          data_ordered <- data_day[order(data_day$rank),]
          data_new$simulated_seasonal[which(data_new$index%in%c(d))] <- data_ordered$cdf[data_new$rank[which(data[[l]]$index%in%c(d))]]
        }
      }  # end for loop
      data_sim[[r]] <- data_new$simulated_seasonal

        if(verbose) cat(".")
        ### next simulation run
    }
    if(verbose) cat("\nFinished.\n")
    ### put observed and simulated data into a data frame
    data_sim <- as.data.frame(data_sim)
    names(data_sim) <- paste("r",seq(1:number_sim),sep="")
    data_stoch <- data.frame(data[[l]][,c("YYYY", "MM", "DD", "timestamp", "Qobs")],
                             data_sim)

    if (GoFtest=="NULL") {
      p_vals <- NULL
    }

    # if(!is.na(out_dir)){
    #      ### set output directory
    #   setwd(out_dir)
    #   if(marginal != "empirical"){
    #     if (marginalpar) {  # also return intermediate results
    #       # return(list(simulation=data_stoch, pars=par_day, p_val=p_vals))
    #       out_list <- list(simulation=data_stoch, pars=par_day, p_val=p_vals)
    #       save(file=paste(l,'_stoch_sim.rda',sep=''),out_list)
    #     } else {
    #       # return(list(simulation=data_stoch, pars=NULL, p_val=p_vals))
    #       out_list <- list(simulation=data_stoch, pars=NULL, p_val=p_vals)
    #       save(file=paste(l,'_stoch_sim.rda',sep=''),out_list)
    #     }
    #   }else{
    #     # return(list(simulation=data_stoch))
    #     out_list <- list(simulation=data_stoch, pars=NULL, p_val=NULL)
    #     save(file=paste(l,'_stoch_sim.rda',sep=''),simulation=out_list)
    #   }
    # }
    #
    # if(is.na(out_dir)){
      ### store values in list
      if(marginal != "empirical"){
        if (marginalpar) {  # also return intermediate results
          # return(list(simulation=data_stoch, pars=par_day, p_val=p_vals))
          out_list[[l]] <- list(simulation=data_stoch, pars=par_day, p_val=p_vals)
        } else {
          # return(list(simulation=data_stoch, pars=NULL, p_val=p_vals))
          out_list[[l]] <- list(simulation=data_stoch, pars=NULL, p_val=p_vals)
        }
      }else{
        # return(list(simulation=data_stoch))
        out_list[[l]] <- list(simulation=data_stoch, pars=NULL, p_val=NULL)
      }
    # }
       ### to to next station
  }
  # if(is.na(out_dir)){
    return(out_list)
  # }
}

# # ### run PRSim.nonstat for two example catchments:
# sim_nonstat_all <- prsim.wave.nonstat(data, station_id="Qobs", number_sim=10, win_h_length=15,
#                                marginal=c("kappa"), n_par=4, n_wave=100, cov_name='T', marginalpar=TRUE,
#                                GoFtest=NULL, verbose=TRUE, suppWarn=FALSE, warming_level=c(2,2))
# ### for comparison, also run stationary PRSim
# sim_stat_all <- prsim.wave(data, station_id="Qobs", number_sim=10, win_h_length=15,
#            marginal=c("kappa"), n_par=4, n_wave=100, marginalpar=TRUE,
#            GoFtest=NULL, verbose=TRUE, suppWarn=FALSE)
#
# ###===============================###===============================###
# ### compare non-stationary and stationary simulations
# ###===============================###===============================###
# dir_analysis <- "~/Projects/DFStaR/stoch_sim_non_stationary/results"
# col_obs <- 'grey'
# col_stat <- '#fc8d59'
# col_nonstat <- '#d7301f'
# sim_nonstat <- sim_nonstat_all[[l]]$simulation
# sim_stat <- sim_stat_all[[l]]$simulation
#
# setwd(dir_analysis)
# pdf('obs_vs_simulated_ts.pdf',width=10,height=6)
# par(mfrow=c(2,1),mar=c(4,4,2,1))
# ### whole ts
# plot(sim_stat$timestamp,sim_stat$Qobs,type='l',xlab='Time (d)',ylab=expression(paste("Discharge (m"^"3", "/s)")),col=col_obs)
# lines(sim_stat$timestamp,sim_stat$r1,col=col_stat)
# lines(sim_nonstat$timestamp,sim_nonstat$r1,col=col_nonstat)
# legend('topright',legend=c('obs','sim stat','sim nonstat'),
#        col=c(col_obs,col_stat,col_nonstat),lty=1,bty='n')
# ### 3 years
# plot(sim_stat$timestamp[1:1000],sim_stat$Qobs[1:1000],type='l',xlab='Time (d)',ylab=expression(paste("Discharge (m"^"3", "/s)")),col=col_obs)
# lines(sim_stat$timestamp[1:1000],sim_stat$r1[1:1000],col=col_stat)
# lines(sim_nonstat$timestamp[1:1000],sim_nonstat$r1[1:1000],col=col_nonstat)
# dev.off()
#
# ### compare distributions
# setwd(dir_analysis)
# pdf('obs_vs_sim_distribution.pdf',width=6,height=4)
# par(mar=c(4,4,2,1))
# boxplot(sim_stat$Qobs,sim_stat$r1,sim_stat$r2,sim_stat$r3,sim_stat$r4,sim_stat$r5,sim_nonstat$r1,sim_nonstat$r2,sim_nonstat$r3,sim_nonstat$r4,sim_nonstat$r5,
#         col=c(col_obs,col_stat,col_stat,col_stat,col_stat,col_stat,col_nonstat,col_nonstat,col_nonstat, col_nonstat,col_nonstat),
#         ylab=expression(paste("Discharge (m"^"3", "/s)")),names=c('Obs','s1','s2','s3','s4','s5','ns1','ns2','ns3','ns4','ns5'))
# dev.off()
#
# setwd(dir_analysis)
# pdf(paste(l,'_stat_vs_nonstat_PRsim','.pdf',sep=''),width=8,height=6.5)
#
# par(mfrow=c(3,3),mar=c(4,4,2,1))
#
# ### extract the regimes for each year
# ###===============================###===============================###
#
# ### observations
# year <- seq(from=min(sim_stat$YYYY,na.rm=T),to=max(sim_stat$YYYY,na.rm=T))
#
# ### compute mean runoff hydrograph
# sim_stat$day_id <- rep(seq(1:365),times=length(year))
# mean_hydrograph_obs <- aggregate(sim_stat$Qobs,by=list(sim_stat$day_id),FUN=mean,simplify=FALSE,na.rm=T)
# # lines(mean_hydrograph_obs,lty=1,lwd=3,col="black")
# plot(unlist(mean_hydrograph_obs[,2]),lty=1,lwd=1,col="black",ylab=expression(bold(paste("Discharge [m"^3,"/s]"))),
#      xlab=expression(bold("Time [d]")),main="Mean hydrographs",ylim=c(0,max(unlist(mean_hydrograph_obs[,2]))*1.25),type="l")
#
# ### add mean runoff hydrographs stationary model
# mean_hydrograph_stat <- list()
# for(r in 5:(length(names(sim_stat))-2)){
#   mean_hydrograph_stat[[r]] <- unlist(aggregate(sim_stat[,r],by=list(sim_stat$day_id),FUN=mean,simplify=FALSE)$x)
#   # lines(mean_hydrograph_stat[[r]],lty=1,lwd=1,col=col_stat)
# }
#
# ### non-stationary regimes
# sim_nonstat$day_id <- rep(seq(1:365),times=length(year))
# mean_hydrograph_nonstat <- list()
# for(r in 5:(length(names(sim_nonstat))-2)){
#   mean_hydrograph_nonstat[[r]] <- unlist(aggregate(sim_nonstat[,r],by=list(sim_nonstat$day_id),FUN=mean,simplify=FALSE)$x)
#   # lines(mean_hydrograph_nonstat[[r]],lty=1,lwd=1,col=col_nonstat)
# }
#
# ### use polygons
# ### use polygons instead of spaghetti plots
# ### stationary polygon
# all_hydro_stat <- do.call(cbind,mean_hydrograph_stat)
# ### compute quantile curves, CIs
# q05 <- apply(all_hydro_stat,MARGIN=1,FUN=quantile,0.05,na.rm=TRUE)
# q95 <- apply(all_hydro_stat,MARGIN=1,FUN=quantile,0.95,na.rm=TRUE)
#
# y.low <- q05
# y.high <- q95
# lines(1:length(y.low),y.low,col=col_stat,lwd=1,lty=3)
# lines(1:length(y.high),y.high,col=col_stat,lwd=1,lty=3)
# polygon(c(1:length(y.low), rev(1:(length(y.low)))), c(y.high, rev(y.low)),
#         col = adjustcolor(col_stat,0.5), border = NA)
#
# ### add polygon nonstationary
# all_hydro_nonstat <- do.call(cbind,mean_hydrograph_nonstat)
# ### compute quantile curves, CIs
# q05 <- apply(all_hydro_nonstat,MARGIN=1,FUN=quantile,0.05,na.rm=TRUE)
# q95 <- apply(all_hydro_nonstat,MARGIN=1,FUN=quantile,0.95,na.rm=TRUE)
# ### plot
# y.low <- q05
# y.high <- q95
# lines(1:length(y.low),y.low,col=col_nonstat,lwd=1,lty=3)
# lines(1:length(y.high),y.high,col=col_nonstat,lwd=1,lty=3)
# polygon(c(1:length(y.low), rev(1:(length(y.low)))), c(y.high, rev(y.low)),
#           col = adjustcolor(col_nonstat,0.5), border = NA)
# ### readd observed mean
# lines(mean_hydrograph_obs,lty=1,lwd=1,col="black")
#
#
# # ### plot observed annual hydrographs vs. simulated annual hydrographs (one simulation run)
# plot(sim_stat$Qobs[which(sim_stat$YYYY%in%year[1:3])],type="l",col='black',
#      ylab=expression(bold(paste("Discharge [m"^3,"/s]"))),xlab=expression(bold("Time [d]")),
#      main="Observed 3 years",ylim=c(0,max(unlist(mean_hydrograph_obs[,2]))*1.75))
#
# ### add stationary simulations
# for(r in 6){
#   for(n in 1:length(year)){
#     lines(sim_stat[which(sim_stat$YYYY%in%year[1:3]),r],col=col_stat)
#   }
# }
# ### add non-stationary simulation
# for(r in 6){
#   for(n in 1:length(year)){
#     lines(sim_nonstat[which(sim_nonstat$YYYY%in%year[1:3]),r],col=col_nonstat)
#   }
# }
# ### autocorrelation
# ###===============================###===============================###
#
# acf_mare <- list()
# acf_obs <- acf(sim_stat$Qobs,plot=FALSE,na.action=na.pass)
# plot(acf_obs$acf,type="l",xlab="Lag",main="Autocorrelation",ylab=expression(bold("ACF")))
# ### stationary
# for(r in 6:(length(names(sim_stat))-2)){
#   acf_sim <- acf(sim_stat[,r],plot=FALSE,na.action=na.pass)
#   lines(acf_sim$acf,col=col_stat,type="l")
#   ### compute mean relative error in the acf
#   acf_mare[[r]]<- mean(abs((acf_obs$acf-acf_sim$acf)/acf_obs$acf))
# }
#
# ### nonstationary
# for(r in 6:(length(names(sim_nonstat))-2)){
#   acf_sim <- acf(sim_nonstat[,r],plot=FALSE,na.action=na.pass)
#   lines(acf_sim$acf,col=col_nonstat,type="l")
#   ### compute mean relative error in the acf
#   acf_mare[[r]]<- mean(abs((acf_obs$acf-acf_sim$acf)/acf_obs$acf))
# }
# lines(acf_obs$acf)
# ### compute mean error over all simulated time series
# mean_error_acf <- mean(unlist(acf_mare))
#
# ### partial autocorrelation function
# pacf_obs <- pacf(sim_stat$Qobs,plot=FALSE,na.action=na.pass)
# pacf_mare <- list()
# plot(pacf_obs$acf,type="l",xlab="Lag",main="Partial autocorrelation",ylab=expression(bold("PACF")))
# ### stationary
# for(r in 6:(length(names(sim_stat))-2)){
#   pacf_sim <- pacf(na.omit(sim_stat[,r]),plot=FALSE)
#   lines(pacf_sim$acf,col=col_stat,type="l")
#   ### compute mean relative error in the acf
#   pacf_mare[[r]]<- mean(abs((pacf_obs$acf-pacf_sim$acf)/pacf_obs$acf))
# }
# ### nonstationary
# for(r in 6:(length(names(sim_nonstat))-2)){
#   pacf_sim <- pacf(na.omit(sim_nonstat[,r]),plot=FALSE)
#   lines(pacf_sim$acf,col=col_nonstat,type="l")
#   ### compute mean relative error in the acf
#   pacf_mare[[r]]<- mean(abs((pacf_obs$acf-pacf_sim$acf)/pacf_obs$acf))
# }
# lines(pacf_obs$acf)
#
# ### compute mean error over all simulated time series
# mean_error_pacf <- mean(unlist(pacf_mare))
#
# ### acfs of rising and receding limb separately
#
#
# ### compute seasonal statistics
# ### Q50,Q05,Q95, boxplots
# ###===============================###===============================###
# ### define seasons: Winter:12,1,2; spring:3,4,5; summer: 6,7,8; fall: 9,10,11
# sim_stat$season <- NA
# sim_stat$season[which(sim_stat$MM%in%c('12','01','02'))] <- "winter"
# sim_stat$season[which(sim_stat$MM%in%c('03','04','05'))] <- "spring"
# sim_stat$season[which(sim_stat$MM%in%c('06','07','08'))] <- "summer"
# sim_stat$season[which(sim_stat$MM%in%c('09','10','11'))] <- "fall"
# sim_nonstat$season <- NA
# sim_nonstat$season[which(sim_nonstat$MM%in%c('12','01','02'))] <- "winter"
# sim_nonstat$season[which(sim_nonstat$MM%in%c('03','04','05'))] <- "spring"
# sim_nonstat$season[which(sim_nonstat$MM%in%c('06','07','08'))] <- "summer"
# sim_nonstat$season[which(sim_nonstat$MM%in%c('09','10','11'))] <- "fall"
#
# ### compute seasonal statistics
# ### all simulated series show the same seasonal statistics. plot only one
# boxplot(sim_stat$Qobs[which(sim_stat$season=="winter")],sim_stat$r1[which(sim_stat$season=="winter")],sim_nonstat$r1[which(sim_nonstat$season=="winter")],
#         sim_stat$Qobs[which(sim_stat$season=="spring")],sim_stat$r1[which(sim_stat$season=="spring")],sim_nonstat$r1[which(sim_nonstat$season=="spring")],
#         sim_stat$Qobs[which(sim_stat$season=="summer")],sim_stat$r1[which(sim_stat$season=="summer")],sim_nonstat$r1[which(sim_nonstat$season=="summer")],
#         sim_stat$Qobs[which(sim_stat$season=="fall")],sim_stat$r1[which(sim_stat$season=="fall")],sim_nonstat$r1[which(sim_nonstat$season=="fall")],
#         border=c("black",col_stat,col_nonstat,"black",col_stat,col_nonstat,"black",col_stat,col_nonstat,"black",col_stat,col_nonstat),xaxt="n",main="Seasonal statistics",outline=FALSE)
# mtext(side=1,text=c("Winter","Spring","Summer","Fall"),at=c(2,5,8,11))
# # legend("topleft",legend=c("Observations","Simulations"),col=c("black",col_sim),pch=1)
#
#
# ### comparison of monthly statistics
# ### plot line of observed statistics
# ### add boxplots of statistics across simulated time series
# ###===============================###===============================###
# ### function for computing monthly statistics across simulated time series
# fun_month_stat_comp <- function(fun,name){
#   ### compute mean monthly hydrograph for observations
#   mean_hydrograph_obs <- aggregate(sim_stat$Qobs,by=list(sim_stat$MM),FUN=fun,simplify=FALSE,na.rm=T)
#   plot(mean_hydrograph_obs,lty=1,lwd=1,col="black",type="l",
#        ylab=expression(bold(paste("Discharge [m"^3,"/s]"))),xlab=expression(bold("Time [m]")),
#        main=name,ylim=c(0,max(unlist(mean_hydrograph_obs$x))*2))
#
#   ### compute mean monthly hydrograph for simulations
#   mean_hydrograph <- list()
#   for(r in 6:(length(names(sim_stat))-3)){
#     mean_hydrograph[[r]] <- unlist(aggregate(sim_stat[,r],by=list(sim_stat$MM),FUN=fun,simplify=FALSE,na.rm=T)$x)
#   }
#
#   ### compute boxplots across simulations for each month
#   monthly_values <- rep(list(rep(list(NA),times=length(mean_hydrograph))),times=12)
#   for(i in c(1:12)){
#     for(r in 1:length(mean_hydrograph)){
#       monthly_values[[i]][r] <- mean_hydrograph[[r]][i]
#     }
#   }
#
#   ### add monthly boxplots to mean observed line
#   for(i in 1:12){
#     boxplot(at=i-0.25,unlist(monthly_values[i]),add=T,col=col_stat,xaxt="n",yaxt="n")
#   }
#
#   ### non-stationary
#   ### compute mean monthly hydrograph for simulations
#   mean_hydrograph <- list()
#   for(r in 6:(length(names(sim_stat))-3)){
#     mean_hydrograph[[r]] <- unlist(aggregate(sim_nonstat[,r],by=list(sim_nonstat$MM),FUN=fun,simplify=FALSE,na.rm=T)$x)
#   }
#
#   ### compute boxplots across simulations for each month
#   monthly_values <- rep(list(rep(list(NA),times=length(mean_hydrograph))),times=12)
#   for(i in c(1:12)){
#     for(r in 1:length(mean_hydrograph)){
#       monthly_values[[i]][r] <- mean_hydrograph[[r]][i]
#     }
#   }
#
#   ### add monthly boxplots to mean observed line
#   for(i in 1:12){
#     boxplot(at=i+0.25,unlist(monthly_values[i]),add=T,col=col_nonstat,xaxt="n",yaxt="n")
#   }
# }
#
# ### monthly mean
# fun_month_stat_comp(fun=mean,name="Mean")
# ### monthly max
# fun_month_stat_comp(fun=max,name="Maximum")
# ### monthly minimum
# fun_month_stat_comp(fun=min,name="Minimum")
# ### standard deviation
# # fun_month_stat_comp(fun=sd,name="Standard deviation")
#
# # ### plot spectrum
# # ### compare spectra of observed and simulated data
# # spec.pgram(na.omit(sim_stat$Qobs),main='Spectrum')
# # for(r in 6:(length(sim_stat)-2)){
# #   spec_new <- spec.pgram(na.omit(sim_stat[,r]),plot=FALSE)
# #   lines(spec_new$freq,spec_new$spec,col=col_stat)
# # }
# # ### non-stationary
# # for(r in 6:(length(sim_nonstat)-2)){
# #   spec_new <- spec.pgram(na.omit(sim_nonstat[,r]),plot=FALSE)
# #   lines(spec_new$freq,spec_new$spec,col=col_nonstat)
# # }
# dev.off()
