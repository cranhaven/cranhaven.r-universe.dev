prsim <- function(data, station_id="Qobs", number_sim=1, win_h_length=15, 
        marginal=c("kappa","empirical"), n_par=4, marginalpar=TRUE, 
        GoFtest=NULL, verbose=TRUE, suppWarn=FALSE, ...) {  

  ifft <- function (x) fft(x, inverse = TRUE)/length(x)
  
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

	if (nrow(data)[1]<730) stop("At least one year of data required.")
	if (is.numeric(station_id)){
	  station_id <- colnames(data)[station_id]
	}
	if (is.na(station_id)||!("Qobs" %in% colnames(data))) stop("Wrong column (name) for observations selected.")
	
	# test for proper format:
	if (any(class(data[,1])%in%c("POSIXct","POSIXt"))){
	  data <- data.frame(YYYY=as.integer(format(data[,1],'%Y')), 
                       MM=as.integer(format(data[,1],'%m')),
	                     DD=as.integer(format(data[,1],'%d')),
	                     Qobs=data[,station_id],
	                     timestamp=data[,1])
	} else {
     if(!all(c("YYYY","MM","DD") %in% colnames(data))) stop("Wrong time column names")

	   data <- data[,c("YYYY","MM","DD", station_id)]
	   tmp <- paste(data$YYYY,data$MM,data$DD,sep=" ")
	   names(data) <- c("YYYY","MM","DD","Qobs")
	   data$timestamp <- as.POSIXct(strptime(tmp, format="%Y %m %d", tz="GMT"))
	}
  
  ### remove February 29
  data <- data[format(data$timestamp, "%m %d") != "02 29",]
  
  if ((nrow( data) %% 365)>0) stop("No missing values allowed. Some days are missing.")

  
	if (verbose) cat(paste0("Detrending with (half-)length ",win_h_length,"...\n"))  
   
  # ### a) Detrend
  # plot(data$Qobs[1:1000])
  # # data$smooth <- loess(c(1:length(data$Qobs))~data$Qobs,span=0.0001)$y
  # # lines(data$smooth[1:1000])
  # ### generate a day index
  data$index <- rep(c(1:365), times=length(unique(data$YYYY)))
  
    
  # ### 1) Normalization
  # ### normalization of the distribution: replace each term with the vlaue which would have had the same non-exceedance probability in the Gussian distribution.
  # ### this step preserves the relative ranks of the data
  data$rank <- rank(data$Qobs)
  # # data$rank <- rank(data$detrend)
  # ### simulate from normal distribution
  # sample_norm <- sort(rnorm(mean=0,sd=1,n=length(data$rank)))
  # ### normalize distribution
  # data$norm <- sample_norm[data$rank]
  # hist(data$norm)
  # plot(data$norm[1:1000],type="l")
  
  data$norm <- NA
  # ### monthly normalization
  # for(m in c(1:12)){
  #   data$rank[which(data$MM%in%c(m))] <- rank(data$Qobs[which(data$MM%in%c(m))])
  #   sample_norm <- sort(rnorm(mean=0,sd=1,n=length(data$rank[which(data$MM%in%c(m))])))
  #   ### normalize distribution
  #   data$norm[which(data$MM%in%c(m))] <- sample_norm[data$rank[which(data$MM%in%c(m))]]
  # }
  
  ### daily normalization: for consistency with daily fitting of Wakeby distribution
  for(d in c(1:365)){
    data$rank[which(data$index%in%c(d))] <- rank(data$Qobs[which(data$index%in%c(d))])
    sample_norm <- sort(rnorm(mean=0,sd=1,n=length(data$rank[which(data$index%in%c(d))])))
    ### normalize distribution
    data$norm[which(data$index%in%c(d))] <- sample_norm[data$rank[which(data$index%in%c(d))]]
  }
  
  
  
  ### daily fitting of Kappa distribution
  ### fit the parameters of the Kappa distribution for each day separately.
  ### To enable a sufficient sample size by using daily values in moving window around day i (i.e., reduce uncertainty due to fitting)
    if(marginal=="kappa"){
      p_vals <- numeric(365) 
      par_day <- matrix(0, nrow=365, ncol=4)
      # density_kap <- list()
            ### define window length  
      win_length <- c(1:win_h_length)
      for(d in c(1:365)){
        ### define start and end of window
        before <- data$index[d+365-win_length]
        after <- data$index[d+365+win_length-1]
        ### define days within window
        ids <- c(before, after)
        ### determine values in window around day i
        data_window <- data$Qobs[which(data$index%in%ids)]
        # par.kappa(data_monthly)
        ll<- Lmoments(data_window)
        
        ### test whether Kappa distribution can be fit
        if (suppWarn) {
          suppressWarnings( test <- try(par.kappa(ll[1],ll[2],ll[4],ll[5]), silent = TRUE) )
        } else {
          test <- try(par.kappa(ll[1],ll[2],ll[4],ll[5]), silent = TRUE)
        }
        
        if(length(test)>1){
          kap_par <- test
          par_day[d,] <- unlist(kap_par)
          ### define vector of quantiles
          quant <- sort(data_window)
          thresh <- kap_par$xi + kap_par$alfa*(1 - kap_par$h^(-kap_par$k))/kap_par$k
          if(!is.na(thresh)){
    ##        min(quant)>thresh
            ### only use quantiles larger than threshold (as in f.kappa function documentation)
            quant <- quant[which(quant>thresh)]
          }
          # kappa_density <- f.kappa(x=quant,xi=kap_par$xi,alfa=kap_par$alfa,k=kap_par$k,h=kap_par$h)
          # plot(kappa_density)
          data_kap <- rand.kappa(length(data_window), xi=kap_par$xi,alfa=kap_par$alfa, k=kap_par$k, h=kap_par$h)
   #       density_kap[[d]] <- density(data_kap)
   #       hist(data_window)
   #       hist(data_kap,add=T,col="red")

          if (tolower(GoFtest)=="ks")
          p_vals[d] <- ks_test(data_window, data_kap) ### kappa distribution not rejected at alpha=0.05
          # p_vals[d] <- ks.test(data_window, data_kap)$p.value ### kappa distribution not rejected at alpha=0.05
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
  
      ###qu: check if subsequent NAs work properly.    
      ### replace NA entries by values of subsequent day
      if(length(which(is.na(par_day[,1])))>0){
        indices <- rev(which(is.na(par_day[,1])))
        for(i in 1:length(indices)){
          par_day[indices[i],] <- par_day[indices[i]+1,]
        }
      }
    }
   
#    which(unlist(p_val_list)<0.05) ### non-rejected for most days, difficulty with winter months (relation to hp production?)
  
  ### use either a predefined distribution in R or define own function
  if(marginal!="kappa" & marginal!="empirical"){
    p_vals <- numeric(365) 
    par_day <- matrix(0, nrow=365, ncol=n_par)
    for(d in c(1:365)){
      ### define window length
      win_length <- seq(1:15)
      ### define start and end of window
      before <- data$index[d+365-win_length]
      after <- data$index[d+365+win_length-1]
      ### define days within window
      ids <- c(before,after)
      ### determine values in window around day i
      data_window <- data$Qobs[which(data$index%in%ids)]
      theta <-  CDF_fit(xdat=data_window, ...)

      ### goodness of fit test
      data_random <- rCDF(n=length(data_window), theta)
      
      # density_gengam[[d]] <- density(data_gengam)
      # hist(data_window)
      # hist(data_random,add=T,col="red")
      if (tolower(GoFtest)=="ks"){
        p_vals[d] <- ks_test(data_window,data_random)
#        p_vals[d] <- ks.test(data_window,data_random)$p.value 
      }
      if (tolower(GoFtest)=="ad"){
        p_vals[d] <-  ad.test(data_window,pCDF,theta)$p.value
      }
      ### store parameters
      par_day[d,] <- theta
    } 
  }
  ### Deseasonalization was not found to be necessary. Use normalized data directly.
    data$des <- data$norm

  
  ### 3) compute fast Fourier transform
  ft <- fft(data$des)

  ### extract phases (argument)
  phases <- Arg(ft)
  # plot(phases,type="h")
#  plot(sort(phases))
#  hist(phases)
  n <- length(ft)
  ### determine first half (left part of data)
  first_part <- 2:(floor(n/2)+1)
  ### determine second half (right part of data)
  second_part <- (n+1)-(1:floor(n/2))
   
  ### repeat stochastic simulation several times
  ### list for storing results
  data_sim <- list()

  if(verbose) cat(paste0("Starting ",number_sim," simulations:\n"))
  for (r in 1:number_sim){
    ### random generation of new phase sequence.
    n <- length(data$des)
    # phases_random <- sample(phases,size=n,replace=TRUE)
    # phases_random <- sample(phases,size=n,replace=FALSE)
    phases_random <- phases
    ### create left part of data
    ### keep first entry (mean), which is 0
    # phases_random[first_part] <- sample(phases[first_part],size=length(first_part),replace=TRUE)
    ### use phases sampled from theoretical uniform distribution [-pi,pi] instead of empirical distribution
    phases_random[first_part] <- runif(n=length(first_part),min=-pi,max=pi)
    ### mirror data
    phases_random[second_part] <- phases_random[first_part]
    # plot(phases_random,type="l",col="red")
    
    ### derive modulus of complex numbers (radius)
    modulus <- Mod(ft)
    
    ### Derive complex numbers from randomly generated phases and same modulus
    ### create empty vector for storing results
    ft_new <- rep(NA, length=n)
    ### add mean value
    ft_new[1] <- ft[1] 
    # ft_new <- complex(modulus=modulus,argument=phases_random)
    ### first half
    ft_new[first_part] <- complex(modulus=modulus[first_part], argument=phases_random[first_part])
    ### second half with conjugate values (opposite imaginary part)
    ft_new[second_part] <- Conj(ft_new[first_part])
    ### what happens if I also change modulus
    # ft_new <- complex(modulus=sample(modulus,length(modulus)),argument=phases_random)
     
    
    ### c) backtransform data. Reverse Fourier transformation back to temporal domain.
    ### test on original complex numbers
    ft_inv <- ifft(ft) ### still complex numbers
    # ft_inv <- fft(ft, inverse = TRUE)/length(ft)
    # ft_inv <- fft(ft, inverse = TRUE)
    ### extract only real part
    ts_invers <- Re(ft_inv)
    ### compare to original series
    # plot(ts_invers,type="l")
    # lines(data$des,col="red")

    ### this procedure reproduces the original time series
    ### apply the same transformation procedure for the newly generated complex numbers
    ft_inv_new <- ifft(ft_new)
    ts_invers_new <- Re(ft_inv_new)
    
    ### compare spectra of observed and simulated data
#    spec.pgram(ts_invers)
#    spec_new <- spec.pgram(ts_invers_new,plot=FALSE)
#    lines(spec_new,col="red")
    
    
    ### create new data frame
    data_new <- data.frame("seasonal"=ts_invers_new)
    ### add months and years
    data_new$MM <- data$MM
    data_new$DD <- data$DD
    data_new$YYYY <- data$YYYY
    data_new$index <- data$index

    ### e) backtransform from normal to actual distribution
    ### apply daily backtransformation
        data_new$simulated_seasonal <- NA

      for(d in c(1:365)){    

        data_day <- data[which(data$index%in%c(d)),]
        #   data_month$rank <- rank(data_month$Qobs)
        #   data_new$rank[which(data$MM%in%c(m))] <- rank(data_new[which(data$MM%in%c(m)),]$seasonal)
        #   ### derive corresponding values from the empirical distribution
        #   ### identify value corresponding to rank in the original time series
        #   data_ordered <- data_month[order(data_month$rank),]
        #   data_new$simulated_seasonal[which(data_new$MM%in%c(m))] <- data_ordered$Qobs[data_new$rank[which(data$MM%in%c(m))]]
        
        ### use kappa distribution for backtransformation
        if(marginal=="kappa"){
          colnames(par_day) <- names(kap_par)

          ### use monthly Kappa distribution for backtransformation
          ### simulate random sample of size n from Kappa disribution
          data_day$kappa <- rand.kappa(length(data_day$Qobs),
              xi=par_day[d,"xi"],alfa=par_day[d,"alfa"],
              k=par_day[d,"k"],h=par_day[d,"h"])
         
        # if(marginal=="wakeby"){
        #   ### use Wakeby distribution for backtrasformation
        #   ### simulate random sample of size n from Wakeby distribution
        #   data_day$kappa <- rlmomco(length(data_day$Qobs),wak_par_day[[d]])
        #}
        
        data_day$rank <- rank(data_day$kappa)
        
        ##QUESTION: had to recopy the line here: possibly more not needed below..
        data_new$rank <- rank(data_new$seasonal)
        data_new$rank[ which(data$index%in%c(d)) ] <- rank(data_new[ which(data$index%in%c(d)), ]$seasonal)
        ### derive corresponding values from the kappa distribution
        ### identify value corresponding to rank in the kappa time series
        data_ordered <- data_day[order(data_day$rank),]
        data_new$simulated_seasonal[which(data_new$index%in%c(d))] <- data_ordered$kappa[data_new$rank[which(data$index%in%c(d))]]
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
        if(marginal=="empirical"){
          data_day$rank <- rank(data_day$Qobs)
          data_new$rank <- rank(data_new$seasonal)
          data_new$rank[which(data$index%in%c(d))] <- rank(data_new[which(data$index%in%c(d)),]$seasonal)
          ### derive corresponding values from the empirical distribution
          ### identify value corresponding to rank in the original time series
          data_ordered <- data_day[order(data_day$rank),]
          data_new$simulated_seasonal[which(data_new$index%in%c(d))] <- data_ordered$Qobs[data_new$rank[which(data$index%in%c(d))]]
          # }
        }
        
        ### use any predefined distribution for backtransformation
        if(marginal!="kappa" & marginal!="empirical"){
          ### use monthly distribution for backtransformation
          ### simulate random sample of size n from disribution
          data_day$cdf <-   rCDF(n=length(data_day$Qobs), par_day[d,])
          data_day$rank <- rank(data_day$cdf)
          
          data_new$rank <- rank(data_new$seasonal)
          
          # hist(data_day$Qobs)
          # hist(data_day$cdf,add=T,col="blue")
          # data_day$rank <- rank(data_day$cdf)
          data_new$rank[which(data$index%in%c(d))] <- rank(data_new[which(data$index%in%c(d)),]$seasonal)
          ### derive corresponding values from the kappa distribution
          ### identify value corresponding to rank in the kappa time series
          data_ordered <- data_day[order(data_day$rank),]
          data_new$simulated_seasonal[which(data_new$index%in%c(d))] <- data_ordered$cdf[data_new$rank[which(data$index%in%c(d))]]
        }
    }  # end for loop
    data_sim[[r]] <- data_new$simulated_seasonal
    
    if(verbose) cat(".")
  }
  if(verbose) cat("\nFinished.\n")
  
   
  ### put observed and simulated data into a data frame
  data_sim <- as.data.frame(data_sim)
  names(data_sim) <- paste("r",seq(1:number_sim),sep="")
  data_stoch <- data.frame(data[,c("YYYY", "MM", "DD", "timestamp", "Qobs")],
                          deseaonalized=data$des,
                          data_sim)

  if (GoFtest=="NULL") {  
    p_vals <- NULL 
  }
  
  if(marginal != "empirical"){
    if (marginalpar) {  # also return intermediate results
       return(list( simulation=data_stoch, pars=par_day, p_val=p_vals))
    } else {
    	 return(list( simulation=data_stoch, pars=NULL, p_val=p_vals)) 
    }
  }else{
    return(list(simulation=data_stoch) )
  }
}
