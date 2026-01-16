
"single_oga" <- function(yt,s=NULL){
  
  ##########################################################################################################
  # Set up an execution indicator
  
  run <- "Yes"
  
  ##########################################################################################################
  # Validate frequency s
  
  if (is.ts(yt)==T){s <- frequency(yt)}
  if (is.null(s)==T){s <- 1}
  if (length(s)==1 && is.numeric(s)){
    if ((s %in% c(1,4,7,12,24,52,60))==F){message("s should be 1, 4, 7, 12, 24, 52 or 60");run <- "No"}
  } else {message("s should be 1, 4, 7, 12, 24, 52 or 60");run <- "No"}
  
  ##########################################################################################################
  # Verify yt is a numeric vector or a ts object. Attempt conversion otherwise
  
  if (is.ts(yt)==T){
    if (is.null(ncol(yt))==F){message("yt should be a single time series");run <- "No"}
  } else {
    if (is.vector(yt)==T){
      if (is.numeric(yt)==F){message("yt must be a numeric vector or a ts object");run <- "No"
      } else {yt <- ts(yt,start=1,frequency=s)}
    } else {
      if (is.matrix(yt) && ncol(yt)){yt <- ts(as.vector(yt),start=1,frequency=s)} else {
        message("yt must be of class vector or ts");run <- "No"}
    }
  }
  if (anyNA(yt)==T){message("Series with NAs are not allowed");run <- "No"}
  
  ##########################################################################################################
  # Check that sample size is large enough
  
  nT <- length(yt)
  if ((s==1)&&(nT<22)){message("Time series is too short");run <- "No"}
  if ((s==4)&&(nT<26)){message("Time series is too short");run <- "No"}
  if ((s==7)&&(nT<29)){message("Time series is too short");run <- "No"}
  if ((s==12)&&(nT<34)){message("Time series is too short");run <- "No"}
  if ((s==24)&&(nT<46)){message("Time series is too short");run <- "No"}
  if ((s==52)&&(nT<74)){message("Time series is too short");run <- "No"}
  if ((s==60)&&(nT<82)){message("Time series is too short");run <- "No"}
  
  ##########################################################################################################
  # Detect long runs of identical values
  
  n_rep <- sum(diff(yt)==0)
  if ((n_rep/nT)>0.5){message("Time series has many consecutive repeated values");run <- "No"}
  
  ##########################################################################################################
  # If all input conditions are met, proceed with the procedure. Otherwise, define outputs and conclude
  
  if (run == "Yes"){
    
    ########################################################################################################
    # First step: (i) Robust AR fitting
    
    rob_fit <- rob_ar(yt,s)
    rob_et <- rob_fit$rob_et
    rob_pis <- rob_fit$rob_pis
    max_lag <- rob_fit$max_lag
    
    ########################################################################################################
    # First step: (ii) Outlier detection with OGA with residuals after robust fitting and get cleaned series
    
    beg_eff <- max_lag
    end_eff <- 6
    clean <- T
    oga_rob <- det_oga(yt,rob_et,rob_pis,beg_eff,end_eff,clean)
    aos_rob <- oga_rob$aos
    lss_rob <- oga_rob$lss
    yt_clean_rob <- oga_rob$yt_clean
    
    ########################################################################################################
    # Second step: Repeat until no more outliers are found
    
    all_aos <- aos_rob
    all_lss <- lss_rob
    yt_clean <- yt_clean_rob
    out <- T
    while (out==T){
      
      # (i) Fit an ARIMA model to the cleaned series and obtain new AR representation and residuals
      
      clean_sarima <- sarima_fit(yt_clean)
      clean_et <- clean_sarima$con_et
      clean_pis <- clean_sarima$con_pis
      arima_fit <- clean_sarima$arima_fit
      
      # Second step: (ii) Detection of outliers with OGA with new residuals
      
      beg_eff <- max(c(arima_fit$arma[1]+arima_fit$arma[6]+s*(arima_fit$arma[3]+arima_fit$arma[7]),
                       arima_fit$arma[2]+arima_fit$arma[4]*s))
      oga_fit <- det_oga(yt_clean,clean_et,clean_pis,beg_eff,end_eff,clean=T)
      aos_fit <- oga_fit$aos
      lss_fit <- oga_fit$lss
      yt_clean_fit <- oga_fit$yt_clean
      
      # Checking

      n_aos_fit <- length(aos_fit)
      n_lss_fit <- length(lss_fit)
      n_out_fit <- n_aos_fit + n_lss_fit
      if (n_out_fit>0){
        yt_clean <- yt_clean_fit
        if (all(aos_fit %in% all_aos) && all(lss_fit %in% all_lss)){
          out <- F
        } else {
          all_aos <- sort(unique(c(all_aos,aos_fit)))
          all_lss <- sort(unique(c(all_lss,lss_fit)))
        }       
      } else {
        out <- F
      }
      
    }
    n_all_aos <- length(all_aos)
    n_all_lss <- length(all_lss)
    n_all_out <- n_all_aos + n_all_lss
    
    ##########################################################################################################
    # Third step: Joint estimation of ARIMA model and outlying effects, if any
    
    if (n_all_out>0){
      out_joint_fit <- joint_fit(yt,all_aos,all_lss,arima_fit)
      aos <- out_joint_fit$aos
      lss <- out_joint_fit$lss
      yt_clean <- out_joint_fit$yt_clean
    } else {
      yt_clean <- yt
      aos <- lss <- NULL
    }
    
  } else {
    yt_clean <- yt
    aos <- lss <- NULL
  }
  
  ##########################################################################################################
  # Return outputs
  
  return(list("yt_clean"=yt_clean,"aos"=aos,"lss"=lss))
  
}

"rob_ar" <- function(yt,s){
  
  ##########################################################################################################
  # Define the vector of lags
  
  nT <- length(yt)
  if (s==1){
    p_lags <- 1:6
  } else if ((s>1)&&((3*s+6)<nT)){
    p_lags <- unique(c(1:6,s:(s+6)))
  } else {
    p_lags <- unique(c(1:6,s:(s+6),2*s:(2*s+6),3*s:(3*s+6)))
  }
  max_lag <- max(p_lags)
  
  ##########################################################################################################
  # Run robust regression
  
  XX <- embed(yt,max_lag+1)
  XX <- data.frame(XX[,c(1,p_lags+1)])
  m1 <- suppressWarnings(robust::lmRob(XX$X1~.,data=XX,control=robust::lmRob.control(seed=1,trace=0)))
  #invisible(capture.output(m1 <- robust::lmRob(XX$X1~.,data=XX,control=robust::lmRob.control(seed=1))))
  rob_et <- c(rep(0,max_lag),as.vector(m1$residuals))
  rob_pis <- rep(0,max_lag)
  rob_pis[p_lags] <- as.vector(m1$coefficients[-1])
  
  ##########################################################################################################
  # Return results
  
  return(list("rob_et"=rob_et,"rob_pis"=rob_pis,"max_lag"=max_lag))
  
}

"det_oga" <- function(yt,et,pis,beg_eff,end_eff,clean){
  
  ##########################################################################################################
  # Compute time series length and number of fitted AR parameters
  
  nT <- length(yt)
  p <- length(pis)
  
  ##########################################################################################################
  # Build predictor matrix of size nT x 2nT (all times for aos and lss go into the matrix)
  
  pis_aos <- if (p<(nT-1)){c(1,-pis,rep(0,nT-1-p))}else{c(1,-pis[1:(nT-1)])}
  pis_lss <- cumsum(pis_aos)
  M <- matrix(1,nrow=nT,ncol=2*nT)
  M[,1:nT] <- toeplitz(pis_aos)
  M[,1:nT][upper.tri(M[,1:nT])] <- 0
  M[,(nT+1):(2*nT)] <- toeplitz(pis_lss)
  M[,(nT+1):(2*nT)][upper.tri(M[,(nT+1):(2*nT)])] <- 0
  
  ##########################################################################################################
  # Run OGA
  
  out_oga <- oga(M,et,beg_eff,end_eff)
  J_Trim <- out_oga$J_Trim
  
  ##########################################################################################################
  # Two scenarios: 
  # (i) If OGA finds no outliers, proceed to defining the outputs; 
  # (ii) If OGA identifies outliers, consider alternative configurations if necessary. Then, define AOs and LSs, 
  # and if required, compute the series without the outliers
  
  n_out <- length(J_Trim)
  if (n_out==0){
    aos <- lss <- NULL
    if (clean){yt_clean <- yt}
  } else {
    if (n_out>1){
      J_Trim <- config_oga(J_Trim,M,et)
      n_out <- length(J_Trim)
    }
    aos <- J_Trim[J_Trim<=nT]
    lss <- J_Trim[J_Trim>nT] - nT
    if (clean){
      M_Trim <- as.data.frame(as.matrix(M[,J_Trim]))
      colnames(M_Trim) <- names(data.frame(M))[J_Trim]
      fit_Trim <- lm(et~.-1,data=M_Trim)
      betahat_Trim <- summary(fit_Trim)
      yt_clean <- yt
      n_aos <- length(aos)
      n_lss <- length(lss)
      if (n_aos>0){
        w_aos <- as.numeric(coefficients(betahat_Trim)[1:n_aos,1])
        yt_clean[aos] <- yt_clean[aos] - w_aos
      } else {
        aos <- NULL
      }
      if (n_lss > 0){
        w_lss <- as.numeric(coefficients(betahat_Trim)[(n_aos+1):n_out,1])
        for (i in 1:n_lss){yt_clean[lss[i]:nT] <- yt_clean[lss[i]:nT] - w_lss[i]}
      } else {
        lss <- NULL
      }
    }
    
  }
  
  ##########################################################################################################
  # Return outputs
  
  if (clean){
    return(list("aos"=aos,"lss"=lss,"yt_clean"=yt_clean))
  } else {
    return(list("aos"=aos,"lss"=lss))
  }
  
}

"oga" <- function(M,et,beg_eff,end_eff){
  
  ##########################################################################################################
  # Preliminary steps
  
  nT <- nrow(M) # Number of rows
  nC <- ncol(M) # Number of columns
  K <- max(1,floor(5*sqrt(nT/log(nC)))) # Maximum number of predictors allowed
  
  ##########################################################################################################
  # Center responses and predictors
  
  d_et <- et - mean(et) # Centered residuals
  d_M <- M - rep(colMeans(M),rep.int(nT,nC)) # Centered predictors
  
  ##########################################################################################################
  # Set up objects and compute predictor norms
  
  J <- sig2 <- rep(0,K) # Initial vectors of selected predictors and residual variances
  MJ <- matrix(0,nT,K) # Initial matrix of centered predictors
  u <- as.matrix(d_et) # Initial vector of residuals
  M_norm <- sqrt(colSums(d_M^2)) # Norms of centered predictors
  d_M_st <- t(t(d_M)/M_norm) # Normalized centered predictors
  
  ##########################################################################################################
  # Select the first predictor by maximizing the squared root of absolute SSE
  
  aSSE <- abs(t(u)%*%d_M_st) # Vector with square roots of the absolute values of SSEs
  aSSE_0 <- c(1:beg_eff,(nT+1):(nT+beg_eff),(nC-end_eff+1):nC) # Beginning and end indices
  aSSE[aSSE_0] <- 0 # Remove beginning and end aSSEs
  J[1] <- which.max(as.vector(aSSE)) # First selected predictor
  
  ##########################################################################################################
  # Define first standardized predictor, residuals and residual variance
  
  MJ[,1] <- d_M_st[,J[1]] # First standardized centered predictor
  u <- u - MJ[,1] %*% t(MJ[,1]) %*% u # First residuals
  sig2[1] <- mean(u^2) # First residual variance
  
  ##########################################################################################################
  # Loop with orthogonal predictors
  
  if (K>1){
    for (k in 2:K){
      aSSE <- abs(t(u)%*%d_M_st) # Recompute aSSEs
      aSSE[aSSE_0] <- 0 # Remove beginning and end aSSEs
      aSSE[J[1:(k-1)]] <- 0 # Remove those that have appeared previously
      J[k] <- which.max(as.vector(aSSE)) # k-th selected predictor
      rq <- d_M[,J[k]]-MJ[,1:(k-1)]%*%t(MJ[,1:(k-1)])%*%d_M[,J[k]] # Orthogonalize new predictor
      MJ[,k] <- (rq/sqrt(sum(rq^2))) # k-th normalized orthogonal predictor
      u <- u - MJ[,k] %*% t(MJ[,k]) %*% u # k-th residuals
      sig2[k] <- mean(u^2) # k-th residual variance
    }
  }
  
  ##########################################################################################################
  # Compute HDAIC for 1:K and select the one that gives the minimum value
  
  hdic <- nT * log(sig2) + (1:K) * 2 * log(nC)
  kn <- which.min(hdic)
  
  ##########################################################################################################
  # Compare with model without predictors
  
  hdic_0 <- nT * log(mean(d_et^2))
  if (hdic_0 < hdic[kn]){
    J_Trim <- NULL # Define output
  } else {
    
    ########################################################################################################
    # Carry out trimming by skipping predictors that leads to large values of HDAIC
    
    benchmark <- hdic[kn] # HDAIC for the selected model
    J_Trim <- J[1:kn] # All indices
    trim_pos <- rep(0,kn) # Initially, all the indices are excluded (value 0)
    if (kn > 1) {
      for (l in 1:(kn-1)){
        JDrop1 <- J_Trim[-l] # Eliminate l-th predictor
        fit <- lm(d_et~.-1,data=data.frame(d_M[,JDrop1])) # Fit with OLS after skipping l-th predictor
        uDrop1 <- fit$residuals # Residuals of the fit
        HDICDrop1 <- nT * log(mean(uDrop1^2)) + (kn-1) * 2 * log(nC) # Compute HDAIC
        if (HDICDrop1>benchmark){trim_pos[l] <- 1} # Include predictor if HDAIC is greater (value 1)
      }
      trim_pos[kn] <- 1 # The last predictor is always included
      J_Trim <- J_Trim[which(trim_pos==1)] # Indices selected
    }
    J_Trim <- sort(J_Trim) # Sort indices selected
    
  }
  
  ##########################################################################################################
  # Return outputs
  
  return(list("J_Trim"=J_Trim))
  
}

"config_oga" <- function(J_Trim,M,et){
  
  ##########################################################################################################
  # Obtain dimensions of M
  
  nT <- nrow(M)
  nC <- ncol(M)
  
  ##########################################################################################################
  # Identify all runs in J_Trim
  
  runs_J_Trim <- runs_vec(J_Trim)
  n_runs <- runs_J_Trim$n_runs
  
  ##########################################################################################################
  # If there are runs, obtain all configurations
  
  if (n_runs>0){
    configs <- list()
    configs[[1]] <- J_Trim
    for (i in 1:n_runs){
      n_configs <- length(configs)
      n_run <- length(runs_J_Trim$runs[[i]])
      if (all(runs_J_Trim$runs[[i]]<=nT)){
        for (j in 1:n_configs){
          configs[[n_configs+j]] <- configs[[j]]
          ind_run <- which(configs[[n_configs+j]] %in% runs_J_Trim$runs[[i]])
          configs[[n_configs+j]] <- configs[[n_configs+j]][-ind_run]
          configs[[n_configs+j]] <- sort(unique(c(configs[[n_configs+j]],runs_J_Trim$runs[[i]][1]+nT,
                                                  runs_J_Trim$runs[[i]][n_run]+1+nT)))
        }
      } else {
        for (j in 1:n_configs){
          configs[[n_configs+2*j-1]] <- configs[[j]]
          ind_run <- which(configs[[n_configs+2*j-1]] %in% runs_J_Trim$runs[[i]])
          configs[[n_configs+2*j-1]] <- configs[[n_configs+2*j-1]][-ind_run]
          configs[[n_configs+2*j-1]] <- sort(unique(c(configs[[n_configs+2*j-1]],runs_J_Trim$runs[[i]][1:(n_run-1)]-nT)))
          configs[[n_configs+2*j]] <- sort(unique(c(configs[[n_configs+2*j-1]],runs_J_Trim$runs[[i]][n_run])))    
        }
      }
    }
    
    ##########################################################################################################
    # Compute HDAIC for all configurations
    
    n_configs <- length(configs)
    HDAIC <- vector(length=n_configs)
    for (i in 1:n_configs){
      M_config <- as.data.frame(M[,configs[[i]]])
      rank_M_config <- qr(M_config)$rank
      if (rank_M_config<ncol(M_config)){
        HDAIC[i] <- NA
      } else {
        fit_M_config <- lm(et~.-1,data=M_config)
        HDAIC[i] <- nT * log(mean(fit_M_config$residuals^2)) + length(configs[[i]]) * 2 * log(nC)
      }
    }
    if (all(is.na(HDAIC))==F){J_Trim <- configs[[which.min(HDAIC)]]}
    
  } 
  
  ##########################################################################################################
  # Return outputs
  
  return(J_Trim)
  
}

"runs_vec" <- function(vec){
  n_vec <- length(vec)
  if (n_vec<2) {
    runs <- NULL
    n_runs <- 0
  } else {
    all_runs <- split(vec,cumsum(c(0,diff(vec)>1)))
    which_runs <- as.numeric(which(lengths(all_runs)>1))
    n_runs <- length(which_runs)
    if (n_runs>0){
      runs <- vector("list",length=n_runs)
      runs[1:n_runs] <- all_runs[which_runs]
    } else {
      runs <- NULL
    }
  } 
  
  ##########################################################################################################
  # Return outputs
  
  return(list("runs"=runs,"n_runs"=n_runs))
  
}

"sarima_fit" <- function(yt_clean){
  
  ##########################################################################################################
  # Carry out identification depending on the frequency
  
  s <- frequency(yt_clean)
  if (s>1) {
    suppressMessages(suppressWarnings(spec_arima <- SLBDD::sarimaSpec(yt_clean,maxorder=c(3,1,0),period=s,method="CSS")))
    order <- spec_arima$order[1:3]
    sorder <- spec_arima$order[4:6]
    include.mean <- spec_arima$include.mean
    suppressMessages(suppressWarnings(arima_fit <- forecast::Arima(yt_clean,order=order,seasonal=list(order=sorder,period=s),
                                                  include.mean=include.mean,method="CSS")))
  } else {
    suppressMessages(suppressWarnings(spec_arima <- SLBDD::arimaSpec(yt_clean,maxorder=c(3,1,0),method="CSS")))
    order <- spec_arima$order
    sorder <- NULL
    include.mean <- spec_arima$include.mean
    suppressMessages(suppressWarnings(arima_fit <- forecast::Arima(yt_clean,order=order,include.mean=include.mean,method="CSS")))
  }
  
  ##########################################################################################################
  # Obtain residuals
  
  con_et <- suppressMessages(suppressWarnings(residuals(forecast::Arima(yt_clean,model=arima_fit))))
  if (anyNA(con_et)==T){con_et <- suppressMessages(suppressWarnings(forecast::na.interp(con_et)))}
  
  ##########################################################################################################
  # Obtain AR representation
  
  n_phi <- arima_fit$arma[1]
  n_theta <- arima_fit$arma[2]
  n_PHI <- arima_fit$arma[3]
  n_THETA <- arima_fit$arma[4]
  s <- arima_fit$arma[5]
  d <- arima_fit$arma[6]
  Ds <- arima_fit$arma[7]
  n_ALL <- 0
  if (n_phi>0){
    phi <- arima_fit$coef[(n_ALL+1):(n_ALL+n_phi)]
    n_ALL <- n_ALL + n_phi
  } else {
    phi <- 0
  }
  if (n_theta>0){
    theta <- arima_fit$coef[(n_ALL+1):(n_ALL+n_theta)]
    n_ALL <- n_ALL + n_theta
  } else {
    theta <- 0
  }
  if (n_PHI>0){
    PHI <- arima_fit$coef[(n_ALL+1):(n_ALL+n_PHI)]
    n_ALL <- n_ALL + n_PHI
  } else {
    PHI <- 0
  }
  if (n_THETA>0){
    THETA <- arima_fit$coef[(n_ALL+1):(n_ALL+n_THETA)]
    n_ALL <- n_ALL + n_THETA
  } else {
    THETA <- 0
  }
  con_pis <- gsarima::arrep(notation="arima",phi=phi,d=d,theta=theta,Phi=PHI,D=Ds,Theta=THETA,frequency=s)
  
  ##########################################################################################################
  # Return outputs
  
  return(list("con_et"=con_et,"con_pis"=con_pis,"arima_fit"=arima_fit))
  
}

"joint_fit" <- function(yt,aos,lss,arima_fit){
  
  ##########################################################################################################
  # Compute length of time series, AOs and LSs
  
  nT <- length(yt)
  n_aos <- length(aos)
  n_lss <- length(lss)
  n_out <- n_aos + n_lss
  
  ##########################################################################################################
  # Define ARIMA model to fit
  
  add_seas <- (arima_fit$arma[3]+arima_fit$arma[4]+arima_fit$arma[7]>0)
  if (add_seas==F){
    n_phi <- arima_fit$arma[1]
    n_theta <- arima_fit$arma[2]
    d <- arima_fit$arma[6]
    if ((n_phi+n_theta)<length(arima_fit$coef)){inc_mean <- T}else{inc_mean <- F}
  } else {
    n_phi <- arima_fit$arma[1]
    n_theta <- arima_fit$arma[2]
    n_PHI <- arima_fit$arma[3]
    n_THETA <- arima_fit$arma[4]
    s <- arima_fit$arma[5]
    d <- arima_fit$arma[6]
    Ds <- arima_fit$arma[7]
    if ((n_phi+n_theta+n_PHI+n_THETA)<length(arima_fit$coef)){inc_mean <- T}else{inc_mean <- F}
  }
  
  ##########################################################################################################
  # Joint estimation of ARIMA and outlier sizes
  
  X_reg <- matrix(0,nrow=nT,ncol=n_out)
  if (n_aos>0){for (i in 1:n_aos){X_reg[aos[i],i] <- 1}}
  if (n_lss>0){for (i in 1:n_lss){X_reg[lss[i]:nT,n_aos+i] <- 1}}
  
  if ((n_aos>0) && (n_lss>0)){
    are_lc <- caret::findLinearCombos(X_reg)
    n_are_lc <- length(are_lc$linearCombos)
    if (n_are_lc > 0){
      elim_col <- NULL
      for (i in 1:n_are_lc){
        are_lc_col <- are_lc$linearCombos[[i]]
        elim_col <- c(elim_col,are_lc_col[-which.max(colSums(X_reg[,are_lc_col]))])
      }
      elim_col <- sort(unique(elim_col))
      
      X_reg <- X_reg[,-elim_col]
      
      elim_col_aos <- elim_col[elim_col<=n_aos]
      elim_col_lss <- elim_col[elim_col>n_aos]-n_aos
      aos <- aos[-elim_col_aos]
      lss <- lss[-elim_col_lss]
      n_aos <- length(aos)
      n_lss <- length(lss)
      n_out <- n_aos + n_lss
    }
  }
  
  if (add_seas==F){
    suppressWarnings(con_sarima_end <- arima(yt,order=c(n_phi,d,n_theta),xreg=X_reg,
                                             include.mean=inc_mean,method="CSS"))
  } else {
    suppressWarnings(con_sarima_end <- arima(yt,order=c(n_phi,d,n_theta),seasonal=list(order=c(n_PHI,Ds,n_THETA),period=s),
                                             xreg=X_reg,include.mean=inc_mean,method="CSS"))
  }
  
  ##########################################################################################################
  # Define outliers and sizes
  
  n_par <- length(coef(con_sarima_end)) - n_out # Number of estimated ARIMA parameters
  w_est <- as.numeric(coef(con_sarima_end)[(n_par+1):(n_par+n_out)]) # Outlier size estimates
  if (n_aos>0){
    aos_end <- matrix(0,nrow=n_aos,ncol=2)
    aos_end[,1] <- aos
    aos_end[,2] <- w_est[1:n_aos]
  } else {
    aos_end <- NULL
  }
  if (n_lss>0){
    lss_end <- matrix(0,nrow=n_lss,ncol=2)
    lss_end[,1] <- lss
    lss_end[,2] <- w_est[(n_aos+1):n_out]
  } else {
    lss_end <- NULL
  }
  
  ##########################################################################################################
  # Obtain the clean time series
  
  if (n_out>0){
    yt_clean <- yt
    if (n_aos>0){for (i in 1:n_aos){yt_clean[aos_end[i]] <- yt_clean[aos_end[i]] - aos_end[i,2]}}
    if (n_lss>0){for (i in 1:n_lss){yt_clean[lss_end[i]:nT] <- yt_clean[lss_end[i]:nT] - lss_end[i,2]}}
  }
  
  #############################################################################################################
  # Return results
  
  return(list("yt_clean"=yt_clean,"aos"=aos_end,"lss"=lss_end))
  
}

"db_hom_oga" <- function(Y,s=NULL){
  
  run <- "Yes"
  
  ##########################################################################################################
  # Check validity of the frequency s
  
  if (is.null(s)==T){s <- 1}
  if (length(s)==1 && is.numeric(s)){
    if ((s %in% c(1,4,7,12,24,52,60))==F){result <- "s should be 1, 4, 7, 12, 24, 52 or 60";run <- "No"}
  } else {
    result <- "s should be 1, 4, 7, 12, 24, 52 or 60";run <- "No"
  }
  
  ##########################################################################################################
  # Check that Y is class matrix and transform if it is class data.frame or mts
  
  if (is.data.frame(Y)==T){Y <- as.matrix(Y)} # Check that Y is class data.frame
  if (is.mts(Y)==T){Y <- as.matrix(Y)} # Check that Y is class mts
  if (is.matrix(Y)==F){result <- "Y should be a matrix or a data.frame";run <- "no"} # Stop if Y is not class matrix
  if (is.numeric(Y)==F){result <- "Y must be numeric";run <- "No"}
  
  ##########################################################################################################
  # Check if there are NAs
  
  if (anyNA(Y)==T){result <- "Series with NAs are not allowed";run <- "No"}
  
  ##########################################################################################################
  # Check sufficiently large sample size
  
  nT <- nrow(Y)
  if ((s==1)&&(nT<22)){message("Time series is too short");run <- "No"}
  if ((s==4)&&(nT<26)){message("Time series is too short");run <- "No"}
  if ((s==7)&&(nT<29)){message("Time series is too short");run <- "No"}
  if ((s==12)&&(nT<34)){message("Time series is too short");run <- "No"}
  if ((s==24)&&(nT<46)){message("Time series is too short");run <- "No"}
  if ((s==52)&&(nT<74)){message("Time series is too short");run <- "No"}
  if ((s==60)&&(nT<82)){message("Time series is too short");run <- "No"}
  
  ##########################################################################################################
  # Run procedure if
  
  if (run == "Yes"){
    
    ##########################################################################################################
    # Set up parallel stuff and run procedure
    
    plan(multisession,workers=availableCores())
    out_bd_oga <- future_apply(Y,2,single_oga,s=s)
    plan(sequential)

    ##########################################################################################################
    # Save results
    
    p <- ncol(Y)
    AOs <- LSs <- vector(mode='list',length=p)
    n_AOs <- n_LSs <- vector(mode='numeric',length=p)
    Y_clean <- matrix(Y,nT,p)
    for (i in 1:p){
      if (is.matrix(out_bd_oga[[i]]$aos)){
        n_AOs[i] <- nrow(out_bd_oga[[i]]$aos)
        AOs[[i]] <- out_bd_oga[[i]]$aos[,1]
      }
      if (is.matrix(out_bd_oga[[i]]$lss)){
        n_LSs[i] <- nrow(out_bd_oga[[i]]$lss)
        LSs[[i]] <- out_bd_oga[[i]]$lss[,1]
      }
      Y_clean[,i] <- as.vector(out_bd_oga[[i]]$yt_clean)
    }
    result <- "The procedure has been successfully completed"
  } else {
    n_AOs <- n_LSs <- AOs <- LSs <- NULL
    Y_clean <- Y
  }
  
  ##########################################################################################################
  # Return results
  
  return(list("n_AOs"=n_AOs,"n_LSs"=n_LSs,"AOs"=AOs,"LSs"=LSs,"Y_clean"=Y_clean,"result"=result))
  
}

"db_het_oga" <- function(Y){
  
  run <- "Yes"
  
  ##########################################################################################################
  # Check that Y is a list of time series (ts objects)
  
  if (is.list(Y)==F){result <- "Y must be a list of time series";run <- "No"}
  if (all(sapply(Y,is.ts))==F){result <- "Y must be a list of time series";run <- "No"}
  
  ##########################################################################################################
  # Obtain number of series and frequencies and check validity of frequencies
  
  nY <- length(Y)
  nT <- lengths(Y)
  s <- unlist(lapply(Y,frequency))
  for (i in 1:nY){
    if ((s[i]==1)&&(nT[i]<22)){result <- "Time series is too short";run <- "No"}
    if ((s[i]==4)&&(nT[i]<26)){result <- "Time series is too short";run <- "No"}
    if ((s[i]==7)&&(nT[i]<29)){result <- "Time series is too short";run <- "No"}
    if ((s[i]==12)&&(nT[i]<34)){result <- "Time series is too short";run <- "No"}
    if ((s[i]==24)&&(nT[i]<46)){result <- "Time series is too short";run <- "No"}
    if ((s[i]==52)&&(nT[i]<74)){result <- "Time series is too short";run <- "No"}
    if ((s[i]==60)&&(nT[i]<82)){result <- "Time series is too short";run <- "No"}
    if ((s[i] %in% c(1,4,7,12,24,52,60))==F){result <- "s should be 1, 4, 7, 12, 24, 52 or 60";run <- "No"}
  }
  
  ##########################################################################################################
  # Check if there are NAs
  
  if (anyNA(Y)==T){result <- "Series with NAs are not allowed";run <- "No"}
  
  ##########################################################################################################
  # Run procedure if everything is fine
  
  if (run == "Yes"){
    
    ##########################################################################################################
    # Set up parallel stuff and run procedure
    
    plan(multisession,workers=availableCores())
    out_bd_oga <- future_lapply(Y,FUN=single_oga)
    plan(sequential)

    ##########################################################################################################
    # Save results
    
    AOs <- LSs <- Y_clean <- vector(mode='list',length=nY)
    n_AOs <- n_LSs <- vector(mode='numeric',length=nY)
    for (i in 1:nY){
      if (is.matrix(out_bd_oga[[i]]$aos)){
        n_AOs[i] <- nrow(out_bd_oga[[i]]$aos)
        AOs[[i]] <- out_bd_oga[[i]]$aos[,1]
      }
      if (is.matrix(out_bd_oga[[i]]$lss)){
        n_LSs[i] <- nrow(out_bd_oga[[i]]$lss)
        LSs[[i]] <- out_bd_oga[[i]]$lss[,1]
      }
      Y_clean[[i]] <- ts(out_bd_oga[[i]]$yt_clean,frequency=s[i])
    }
    result <- "The procedure has been successfully completed"
  } else {
    n_AOs <- n_LSs <- AOs <- LSs <- NULL
    Y_clean <- Y
  }
  
  ##########################################################################################################
  # Return results
  
  return(list("n_AOs"=n_AOs,"n_LSs"=n_LSs,"AOs"=AOs,"LSs"=LSs,"Y_clean"=Y_clean,"result"=result))
  
}
