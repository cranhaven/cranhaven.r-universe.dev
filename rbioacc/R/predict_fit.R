#' Prediction function using \code{fitTK} object
#' 
#' @rdname predict
#' 
#' @param object An object of \code{stanfit}
#' @param data A data set with one column \code{time} and 1 to 4 exposure 
#' @param mcmc_size Size of mcmc chain if needed to be reduced
#' @param fixed_init If \code{TRUE} fix the initial conditions of internal concentration.
#' columns with name in \code{expw}, \code{exps}, \code{expf} and \code{exppw}
#' @param \dots Additional arguments
#' 
#' @importFrom stats rnorm
#' 
#' @return An object of class \code{predictTK}
#' 
#' @export
#' 
predict.fitTK <- function(object, data, mcmc_size = NULL, fixed_init = TRUE, ...){
  
  fit <- object
  
  fitDATA <- fit[["stanTKdata"]]
  fitMCMC <- rstan::extract(fit[["stanfit"]])

  n_met <- fitDATA$n_met
  len_MCMC <- nrow(fitMCMC$ku)
  # Exposure match
  data_origin <- fitDATA$origin_data
  
  col_names_origin <- colnames(data_origin)[ .index_col_exposure(data_origin)]
  Cexp_origin <- as.data.frame(data_origin[, col_names_origin])
  colnames(Cexp_origin) <- col_names_origin
  
  col_names <- colnames(data)[ .index_col_exposure(data)]
  Cexp <- as.data.frame(data[, col_names])
  colnames(Cexp) <- col_names
  
  if(!(all(colnames(Cexp) %in% colnames(Cexp_origin)) && 
       all(colnames(Cexp_origin) %in% colnames(Cexp)))){
    stop("Exposure routes differ between 'fit' and 'data'")
  }
  
  n_exp <- ncol(Cexp)
  n_out <- fitDATA$n_out

  lentp <- nrow(data)
  time <- data$time
  
  tacc <- fitDATA$tacc
  rankacc <- match(tacc, time)
  if(is.na(rankacc)){
    stop("time for accumulation should be in data time vector")
  }
  
  if(n_met == 0){
    M = 0
  } else{
    M = fitMCMC$M
  }
  E = fitMCMC$E
   
  U = matrix(NA, ncol = lentp, nrow = len_MCMC)
  R = matrix(NA, ncol = lentp, nrow = len_MCMC)
  for(t in 1:lentp){
    for(i in 1:len_MCMC){
      U[i,t] =  sum(Cexp[t,] * fitMCMC$ku[i,])
    }
    R[,t] =  U[,t] / (E + M) ;
  }
  if(n_met > 0){
    D = matrix(NA, ncol = fitDATA$n_met, nrow = len_MCMC)
    for(m in 1:n_met){
      D[,m] =  fitMCMC$kem[,m] - (E + M) ;
    }
  }
  # ACCUMULATION PHASE (0 =< t =< tacc)
  # C0 <- fitMCMC$CGobs_out[,,1][,1]
  C0 <- fitDATA$C0
   
  CGobs_out <- rep(NA, len_MCMC*lentp*n_out)
  dim(CGobs_out) <- c(len_MCMC,lentp,n_out)
  
  if(n_met > 0){
    Cmet_out <-  rep(NA, len_MCMC*lentp*n_met)
    dim(Cmet_out) <- c(len_MCMC,lentp,n_met)
  } else{
    Cmet_out <- NA
  }
  
  km <- fitMCMC$km
  kem <- fitMCMC$kem
  
  for(t in 1:rankacc){
    # Parent compound
    CGobs_out[,t,1] = .var_init(
      len_MCMC,
      (C0 - R[,t]) * exp(-(E + M) * time[t]) + R[,t],
      fitMCMC$sigmaCGpred[,1],
      fixed_init
    )
    # Metabolites
    if(n_met > 0){
      for(m in 1:n_met){
        Cmet_out[,t,m] = .var_init(
          len_MCMC,
          km[,m] * (
            (C0-R[,t])/ D[,m] * (exp(-(E + M)*time[t])-exp(-kem[,m] * time[t])) +
            R[,t] / kem[,m] * (1 - exp(-(kem[,m] * time[t])))
          ),
          fitMCMC$sigmaCmetpred[,m],
          fixed_init
        )
      }
    }
  }
  # DEPURATION PHASE (t > tacc)
  for(t in (rankacc+1):lentp){
    # Parent compound
    CGobs_out[,t,1] = .var_init(
      len_MCMC,
      (C0 - R[,t] * (1 - exp((E + M)*tacc))) * exp(-(E + M) * time[t]),
      fitMCMC$sigmaCGpred[,1],
      fixed_init
    )
    # Metabolites
    if(n_met > 0){
      for(m in 1:n_met){
        Cmet_out[,t,m] =  .var_init(
          len_MCMC,
          km[,m] * (
            (C0-R[,t]) / D[m] * (exp(-(E + M) * time[t]) - exp(-kem[,m] * time[t])) + 
            R[,t] / kem[,m] * (exp(-kem[,m] * (time[t]-tacc)) - exp(-kem[,m] * time[t])) +
            R[,t] / D[m] * (exp(-(E+M)*(time[t]-tacc)) - exp(-kem[,m] * (time[t] - tacc)))
          ),
          fitMCMC$sigmaCmetpred[,m],
          fixed_init
        )
      }
    }
  }
  # GROWTH
  if(n_out == 2){
    G0 <- fitMCMC$G0
    gmax <- fitMCMC$gmax
    keg <- fitMCMC$ke[,2]
    for(t in 1:lentp){
      CGobs_out[,t,2] = .var_init(
        len_MCMC,
        (G0 - gmax) * exp(-keg * time[t]) + gmax,
        fitMCMC$sigmaCGpred[,2],
        fixed_init
      )
    }
  }
  
  predict_out <- list(
    time = time,
    CGobs_out = CGobs_out,
    Cmet_out = Cmet_out
  )
  
  class(predict_out) <- append("predictTK", class(predict_out))
  
  return(predict_out)
}

# Additional variance for initial condition
# 
# @importFrom stats rnorm
# 
# @return a vector of numeric
# 
.var_init <- function(n,x,sd,fixed_init){
  if(fixed_init == TRUE){
    return(rnorm(n,x,sd))
  } else{
    return(x)
  }
}

################################################################################
#' Sampler of TK model using stan inference machine
#'
#' @rdname predict
#' 
#' @param time_interp A vector with additional time point to interpolate. 
#' Time point of the original data set are conserved.
#' @param iter Number of time steps
#' 
#' @export
#'
predict_stan <- function(object, data, mcmc_size = NULL, fixed_init = TRUE,
                         time_interp = NULL, iter = 1000, ...) {
  
  iter <- ifelse(is.null(mcmc_size), iter, mcmc_size)

  stanTKdata <- modelData_predictStan(object, data, mcmc_size, fixed_init, time_interp)
  
  stanfit <- rstan::sampling(stanmodels$TK_predict, data = stanTKdata, algorithm ="Fixed_param",  ...)
  
  out <- list(stanTKdata = stanTKdata, stanfit = stanfit, originTKdata = object$stanTKdata)
  class(out) <- append("predictTKstan", class(out))
  return(out)
}

# @rdname modelData
# 
# @export
# 
modelData_predictStan <- function(object, data, mcmc_size = NULL, fixed_init = TRUE, time_interp = NULL, ...){
  
  fit <- object
  
  fitDATA <- fit[["stanTKdata"]]
  fitMCMC <- rstan::extract(fit[["stanfit"]])
  
  n_met <- fitDATA$n_met
  N_samples <- nrow(fitMCMC$ku)
  
  # Exposure match
  data_origin <- fitDATA$origin_data
  
  col_names_origin <- colnames(data_origin)[ .index_col_exposure(data_origin)]
  Cexp_origin <- as.data.frame(data_origin[, col_names_origin])
  colnames(Cexp_origin) <- col_names_origin
  
  col_names <- colnames(data)[ .index_col_exposure(data)]
  Cexp <- as.data.frame(data[, col_names])
  colnames(Cexp) <- col_names
  
  if(!(all(colnames(Cexp) %in% colnames(Cexp_origin)) && 
       all(colnames(Cexp_origin) %in% colnames(Cexp)))){
    stop("Exposure routes differ between 'fit' and 'data'")
  }
  
  n_exp <- ncol(Cexp)
  n_out <- fitDATA$n_out
  
  if(is.null(time_interp)){
    lentp <- nrow(data)
    tp <- data$time
    # EXPOSURE
    len_vt <- lentp
    vt <- tp
  } else{
    tp <- unique(sort(c(time_interp, data$time)))
    lentp <- length(tp)
    # EXPOSURE
    len_vt <- nrow(data)
    vt <- data$time
  }
  
  tacc <- fitDATA$tacc
  rankacc <- match(tacc, tp)
  
  if(is.na(rankacc)){
    stop("time for accumulation should be in data time vector")
  }
  
  C0 <- fitDATA$C0
  
  km <- fitMCMC$km
  kem <- fitMCMC$kem
  
  if(n_met == 0){
    M = rep(0, N_samples)
  } else{
    M = fitMCMC$M
  }

  if(n_met == 0){
    log10km = matrix(0, nrow = N_samples, ncol = n_met)
    log10kem = matrix(0, nrow = N_samples, ncol = n_met)
    sigmaCmetpred = matrix(0, nrow = N_samples, ncol = n_met)
  } else{
    log10km = fitMCMC$log10km
    log10kem = fitMCMC$log10kem
    sigmaCmetpred = fitMCMC$sigmaCmetpred
  }
  
  if(is.null( fitMCMC$gmax)){
    gmax =  matrix(0, nrow = N_samples, ncol = n_out - 1)
    G0 = matrix(0, nrow = N_samples, ncol = n_out - 1)
  } else{
    gmax = fitMCMC$gmax
    G0 = fitMCMC$G0
  }

  list_stan <- list(
    lentp = lentp,
    tp = tp,
    
    n_exp = n_exp,
    n_out = n_out,
    n_met = n_met,
    Cexp = Cexp,
    rankacc = rankacc,
    tacc = tacc,
    C0 = C0,
    
    len_vt = len_vt,
    vt = vt,
    
    elim_rate = fitDATA$elim_rate,
    
    N_samples = N_samples,
    log10ku = fitMCMC$log10ku,
    log10ke = fitMCMC$log10ke,
    
    log10km = log10km,
    log10kem = log10kem,
    
    M = M,
    E = fitMCMC$E,
    
    sigmaCGpred = fitMCMC$sigmaCGpred,
    sigmaCmetpred = sigmaCmetpred,
    
    gmax = gmax,
    G0 = G0
  )
  
  return(list_stan)
  
}

