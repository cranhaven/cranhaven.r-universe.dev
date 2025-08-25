#' Prediction function using \code{fitTK} object
#' 
#' Use when parameter are manually given by the user.
#' 
#' @rdname predict
#' 
#' @param param A dataframe with name of parameters \code{kee}, \code{keg}, \code{ku1}, 
#' \code{ku2}, ...,  \code{km1}, \code{km2}, ... and \code{kem1}, \code{kem2}, ...,
#' \code{sigmaConc}, \code{sigmaCmet} (if metabolites) and \code{sigmaGrowth} (if growth).
#' The parameter \code{kee} is mandatory.
#' @param data A data set with one column \code{time} and 1 to 4 exposure 
#' @param time_accumulation the time of accumulation.
#' @param C0 Gives the initial conditions of internal concentration.
#' @param G0 initial condition of G0 (require if \code{keg} is provided)
#' @param gmax gmax (require if \code{keg} is provided)
#' columns with name in \code{expw}, \code{exps}, \code{expf} and \code{exppw}
#' 
#' @return An object of class \code{predictTK}
#' 
#' @export
#' 
#' 
predict_manual <- function(param, data, time_accumulation = NULL, C0=0.0, G0=NA, gmax = NA){

  # R DOES NOT LIKE matrix with a single row !!!
  if(nrow(param) == 1){
    mcmc <- rbind(param,param)
  } else{
    mcmc <- param
  }
  
  col_names_MCMC <- base::colnames(mcmc)
  len_MCMC <- nrow(mcmc)
  
  ku_index <- .index_col(col_names_MCMC, "ku")
  ku <- mcmc[, ku_index]
  if(length(ku_index)==1){
    dim(ku) <- c(len_MCMC, 1)
  }
  
  km_index <- .index_col(col_names_MCMC, "km")
  kem_index <- .index_col(col_names_MCMC, "kem")
  sigmaCmet_index <- .index_col(col_names_MCMC, "sigmaCmet")
  n_met <- length(km_index)
  if(n_met > 0){
    km <- mcmc[, km_index]
    kem <- mcmc[, kem_index]
    sigmaCmet <- mcmc[, sigmaCmet_index]
    if(n_met == 1){
      dim(km) <- c(len_MCMC, 1)
      dim(kem) <- c(len_MCMC, 1)
      dim(sigmaCmet) <- c(len_MCMC, 1)
    }
  }
  
  col_names <- colnames(data)[ .index_col_exposure(data)]
  Cexp <- as.data.frame(data[, col_names])
  colnames(Cexp) <- col_names
  
  n_exp <- ncol(Cexp)
  n_out <- ifelse('keg' %in% colnames(mcmc), 2, 1)

 
  tacc <- time_accumulation
  lentp <- nrow(data)
  time <- data$time
  
  rankacc <- match(tacc, time)
  if(is.na(rankacc)){
    stop("time for accumulation should be in data time vector")
  }
  
  if(n_met == 0){
    M = 0
  } else{
    M = rowSums(km)
  }
  
  if(n_out == 1){
    E = mcmc$kee
  }else{
    E = mcmc$kee + mcmc$keg
  }
  
  sigmaConc <- mcmc$sigmaConc
  if(n_out == 2){
    sigmaGrowth <- mcmc$sigmaGrowth
  }
  
  # SET NO VARIANCE IF MCMC=1
  if(nrow(param) == 1){
    sigmaConc = 0
    sigmaGrowth = 0
    sigmaCmet = matrix(0,ncol=n_met,nrow=1)
  }

  U = matrix(NA, ncol = lentp, nrow = len_MCMC)
  R = matrix(NA, ncol = lentp, nrow = len_MCMC)
  for(t in 1:lentp){
    for(i in 1:len_MCMC){
      U[i,t] =  sum(Cexp[t,] * ku[i,])
    }
    R[,t] =  U[,t] / (E + M) ;
  }
  if(n_met > 0){
    D = matrix(NA, ncol = n_met, nrow = len_MCMC)
    for(m in 1:n_met){
      D[,m] =  kem[,m] - (E + M) ;
    }
  }
  
  CGobs_out <- rep(NA, len_MCMC*lentp*n_out)
  dim(CGobs_out) <- c(len_MCMC,lentp,n_out)
  
  if(n_met > 0){
    Cmet_out <-  rep(NA, len_MCMC*lentp*n_met)
    dim(Cmet_out) <- c(len_MCMC,lentp,n_met)
  } else{
    Cmet_out <- NA
  }
  
  for(t in 1:rankacc){
    # Parent compound
    CGobs_out[,t,1] = rnorm(
      len_MCMC,
      (C0 - R[,t]) * exp(-(E + M) * time[t]) + R[,t],
      sigmaConc
    )
    # Metabolites
    if(n_met > 0){
      for(m in 1:n_met){
        Cmet_out[,t,m] = rnorm(
          len_MCMC,
          km[,m] * (
            (C0-R[,t])/ D[,m] * (exp(-(E + M)*time[t])-exp(-kem[,m] * time[t])) +
              R[,t] / kem[,m] * (1 - exp(-(kem[,m] * time[t])))
          ),
          sigmaCmet[,m]
        )
      }
    }
  }
  # DEPURATION PHASE (t > tacc)
  for(t in (rankacc+1):lentp){
    # Parent compound
    CGobs_out[,t,1] = rnorm(
      len_MCMC,
      (C0 - R[,t] * (1 - exp((E + M)*tacc))) * exp(-(E + M) * time[t]),
      sigmaConc
    )
    # Metabolites
    if(n_met > 0){
      for(m in 1:n_met){
        Cmet_out[,t,m] =  rnorm(
          len_MCMC,
          km[,m] * (
            (C0-R[,t]) / D[m] * (exp(-(E + M) * time[t]) - exp(-kem[,m] * time[t])) +
              R[,t] / kem[,m] * (exp(-kem[,m] * (time[t]-tacc)) - exp(-kem[,m] * time[t])) +
              R[,t] / D[m] * (exp(-(E+M)*(time[t]-tacc)) - exp(-kem[,m] * (time[t] - tacc)))
          ),
          sigmaCmet[,m]
        )
      }
    }
  }
  # GROWTH
  if(n_out == 2){
    for(t in 1:lentp){
      CGobs_out[,t,2] = rnorm(
        len_MCMC,
        (G0 - gmax) * exp(-mcmc$keg * time[t]) + gmax,
        sigmaGrowth
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


.index_col <- function(obj_colname, str){
  col <- obj_colname[sapply(obj_colname, function(x){ regexpr(str, x) == TRUE})]
  col <- match(col[col != str], obj_colname)
  return(col[!base::is.na(col)])
}

