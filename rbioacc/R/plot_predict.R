#' Plotting method for \code{predictTK} objects
#'
#' This is the generic \code{plot} S3 method for the
#' \code{predictTK}.
#' 
#' @rdname plot_predict
#' 
#' @param x An object of class \code{predictTK} returned by predict
#' @param \dots Additional arguments
#' 
#' @return A plot of class \code{ggplot}
#' 
#' @export
#' 
plot.predictTK <- function(x, ...){
  
  predict <- x
  
  df <- .df_for_plot_predict(predict)

  # HACK TO BE > 0
  df$q50 <- ifelse(df$q50<0,0,df$q50)
  df$qinf95 <- ifelse(df$qinf95<0,0,df$qinf95)
  df$qsup95 <- ifelse(df$qsup95<0,0,df$qsup95)
    
  plt <- ggplot(data = df) + 
    theme_classic() +
    labs(x = "Time", y = "Concentration") +
    # scale_y_continuous(limits = c(0,NA)) +
    geom_ribbon(
      aes_string(x = 'time', ymin = 'qinf95', ymax = 'qsup95'), fill = "grey80") +
    geom_line(aes_string(x = 'time', y = 'q50'), color = "orange") +
    facet_wrap(~variable, scales = "free")
  
  return(plt)
  
}

#' @rdname plot_predict
#' 
#' @param add_data logical TRUE or FALSE to add the orignal data of the fit object
#' \code{x}
#' 
#' @export
#' 
plot.predictTKstan <- function(x, add_data=FALSE, ...){
  
  predict <- x
  
  df <- .df_for_predictStan(predict)

  # HACK TO BE > 0
  df$q50 <- ifelse(df$q50<0,0,df$q50)
  df$qinf95 <- ifelse(df$qinf95<0,0,df$qinf95)
  df$qsup95 <- ifelse(df$qsup95<0,0,df$qsup95)
  
  plt <- ggplot(data = df) + 
    theme_classic() +
    labs(x = "Time", y = "Concentration") +
    # scale_y_continuous(limits = c(0,NA)) +
    geom_ribbon(
      aes_string(x = 'time', ymin = 'qinf95', ymax = 'qsup95'), fill = "grey80") +
    geom_line(aes_string(x = 'time', y = 'q50'), color = "orange") +
    facet_wrap(~variable, scales = "free")

  if(add_data == TRUE){
    
    fit_data = list(
      stanfit = x$stanfit,
      stanTKdata = x$originTKdata
    )
    
    df_data <- .df_for_plot_data(fit_data)
    
    plt <- plt +  geom_point(
      data = df_data,
      aes_string(x = 'time', y = 'observation' ))
  }
  
  return(plt)
  
}





############################################# INTERNAL

.df_for_plot_predict <- function(predict){
  
  ls_out <- list()
  ls_out$conc <- .add_data_predict(
    .df_quant95(predict$CGobs_out[,,1], na.rm=TRUE),
    predict$time,
    "conc"
  )
  if(dim(predict$CGobs_out)[3] == 2){
    ls_out$growth <- .add_data_predict(
      .df_quant95(predict$CGobs_out[,,2]),
      predict$time,
      "growth"
    )
  }
  if(!is.null(dim(predict$Cmet_out)[3])){
    for(i in 1:dim(predict$Cmet_out)[3]){
      name <- paste0("concm",i)
      ls_out[[name]] <- .add_data_predict(
        .df_quant95(predict$Cmet_out[,,i]),
        predict$time,
        name
      )
    }
  }
  
  df <- do.call("rbind", ls_out)
  
  return(df)
}

.df_for_predictStan <- function(fit){
  fitMCMC = rstan::extract(fit$stanfit)
  # 
  ls_out <- list()
  ls_out$conc <- .add_data_predict(
    .df_quant95(fitMCMC$CGobs_out[,,1]),
    fit$stanTKdata$tp,
    "conc"
  )
  if(dim(fitMCMC$CGobs_out)[3] == 2){
    ls_out$growth <- .add_data_predict(
      .df_quant95(fitMCMC$CGobs_out[,,2]),
      fit$stanTKdata$tp,
      "growth"
    )
  }
  if("Cmet_out" %in% names(fitMCMC)){
    for(i in 1:fit$stanTKdata$n_met){
      name <- paste0("concm",i)
      ls_out[[name]] <- .add_data_predict(
        .df_quant95(fitMCMC$Cmet_out[,,i]),
        fit$stanTKdata$tp,
        name
      )
    }
  }
  
  df <- do.call("rbind", ls_out)
  
  return(df)
}
