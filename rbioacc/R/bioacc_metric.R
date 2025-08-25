#' @export
#' 
#' @rdname bioacc_metric
#' 
#' @param fit An \code{stanFit} object
#' @param \dots Further arguments to be passed to generic methods
#' 
#' 
bioacc_metric <- function(fit, ...){
  UseMethod("bioacc_metric")
}

#' Biaccumulation metrics
#' 
#' 
#' @rdname bioacc_metric
#' 
#' @param type A string with the type of metric: \code{k} for the kinetics
#' BioConcentration Factor, \code{ss} for the steady state BioConcentration Factor.
#' @param route Provide exposure route: \code{all}
#' 
#' @return a data frame 
#' 
#' @export
#' 
bioacc_metric.fitTK <- function(fit, type = "k", route = "all", ...){
  
  fitMCMC <- rstan::extract(fit[["stanfit"]])
  
  if(type == "k"){
    if(!is.null(fitMCMC$km)){
      sum_ <- apply(fitMCMC$ke, 1, sum) + apply(fitMCMC$km, 1, sum)
    } else{
      sum_ <- apply(fitMCMC$ke, 1, sum)
    }
    ls_out <- lapply(1:ncol(fitMCMC$ku), function(i) fitMCMC$ku[,i] / sum_)
  }
  if(type == "ss"){
    rankacc <- fit$stanTKdata$rankacc
    conc_sim <- fitMCMC$CGobs_out[,rankacc,1]
    Cexp <- fit$stanTKdata$Cexp
    ls_out <- lapply(1:ncol(Cexp), function(i) conc_sim / Cexp[rankacc, i])
  }
  names(ls_out) <- exposure_names(fit$stanTKdata$origin_data)
  
  df <- data.frame(do.call("cbind", ls_out))
  if(route != "all"){
    df <- as.data.frame(df[, route])
    colnames(df) <- route
  }
  if(type == "k"){
    colnames(df) <- .switch_k(colnames(df))
  }
  if(type == "ss"){
    colnames(df) <- .switch_ss(colnames(df))
  }
  
  class(df) <- append("bioaccMetric", class(df))
  
  return(df)
}

#' Plot function for object of class \code{bioaccMetric}
#' 
#' @param x a data frame
#' @param \dots Additional arguments
#'  
#' @export
#' 
#' @return A plot of class \code{ggplot}
#' 
plot.bioaccMetric <- function(x, ...){
  df <- x

  df_plt <- .fonte(df, "exposure","value")
  
  df_quant <- .fonte(.df_quant95(df), "Quantile", "value")
  df_quant$exposure <- rep(colnames(df), 3)
  
  plt <- ggplot() + 
    theme_minimal() +
    labs(x = "Bioacc Metric", y = "Density") +
    geom_density(data = df_plt,
                 aes_string(x = 'value', fill = 'exposure'), fill = "grey", color = NA) +
    geom_vline(data = df_quant,
               aes_string(xintercept = 'value', group = 'Quantile'), linetype = "dashed") +
    facet_wrap(~exposure, scales = "free")
    
  return(plt)
}

#' Retrieve exposure routes names from object
#' 
#' @param object a data frame.
#'  
#' @export
#' 
#' @return A vector of string
#' 
exposure_names <- function(object){
  col_exposure <- .index_col_exposure(object)
  sub <- substring(colnames(object)[col_exposure], first = 4)
  return(sub)
}

# ---- INTERNAL
.switch_k <- function(x){
  sapply(seq_along(x), function(i){
    switch(x[i],
           "w"="BCFk",
           "s"="BSAFk",
           "f"="BMFk",
           "pw"="BCFpwk")
  })
}
.switch_ss <- function(x){
  sapply(seq_along(x), function(i){
    switch(x[i],
           "w"="BCFss",
           "s"="BSAFss",
           "f"="BMFss",
           "pw"="BCFpwss")
  })
}
