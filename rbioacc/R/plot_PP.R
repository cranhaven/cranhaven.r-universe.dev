#' Data frame of Posterior over Prior 
#' 
#' @rdname df_PP
#' 
#' @param \dots Additional arguments
#' 
#' @export
#' 
df_PriorPost <- function(fit, ...){
  UseMethod("df_PriorPost")
}

#' Data frame of Posterior over Prior 
#' 
#' @rdname df_PP
#' 
#' @param fit An object of class \code{fitTK} returned by the function \code{fitTK()}.
#' @param select A string selecting the parameters. Defaults is \code{"all"} and
#' select all parameters.Deterministc parameters can be selected by setting
#' \code{"deterministic"} and stochastic parameter with \code{"stochastic"}
#' 
#' @return An object of class \code{data.frame}
#' 
#' @export
#' @importFrom stats runif
#' 
df_PriorPost.fitTK <- function(fit, select = "all", ...){
  fitMCMC <- rstan::extract(fit[["stanfit"]])
  elim_rate <- fit[["stanTKdata"]]$elim_rate
  lengthMCMC <- nrow(fitMCMC$ku)
  
  ls_post <- list()
  ls_prior <- list()
  
  if(select != "stochastic"){
    ls_post$ku <- lapply(1:ncol(fitMCMC$ku), function(i) fitMCMC$ku[, i] )
    ls_prior$ku <- lapply(1:ncol(fitMCMC$ku), function(i) 10^runif(lengthMCMC, -5, 5))
    
    if(is.infinite(elim_rate)){
      ls_post$kee <- list(fitMCMC$ke[, 1] )
      ls_prior$kee <- list(10^runif(lengthMCMC, -5, 5))
      if(ncol(fitMCMC$ke) == 2){
        ls_post$keg <- list(fitMCMC$ke[, 2] )
        ls_prior$keg <- list(10^runif(lengthMCMC, -5, 5))
      }
    }
    if("km" %in% names(fitMCMC)){
      ls_post$km <- lapply(1:ncol(fitMCMC$km), function(i) fitMCMC$km[, i] )
      ls_prior$km <- lapply(1:ncol(fitMCMC$km), function(i) 10^runif(lengthMCMC, -5, 5))
    }
    if("kem" %in% names(fitMCMC)){
      ls_post$kem <- lapply(1:ncol(fitMCMC$kem), function(i) fitMCMC$kem[, i] )
      ls_prior$kem <- lapply(1:ncol(fitMCMC$kem), function(i) 10^runif(lengthMCMC, -5, 5))
    }
  }
  if(select != "deterministic"){
    ls_post$sigmaConc <- list(fitMCMC$sigmaCGpred[, 1] )
    ls_prior$sigmaConc <- list(runif(lengthMCMC, 0,  fit$stanTKdata$unifMax))
    
    if(ncol(fitMCMC$ke) == 2){
      ls_post$sigmaGrowth <- list(fitMCMC$sigmaCGpred[, 2] )
      ls_prior$sigmaGrowth <- list(runif(lengthMCMC, 0,  fit$stanTKdata$unifMax))
    }
    if("km" %in% names(fitMCMC)){
      ls_post$sigmaCmet <- lapply(1:ncol(fitMCMC$sigmaCmetpred), function(i) fitMCMC$sigmaCmetpred[, i] )
      ls_prior$sigmaCmet <- lapply(1:ncol(fitMCMC$sigmaCmetpred), function(i) runif(lengthMCMC, 0,  fit$stanTKdata$unifMax))
    }
  }
  
  c_post <- do.call("c", ls_post)
  c_prior <- do.call("c", ls_prior)
  c_var1 <- rep(names(c_post), each = lengthMCMC)
  c_var2 <- rep(names(c_prior), each = lengthMCMC)
  
  df <- data.frame(
    parameter = c(c_var1,c_var2),
    type = c(rep("posterior", length(c_var1)), rep("prior", length(c_var2))),
    value = c(do.call("c", c_post), do.call("c", c_prior))
  )
  class(df) <- append("df_PP", class(df))
  
  return(df)
}

#' Plot Posterior over Prior 
#' 
#' @rdname plot_PP
#' 
#' @param \dots Additional arguments
#' 
#' @export
#' 
plot_PriorPost <- function(x, ...){
  UseMethod("plot_PriorPost")
}


#' Plot Posterior over Prior 
#' 
#' @rdname plot_PP
#'
#' 
#' @param x An object of class \code{fitTK} returned by the function \code{fitTK()}.
#' @param select A string selecting the parameters. Defaults is \code{"all"} and select all parameters.
#' Deterministic parameters can be selected by setting \code{"deterministic"} and 
#' stochastic parameter with \code{"stochastic"}.
#' @param \dots addition arguments
#' 
#' @return A plot of class \code{ggplot}.
#' 
#' @export
#' 
#' 
plot_PriorPost.fitTK <- function(x, select = "all", ...){
  
  df <- df_PriorPost(x, select)
  
  df$group <- paste0(df$parameter, df$type)
  
  ggplot(data = df, aes_string('value')) + 
    theme_classic() +
    scale_fill_manual(values = c("orange", "grey")) +
    scale_x_log10() +
    geom_density(aes_string(group = 'group', fill = 'type'), alpha = 0.5, color = NA) +
    facet_wrap(~parameter, scales = "free")
  
}

#' @rdname plot_PP
#'
#' 
#' @param x A data.frame of class \code{df_PP} returned by the function \code{df_PriorPost()}.
#' @param select A string selecting the parameters. Defaults is \code{"all"} and select all parameters.
#' Deterministic parameters can be selected by setting \code{"deterministic"} and 
#' stochastic parameter with \code{"stochastic"}.
#' @param \dots addition arguments
#' 
#' @return A plot of class \code{ggplot}.
#' 
#' @export
#' 
#' 
plot_PriorPost.df_PP <- function(x, select = "all", ...){
  
  df <- x
  
  df$group <- paste0(df$parameter, df$type)
  
  ggplot(data = df, aes_string('value')) + 
    theme_classic() +
    scale_fill_manual(values = c("orange", "grey")) +
    scale_x_log10() +
    geom_density(aes_string(group = 'group', fill = 'type'), alpha = 0.5, color = NA) +
    facet_wrap(~parameter, scales = "free")
  
}