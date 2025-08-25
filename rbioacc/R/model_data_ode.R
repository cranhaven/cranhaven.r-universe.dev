#' Create a list giving data and parameters to use in the model inference.
#' 
#' 
#' @export
#' 
#' @return A \code{list} with data and parameters require for model inference.
#' 
#' 
modelData_ode <- function(df_exposure, df_internal, ...){
  UseMethod("modelData_ode")
}


#' @rdname modelData_ode
#' 
#' @param df_exposure Dataframe of exposure with 2 column (\code{time} and \code{value})
#' @param df_internal Dataframe of internal concentration with 2 column (\code{time} and \code{value})
#' @param y0 Initial concentration
#' @param t0 initial time point
#' @param unifMax Hyperparameter value
#' @param time_accumulation Time of accumulation
#' @param minK Hyperparameter value
#' @param maxK Hyperparameter value
#' @param \dots Additional arguments
#' 
#' @export
#' 
modelData_ode = function(
  df_exposure, df_internal, 
  y0 = 1, t0 = -0.001, unifMax=10, time_accumulation = NULL, minK=-5, maxK=5, ...){
  
  rtrn_ls <- list()
  
  rtrn_ls$n_rep <- 1
  rtrn_ls$n_exp <- 1
  rtrn_ls$n_out <- 1
  rtrn_ls$n_met <- 0
  
  rtrn_ls$lentp <- nrow(df_internal)
  rtrn_ls$tp <- df_internal$time
  rtrn_ls$tacc <- time_accumulation
  rtrn_ls$rankacc <- match(time_accumulation, rtrn_ls$tp)
                          
  rtrn_ls$lentp_rmNA = nrow(df_exposure)
  rtrn_ls$tp_rmNA = df_exposure$time
  
  Cexp_rmNA = df_exposure$value
  dim(Cexp_rmNA) = c(nrow(df_exposure), 1 )
  rtrn_ls$Cexp_rmNA = Cexp_rmNA

  CGobs <- df_internal$value
  dim(CGobs) <- c(nrow(df_internal), 1, 1)
  rtrn_ls$CGobs <- CGobs
  
  Cmet <- matrix(0, ncol = 0, nrow =  nrow(df_internal))
  dim(Cmet) <- c(nrow(df_internal), 0, 1)
  rtrn_ls$Cmet <- Cmet
  
  dim(y0) <- 1
  rtrn_ls$y0 <- y0
  
  rtrn_ls$t0 <- t0
  
  rtrn_ls$gmaxsup <- 1
  rtrn_ls$C0 <- 0
  
  rtrn_ls$unifMax <- unifMax
  rtrn_ls$minK <- minK
  rtrn_ls$maxK <- maxK
  
  rtrn_ls$len_vt <- nrow(df_internal)
  rtrn_ls$vt <-   df_internal$time
  
  class(rtrn_ls) <- append("stanTKdataVAR", class(rtrn_ls))
  
  return(rtrn_ls)
}
