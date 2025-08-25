#' Create a list giving data and parameters to use in the model inference.
#' 
#' @param object An object of class \code{data.frame}
#' @param time_accumulation A scalar givin accumulation time
#' @param elimination_rate A scalar for the elimination rate. Default is \code{NA}.
#' To remove elimination rate, set \code{elimination_rate = 0}.
#' @param \dots Further arguments to be passed to generic methods
#' 
#' @export
#' 
#' @return A \code{list} with data and parameters require for model inference.
#' 
#' 
modelData <- function(object, ...){
  UseMethod("modelData")
}


#' @rdname modelData
#' 
#' @export
#' @importFrom stats na.omit
modelData.data.frame <- function(object, time_accumulation, elimination_rate = NA, ...){
  
  .check_modelData_object(object)
  
  obj_colname = base::colnames(object)
  rtrn_ls = list()
  
  # Priors on concentrion 
  # PRIORS ENLEVER - LA CONCENTRATION MAX !!!
  rtrn_ls$unifMax = 500 * max(object$conc, na.rm = TRUE)
  
  if(!is.na(elimination_rate)){
    rtrn_ls$elim_rate = elimination_rate
  } else{
    rtrn_ls$elim_rate = Inf
  }
  
  ########################
  # 0. GENERAL TIME VECTOR
  rtrn_ls$tp <- sort(unique(object$time))
  rtrn_ls$lentp <- length(rtrn_ls$tp)
  
  # 1. Handle Replicate
  ls_object <- base::split(object, object$replicate)
  rtrn_ls$n_rep <- length(ls_object)
  
  ls_object <- lapply(ls_object, .readapt_time, time_reference = rtrn_ls$tp)
  
  # 2. Exposure routes
  col_exposure <- .index_col_exposure(object)
  rtrn_ls$n_exp <- length(col_exposure)
  
  col_metabolite <- .index_col_metabolite(object)
  rtrn_ls$n_met <- length(col_metabolite)

  ls_object <- lapply(
    ls_object, .modelDataSingle,
    list(col_exposure=col_exposure, col_metabolite=col_metabolite)
  )
  
  Cexp <- do.call("cbind", sapply(ls_object, `[`, 1))
  dim(Cexp) <- c(rtrn_ls$lentp,rtrn_ls$n_exp,rtrn_ls$n_rep)
  if(all(sapply(1:rtrn_ls$n_rep, function(i){ .is_equal_rmInf(Cexp[,,1], Cexp[,,i]) } ))){
    rtrn_ls$Cexp <- as.matrix(Cexp[,,1])
  } else{
    stop("Replicates must have same Exposure profile")
  }

  Cmet <- do.call("cbind", sapply(ls_object, `[`, 2))
  dim(Cmet) <- c(rtrn_ls$lentp,rtrn_ls$n_met,rtrn_ls$n_rep)
  rtrn_ls$Cmet <- Cmet

  CGobs <- do.call("cbind", sapply(ls_object, `[`, 3))
  # 5. Priors Growth
  if("growth" %in% colnames(object)){
    rtrn_ls$n_out <- 2
    rtrn_ls$gmaxsup <- 3*max(na.omit(object$growth, na.rm = TRUE))

  } else{
    rtrn_ls$n_out <- 1
    rtrn_ls$gmaxsup <- 0
  }
  
  dim(CGobs) <- c(rtrn_ls$lentp,rtrn_ls$n_out,rtrn_ls$n_rep)
  rtrn_ls$CGobs <- CGobs
  
  # 3. Accumulation time
  rtrn_ls$tacc <- time_accumulation
  rtrn_ls$rankacc <- match(time_accumulation, rtrn_ls$tp)

  rtrn_ls$C0 <- mean(object[object$time == 0, ]$conc, na.rm = TRUE)
  
  # # 4. Prediction
  nsimu <- 500 # number of time points to simulate the model
  vt <- base::seq(0.0000001, max(rtrn_ls$tp)-0.0000001, length.out = nsimu)
  rtrn_ls$vt <- base::sort(unique(c(vt, rtrn_ls$tp)))
  rtrn_ls$len_vt <- length(rtrn_ls$vt)

  rtrn_ls$origin_data <- object
  
  class(rtrn_ls) <- append("stanTKdataCST", class(rtrn_ls))
  
  return(rtrn_ls)
}






