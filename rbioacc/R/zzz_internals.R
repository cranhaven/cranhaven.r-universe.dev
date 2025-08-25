#' A simple implementation of to pivot_longer of tidyr
#' 
#' @param df A data frame to pivot.
#' @param names_to A string specifying the name of the column to create from
#' the data stored in the column names of \code{df}.
#' @param values_to A string specifying the name of the column to create from
#'  the data stored in cell values.
#'  
#' @return The data frame with a "lengthens" shape: more rows, less columns 
#' 
.fonte <- function(df, names_to, values_to){
  dfout <- data.frame(
    names_to = rep(colnames(df), each = nrow(df)),
    values_to = do.call("c", lapply(1:ncol(df), function(i) df[,i]))
  )
  colnames(dfout) <- c(names_to, values_to)
  return(dfout)
}


#' Check if two vectors \code{x} and \code{y} are equal after remove \code{Inf}
#' 
#' @param x A vector
#' @param y A vector
#' 
#' @return A logical value
#' 
.is_equal_rmInf <- function(x,y){ 
  ux = unique(x) ; uy = unique(y)
  ux = ux[ux != Inf] ; uy = uy[uy != Inf]
  return(all(ux == uy))
}


#' Return column matching "expw", "exps", "expf", "exppw" of a \code{data.frame}
#' 
#' @param data_frame a dataframe
#' 
#' @return A vector of numeric
#' 
.index_col_exposure <- function(data_frame){
  col_exp <- base::match(c("expw", "exps", "expf", "exppw"), base::colnames(data_frame))
  return(col_exp[!base::is.na(col_exp)])
}

#' Return column matching "concX" of a \code{data.frame} where X is metabolite
#' 
#' @param data_frame a dataframe
#' 
#' @return A vector of numeric
#' 
.index_col_metabolite <- function(data_frame){
  obj_colname <- base::colnames(data_frame)
  col_conc <- obj_colname[sapply(obj_colname, function(x){ regexpr("conc", x) == TRUE})]
  col_conc <- match(col_conc[col_conc != "conc"], obj_colname)
  return(col_conc[!base::is.na(col_conc)])
}

#' Replace element of a vector
#' 
#' @param x a vector
#' @param from a vector of elements to replace
#' @param to a vector with replacing elements
#' 
#' @return a vector
#' 
#' @export
#' 
#' @examples  
#' replace_(1:10,c(2,4,5,8), c(0,0,0,0))
#' replace_(c(1,2,2,3,2),c(3,2), c(4,5))
#' 
#' 
replace_ <- function(x,from,to){
  if(length(from) != length(to)){
    stop("length of `from` and `to` differs.")
  }
  for(i in 1:length(from)){
    x <- replace(x, x %in% from[i], to[i])
  }
  return(x)
}



.readapt_time <- function(object, time_reference){
  obj_colname = base::colnames(object)
  lentp=length(time_reference)
  
  time_loc <- !(time_reference %in% object$time)
  if(sum(time_loc) > 0){
    time_NA <- time_reference[time_loc]
    # 1. time column as first column
    object <- cbind(time = object$time, object[, obj_colname != "time"])
    # 2. rebuild object with addition NA lines
    ls_NA <- lapply(1:length(time_NA), function(i){c(time_NA[i],rep(NA,ncol(object)-1))})
    row_NA <- do.call("rbind", ls_NA)
    colnames(row_NA) <- colnames(object)
    
    object <- rbind(object, row_NA)
    object <- object[order(object[, "time"]), ,drop = TRUE]
  }
  
  # convert NA to Inf because Stan does not support NA!
  object[is.na(object)] <- Inf
  return(object)
}

.interpolate_Inf <- function(v,t){
  min_t <- min(t[which(v != Inf)])
  max_t <- max(t[which(v != Inf)])
  v[t < min_t] <- 0
  v[t > max_t] <- 0
  v[!is.finite(v)] <- NA
  return(as.numeric(zoo::na.approx(v, t)))
}

.regularize_Inf <- function(v){
  v[!is.finite(v)] <- unique(v[is.finite(v)])
  return(v)
}

.modelDataSingle <- function(object, ls_col, ...){
  
  col_exp = ls_col$col_exposure
  col_conc = ls_col$col_metabolite
  
  rtrn_ls = list()
  # 1. Exposure
  rtrn_ls$Cexp = as.matrix(object[, col_exp])
  # linear interpolation
  for( i in 1:length(col_exp)){
    rtrn_ls$Cexp[,i] <- .regularize_Inf(rtrn_ls$Cexp[,i])
  }
  
  # 2. Parent Conc
  # rtrn_ls$Cobs = as.matrix(object$conc)
  # 3. Metabolites
  if(length(col_conc) > 0){
    rtrn_ls$Cmet = as.matrix( object[, col_conc])
  } else{
    rtrn_ls$Cmet = matrix(0, nrow = nrow(object), ncol = 0)
  }
  # 4. Growth
  if("growth" %in% colnames(object)){
    # rtrn_ls$Gobs = object$growth
    rtrn_ls$CGobs = matrix(c(object$conc,object$growth), byrow=FALSE, ncol=2)
  } else{
    # rtrn_ls$Gobs = matrix(0, nrow= nrow(object), ncol = 0)
    rtrn_ls$CGobs = as.matrix(object$conc)
  }
  return(rtrn_ls)
}


.check_modelData_object <- function(object){
  
  obj_colname <- base::colnames(object)
  
  # time x replicate
  if(!('time' %in% obj_colname)) stop("`time` not a column name.")
  if(!('replicate' %in% obj_colname)) stop("`replicate` not a column name.")
  #☺ check unique time / replicate
  ls_object <- base::split(object, object$replicate)
  check_timereplicate <- sapply(1:length(ls_object), function(i) length(unique(ls_object[[i]]$time)) == length(ls_object[[i]]$time))
  if(!all(check_timereplicate)) stop("no unique `time` per `replicate`")
  
  if(!('conc' %in% obj_colname)) stop("`conc` not a column name.")
  
  if(!any(c("expw", "exps", "expf", "exppw") %in% obj_colname)) stop("no exposure routes provided.")
  
}

#' @importFrom stats quantile
.df_quant95 <- function(x,...){
  
  mat_quants = apply(x, 2, quantile, probs = c(0.025, 0.5, 0.975), ...)
  
  df = data.frame(
    q50 = mat_quants[2,],
    qinf95 = mat_quants[1,],
    qsup95 = mat_quants[3,]
  )
  return(df)
}

.add_data = function(df_quant95,tp,data,id){
  if(is.vector(data)){
    df_quant95$time = tp
    df_quant95$observation = data
    df_quant95$replicate = 1
    df <- df_quant95
  } else{
    ls <- lapply(1:ncol(data),
                 function(i){
                   df_quant95$time = tp
                   df_quant95$observation = data[,i]
                   df_quant95$replicate = i
                   return(df_quant95)
                 })
    df <- do.call("rbind", ls)
  }
  df <- df[df$observation != Inf,]
  df$variable <-  id
  return(df)
}

.add_data_predict = function(df_quant95,tp,id){
  df_quant95$time <- tp
  df <- df_quant95
  df$variable <-  id
  return(df)
}

.df_for_plot <- function(fit){
  fitMCMC = rstan::extract(fit$stanfit)
  # 
  ls_out <- list()
  ls_out$conc <- .add_data(
    .df_quant95(fitMCMC$CGobs_out[,,1]),
    fit$stanTKdata$tp,
    fit$stanTKdata$CGobs[,1,],
    "conc"
  )
  if(dim(fitMCMC$CGobs_out)[3] == 2){
    ls_out$growth <- .add_data(
      .df_quant95(fitMCMC$CGobs_out[,,2]),
      fit$stanTKdata$tp,
      fit$stanTKdata$CGobs[,2,],
      "growth"
    )
  }
  if("Cmet_out" %in% names(fitMCMC)){
    for(i in 1:fit$stanTKdata$n_met){
      name <- paste0("concm",i)
      ls_out[[name]] <- .add_data(
        .df_quant95(fitMCMC$Cmet_out[,,i]),
        fit$stanTKdata$tp,
        fit$stanTKdata$Cmet[,i,],
        name
      )
    }
  }
  
  df <- do.call("rbind", ls_out)
  
  return(df)
}


.add_data_only = function(tp,data,id){
  if(is.vector(data)){
    df_init <- data.frame(data = rep("data", length(data)))
    df_init$time = tp
    df_init$observation = data
    df_init$replicate = 1
    df <- df_init
  } else{
    df_init <- data.frame(data = rep("data", nrow(data)))
    ls <- lapply(1:ncol(data),
                 function(i){
                   df_init$time = tp
                   df_init$observation = data[,i]
                   df_init$replicate = i
                   return(df_init)
                 })
    df <- do.call("rbind", ls)
  }
  df <- df[df$observation != Inf,]
  df$variable <-  id
  return(df)
}

.df_for_plot_data <- function(fit){
  fitMCMC = rstan::extract(fit$stanfit)
  # 
  ls_out <- list()
  ls_out$conc <- .add_data_only(
    fit$stanTKdata$tp,
    fit$stanTKdata$CGobs[,1,],
    "conc"
  )
  if(dim(fitMCMC$CGobs_out)[3] == 2){
    ls_out$growth <- .add_data_only(
      fit$stanTKdata$tp,
      fit$stanTKdata$CGobs[,2,],
      "growth"
    )
  }
  if("Cmet_out" %in% names(fitMCMC)){
    for(i in 1:fit$stanTKdata$n_met){
      name <- paste0("concm",i)
      ls_out[[name]] <- .add_data_only(
        fit$stanTKdata$tp,
        fit$stanTKdata$Cmet[,i,],
        name
      )
    }
  }
  
  df <- do.call("rbind", ls_out)
  
  return(df)
}
