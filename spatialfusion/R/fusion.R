fusion <- function(data, n.latent = 1, bans = 0, pp.offset, verbose = FALSE, ...){


# some checks -------------------------------------------------------------

  if (missing(data)){
    stop("the object `data` is missing")
  } else {
    if (!class(data) %in% c("dinla", "dstan")) stop("argument `data` must be an output of `fusionData()`")
  }

  if (data$n_point_var + data$n_area_var + data$n_pp_var < n.latent){
    stop("the number of response variables must be greater than or equal to the number of latent Gaussian processes")
  }

  if (identical(bans, 0)){
  } else if (any(class(bans) == "matrix")){
    if (dim(bans)[1] == data$n_point_var + data$n_area_var + data$n_pp_var & dim(bans)[2] == n.latent){
      if (! length(unique(c(bans))) == 2 & all(c(0,1) %in% unique(c(bans)))){
        stop("bans must contain only 0s and 1s")
      } else {
        if (any(rowSums(bans) == ncol(bans))) stop("at least one response variable is not associated with the latent process due to banning, consider removing it in spatial fusion modeling")
      }
    } else {
      stop(paste("bans must be a", data$n_point_var + data$n_area_var + data$n_pp_var, "by", n.latent, "matrix"))
    }
  } else {
    stop("bans must be either a matrix with 0s and 1s or a integer of 0 indicating no banning")
  }

UseMethod("fusion", data)

}
