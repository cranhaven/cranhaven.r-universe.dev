# generic S3 method -------------------------------------------------------

"predict.fusionModel" <- function(object, new.locs, type = c("summary", "full"), ...){
  if (!inherits(object, "fusionModel")) stop("fusion.model must be an output of either fusionStan or fusionINLA")
  if (missing(new.locs)) stop("new.locs must be supplied")
  if (missing(type)) type <- "summary"

  if (class(new.locs)[1] == "data.frame"){
    if (!all(c("x","y") %in% names(new.locs))) stop("missing x and y coordinates in the data frame new.locs")
  } else if (inherits(new.locs, "sf")){
    new.locs <- as(new.locs, "Spatial")
    new.locs <- new.locs@coords
    colnames(new.locs) <- c("x","y")
  } else {
    stop("new.locs must be of class data.frame or sf")
  }

  if (inherits(object$model, "inla")){
    predictINLA(object, new.locs = new.locs, type = type)
  } else {
    predictStan(object, new.locs = new.locs, type = type)
  }
}
# INLA model prediction ----------------------------------------------------------

predictINLA <- function(fusion.model, new.locs, type){

  if (!requireNamespace("INLA", quietly = TRUE)) {
    stop("Package 'INLA' needed for this function to work. Please install it first, visit https://www.r-inla.org/download-install",
         call. = FALSE)
  }

  data <- fusion.model$data
  model <- fusion.model$model

  switch(type,
         "summary" = {
           latent <- lapply(1:length(model$summary.random),
                            function(i) INLA::inla.mesh.project(INLA::inla.mesh.projector(fusion.model$mesh, loc = as.matrix(new.locs)),
                                                                model$summary.random[[i]]$`0.5quant`))
         },
         "full" = {
           latent <- lapply(1:length(model$summary.random),
                            function(i) INLA::inla.mesh.project(INLA::inla.mesh.projector(fusion.model$mesh, loc = as.matrix(new.locs)),
                                                                model$summary.random[[i]]))
         },
         stop("type must be one of c(summary, full)"))
  names(latent) <- paste0("latent.", names(model$summary.random))
  latent <- as.data.frame(latent)
  latent$x <- new.locs[,1]
  latent$y <- new.locs[,2]
  result <- st_as_sf(latent, coords =c("x", "y"))
  return(result)
}

# Stan model prediction ----------------------------------------------------------

predictStan <- function(fusion.model, new.locs, type){
  data <- fusion.model$data
  model <- fusion.model$model

  predict_nngp_w <- function(phi, pred, locs, iter, n.neighbor, w){

    # Helper functions
    get_index_dist <- function(pred, locs , n.neighbor, n) {

      i_index <- function(i, pred, locs , n.neighbor) {
        dist <- rdist(pred[i,], locs)
        im <- sort(order(dist)[1:n.neighbor])
        return(im)
      }
      # distance matrix for location i and its neighbors
      i_dist <- function(i, neighbor_index, pred, locs )	rdist(pred[i,],locs[neighbor_index[,i],])
      # get index of neighborhood
      neighbor_index <- sapply(1:n, i_index, pred, locs , n.neighbor)
      # get distance matrix for each i
      neighbor_dist <- sapply(1:n, i_dist, neighbor_index, pred, locs )
      return(list(i = neighbor_index, d = neighbor_dist))
    }
    get_neardistM <- function (ind, neighbor_index, locs) {
      M_i <- c(dist(locs[neighbor_index[,ind],])) # unlist the distance matrix of neighbor - neighbors
      return(M_i)
    }
    n <- nrow(pred)
    w.pred <- array(0, dim = c(dim(w)[1:2], n)) # array with dim(iter, n.latent, n.pred)
    ind_distM <- get_index_dist(pred, locs, n.neighbor, n)
    neardistM <- t(sapply(1:n, get_neardistM, ind_distM$i, locs))
    neardist <- t(ind_distM$d)
    neardist[neardist==0] <- min(neardist[neardist!=0])/2 # set coincided location to half of minimum distance
    nearind <- t(ind_distM$i)
    Ft <- BtW <- numeric(iter)
    for (i in 1:n) { # for each predicted location
      C_nei <- diag(1.001,n.neighbor) # m by m
      h = 0;
      for (j in 1:(n.neighbor-1)){
        for (k in (j+1):n.neighbor){
          h = h + 1
          C_nei[j, k] <- exp(- neardistM[i, h])
          C_nei[k, j] <- C_nei[j, k]
        }
      }
      C_site_nei <- exp(- neardist[i, ])
      for (j in 1:dim(w)[2]){ # for each latent process
        for (it in 1:iter){ # for each iteration
          C_site_nei_C_nei_inv <- solve(C_nei^(1/phi[it, j]), C_site_nei^(1/phi[it, j]))# m by m times m by n
          Ft[it] <- C_site_nei_C_nei_inv %*% C_site_nei^(1/phi[it, j])
          BtW[it] <- C_site_nei_C_nei_inv %*% w[it, j, nearind[i,]] # 1 by m, m by 1
        }
        w.pred[, j, i] <- rnorm(it, BtW, sqrt(1 - Ft))
      }
    }
    return(w.pred)
  }
  iters <- sum(sapply(model@stan_args, function(x) x$iter - x$warmup))

  if (data$n_point_var + data$n_pp_var > 0){
    fit.samples.ws <- rstan::extract(model, pars = "w")$w
  }

  if (data$n_area_var > 0){
    fit.samples.wa <- rstan::extract(model, pars = "wa")$wa
    if (data$n_point_var + data$n_pp_var > 0){
      fit.samples.ws <- array(c(fit.samples.ws, fit.samples.wa),
                              dim = c(dim(fit.samples.ws)[1:2], dim(fit.samples.ws)[3] + dim(fit.samples.wa)[3]))
    } else {
      fit.samples.ws <- fit.samples.wa
    }
  }

  fit.samples.phi <- rstan::extract(model, pars = "phi")$phi
  fit.samples.Z <- rstan::extract(model, pars = grep("Z_",model@model_pars, value = T))

  pred.w <- predict_nngp_w(fit.samples.phi, new.locs, data$locs, iters, data$n_neighbor, fit.samples.ws)
  n.latent <- dim(pred.w)[2]
  latent <- list()
  switch(type,
         "summary" = {
           for (i in 1:n.latent){
             latent[[i]] <- apply(pred.w[,i,], 2, median)
           }
         },
         "full" = {
           for (i in 1:n.latent){
             latent[[i]] <- pred.w[,i,]
           }
         },
         stop("type must be one of c(summary, full)"))
  names(latent) <- paste0("latent", 1:n.latent)
  latent <- as.data.frame(latent)
  latent$x <- new.locs[,1]
  latent$y <- new.locs[,2]
  result <- st_as_sf(latent, coords =c("x", "y"))
  return(latent)
}
