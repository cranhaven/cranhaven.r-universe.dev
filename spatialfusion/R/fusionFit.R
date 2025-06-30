
# generic S3 method -------------------------------------------------------

"fitted.fusionModel" <- function(object, type = c("link","summary","full","latent"), ...){
  if (!inherits(object, "fusionModel")) stop("fusion.model must be an output of either fusionStan or fusionINLA")
  if (missing(type)) type <- "link"
  if (inherits(object$model, "inla")){
    fittedINLA(object, type = type)
  } else {
    fittedStan(object, type = type)
  }
}


# INLA model fit ----------------------------------------------------------


fittedINLA <- function(fusion.model, type){
  model <- fusion.model$model
  data <- fusion.model$data

  switch(type,
         "link" = {
           if (data$n_point_var > 0){
             for (i in 1:data$n_point_var){
               assign(paste0("point",i), model$summary.linear.predictor$`0.5quant`[1:data$n_point+(i-1)*data$n_point])
             }
           }

           if (data$n_area_var > 0){
             for (i in 1:data$n_area_var){
               assign(paste0("area",i), model$summary.linear.predictor$`0.5quant`[(data$n_point + 1):(data$n_point + data$n_area)+(i-1)*data$n_area])
             }
           }
         },
         "latent" = {
           for (i in 1:length(model$summary.random)){
             assign(paste0("latent",i), data.frame(x = fusion.model$mesh$loc[,1], y = fusion.model$mesh$loc[,2], latent = model$summary.random[[i]]$`0.5quant`))
           }
         },
         "summary" = {
           if (data$n_point_var > 0){
             for (i in 1:data$n_point_var){
                 assign(paste0("point",i), model$summary.fitted.values[1:data$n_point+(i-1)*data$n_point,])
             }
           }

           if (data$n_area_var > 0){
             for (i in 1:data$n_area_var){
                 assign(paste0("area",i), model$summary.fitted.values[(data$n_point + 1):(data$n_point + data$n_area)+(i-1)*data$n_area,])
             }
           }
         },
         "full" = {
           if (data$n_point_var > 0){
             for (i in 1:data$n_point_var){
               assign(paste0("point",i), model$marginals.fitted.values[1:data$n_point+(i-1)*data$n_point])
             }
           }

           if (data$n_area_var > 0){
             for (i in 1:data$n_area_var){
               assign(paste0("area",i), model$marginals.fitted.values[(data$n_point + 1):(data$n_point + data$n_area)+(i-1)*data$n_area])
             }
           }
         },
         stop("type must be one of c(link, latent, summary, full)"))

  switch(type,
         "link" = , "summary" = , "full" = {
           out.names <- c(if(data$n_point_var > 0) paste0("point", 1:data$n_point_var),
                          if(data$n_area_var > 0) paste0("area", 1:data$n_area_var))
         },
         "latent" = {
           out.names <- paste0("latent", 1:length(model$summary.random))
         })

  out <- lapply(out.names, function(x) eval(parse(text = x)))
  names(out) <- out.names
  return(out)
}


# Stan model fit ----------------------------------------------------------


fittedStan <- function(fusion.model, type){
  data <- fusion.model$data
  model <- fusion.model$model
  dists <- fusion.model$data$distributions
  iters <- (model@stan_args[[1]]$iter - model@stan_args[[1]]$warmup) * length(model@stan_args)

  pars <- c("phi", grep("beta",model@model_pars, value = T), grep("Z_",model@model_pars, value = T))
  if (data$n_norm > 0) pars <- c(pars, "tau_sq")

  fit.samples.pars <- rstan::extract(model, pars = pars)
  if (data$n_point_var + data$n_pp_var > 0) fit.samples.w <- rstan::extract(model, pars = "w")$w

  n.latent <- data$n_w

  switch(type,
         "link" = {
           # linear predictor
           if (data$n_point_var > 0){
             mu_point <- list()
             point <- list()
             for (i in 1:data$n_point_var){
               mu_point[[i]] <- data$X_point %*% t(fit.samples.pars$beta_p[,i,]) +
                        sapply(1:iters, function(j) eval(parse(text = paste0('fit.samples.pars$Z_',i)))[j,] %*% fit.samples.w[j,,1:data$n_point])
               point[[i]] <- apply(mu_point[[i]], 1, median)
             }
           }

           if (data$n_area_var > 0){
             mu_area <- list()
             area <- list()
             fit.samples.wA <- rstan::extract(model, pars = "wA")$wA
             for (i in 1:data$n_area_var){
               mu_area[[i]] <- data$X_area %*% t(fit.samples.pars$beta_a[,i,]) + t(fit.samples.wA[,1,])
               area[[i]] <- apply(mu_area[[i]], 1, median)
             }
           }

           if (data$n_pp_var > 0){
             mu_pp <- list()
             pp <- list()
             for (i in 1:data$n_pp_var){
               mu_pp[[i]] <- exp(log(data$area) + log(data$offset[,i]) + sapply(1:iters, function(j)
                 eval(parse(text = paste0('fit.samples.pars$Z_',i + data$n_point_var + data$n_area_var)))[j,] %*% fit.samples.w[j,,(data$n_point + 1):(data$n_point + data$n_grid)]))
               pp[[i]] <- apply(mu_pp[[i]], 1, median)
             }
           }
         },
         "latent" = {
           if (data$n_area_var > 0){
             fit.samples.wa <- rstan::extract(model, pars = "wa")$wa
             fit.samples.ws <- array(c(fit.samples.w, fit.samples.wa),
                                     dim = c(dim(fit.samples.w)[1:2], dim(fit.samples.w)[3] + dim(fit.samples.wa)[3]))
             # fit.samples.ws <- abind::abind(fit.samples.w, fit.samples.wa, along = 3)
           } else {
             fit.samples.ws <- fit.samples.w
           }

           latent <- list()
           for (i in 1:n.latent){
             latent[[i]] <- data.frame(data$locs, latent = apply(fit.samples.ws[,i,], 2, median))
           }

         },
         "summary" = {
           if (data$n_point_var > 0){
             mu_point <- list()
             point <- list()
             for (i in 1:data$n_point_var){
               mu_point[[i]] <- data$X_point %*% t(fit.samples.pars$beta_p[,i,]) +
                 sapply(1:iters, function(j) eval(parse(text = paste0('fit.samples.pars$Z_',i)))[j,] %*% fit.samples.w[j,,1:data$n_point])
               if (dists[i] == "poisson"){
                 mu_point[[i]] <- exp(mu_point[[i]])
               } else if (dists[i] == "bernoulli"){
                 mu_point[[i]] <- exp(mu_point[[i]])/(1 + exp(mu_point[[i]]))
               }
               point[[i]] <- do.call(rbind, apply(mu_point[[i]], 1, summaries))
               rownames(point[[i]]) <- 1:data$n_point
             }
           }

           if (data$n_area_var > 0){
             mu_area <- list()
             area <- list()
             fit.samples.wA <- rstan::extract(model, pars = "wA")$wA
             for (i in 1:data$n_area_var){
               mu_area[[i]] <- data$X_area %*% t(fit.samples.pars$beta_a[,i,]) + t(fit.samples.wA[,1,])
               if (dists[i + data$n_point_var] == "poisson"){
                 mu_area[[i]] <- exp(mu_area[[i]])
               } else if (dists[i + data$n_point_var] == "bernoulli"){
                 mu_area[[i]] <- exp(mu_area[[i]])/(1 + exp(mu_area[[i]]))
               }
               area[[i]] <- do.call(rbind, apply(mu_area[[i]], 1, summaries))
               rownames(area[[i]]) <- 1:data$n_area
             }
           }

           if (data$n_pp_var > 0){
             mu_pp <- list()
             pp <- list()
             for (i in 1:data$n_pp_var){
               mu_pp[[i]] <- exp(log(data$area) + log(data$offset[,i]) + sapply(1:iters, function(j)
                 eval(parse(text = paste0('fit.samples.pars$Z_',i + data$n_point_var + data$n_area_var)))[j,] %*% fit.samples.w[j,,(data$n_point + 1):(data$n_point + data$n_grid)]))
               pp[[i]] <- do.call(rbind, apply(mu_pp[[i]], 1, summaries))
               rownames(pp[[i]]) <- 1:data$n_grid
             }
           }
         },
         "full" = {
           if (data$n_point_var > 0){
             point <- list()
             for (i in 1:data$n_point_var){
               point[[i]] <- data$X_point %*% t(fit.samples.pars$beta_p[,i,]) +
                 sapply(1:iters, function(j) eval(parse(text = paste0('fit.samples.pars$Z_',i)))[j,] %*% fit.samples.w[j,,1:data$n_point])
               if (dists[i] == "poisson"){
                 point[[i]] <- exp(point[[i]])
               } else if (dists[i] == "bernoulli"){
                 point[[i]] <- exp(point[[i]])/(1 + exp(point[[i]]))
               }
             }
           }

           if (data$n_area_var > 0){
             area <- list()
             fit.samples.wA <- rstan::extract(model, pars = "wA")$wA
             for (i in 1:data$n_area_var){
               area[[i]] <- data$X_area %*% t(fit.samples.pars$beta_a[,i,]) + t(fit.samples.wA[,1,])
               if (dists[i + data$n_point_var] == "poisson"){
                 area[[i]] <- exp(area[[i]])
               } else if (dists[i + data$n_point_var] == "bernoulli"){
                 area[[i]] <- exp(area[[i]])/(1 + exp(area[[i]]))
               }
             }
           }

           if (data$n_pp_var > 0){
             pp <- list()
             for (i in 1:data$n_pp_var){
               pp[[i]] <- exp(log(data$area) + log(data$offset[,i]) + sapply(1:iters, function(j)
                 eval(parse(text = paste0('fit.samples.pars$Z_',i + data$n_point_var + data$n_area_var)))[j,] %*% fit.samples.w[j,,(data$n_point + 1):(data$n_point + data$n_grid)]))
             }
           }
         },
         stop("type must be one of c(link, latent, summary, full)"))

  switch(type,
         "link" = , "summary" = , "full" = {
           out <- unlist(list(if(data$n_point_var > 0) point,
                       if(data$n_area_var > 0) area,
                       if(data$n_pp_var > 0) pp), recursive = FALSE)
           out.names <- c(if(data$n_point_var > 0) paste0("point", 1:data$n_point_var),
                          if(data$n_area_var > 0) paste0("area", 1:data$n_area_var),
                          if(data$n_pp_var > 0) paste0("pp", 1:data$n_pp_var))
         },
         "latent" = {
           out <- latent
           out.names <- paste0("latent", 1:n.latent)
         })

  names(out) <- out.names
  return(out)
}
