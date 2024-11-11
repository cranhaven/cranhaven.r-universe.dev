methods::setClass("distcompare",
         list(parameters = "data.frame", 
              predictions = "data.frame", 
              p = "numeric"))

#' Compares Optimal Transport Distances Between WpProj and Original Models
#'
#' @param models A list of models from WpProj methods
#' @param target The target to compare the methods to. Should be a list with slots "parameters" to compare the parameters and "predictions" to compare predictions
#' @param power The power parameter of the Wasserstein distance.
#' @param method Which approximation to the Wasserstein distance to use. Should be one of the outputs of [transport_options()].
#' @param quantity Should the function target the "parameters" or the "predictions". Can choose both.
#' @param parallel Parallel backend to use for the `foreach` package. See `foreach::registerDoParallel(` for more details.
#' @param transform Transformation function for the predictions.
#' @param ... other options passed to the [wasserstein()] distance function
#'
#' @return an object of class `distcompare` with slots `parameters`, `predictions`, and `p`. The slots `parameters` and `predictions` are data frames. See the details for more info. The slot `p` is the power parameter of the Wasserstein distance used in the distance calculation. 
#' 
#' @details 
#' For the data frames, `dist` is the Wasserstein distance, `nactive` is the number of active variables in the model, `groups` is the name distinguishing the model, and `method` is the method used to calculate the distance (i.e., exact, sinkhorn, etc.). If the list in `models` is named, these will be used as the group names otherwise the group names will be created based on the call from the `WpProj` method.
#' 
#' @export
#' 
#' @description 
#' `r lifecycle::badge("experimental")`
#' Will compare the Wasserstein distance between the original model and the `WpProj` model.
#' 
#' @examples
#' if(rlang::is_installed("stats")) {
#' n <- 32
#' p <- 10
#' s <- 21
#' x <- matrix( stats::rnorm( p * n ), nrow = n, ncol = p )
#' beta <- (1:10)/10
#' y <- x %*% beta + stats::rnorm(n)
#' post_beta <- matrix(beta, nrow=p, ncol=s) + stats::rnorm(p*s, 0, 0.1)
#' post_mu <- x %*% post_beta
#' 
#' fit1 <-  WpProj(X=x, eta=post_mu, power = 2.0,
#'                options = list(penalty = "lasso")
#' )
#' fit2 <-  WpProj(X=x, eta=post_mu, theta = post_beta, power = 2.0,
#'                method = "binary program", solver = "lasso",
#'                options = list(solver.options = list(penalty = "mcp"))
#' )
#' dc <- distCompare(models = list("L1" = fit1, "BP" = fit2),
#'                  target = list(parameters = post_beta, predictions = post_mu))
#' plot(dc)
#' }
distCompare <- function(models, target = list(parameters = NULL, predictions = NULL), power = 2, 
                         method = "exact", 
                         quantity = c("parameters","predictions"),
                         parallel=NULL, transform = function(x){return(x)}, ...) 
{
  method <- match.arg(method, c("mse", transport_options()), 
                      several.ok = TRUE)
  quantity <- match.arg(quantity, several.ok = TRUE)
  
  if ( !is.null(parallel) ) {
      if ( !inherits(parallel, "cluster") ) {
        stop("parallel must be a registered cluster backend or null")
      }
      doParallel::registerDoParallel(parallel)
    
    # display.progress <- FALSE
  } else {
    foreach::registerDoSEQ()
  }
  
  if (inherits(models, "WpProj") ) {
    models <- list(models)
  } else {
    stopifnot(all(sapply(models, inherits, "WpProj")))
  }
  
  p <- ground_p <- power
  
  dist_df <- dist_mu_df <- nactive <- groups <- plot <- plot_mu <- NULL
  
  nzero <- lapply(models, function(mm) mm$nzero)
  
  data <- set_dist_data(target, models, quantity, method, transform)
  group_names <- data$group_names
  n <- data$n
  d <- data$d
  s <- data$s
  
  if ("parameters" %in% quantity){
    parameters <- data$parameters
    dist_list <- mapply(function(mc, proj){
        dist_fun(mc, mu = parameters$target, p = p, ground_p = ground_p, 
                 method = parameters$method, observation.orientation = parameters$obs.direction,
                 projection = proj, ...) }, mc = parameters$source, proj = parameters$projection,
        SIMPLIFY = FALSE)
    
    dist <- unlist(dist_list)
    nactive <- unlist(nzero)
    groups <- mapply(function(x,z){return(rep(x, each=z))}, 
                     x = group_names, z = sapply(dist_list, length))
    
    if(parameters$isMSE)
    {
      dist <- (dist^2)/d
      parameters$method <- "mse"
    }    
    dist_df <- data.frame(dist = dist,
                          nactive = unlist(nactive),
                          groups=factor(unlist(groups)),
                          method = parameters$method)
  }
  
  if ("predictions" %in% quantity){
    
    mu <- data$predictions
    dist_list_mu <- mapply(function(mc, proj){
      dist_fun(mc, mu = mu$target, p = p, ground_p = ground_p,
               method = mu$method, observation.orientation = mu$obs.direction,
               projection = proj, ...)}, mc = mu$source, proj = mu$projection,
      SIMPLIFY = FALSE)
    
    dist_mu <- unlist(dist_list_mu)
    if (is.null(nactive)) nactive <- unlist(nzero)
    
    # if (is.null(groups)) groups <- sapply(group_names, function(g) rep(g, nrow(dist_list_mu)))
    if (is.null(groups)) groups <- mapply(function(x,z){return(rep(x, each=z))}, 
                                          x = group_names, z = sapply(dist_list_mu, length))
    
    if( mu$isMSE )
    {
      dist_mu <- (dist_mu^2)/n
      mu$method <- "mse"
    }   

    dist_mu_df <- data.frame(dist = dist_mu,
                             nactive = unlist(nactive),
                             groups=factor(unlist(groups)),
                             method = mu$method)
    
  }
  
  # if (parallel) parallel::stopCluster(cl)
  output <- list(parameters = dist_df, predictions = dist_mu_df, p = p)
  class(output) <- c("distcompare","WpProj")
  
  return(output)
}

is.distcompare <- function(x) inherits(x, "distcompare")

dist_fun <- function(mulist, mu, p, ground_p, method, observation.orientation, projection, ...) {
  m <- NULL
  dist <-
    foreach::foreach(m=mulist, .combine = c) %dopar% {
        wp <- if(projection & method == "exact" & p == 2) {
          denom <- max(ncol(mu), ncol(m))
          sqrt(sum((c(m) - c(mu))^2)/denom)
        } else {
          WpProj::wasserstein(X = m, Y = mu, p = p, 
                                       ground_p = ground_p, 
                                       observation.orientation = observation.orientation, 
                                       method = method, ...)
        }
        return(wp)
      }
  return(dist)
}

set_dist_data <- function(target, models, quantity, method, transform) {
  
  post_targ <- mean_targ <- NULL
  if (length(quantity) > 2) {
    quantity <- sort(unique(quantity), decreasing=TRUE)
    warning("Arbitrarily shortening quantity length. Length should be less than or equal to 2.")
  }
  isMSE <- rep(FALSE, 2)
  
  ### method checks ###
  if (length(quantity) == 1 &  1 < length(method)) {
    if ( !is.null(names(method)) ) {
      method <- method[order(names(method), decreasing=TRUE)]
    } 
    if("parameters" %in% quantity) {
      meth.rm <- 2L
    } else if ("predictions" %in% quantity) {
      meth.rm <- 1L
    }
    method[meth.rm] <- NA
    warning("1 == length(quantity) < length(method). Arbitrarily shortening method length to agree with quantity length.")
  } else if (length(method) < length(quantity)) {
    method <- rep(method, length(quantity))
  }
  if (length(method) != 2) {
    method <- rep(unique(method), 2)
  }
  if ( length(method ) > 2 ) {
    method <- method[1:2]
  }
  ### Target checks ###
  if (!is.null(target) ) {
    if (length(quantity) > 1 & (!is.list(target) | (is.list(target) & length(target) ==1) ) ) {
      stop("For more than one quantity, target should be a list of length 2. The list should either be named with slots 'parameters' and 'predictions' OR the parameters target should be in the first slot and the predictions target in the second.")
    }
    if (!is.list(target)) {
      target <- list(target)
    }
    if(is.null(names(target))) names(target) <- sort(quantity, decreasing = TRUE) 
    if ( any (names(target) != quantity) ) {
      target <- target[sort(quantity, decreasing = TRUE) ]
    }
    if ("parameters" %in% quantity) {
      post_targ <- target$parameters
    }
    if ("predictions" %in% quantity) {
      mean_targ <- target$predictions
    }
  } else {
    target <- list(parameters = NULL, predictions = NULL)
  }
  
  if(any(sapply(target, is.null)) ){
    avail.models <- unlist(sapply(models, function(x) x$method))
    if (!("binary program" %in% avail.models)) {
      stop("No way of setting target(s). Must either provide a named list or one of the methods should be the 'selection.variable' method since it preserves the original parameters.")
    } else {
      get_idx <- which(avail.models == "binary program")
      if(length(get_idx) > 1) {
        warning("Multiple models with same method 'binary program'")
        get_idx <- get_idx[1]
      }
    }
    selMod <- models[[get_idx]]
    last <- length(selMod$nzero)
    if (is.null(target$parameters)  & "parameters" %in% quantity) {
      post_targ <- selMod$theta[[last]]
    }
    if (is.null(target$predictions) & "predictions" %in% quantity) {
      mean_targ <- transform(selMod$fitted.values[[last]])
    }
  }
  
  ### mse checks ###
  if (any(method == "mse") ) {
    mse.sel <- which(method == "mse")
    method[mse.sel] <- "exact"
    isMSE[mse.sel] <- TRUE
  }
  
  ### observation direction checks ###
  # obs.direction <- ifelse(grepl("univariate", method),"rowwise","colwise")
  obs.direction <- rep("colwise", length(method))
  
  
  ### model checks ###
  if (inherits(models, "WpProj")) {
    models <- list(models)
  } else {
    if(!is.list(models)) stop("models must be a WpProj fit or a list of fits")
  }
  
  ### set group names ###
  group_names <- names(models)
  if (is.null(group_names)) group_names <- seq.int(length(models))
  
  ### get dimensions ###
  n <- dim(models[[1]]$fitted.values[[1]])[1]
  d <- dim(models[[1]]$theta[[1]])[1]
  s <- dim(models[[1]]$theta[[1]])[2]
  
  ### set parameters data if present ###
  if ("parameters" %in% quantity) {
    theta <- lapply(models, function(mm) mm$theta)
    if(!is.matrix(post_targ)) post_targ <- as.matrix(post_targ)
    if(nrow(post_targ) != d) post_targ <- t(post_targ)
    if(nrow(post_targ) != d) stop("Number of parameters in parameters target isn't equal to the number of parameters in theta")
    
    if(ncol(post_targ) == 1 & method[1] != "exact") {
      method[1] <- "exact"
      obs.direction[1] <- "colwise"
      warning("parameters target only has 1 observation. Changing to exact method which will be fast in this case.")
    }
    
    parameters <- list(source = theta,
                      target = post_targ,
                      method = method[1],
                      obs.direction = obs.direction[1],
                      isMSE = isMSE[1],
                      projection = rep(FALSE, length(theta))
    )
  } else {
    parameters <- NULL
  }
  
  ### set predictions data if present ###
  if ("predictions" %in% quantity) {
    eta <- lapply(models, function(mm) lapply(mm$fitted.values, transform))
    
    if(!is.matrix(mean_targ)) mean_targ <- as.matrix(mean_targ)
    if(any(dim(mean_targ) %in% c(n,s))) {
      if(nrow(mean_targ) == s) mean_targ <- t(mean_targ)
    }
    if( nrow (mean_targ) != n) stop("Number of obsersvations of predictions target not equal to number of observations in sample")
    if(ncol(mean_targ) == 1 & method[2] != "exact") {
      method[2] <- "exact"
      obs.direction[2] <- "colwise"
      warning("predictions target only has 1 observation. Changing to exact method which will be fast in this case")
    }
    
    predictions <- list( source = eta,
                  target = mean_targ,
                  method = method[2],
                  obs.direction = obs.direction[2],
                  isMSE = isMSE[2],
                  projection = ifelse(sapply(models, function(mm) mm$method) == "projection",TRUE, FALSE)
    )
  } else {
    predictions <- NULL
  }
  
  ### set output matrix ###
  output <- list( parameters = parameters,
                  predictions = predictions,
                  models = models,
                  quantity = quantity,
                  n = n, d = d, s = s,
                  group_names = group_names
                )
  
  return(output)
}

set_equal_y_limits.distcompare <- function(x){
  distance_data <- x
  dist <- ylim <- list(parameters = NULL, predictions = NULL)
  for(i in c("parameters", "predictions")){
    dist[[i]] <- list(dist = unlist(sapply(distance_data, function(x) x[[i]]$dist)))
    ylim[[i]] <- set_y_limits(dist, ylim[[i]], i)
  }
  return(ylim)
}

#' Ranks `distcompare` Objects
#'
#' @param distances A `distcompare` object
#'
#' @keywords internal
#'
#' @return The ranks of a `distcompare` object as a list containing slots "predictions" and "parameters".
rank_distcompare <- function(distances) {
  if(!is.distcompare(distances)) stop("Must be distcompare object")
  rank.fun <- function(distance, quant) {
    dist <- distance[[quant]]$dist
    idx <- distance[[quant]]$nactive
    names(dist) <- distance[[quant]]$groups
    ranks <- tapply(dist, idx, rank, ties.method = "min")
    nactive <- unlist(mapply(FUN = function(n,r) {return(rep(n, r))}, 
                             n = names(ranks), r = sapply(ranks, length)))
    groups <- unlist(sapply(ranks, names))
    if(is.null(names(groups)) & length(levels(distance[[quant]]$groups)) == 1) groups <- levels(distance[[quant]]$groups)
    return( data.frame(ranks = unlist(ranks), nactive=nactive, groups = groups) )
  }
  rp.df <- rm.df <- NULL
  
  if ( !is.null(distances$parameters) ) {
    rp.df <- rank.fun(distances, "parameters")
  }
  
  if ( !is.null(distances$predictions) ) {
    rm.df <- rank.fun(distances, "predictions")
  }
  
  return(list(parameters = rp.df, predictions = rm.df))
}
