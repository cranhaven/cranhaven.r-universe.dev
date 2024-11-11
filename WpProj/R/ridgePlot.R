#' Ridge Plots for a Range of Coefficients
#'
#' @param fit A `WpProj` object or list of `WpProj` objects
#' @param index The observation number to select. Can be a vector
#' @param minCoef The minimum number of coefficients to use
#' @param maxCoef The maximum number of coefficients to use
#' @param scale How the densities should be scale
#' @param alpha Alpha term from ggplot2 object
#' @param full "True" prediction to compare to
#' @param transform transform for predictions
#' @param xlab x-axis label
#' @param bandwidth Bandwidth for kernel
#'
#' @return a [ggplot2::ggplot()] plot
#' @export
#' 
#' @description 
#' `r lifecycle::badge("experimental")`
#' This function will plot the distribution of predictions for a range of active coefficients
#' 
#' @examples
#' if(rlang::is_installed("stats")) {
#' n <- 128
#' p <- 10
#' s <- 99
#' x <- matrix(stats::rnorm(n*p), nrow = n, ncol = p )
#' beta <- (1:10)/10
#' y <- x %*% beta + stats::rnorm(n)
#' post_beta <- matrix(beta, nrow=p, ncol=s) + matrix(stats::rnorm(p*s, 0, 0.1), p, s)
#' post_mu <- x %*% post_beta
#' fit <-  WpProj(X=x, eta=post_mu, 
#'              power = 2
#' )
#' ridgePlot(fit)
#' }
ridgePlot <- function(fit, index = 1, minCoef = 1,maxCoef = 10, 
                      scale = 1, alpha = 0.5, full = NULL, 
                      transform = function(x){x}, xlab = "Predictions",
                      bandwidth = NULL) {
  
  idx <- index
  conditions <- list(idx = idx,
                     maxCoef = maxCoef,
                     minCoef = minCoef,
                     transform = transform)
  if(length(idx) > 1) {
    ridgeplot <- lapply(idx, function(i)
      ridgePlot(fit,i, minCoef, maxCoef, scale, alpha, full, transform, xlab)
    )
  } else {
    if ( inherits(fit, "WpProj") ) {
      # n <- nrow(fit$eta[[1]])
      # s <- ncol(fit$eta[[1]])
      # whichModel <- which(fit$nzero <= maxCoef & fit$nzero>= minCoef)
      # ncoef <- rep(fit$nzero[whichModel], each = s)
      # eta <- lapply(fit$eta[whichModel], function(ee) transform(ee[idx,,drop=FALSE]))
      # 
      # 
      # df_ridge <- data.frame(value = unlist(eta),
      #                        ncoef = factor(ncoef))
      processed <- ridgeData(fit,conditions)
      n <- processed$n
      s <- processed$s
      df_ridge <- processed$df
    }
    else if (is.list(fit) & is.list(fit[[1]]) & inherits(fit[[1]][[1]],"WpProj")) {
      if (is.null(names(fit)) & length(fit) > 1) names(fit) <- paste0("Group ", 1:length(fit))
      if (!(all(sapply(unlist(fit, recursive = FALSE), function(f) inherits(f, "WpProj"))))) {
        stop("Must be a fitted model from the 'WpProj' package")
      }
      dfs <- vector("list", length(unlist(fit, recursive = FALSE)))
      count <- 1
      for( i in names(fit)) {
        for(j in names(fit[[i]])) {
          dfs[[count]] <- ridgeData(fit[[i]][[j]],conditions, method = j, group = i)$df
          count <- count + 1
        }
      }
      processed <- ridgeData(fit[[i]][[j]],conditions, method = j, group = i)
      n <- processed$n
      s <- processed$s
      df_ridge <- do.call("rbind", dfs)
    } else {
      if (is.null(names(fit)) & length(fit) > 1) names(fit) <- paste0("Model ", 1:length(fit))
      if (!(all(sapply(fit, function(f) inherits(f, "WpProj"))))) {
        stop("Must be a fitted model from the 'WpProj' package")
      }
      # if ( length(idx) > 1) stop("Can only do ridgeline plots for one observation at a time")
      processed <- lapply(names(fit), function(f) ridgeData(fit[[f]],conditions, method = f))
      names(processed) <- names(fit)
      n <- processed[[1]]$n
      s <- processed[[1]]$s
      dfs <- lapply(processed, function(l) l$df)
      # for(i in seq_along(dfs)) dfs[[i]]$Method <- names(dfs)[[i]]
      df_ridge <- do.call("rbind", dfs)
      
      # n <- nrow(fit[[1]]$eta[[1]])
      # s <- ncol(fit[[1]]$eta[[1]])
      # 
      # whichModel <- lapply(fit, function(nn) which(nn$nzero <= maxCoef & nn$nzero>= minCoef))
      # ncoef <- mapply(function(f,wm) {rep(f$nzero[wm], each=s)},
      #                 f = fit, wm = whichModel)
      # 
      # eta <- mapply(function(f,wm) {lapply(f$eta[wm],
      #                                      function(e) transform(e[idx,,drop=FALSE]))},
      #               f = fit, wm = whichModel, SIMPLIFY=FALSE)
      # 
      # df_ridge <- data.frame(value = unlist(eta), ncoef = factor(unlist(ncoef)))
      # df_ridge$Method <- factor(unlist(mapply(function(n,l) {rep(n,each=l*s)}, n = names(fit), l = sapply(eta, length))))
    }
    
    if(!is.null(full)) {
      if(is.matrix(full)) {
        if(nrow(full)>1) {
          if(nrow(full) != n) stop("'full' must be a vector of predictions for correct observation or matrix of predictions for every observation")
          if(ncol(full) != s) stop("'full' must have the same number of predictions per observation as are found in the coarse posterior version")
          full <- transform(full[idx,])
        }
      } else {
        if(length(full) != s) stop("'full' must have the same number of predictions as found in the coarse posterior version")
      }
      
      # ncoef_vec <- c(as.character(df_ridge$ncoef ), rep("Full", s))
      # ncoef_lev <- c(levels(df_ridge$ncoef), "Full")
      #
      # method_vec <- c(as.character(df_ridge$Method ), rep("Full", s))
      # method_lev <- c(levels(df_ridge$Method), "Full")
      
      ncoef_vec <- rep("Full", s)
      ncoef_lev <- "Full"
      
      method_vec <- rep("Full", s)
      method_lev <- "Full"
      full_df <- data.frame(value = c(full),
                            ncoef = factor(ncoef_vec, levels=ncoef_lev),
                            Method = factor(method_vec, levels=method_lev))
      if("Group" %in% colnames(df_ridge)){
        groups <- unique(df_ridge$Group)
        full_list <- lapply(groups, function(g) cbind(full_df, Group = factor(g)))
        full_df <- do.call("rbind", full_list)
      }
      df_ridge <- rbind(df_ridge, full_df)
    }
    
    # initialize ridgeplot
    ncoef <- value <- Method <- NULL
    ridgeplot <- ggplot2::ggplot(df_ridge, ggplot2::aes(y = factor(ncoef)))
    
    #comparison chekcs
    if(length(fit) > 1) {
      ridgeplot <- ridgeplot + ggridges::geom_density_ridges(ggplot2::aes(x = value, fill=Method),
                                                             bandwidth = bandwidth, scale=scale, alpha=alpha,
                                                             panel_scaling = FALSE)
      if("Group" %in% colnames(df_ridge)){
        ridgeplot <- ridgeplot + ggplot2::facet_wrap(~Group)
      }
    } else { # if single fit
      ridgeplot <- ridgeplot + ggridges::geom_density_ridges(ggplot2::aes(x = value),  bandwidth = bandwidth, scale=scale, alpha=alpha)
    }
    
    #set themes and labels
    ridgeplot <- ridgeplot +
      ggridges::theme_ridges() +
      ggplot2::ylab("Number of Active Coefficients") +
      ggplot2::xlab(xlab)
    
    # remove full from legend
    if (!inherits(fit, "WpProj")) {
      levs <- levels(df_ridge$Method)
      levs <- levs[levs != "Full"]
      cols <- c(ggsci::pal_jama("default")(length(levs)), "#e41a1c")
      ridgeplot <- ridgeplot +
        ggplot2::scale_fill_manual(breaks=levs, values=cols)
    } else {
      ridgeplot <- ridgeplot + ggsci::scale_fill_jama()
    }
  }
  
  return(ridgeplot)
}


ridgeData <- function(fit, conditions, method = NULL, group = NULL) {
  idx <- conditions$idx
  maxCoef <- conditions$maxCoef
  minCoef <- conditions$minCoef
  transform <- conditions$transform
  
  n <- nrow(fit$fitted.values[[1]])
  s <- ncol(fit$fitted.values[[1]])
  whichModel <- which(fit$nzero <= maxCoef & fit$nzero>= minCoef)
  ncoef <- rep(fit$nzero[whichModel], each = s)
  eta <- lapply(fit$fitted.values[whichModel], function(ee) transform(ee[idx,,drop=FALSE]))
  
  
  df_ridge <- data.frame(value = unlist(eta),
                         ncoef = factor(ncoef))
  if(!is.null(method)) df_ridge$Method <- factor(method)
  if(!is.null(group)) df_ridge$Group <- factor(group)
  return(list(df = df_ridge, n = n, s= s))
}
