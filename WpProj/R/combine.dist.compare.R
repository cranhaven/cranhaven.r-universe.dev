methods::setClass("combine.distcompare")

#' Combine distance calculations from the distCompare function
#'
#' @param ... `distcompare` objects that are the result of [distCompare()]
#'
#' @return an object of class `combine.distcompare`, the combined `distcompare` class objects as returned by [distCompare()] function
#' @keywords internal
# n <- 32
# p <- 10
# s <- 99
# x <- matrix( 1, nrow = n, ncol = p )
# beta <- (1:10)/10
# y <- x %*% beta
# post_beta <- matrix(beta, nrow=p, ncol=s)
# post_mu <- x %*% post_beta
# 
# fit1 <-  WPL1(X=x, Y=post_mu, power = 2.0,
#                penalty = "lasso",
#                method = "projection" #default
# )
# fit2 <-  WPL1(X=x, Y=post_mu, power = 1.0,
#                penalty = "lasso",
#                method = "projection" #default
# )
# dc1 <- distCompare(list(fit1, fit2))
# 
# fit3 <-  WPL1(X=x, Y=post_mu, power = Inf,
#                penalty = "lasso",
#                method = "projection" #default
# )
# fit4 <-  WPL1(X=x, Y=post_mu, power = 3.0,
#                penalty = "lasso",
#                method = "projection" #default
# )
# dc2 <- distCompare(list(fit3, fit4))
# combine <- combine.distcompare(dc1, dc2)
combine.distcompare <- function(...) {
  
  if( (...length() == 1) && is.list(...) && !is.distcompare(...)) {
    distances <- ...elt(1L)
    } else if ( (...length() == 1) && is.distcompare(...)) {
      message("Only one `distcompare` object was passed to the function. Returning original object")
      return(...elt(1L))
    } else {
      distances <- list(...)
    } 
  
  stopifnot(is.list(distances))
  if (!all(sapply(distances, is.distcompare))) {
    stop("All members must be distcompare object")
  }
  niter <- length(distances)
  cmb <- list(parameters = NULL, predictions = NULL, p = NULL)
  
  ps <- sapply(distances, function(d) d$p)
  stopifnot(all(diff(ps)==0))
  
  cmb$p <- ps[1]
  post <- do.call("rbind", lapply(distances, function(d) d$parameters))
  predictions <- do.call("rbind", lapply(distances, function(d) d$predictions))
  # methods <- do.call("rbind", lapply(distances, function(d) d$method))
  
  if(! is.null(post)) {
    cmb$parameters <- post
  }
  
  if (!is.null(predictions)) {
    cmb$predictions <- predictions
  }
  
  class(cmb) <- class(distances[[1L]])
  
  return(cmb)
}


combine_and_augment_distcompare <- function(...) {
  
  distances <- list(...)
  if(is.list(...)) distances <- unlist(distances, recursive = FALSE)
  
  stopifnot(is.list(distances))
  if (!all(sapply(distances, is.distcompare))) {
    stop("All members must be distcompare object")
  }
  niter <- length(distances)
  length.each <- sapply(distances, function(i) nrow(i$predictions))
  cmb <- list(parameters = NULL, predictions = NULL, p = NULL)
  
  ps <- sapply(distances, function(d) d$p)
  stopifnot(all(diff(ps)==0))
  
  ranks.list <- lapply(distances, rank_distcompare)
  ranks.post <- unlist(sapply(ranks.list, function(r) r$parameters$ranks))
  ranks.predictions <- unlist(sapply(ranks.list, function(r) r$predictions$ranks))
  iter <- unlist(sapply(1:niter, function(i) rep(i, length.each[i])))
  
  cmb$p <- ps[1]
  post <- do.call("rbind", lapply(distances, function(d) d$parameters))
  predictions <- do.call("rbind", lapply(distances, function(d) d$predictions))
  # methods <- do.call("rbind", lapply(distances, function(d) d$method))
  
  if(! is.null(post)) {
    cmb$parameters <- post
    cmb$parameters <- cbind(cmb$parameters, ranks = ranks.post, iter = iter)
  }
  
  if (!is.null(predictions)) {
    cmb$predictions <- predictions
    if(!is.null(cmb$predictions)) cmb$predictions <- cbind(cmb$predictions, ranks = ranks.predictions, iter = iter)
  }
  
  class(cmb) <- c("combine_distcompare","WpProj")
  
  return(cmb)
}

setOldClass("combine_distcompare")
plot.combine_distcompare <- function (x, ylim = NULL, ylabs = c(NULL,NULL), facet.group = NULL, ...) {
  distances <- x
  stopifnot(inherits(distances, "combine_distcompare"))
  dots <- list(...)
  alpha <- dots$alpha
  base_size <- dots$base_size
  
  CI <- dots$CI
  if(is.null(CI)) CI <- "none"
  CI <- match.arg(CI, c("none","ribbon","bar"))
  xlab <- dots$xlab
  leg.pos <- dots$legend.position
  if(is.null(alpha)) alpha <- 0.3
  if(is.null(base_size)) base_size <- 11
  if(is.null(xlab)) xlab <- "Number of active coefficients"
  if(is.null(leg.pos)) leg.pos <- NULL
  
  # methods <- levels(distances$predictions$groups)
  d <- max(distances$predictions$nactive)
  numactive <- 1:d
  
  # df <- data.frame(numactive = numactive, method = rep(methods, each = d))
  
  ppost <- pmean <- NULL
  
  # avoid cmd check errors
  dist <- nactive <- groups <- low <- hi <- NULL
  
  if ( !is.null(distances$parameters) ) {
    dd <- distances$parameters
    
    dd$groups <- factor(dd$groups)
    if(!is.null(facet.group)) {
      grouping <- c("groups", facet.group, "nactive")
    } else {
      grouping <- c("groups", "nactive")
    }
    df <- dd %>% dplyr::group_by(.dots = grouping) %>% dplyr::summarise(
                                                    low = stats::quantile(dist, 0.025),
                                                    hi = stats::quantile(dist, 0.975),
                                                    dist = mean(dist)
                                                    )
    # E <- tapply(dd$dist, INDEX = grouping, mean)
    # sigma <- tapply(dd$dist, INDEX = grouping, sd)
    # 
    # M <- tapply(dd$dist, INDEX = grouping, median)
    # hi <- tapply(dd$dist, INDEX = grouping, quantile, 0.975)
    # low <- tapply(dd$dist, INDEX = grouping, quantile, 0.025)
    
    # df <- data.frame(dist = c(M), low = c(low), hi = c(hi), 
    #                  groups = rep(colnames(M), each = nrow(M)),
    #                  nactive = as.numeric(rep(rownames(M), ncol(M))),
    #                  row.names = NULL)
    # df <- df[complete.cases(df),]
    
    ylim_post <- set_y_limits(df, ylim, "parameters")
    ppost <- ggplot2::ggplot( df, 
                              ggplot2::aes(x=nactive, y=dist, 
                                           color = groups, fill = groups,
                                           group=groups ))
    if(CI == "ribbon") {
      ppost <- ppost + ggplot2::geom_ribbon(ggplot2::aes(ymin = low, ymax = hi), alpha = alpha, linetype=0)
    } else if (CI == "bar") {
      ppost <- ppost + ggplot2::geom_errorbar(ggplot2::aes(ymin = low, ymax = hi), alpha = alpha, position = ggplot2::position_dodge(width=0.25))
    }
    ppost <- ppost + ggplot2::geom_line(position = ggplot2::position_dodge(width=0.25)) +
      ggplot2::geom_point(position = ggplot2::position_dodge(width=0.25)) +
      ggsci::scale_color_jama() + 
      ggsci::scale_fill_jama() +
      ggplot2::labs(fill ="Method", color="Method") +
      ggplot2::xlab(xlab) + 
      ggplot2::ylab(ylabs[1]) + ggplot2::theme_bw(base_size) +
      ggplot2::scale_x_continuous(expand = c(0, 0)) +
      ggplot2::scale_y_continuous(expand = c(0, 0), limits = ylim_post ) + 
      ggplot2::theme(legend.position = leg.pos)
    if(!is.null(facet.group)) {
      ppost <- ppost + ggplot2::facet_grid(stats::reformulate(facet.group))
    }
  }
  
  if (!is.null(distances$predictions)){
    dd <- distances$predictions
    dd$groups <- factor(dd$groups)
    if(!is.null(facet.group)) {
      grouping <- c("groups", facet.group, "nactive")
    } else {
      grouping <- c("groups", "nactive")
    }
    df <- dd %>% dplyr::group_by(.dots = grouping) %>% dplyr::summarise(
      low = stats::quantile(dist, 0.025),
      hi = stats::quantile(dist, 0.975),
      dist = mean(dist)
    )
    # E <- tapply(dd$dist, INDEX = list(dd$nactive, dd$groups), mean)
    # sigma <- tapply(dd$dist, INDEX = list(dd$nactive, dd$groups), sd)
    # 
    # M <- tapply(dd$dist, INDEX = list(dd$nactive, dd$groups), median)
    # hi <- tapply(dd$dist, INDEX = list(dd$nactive, dd$groups), quantile, 0.975)
    # low <- tapply(dd$dist, INDEX = list(dd$nactive, dd$groups), quantile, 0.025)
    # 
    # df <- data.frame(dist = c(M), low = c(low), hi = c(hi), 
    #                  groups = rep(colnames(M), each = nrow(M)),
    #                  nactive = as.numeric(rep(rownames(M), ncol(M))),
    #                  row.names = NULL)
    
    ylim_mean <- set_y_limits(df, ylim, "predictions")
    
    pmean <- ggplot2::ggplot( df, 
                              ggplot2::aes(x=nactive, y=dist, 
                                           color = groups, fill = groups,
                                           group=groups ))
    if(CI == "ribbon") {
      pmean <- pmean + ggplot2::geom_ribbon(ggplot2::aes(ymin = low, ymax = hi), alpha = alpha, linetype=0)
    } else if (CI == "bar") {
      pmean <- pmean + ggplot2::geom_errorbar(ggplot2::aes(ymin = low, ymax = hi), alpha = alpha, position = ggplot2::position_dodge(width=0.25))
    }
    pmean <- pmean + ggplot2::geom_line(position = ggplot2::position_dodge(width=0.25)) +
      ggplot2::geom_point(position = ggplot2::position_dodge(width=0.25)) +
      ggsci::scale_color_jama() + 
      ggsci::scale_fill_jama() +
      ggplot2::labs(fill ="Method", color="Method") +
      ggplot2::xlab(xlab) + 
      ggplot2::ylab(ylabs[1]) + ggplot2::theme_bw(base_size) +
      ggplot2::scale_x_continuous(expand = c(0, 0)) +
      ggplot2::scale_y_continuous(expand = c(0, 0), limits = ylim_mean ) + 
      ggplot2::theme(legend.position = leg.pos)
    if(!is.null(facet.group)) {
      pmean <- pmean + ggplot2::facet_grid(stats::reformulate(facet.group))
    }
  }
  
  plots <- list(parameters = ppost, predictions = pmean)
  class(plots) <- c("plotcombine","WpProj")
  return(plots)
}

methods::setClass("plotcombine")
print.plotcombine <- function(x) {
  for(i in 1:length(x)) {
    if(is.null(x[[i]])) next
    print(x[[i]])
  }
}


#' Plot the Rankings of the 'combine.distcompare' Objects
#'
#' @param distances A `combine.distcompare` object resulting from the [combine.distcompare()] function
#' @param ylim y-axis limits
#' @param ylabs y-axis labels
#' @param ... additional plot arguments like alpha parameter in ggplot2
#'
#' @return object of class `plotrank` which is a list with slots "parameters" and "predictions"
#' @keywords internal
# n <- 32
# p <- 10
# s <- 99
# x <- matrix( 1, nrow = n, ncol = p )
# beta <- (1:10)/10
# y <- x %*% beta
# post_beta <- matrix(beta, nrow=p, ncol=s)
# post_mu <- x %*% post_beta
# 
# fit1 <-  WPL1(X=x, Y=post_mu, power = 2.0,
#                penalty = "lasso",
#                method = "projection" #default
# )
# fit2 <-  WPL1(X=x, Y=post_mu, power = 1.0,
#                penalty = "lasso",
#                method = "projection" #default
# )
# dc1 <- distCompare(list(fit1, fit2))
# 
# fit3 <-  WPL1(X=x, Y=post_mu, power = Inf,
#                penalty = "lasso",
#                method = "projection" #default
# )
# fit4 <-  WPL1(X=x, Y=post_mu, power = 3.0,
#                penalty = "lasso",
#                method = "projection" #default
# )
# dc2 <- distCompare(list(fit3, fit4))
# combine <- combine.distcompare(list(dc1, dc2))
# pr <- plot_ranks(combine)
# print(pr)
plot_ranks <- function(distances, ylim = NULL, ylabs = c(NULL,NULL), ...) {
  stopifnot(inherits(distances, "combine.distcompare"))
  dots <- list(...)
  alpha <- dots$alpha
  if(is.null(alpha)) alpha <- 0.1
  ppost <- pmean <- NULL
  
  countfun <- function(x) {
    tab <- table(x)
    n <- sum(tab)
    return(tab/n)
  }
  
  if ( !is.null(distances$parameters) ) {
    dd <- distances$parameters
    index <- list(dd$nactive, dd$groups)
    
    M <- tapply(dd$ranks, INDEX = index, mean)
    low <- tapply(dd$ranks, INDEX = index, stats::quantile, 0.025)
    hi <- tapply(dd$ranks, INDEX = index, stats::quantile, 0.975)
    
    df <- data.frame(dist = c(M), low = c(low), hi = c(hi), 
                     groups = rep(colnames(M), each = nrow(M)),
                     nactive = as.numeric(rep(rownames(M), ncol(M))),
                     row.names = NULL)
    
    nactive <- dist <- groups <- xlab <- NULL
    
    ylim <- set_y_limits(distances, ylim, "parameters")
    ppost <- ggplot2::ggplot( df, 
                              ggplot2::aes(x=nactive, y=dist, 
                                           color = groups, fill = groups,
                                           group=groups )) +
      ggplot2::geom_line() + 
      ggplot2::geom_ribbon(ggplot2::aes(ymin = low, ymax = hi), alpha = alpha, linetype=0) + 
      ggsci::scale_color_jama() + 
      ggsci::scale_fill_jama() +
      ggplot2::labs(fill ="Method", color="Method") +
      ggplot2::xlab("Num. Coef.") + 
      ggplot2::ylab(ylabs[1]) + ggplot2::theme_bw() +
      ggplot2::scale_x_continuous(expand = c(0, 0)) +
      ggplot2::scale_y_continuous(expand = c(0, 0), limits = ylim )
  }
  
  if (!is.null(distances$predictions)){
    dd <- distances$predictions
    index <- list(dd$nactive, dd$groups)
    
    M <- tapply(dd$ranks, INDEX = index, mean)
    low <- tapply(dd$ranks, INDEX = index, stats::quantile, 0.025)
    hi <- tapply(dd$ranks, INDEX = index, stats::quantile, 0.975)
    
    df <- data.frame(dist = c(M), low = c(low), hi = c(hi), 
                     groups = rep(colnames(M), each = nrow(M)),
                     nactive = as.numeric(rep(rownames(M), ncol(M))),
                     row.names = NULL)
    
    ylim <- set_y_limits(distances, ylim, "predictions")
    pmean <- ggplot2::ggplot( df, 
                              ggplot2::aes(x=nactive, y=dist, 
                                           color = groups, fill = groups,
                                           group=groups )) +
      ggplot2::geom_line() + 
      ggplot2::geom_ribbon(ggplot2::aes(ymin = low, ymax = hi), alpha = alpha, linetype=0) + 
      ggsci::scale_color_jama() + 
      ggsci::scale_fill_jama() +
      ggplot2::labs(fill ="Method", color="Method") +
      ggplot2::xlab("Num. Coef.") + 
      ggplot2::ylab(ylabs[1]) + ggplot2::theme_bw() +
      ggplot2::scale_x_continuous(expand = c(0, 0)) +
      ggplot2::scale_y_continuous(expand = c(0, 0), limits = ylim )
  }
  
  plots <- list(parameters = ppost, predictions = pmean)
  class(plots) <- c("plotrank","WpProj")
  return(plots)
}

methods::setClass("plotrank")

#' @exportS3Method base::print
print.plotrank <- function(x,...) {
  for(i in 1:length(x)) {
    if(is.null(x[[i]])) next
    print(x[[i]])
  }
  return(invisible(NULL))
}

#' Plot 'combine.distcompare' Objects
#'
#' @param x `combine.distcompare` objects resulting from the [combine.distcompare()] function
#' @param ylim y-axis limits
#' @param ylabs y-axis labels
#' @param facet.group groups to facet by
#' @param ... additional plotting parameters like alpha
#'
#' @return An object of class `plotcombine`that is a list of various diagnostic plots of the `WpProj` objects
#' @keywords internal
# @examples
# if(rlang::is_installed("stats")) {
# n <- 128
# p <- 10
# s <- 99
# x <- matrix( stats::rnorm( p * n ), nrow = n, ncol = p )
# beta <- (1:10)/10
# y <- x %*% beta + stats::rnorm(n)
# post_beta <- matrix(beta, nrow=p, ncol=s) + stats::rnorm(p*s, 0, 0.1)
# post_mu <- x %*% post_beta
# 
# fit1 <-  WPL1(X=x, Y=post_mu, power = 2.0,
#                penalty = "lasso",
#                method = "projection" #default
# )
# fit2 <-  WPL1(X=x, Y=post_mu, power = 1.0,
#                penalty = "lasso",
#                method = "projection" #default
# )
# dc1 <- distCompare(list(fit1, fit2))
# 
# fit3 <-  WPL1(X=x, Y=post_mu, power = Inf,
#                penalty = "lasso",
#                method = "projection" #default
# )
# fit4 <-  WPL1(X=x, Y=post_mu, power = 3.0,
#                penalty = "lasso",
#                method = "projection" #default
# )
# dc2 <- distCompare(list(fit3, fit4))
# combine <- combine.distcompare(list(dc1, dc2))
# plot(combine)
# }
methods::setMethod("plot", c("x" ="combine_distcompare"), plot.combine_distcompare)
methods::setMethod("print", c("x" ="plotcombine"), print.plotcombine)
methods::setMethod("print", c("x" ="plotrank"), print.plotrank)
