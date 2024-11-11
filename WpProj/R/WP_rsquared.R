# WPR2 <- function(predictions, projected_model, p = 2, method = "exact",...) {UseMethod("WPR2")}
methods::setClass("WPR2",
         slots = c(r2 = "numeric", 
              nactive = "integer", 
              groups = "factor",
              method = "factor",
              p = "numeric",
              base = "character"),
         contains = "data.frame")

#' \eqn{W_p R^2} Function to Evaluate Performance
#'
#' @param predictions Predictions of interest, likely from the original model
#' @param projected_model A matrix of competing predictions, possibly from a WpProj fit, a WpProj fit itself, or a list of WpProj objects
#' @param p Power of the Wasserstein distance to use in distance calculations
#' @param method Method for calculating Wasserstein distance
#' @param base The baseline result to compare to. If not provided, defaults to the model with no covariates and only an intercept.
#' @param ... Arguments passed to Wasserstein distance calculation. See \code{\link{wasserstein}}
#'
#' @return \eqn{W_p R ^2} values
#' 
#' @description 
#' `r lifecycle::badge("experimental")`
#' This function will calculate p-Wasserstein distances between the predictions of interest and the projected model.
#' 
#' @export
#' 
#' @examples
#' if (rlang::is_installed("stats")) {
#' # this example is not a true posterior estimation, but is used for illustration
#' n <- 32
#' p <- 10
#' s <- 21
#' x <- matrix( stats::rnorm(n*p), nrow = n, ncol = p )
#' beta <- (1:10)/10
#' y <- x %*% beta + stats::rnorm(n)
#' post_beta <- matrix(beta, nrow=p, ncol=s) + 
#'     matrix(rnorm(p*s), p, s) # not a true posterior
#' post_mu <- x %*% post_beta
#' 
#' fit <-  WpProj(X=x, eta=post_mu, power = 2.0)
#' 
#' out <- WPR2(predictions = post_mu, projected_model = fit, 
#' base = rowMeans(post_mu) # same as intercept only projection
#' )
#' }
methods::setGeneric("WPR2", function(predictions = NULL, projected_model, p = 2, method = "exact", base = NULL, ...) standardGeneric("WPR2"))

# WPR2 <- function(predictions = NULL, projected_model, p = 2, method = "exact", base = NULL, ...) UseMethod("WPR2")
WPR2.matrix <- function(predictions, projected_model, p = 2, method = "exact", base = NULL, ...) {
  
  stopifnot(p >= 1)
  
  n <- nrow(predictions)
  d <- ncol(predictions)
  
  dotnames <- ...names()
  if(!is.null(dotnames)) {
    if(!any(grepl("observation.orientation", dotnames))) {
      predictions <- t(predictions)
      projected_model <- t(projected_model)
    }
  }
  
  wp_mod <- WpProj::wasserstein(predictions, projected_model, p = p, ground_p = p,
                                method = method, ...)^p
  
  if(is.null(base)) {
    stat <- if(p == 2) {
      colMeans(predictions)
    } else if (p == 1) {
      apply(predictions, 2, stats::median)
    } else {
      stop("Default base model selection not yet implemented for p !=1 or p != 3. Please provide your own!")
    }
    mu <- matrix(stat, n, d, byrow=TRUE)
    base_used <- "dist.from.expectation"
  } else {
    if(!is.matrix(base)) base <- as.matrix(base)
    if(ncol(base) == 1){
      mu <- matrix(base, n, d, byrow=TRUE)
    } else {
      mu <- base
      stopifnot(all(dim(base) %in% c(n,d)))
    }
    base_used <- "dist.from.null"
  }
  # wp_base <- if(method == "exact") {
  #   mean(colSums((predictions - mu)^p))
  # } else {
  #   WpProj::wasserstein(predictions, mu, p = p, 
  #                      ground_p = p,
  #                      method = method, 
  #                      ...)^p
  # }
  wp_base <- WpProj::wasserstein(predictions, mu, p = p, 
                                 ground_p = p,
                                 method = method, 
                                 ...)^p
  
  r2 <- 1 - wp_mod/wp_base # pmax(1 - wp_mod/wp_base, 0)
  output <- data.frame(r2 = r2, nactive = NA_real_, groups = NA_character_, method = method, p = p,
                       base = base_used)
  class(output) <- c("WPR2", class(output))
  return(output)
  
}

#' @rdname WPR2
#' @export
methods::setMethod("WPR2", signature = c("predictions" = "ANY", projected_model = "matrix"), definition = WPR2.matrix)

WPR2.distcompare <- function(predictions=NULL, projected_model, ...) {
  
  stopifnot(inherits(projected_model, "distcompare"))
  
  df <- projected_model$predictions
  p <- projected_model$p
  method <- df$method
  
  stopifnot(p >= 1)
  
  
  if(!is.null(predictions)) {
    stopifnot(inherits(predictions, "matrix"))
    meth.table <- table(method)
    method.use <- names(meth.table)[which.max(meth.table)]
    wass.args <- list(X = predictions, Y = as.matrix(rowMeans(predictions)),
                      p = as.numeric(p), method = method.use,
                      ...)
    wass.args <- wass.args[!duplicated(names(wass.args))]
    if(is.null(wass.args$observation.orientation)) wass.args$observation.orientation <- "colwise"
    argn <- lapply(names(wass.args), as.name)
    names(argn) <- names(wass.args)
    
    wass.call <- as.call(c(list(as.name("wasserstein")), argn))
    
    max_vals <- eval(wass.call, envir = wass.args)
    max_vec <- rep(max_vals, length(df$dist))
    base <- "dist.from.expectation"
  } else {
    max_vals <- tapply(df$dist, df$groups, max)
    max_vec <- max_vals[as.numeric(df$groups)]
    base <- "dist.from.null"
  }
  
  
  
  r2 <- 1- df$dist^p/max_vec^p #pmax(1- df$dist^p/max_vec^p, 0)
  
  df$dist <- r2
  # df$nactive <- NA_real_
  # df$groups <- NA_character_
  df$method <- method
  df$p <- p
  df$base <- base
  colnames(df)[1] <- "r2"
  
  class(df) <- c("WPR2", class(df))
  return(df)
  
}

methods::setOldClass("distcompare")

#' @rdname WPR2
#' @export
methods::setMethod("WPR2", signature = c("predictions" = "ANY", projected_model = "distcompare"), definition = WPR2.distcompare)

WPR2.list <- function(predictions, projected_model, p = 2, method = "exact", base = NULL, ...) {
  
  stopifnot(all(sapply(projected_model, inherits, "WpProj")))
  
  df <- lapply(projected_model, function(nn) {
    do.call("rbind", lapply(nn$fitted.values, function(ee) WPR2.matrix(predictions = predictions, projected_model = ee, p = p, base = base, ...)))
  })
  temp_names <- NULL
  for(nn in seq_along(df)) {
    df[[nn]]$nactive <- projected_model[[nn]]$nzero
    temp_names <- names(projected_model)[[nn]]
    if(is.null(temp_names)) temp_names <- as.character(nn)
    df[[nn]]$groups <- temp_names
  }
  
  output <- do.call("rbind", df)
  
  # class(output) <- c("WPR2", class(output))
  
  
  return(output)
  
}

#' @rdname WPR2
#' @export
methods::setMethod("WPR2", signature = c("predictions" = "ANY", projected_model = "list"), definition = WPR2.list)

# setMethod("WPR2", c("predictions" = "matrix", "projected_model" = "matrix"), WPR2.matrix)
# setMethod("WPR2", c("projected_model" = "distcompare"), WPR2.distcompare)
# setMethod("WPR2", c("projected_model" = "list"), WPR2.list)

methods::setOldClass("WpProj")

WPR2.WpProj <- function(predictions, projected_model, ...) {
  
  stopifnot(inherits(projected_model, "WpProj"))
  
  df <- lapply(projected_model$fitted.values, function(ee) WPR2.matrix(predictions = predictions, projected_model = ee, ...))
  df <- do.call("rbind", df)
  df$nactive <- projected_model$nzero
  df$groups <- paste0("power = ", projected_model$power, ", method = ", projected_model$method, ", solver = ", projected_model$solver)
  df$method <- projected_model$method
  df$p <- df$p[1]
  df$base <- "dist.from.null"
  class(df) <- c("WPR2", class(df))
  return(df)
  
}

#' @rdname WPR2
#' @export
methods::setMethod("WPR2", signature = c("predictions" = "ANY", projected_model = "WpProj"), definition = WPR2.WpProj)

plot.WPR2 <- function(x, xlim = NULL, ylim = NULL, linesize = 0.5, pointsize = 1.5, facet.group = NULL, ...) {
  object <- x
  obj <- object
  stopifnot(inherits(obj, "WPR2"))
  dots <- list(...)
  alpha <- dots$alpha
  base_size <- dots$base_size
  CI <- dots$CI
  xlab <- dots$xlab
  ylab <- dots$ylab
  leg.pos <- dots$legend.position
  if(is.null(CI)) CI <- "none"
  if(is.null(alpha)) alpha <- 0.3
  if(is.null(base_size)) base_size <- 11
  if(is.null(xlab)) xlab <- "Number of active coefficients"
  if(is.null(leg.pos)) leg.pos <- NULL
  CI <- match.arg(CI, c("none", "ribbon","bar"))
  
  wp_calc_method <- obj$method
  opt_method <- obj$groups
  p <- unique(obj$p)[1]
  base <- obj$base
  nactive <- obj$nactive
  
  
  ylim <- set_y_limits_gen(obj$r2, ylim)
  if(is.null(ylab)) {
    ylab <- bquote(W[.(p)]~R^2)
  }
  
  if(all(!is.na(nactive))) {
    if(is.null(xlab)) {
      xlab <- "Number of active coefficients" 
    }
    xlim <- set_x_limits_gen(obj$nactive, xlim)
    if(!is.null(facet.group)) {
      grouping <- c("groups", facet.group, "nactive")
    } else {
      grouping <- c("groups", "nactive")
    }
    r2 <- NULL
    df <- obj %>% 
      dplyr::group_by(dplyr::across(dplyr::any_of(grouping))) %>% 
      dplyr::summarise(
        low = stats::quantile(r2, 0.025),
        hi = stats::quantile(r2, 0.975),
        dist = mean(r2)
      )
    # E <- tapply(obj$r2, INDEX = list(obj$nactive, obj$groups), mean)
    # # sigma <- tapply(obj$dist, INDEX = list(obj$nactive, obj$groups), sd)
    # 
    # M <- tapply(obj$r2, INDEX = list(obj$nactive, obj$groups), median)
    # hi <- tapply(obj$r2, INDEX = list(obj$nactive, obj$groups), quantile, 0.975)
    # low <- tapply(obj$r2, INDEX = list(obj$nactive, obj$groups), quantile, 0.025)
    # 
    # df <- data.frame(dist = c(M), low = c(low), hi = c(hi), 
    #                  groups = rep(colnames(M), each = nrow(M)),
    #                  nactive = as.numeric(rep(rownames(M), ncol(M))),
    #                  row.names = NULL)
    # if(!is.null(facet.group)) {
    #   fg <- tapply(obj[[facet.group]], INDEX = list(obj$nactive, obj$groups), FUN = function(x){x[1]})
    #   df[[facet.group]] <- fg
    # }
    nactive <- dist <- groups <- low <- hi <- NULL # to avoid check errors
    plot <- ggplot2::ggplot( df, 
                             ggplot2::aes(x=nactive, y=dist, 
                                          color = groups, fill = groups,
                                          group=groups ))
    if(CI == "ribbon") {
      plot <- plot + ggplot2::geom_ribbon(ggplot2::aes(ymin = low, ymax = hi), alpha = alpha, linetype=0)
    } else if(CI == "bar") {
      plot <- plot + ggplot2::geom_errorbar(ggplot2::aes(ymin = low, ymax = hi), alpha = alpha, position = ggplot2::position_dodge(width=0.25))
    }
    plot <- plot + ggplot2::geom_line(position = ggplot2::position_dodge(width=0.25), linewidth = linesize) +
      ggplot2::geom_point(position = ggplot2::position_dodge(width=0.25), size = pointsize) +
      ggsci::scale_color_jama() + 
      ggsci::scale_fill_jama() +
      ggplot2::labs(fill ="Method", color="Method") +
      ggplot2::xlab(xlab) + 
      ggplot2::ylab(ylab) + ggplot2::theme_bw(base_size) +
      ggplot2::scale_x_continuous(expand = c(0, 0), limits = xlim) +
      ggplot2::scale_y_continuous(expand = c(0.01, 0.0), limits = ylim ) + 
      ggplot2::theme(legend.position = leg.pos)
  } else {
    plot <- ggplot2::ggplot(data = obj, mapping = ggplot2::aes(y = r2, fill = groups)) +
      ggplot2::geom_bar() + ggsci::scale_fill_jama() +
      ggplot2::labs(fill ="Method", color="Method") +
      ggplot2::xlab(xlab) + 
      ggplot2::ylab(ylab) + ggplot2::theme_bw(base_size) +
      ggplot2::scale_x_continuous(expand = c(0, 0), limits = xlim) +
      ggplot2::scale_y_continuous(expand = c(0.01, 0), limits = ylim ) + 
      ggplot2::theme(legend.position = leg.pos)
    if(nrow(obj) == 1) {
      plot <- plot + ggplot2::theme(legend.position = "none")
    }
  }
  if(!is.null(facet.group)) {
    plot <- plot + ggplot2::facet_grid(stats::reformulate(facet.group))
  }
  return(plot)
}

#' A Function to Combine \eqn{W_p R ^2} Objects
#' 
#' @param ... List of \eqn{W_p R^2} objects
#'
#' @return A vector of \eqn{W_p R^2} objects
#' @seealso [WPR2()]
#' 
#' @export
#' 
#' @description 
#' `r lifecycle::badge("experimental")`
#' Will combine \eqn{W_p R ^2} objects into a single object.
#' 
#' @examples
#' if (rlang::is_installed("stats")) {
#' n <- 128
#' p <- 10
#' s <- 99
#' x <- matrix( stats::rnorm( p * n ), nrow = n, ncol = p )
#' beta <- (1:10)/10
#' y <- x %*% beta + stats::rnorm(n)
#' post_beta <- matrix(beta, nrow=p, ncol=s) + stats::rnorm(p*s, 0, 0.1)
#' post_mu <- x %*% post_beta
#' 
#' 
#' fit1 <-  WpProj(X=x, eta=post_mu, theta = post_beta,
#'                power = 2.0, method = "binary program")
#' fit2 <-  WpProj(X=x, eta=post_mu, power = 2.0,
#'                options = list(penalty = "lasso")
#' )
#' 
#' 
#' 
#' out1 <- WPR2(predictions = post_mu, projected_model = fit1)
#' out2 <- WPR2(predictions = post_mu, projected_model = fit2)
#' 
#' combine <- combine.WPR2(out1, out2)
#' }
combine.WPR2 <- function(...) {
  if(...length()>1){
    objects <- list(...)
  } else {
    objects <- c(...)
  }
  stopifnot(is.list(objects))
  if (!all(sapply(objects, inherits, "WPR2"))) {
    stop("All objects must be WPR2 object")
  }
  niter <- length(objects)
  length.each <- sapply(objects, nrow)
  
  ps <- unlist(sapply(objects, function(d) d$p))
  if(!all(diff(ps)==0)) {
    stop("Some of the wasserstein powers in the WPR2 objects are different")
  }
  
  base <- unlist(sapply(objects, function(d) d$base))
  if((any(is.na(base)) & !all(is.na(base))) | !all(base == base[1])) {
    stop("Some of the objects are using different projected_model points (i.e. null model vs expectation of full model)")
  }
  
  wpmeth <- unlist(sapply(objects, function(d) d$method))
  if(!all(wpmeth == wpmeth[1])) {
    warning("Some of the Wasserstein distances were calculated with different methods. 
            Advisable not to compare some of these objects")
  }
  
  cmb <- do.call("rbind", objects)
  
  # cmb$iter <- unlist(sapply(1:niter, function(i) rep(i, length.each[i])))
  
  # class(cmb) <- c("WPR2", class(cmb))
  
  return(cmb)
}

#' Plot Function for \eqn{W_p R^2} Objects
#' @param x A \eqn{W_p R^2} object
#' @param xlim x-axis limits
#' @param ylim y-axis limits
#' @param linesize Linesize for \link[ggplot2]{geom_line}
#' @param pointsize Point size for \link[ggplot2]{geom_point}
#' @param facet.group Group to do facet_grid by
#' @param ... Currently not used
#'
#' @return a [ggplot2::ggplot()] object
#' @export
#' 
#' @examples
#' n <- 128
#' p <- 10
#' s <- 99
#' x <- matrix( stats::rnorm( p * n ), nrow = n, ncol = p )
#' beta <- (1:10)/10
#' y <- x %*% beta + stats::rnorm(n)
#' post_beta <- matrix(beta, nrow=p, ncol=s) + stats::rnorm(p*s, 0, 0.1)
#' post_mu <- x %*% post_beta
#' 
#' fit <-  WpProj(X=x, eta=post_mu, power = 2.0,
#'                options = list(penalty = "lasso")
#' )
#' obj <- WPR2(predictions = post_mu, projected_model = fit)
#' p <- plot(obj)
setMethod("plot", c("x" = "WPR2"), plot.WPR2)

set_y_limits_gen <- function(vals, ylim){
  if (!is.null(ylim)) {
    if (is.numeric(ylim)) {
      if(length(ylim) == 2) {
        return(ylim)
      } else {
        stop("if provided, ylim must be of length two")
      }
    } else {
      stop("ylim must be a numeric vector")
    }
  }
  if(is.null(vals)) return(ylim)
  # range.size <- diff(range(vals))
  # add.factor <- range.size * 1.2 - range.size
  # min_y <- max(0, min(vals) - add.factor)
  # max_y <- max(vals) + add.factor
  # ylim <- c(min_y, max_y)
  # ylim <- c(0,1)
  return(ylim)
}

set_x_limits_gen <- function(vals, xlim){
  
  if (!is.null(xlim)) {
    if (is.numeric(xlim)) {
        if(length(xlim) == 2) {
          return(xlim)
        } else {
          stop("if provided, xlim must be of length two")
        }
    } else {
      stop("xlim must be a numeric vector")
    }
  }
  
  if (is.null(vals)) return(NULL)
  min_x <- min(vals)
  max_x <- max(vals)
  xlim <- c(min_x, max_x)
  return(xlim)
}
