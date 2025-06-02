################################################################################
#
# A function to build basic constraint matrices from keywords
#
################################################################################

cgaim.fit <- function(y, x, index, w, Cmat = NULL, bvec = NULL, 
  intercept = TRUE, sm_mod, control)
{
  # Useful objects
  n <- length(y)
  p <- max(index)
  d <- length(index)
  ind_pos <- split(1:d, index)
  # Initialize alpha
  if (is.null(control$alpha.start)){
    if (control$init.type == "random"){
      pars <- control$sample_pars
      pars$G <- Cmat
      pars$H <- bvec
      res <- suppressWarnings(do.call(limSolve::xsample, pars))$X
      alpha <- res[nrow(res),]
    } else if(control$init.type == "regression"){
      pars <- control[names(control) %in% methods::formalArgs(alpha_update)]
      pars <- c(pars, list(
        y = y, x = x, w = w, index = index, Cmat = Cmat, bvec = bvec,
        dgz <- matrix(1, n, p), alpha = rep(0, d),
        delta <- FALSE
      ))
      alpha <- do.call(alpha_update, pars)$alpha
    }
  } else {
    alpha <- unlist(control$alpha.start)
    if(length(alpha) != d){
      warning(paste0("alpha.start length is inconsistent with index matrix ", 
        "and is recycled"))
      alpha <- rep_len(alpha, d)
    }
  }
  alpha <- unlist(tapply(alpha, index, normalize, type = control$norm.type))
  # Indice computation
  zs <- sapply(ind_pos, function(i, x, a) x[,i] %*% a[i], 
    x = x, a = alpha)
  colnames(zs) <- unique(names(index))
  # Initial smoothing
  smooth_fun <- sprintf("smooth_%s", control$sm_method)
  smo_par <- c(sm_mod, list(y = y, x = zs, weights = w))
  gz <- do.call(smooth_fun, smo_par)   
  yhat <- gz$intercept + rowSums(gz$gz)
  r <- y - yhat  
  # Convergence criterion
  eps <- control$tol + 1
  c1 <- 1      
  l2 <- L2(y, yhat, w)  
  # If requested: tracing the algorithm evolution
  if (control$trace){
    paste0("step ", c1, ", crit = ", format(signif(l2, 3)), ", alpha = ",
      paste(signif(alpha, 3), collapse = ", "))
  }
  #----- Gauss-Newton search
  stopflag <- 0
  while(stopflag == 0){
    # Update alphas
    alphaup <- alpha_update(y = r, x = x, w = w, index = index, Cmat = Cmat,
      bvec = bvec, delta = TRUE, dgz = gz$dgz, alpha = alpha, 
      solver = control$solver, qp_pars = control$qp_pars, ctol = control$ctol)
    delta <- alphaup$alpha
    # Halving in case of bad steps
    cuts <- 1
    repeat {   
      alpha.new <- alpha + delta * cuts
      alpha.new <- unlist(tapply(alpha.new, index, normalize, 
        type = control$norm.type))
      zs <- sapply(ind_pos, function(i, x, a) x[,i] %*% a[i], 
        x = x, a = alpha.new)
      colnames(zs) <- unique(names(index))   
      smo_par$x <- zs
      gz <- do.call(smooth_fun, smo_par)
      yhat <- gz$intercept + rowSums(gz$gz)
      l2.new <- L2(y, yhat, w)
      if (l2 - l2.new > 0 || !control$halving){
        break
      }
      cuts <- cuts / 2
      if (cuts < control$min.step.len){ 
        stopflag <- 2
        break
      }
    }
    if (control$convergence_criterion == "offset"){
      eps <- offset_convergence(r, x, gz$dgz[,index])
    } else if (control$convergence_criterion == "alpha") {
      eps <- max(abs(alpha.new - alpha) / abs(alpha))
    } else {
      eps <- (l2 - l2.new) / l2
    }
    r <- y - yhat
    alpha <- alpha.new
    l2 <- l2.new
    c1 <- c1 + 1       
    if (control$trace){ # Tracing the algorithm evolution
      paste0("step ", c1, ", crit = ", format(signif(l2, 3)), ", alpha = ",
        paste(signif(alpha, 3), collapse = ", "))
    }
    if (all(eps < control$tol)){
      stopflag <- 1
    } else { 
      if (c1 >= control$max.iter){
        stopflag <- 3
        warning(paste0("Fitting did not converge after ", control$max.iter, 
          " iterations. Consider revising the constraints"))
      } 
    }
  }
  names(alpha) <- colnames(x)
  final.gz <- scale(gz$gz, center = intercept)
  final.gz[,apply(gz$gz, 2, stats::sd) == 0] <- 
    gz$gz[,apply(gz$gz, 2, stats::sd) == 0]
  betas <- attr(final.gz, "scaled:scale")
  beta0 <- gz$intercept + sum(attr(final.gz, "scaled:center"))
  names(beta0) <- "(Intercept)"
  r <- y - yhat
  edf <- gz$edf + d - sum(alphaup$active)
  gcv <- l2 / (1 - (edf / n))^2
  sig2 <- sum(r^2) / (n - edf)
  output <- list(alpha = alpha, gfit = final.gz, indexfit = zs, 
    beta = c(beta0, betas),
    index = index, fitted = yhat, residuals = r, rss = l2, flag = stopflag, 
    niter = c1, edf = edf, gcv = gcv, dg = gz$dgz, gse = gz$se, 
    active = alphaup$active, Cmat = Cmat, bvec = bvec)
  return(output)   
}