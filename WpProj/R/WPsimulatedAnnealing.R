#' p-Wasserstein distance projections using simulated annealing
#'
#' @param X Covariate vector
#' @param Y Predictions
#' @param theta Optional matrix of parameters for generating predictions
#' @param power Power of the Wasserstein distance
#' @param force Any covariates to force into the model?
#' @param model.size Maximum number of coefficients
#' @param nvars The number of variables to explore. Should be an integer vector of model sizes. Default is NULL which will explore all models from 1 to `model.size`.
#' @param maxit Maximum number of iterations
#' @param temps Number of temperatures
#' @param max.time Maximum time in seconds to run
#' @param const Maximum value for simulated annealing distance
#' @param proposal Proposal function. There is a default method but can provide your own with parameters `xty`, `cur`, `idx`, `force`, `d`, `method`
#' @param options Options for simulated annealing
#' @param display.progress Whether to display solver progress. TRUE or FALSE. Default is FALSE.
#' @param parallel A [foreach::foreach()] backend
#' @param calc.theta Should the model save the linear coefficients? TRUE or FALSE. Default is TRUE
#' @param xtx precomputed crossproduct \code{crossprod(X,X)}
#' @param xty precomputed \code{crossprod(X, Y)}
#'
#' @return An object of class `WpProj`
#' @keywords internal
#' 
# @examples
# if(rlang::is_installed("stats")) {
# n <- 128
# p <- 10
# s <- 99
# x <- matrix( stats::rnorm( p * n ), nrow = n, ncol = p )
# x_ <- t(x)
# beta <- (1:10)/10
# y <- x %*% beta + rnorm(n)
# post_beta <- matrix(beta, nrow=p, ncol=s) + rnorm(p*s, 0, 0.1)
# post_mu <- x %*% post_beta
# sv <-  WPSA(X=x, Y=t(post_mu), theta = t(post_beta),
# force = 1, p = 2, nvars = 2:(p-1),
# maxit=5, temps = 5,
# max.time = 100,
# options = list(method = c("selection.variable"),
#                energy.distribution = "boltzman",
#                transport.method = "exact",
#                cooling.schedule = "Geman-Geman",
#                proposal.method = "covariance")
# )
# }
WPSA <- function(X, Y=NULL, theta, 
                 power = 2, force = NULL, 
                 model.size = 3,
                 nvars = NULL,
                 # groups = NULL,
                 maxit=1, temps = 1000,
                 max.time = 3600, const = NULL,
                 proposal = proposal.fun,
                 options = list(method = c("selection.variable","scale","projection"),
                                transport.method = transport_options(),
                                energy.distribution = "boltzman",
                                cooling.schedule = "Geman-Geman",
                                proposal.method = "covariance",
                                epsilon = 0.05,
                                OTmaxit = 100),
                 display.progress = FALSE,
                 parallel = NULL,
                 calc.theta = TRUE,
                 xtx = NULL,
                 xty = NULL,
                 ...
) 
{
  # groups <- as.integer(groups)
  # unique.groups <- as.integer(unique.groups)
  this.call <- as.list(match.call()[-1])
  if(!is.matrix(X)) X <- as.matrix(X)
  if(!is.matrix(theta)) theta <- as.matrix(theta)
  time.exceed <- function(max.time, start.time) {
    elapsed <- proc.time() - start.time
    return(elapsed[3] > max.time)
  }
  
  get.theta <- calc.theta
  iter <- maxit
  
  p <- ground_p <- power
  if (is.null(model.size)) {
    model.size <- ncol(X)-1L
  }
  
  if (is.null(nvars)) {
    nvars <- 1:model.size
  }
  
  names(options) <- match.arg(names(options), c("energy.distribution",
                                                "method",
                                                "transport.method",
                                                "cooling.schedule",
                                                "proposal.method"),
                              several.ok = TRUE)
  energy.dist <- match.arg(options$energy.distribution, c("boltzman","bose-einstein"))
  meth <- match.arg(options$method, c("selection.variable","scale","projection"))
  transmeth <- match.arg(options$transport.method, transport_options() )
  cool.sched <- match.arg(options$cooling.schedule, c("Geman-Geman","exponential"))
  obs.direction <- "colwise"
  prop.meth <- match.arg(options$proposal.method, c("covariance","random"))
  epsilon <- options$epsilon
  OTmaxit <- options$OTmaxit
  
  if(is.null(epsilon)) epsilon <- 0.05
  if(is.null(OTmaxit)) OTmaxit <- 100
  
  if(ncol(theta) == ncol(X)){
    theta_ <- t(theta)
  } else {
    theta_ <- theta
  }
  
  #transpose X
  X_ <- t(X)
  
  if(is.null(Y)) {
    same <- TRUE
    Y_ <- crossprod(X_, theta_)
  } else{
    Y <- as.matrix(Y)
    if(!any(dim(Y) %in% dim(X_))) stop("dimensions of Y must match X")
    if(!is.matrix(Y)) Y <- as.matrix(Y)
    if(nrow(Y) != ncol(X_)){ 
      Y_ <- t(Y)
    } else{
      Y_ <- Y
    }
    same <- FALSE
    if(all(Y_==crossprod(X_, theta_))) same <- TRUE
  }
  OToptions <- list(same = as.logical(same),
                    method = as.character(meth),
                    transport.method = as.character(transmeth),
                    epsilon = as.double(epsilon),
                    niter = as.integer(OTmaxit))
  
  d <- nrow(X_)
  n <- ncol(X_)
  
  if (is.null(xtx) | is.null(xty) ) {
    augmat <- sufficientStatistics(X, Y_, t(theta_), OToptions)
    xtx <- augmat$XtX
    xty <- augmat$XtY
  }
  
  if(!is.null(parallel)){
    if(!inherits(parallel, "cluster")) {
      stop("parallel must be a registered cluster backend")
    }
    doParallel::registerDoParallel(parallel)
    display.progress <- FALSE
  } else{
    foreach::registerDoSEQ()
  }
  
  message <- "completed"
  start.time <- proc.time()
  
  idx <- 1:d 
  if (!is.null(force) & is.null(const)) {
    beta_temp <- calc.beta(xtx=xtx, xty=xty, force, meth, OToptions = OToptions, x=X_, theta_, Y_, niter=500)
    if(meth != "projection"){
      temp_mu <- selVarMeanGen(X_, theta_, beta_temp) 
    } else {
      temp_mu <- crossprod(X_, beta_temp)
    }
    const <- WpProj::wasserstein(temp_mu, 
                                          Y_, p, ground_p, 
                                          obs.direction, transmeth)
  } else if (is.null(const)) {
    if (meth == "projection") {
      mu_calc <- function(X, theta, beta) {
        return(crossprod(X, beta))
      }
    } else {
      mu_calc <- function(X, theta, beta) {
        return(selVarMeanGen(X, theta, beta))
      }
    }
    w2s <- foreach::foreach(i = idx, .combine = "unlist") %dopar%
      {
        beta_temp <- calc.beta(xtx=xtx, xty=xty, i, meth, OToptions = OToptions, x=X_, theta_, Y_, niter=500)
        temp_mu <- mu_calc(X_, theta_, beta_temp)
        return(WpProj::wasserstein(temp_mu, Y_, p, ground_p, obs.direction, transmeth) )
      }
    const <- max(w2s)
  }
  
  if(length(nvars) == 1) {
    if(length(force) == max(nvars)) {
      # warning("Number of variables forced into model the same as desired model size")
      old.dist <- best.dist <- dist.fun(X_, force, theta_, Y_, xtx, xty, OToptions, obs.direction, p, ground_p)
      final.beta <- best.beta <- calc.beta(xtx,xty,force, meth, OToptions = OToptions,
                                           x=X_, theta= theta_, Y = Y_, niter = 500)
      output <- list(final = list(index = force, 
                                  distance = old.dist,
                                  beta = final.beta),
                     optimal = list(index = force,
                                    distance = best.dist,
                                    beta = best.beta),
                     force = force,
                     history = old.dist,
                     temps = NULL,
                     accept.pct = NULL,
                     call = this.call,
                     message = message,
                     method = meth)
      
      class(output)   <- c("WpProj", "annealing")
      
      if(get.theta == TRUE){
        extract       <- extractTheta(output, theta_)
        output$theta  <- extract$theta
        output$nzero  <- extract$nzero
        output$eta    <- lapply(output$theta, function(tt) crossprod(X_, tt))
      } else {
        output$theta  <- NULL
        output$nzero  <- nvars
        output$eta    <- NULL
      }
      
      return(output) 
    }
    
    if (iter <= 0) {
      stop("iter should be greater than 0")
    }
    if (temps <= 0) {
      stop("temps should be greater than 0")
    }
    
    # tY <- t(Y)
    
    # if (grepl("univariate", transmeth) ) {
    #   obs.direction <- "rowwise"
    #   # X_ <- t(X_)
    #   # Y_ <- t(Y_)
    # } else {
      obs.direction <- "colwise"
    # }
    
    if(is.null(proposal)) proposal <- proposal.fun
    # wt <- pseudo.obs/(n+pseudo.obs)
    
    # theta_norm <- rowMeans(theta_^2)
    # w_xtx <- wt * diag(theta_norm) + (1-wt) * xtx
    # w_xty <- wt * theta_norm + (1-wt) * xty
    
    if(d == nvars) {
      warning("Model is same size as number of parameters")
      
      old.dist <- best.dist <- dist.fun(X_, 1:d, theta_, Y_, xtx, xty, OToptions, obs.direction, p, ground_p)
      final.beta <- best.beta <- calc.beta(xtx,xty,1:d, meth, OToptions = OToptions,
                                           x=X_, theta= theta_, Y = Y_, niter = 500)
      # transport.method
      
      output <- list(final = list(index = 1:d, 
                                  distance = old.dist,
                                  beta = final.beta),
                     optimal = list(index = 1:d,
                                    distance = best.dist,
                                    beta = best.beta),
                     force = force,
                     history = old.dist,
                     temps = NULL,
                     accept.pct = NULL,
                     call = this.call,
                     message = message,
                     method = meth)
      
      class(output)   <- c("WpProj", "annealing")
      
      if(get.theta == TRUE){
        extract       <- extractTheta(output, theta_)
        output$theta  <- extract$theta
        output$nzero  <- extract$nzero
        output$eta    <- lapply(output$theta, function(tt) crossprod(X_, tt))
      } else {
        output$theta  <- NULL
        output$nzero  <- nvars
        output$eta    <- NULL
      }
      
      return(output) 
      
    }
    
    temp_sched <- coolingSchedule(1:temps, const, cool.sched)
    
    previous   <- const
    active.set <- idx[!(idx %in% force)]
    mod.size   <- nvars - length(force)
    
    active.idx <- sort(c(force, sample(active.set, mod.size)))
    old.dist   <- dist.fun(X_, active.idx, theta_, Y_, xtx, xty, OToptions, obs.direction, p,ground_p)
    
    dist.hist  <- matrix(NA, nrow=iter, ncol=temps)
    colnames(dist.hist) <- as.character(temp_sched)
    rownames(dist.hist) <- as.character(1:iter)
    
    best.idx   <- active.idx
    best.dist  <- old.dist
    acpct      <- rep(NA, temps) #accept percentage
    accept_cnt <- zero_counter <- 0
    max.save   <- min(c(choose(d, nvars), length(temp_sched)*iter, 1e6))
    saved.dist <- rep(Inf, max.save)
    idx.key    <- rep(-Inf, max.save)
    # pot.comb <- rbind(force, combn(idx[!(idx %in% force)], nvars-length(force)))
    # names(saved.dist) <- apply(pot.comb,2,paste, collapse="")
    if(display.progress) pb <- utils::txtProgressBar(min = 0, max = length(temp_sched), style = 3)
    
    for (i in seq_along(temp_sched) ){
      # print(i)
      if(display.progress) utils::setTxtProgressBar(pb, i)
      for (j in 1:iter) {
        #get proposal
        proposed <- proposal(xty, active.idx, idx, force, d, prop.meth)
        
        # calculate proposed distance
        idx.switch <- proposed$proposal
        idx.name <- calc_key(idx.switch$active)
        if( !(idx.name %in% idx.key )) {
          proposed.dist <- dist.fun(X_, idx.switch$active, theta_, Y_, xtx, xty,OToptions, obs.direction, p, ground_p)
          if(proposed.dist < saved.dist[max.save]){
            # names(proposed.dist) <- idx.name
            ords <- order(c(saved.dist, proposed.dist))
            saved.dist <- c(saved.dist, proposed.dist)[ords[1:max.save]]
            idx.key <- c(idx.key, idx.name)[ords[1:max.save]]
          }
          # saved.dist[idx.name] <- proposed.dist
        } else {
          proposed.dist <- saved.dist[idx.key == idx.name]
        }
        
        # get transition probs
        log.proposal.probs <- log(proposed$probs)
        
        #calculate MH prob
        # if(all(idx.switch$active %in% c(1,3,6,7))) browser()
        log.MHprob <- energy.fun(temp_sched[i], proposed.dist, old.dist, energy.dist) + log.proposal.probs[1] - log.proposal.probs[2]
        # if(is.nan(log.MHprob)) browser()
        # if(is.na(log.MHprob)) browser()
        
        # if(all(idx.switch$active %in% c(1,6,7))) browser()
        if(log(stats::runif(1)) < log.MHprob){
          active.idx <- idx.switch$active
          old.dist <- proposed.dist
          accept_cnt <- accept_cnt + 1
          if(old.dist < best.dist) {
            best.dist <- old.dist
            best.idx <- active.idx
          }
        }
        # if(length(old.dist)>1) browser()
        dist.hist[j,i] <- old.dist
      }
      acpct[i] <- accept_cnt/iter
      if(accept_cnt == 0) { 
        zero_counter <- zero_counter + 1
      } else {
        zero_counter <- 0
      }
      accept_cnt <- 0
      
      if(zero_counter > 2) {
        temp_sched[is.na(acpct)] <- NA
        break
      }
      if(j %% 10) {
        if(time.exceed(max.time, start.time)) {
          messages <- "Hit max time. Did not reach final temperature"
          break
        }
      }
    }
    if(display.progress) close(pb)
    final.beta <- calc.beta(xtx, xty, active.idx, meth, OToptions = OToptions, X_, theta_)
    best.beta  <- calc.beta(xtx, xty, best.idx,   meth, OToptions = OToptions, X_, theta_)
    
    output <- list(final = list(index = active.idx, 
                                distance = old.dist,
                                beta = final.beta),
                   optimal = list(index = best.idx,
                                  distance = best.dist,
                                  beta = best.beta),
                   force = force,
                   history = dist.hist,
                   temps = temp_sched,
                   accept.pct = acpct,
                   call = this.call,
                   message = message,
                   method = meth)
  }
  else if (length(nvars) > 1) {
    if(display.progress) pb <- utils::txtProgressBar(min = 0, max = length(nvars), style = 3)
    # vector("list", length(nvars))
    out <- foreach::foreach( i = seq_along(nvars), 
                             .inorder = FALSE) %dorng% {
      mm <- nvars[i]
      if(display.progress) utils::setTxtProgressBar(pb, i)
      if ( time.exceed(max.time, start.time) ) { # doesn't work in parallel
        # message <- "Hit max time exploring model sizes"
        # if (i < length(out)) out[(i + 1):length(out)] <- NULL
        return(NULL)
      }
      result <- WPSA(X=X, Y=Y_, theta=theta_, 
                       force = force, power = p, nvars = mm,
                       maxit=iter, temps = temps,
                       max.time = max.time/length(nvars), const = const,
                       proposal = proposal,
                       options = options,
                       display.progress = FALSE,
                       parallel = NULL,
                       calc.theta = FALSE,
                       xtx = xtx,
                       xty = xty)
      return(result)
    }
    if( time.exceed(max.time, start.time) ) {
      message <- "Hit max time exploring model sizes"
    }
    if(display.progress) close(pb)
    # final <-  lapply(out, function(o) o$final)
    # optimal <-  lapply(out, function(o) o$optimal)
    # dist.hist <- lapply(out, function(o) o$history)
    # temp_sched <-  lapply(out, function(o) o$temps)
    # acpct <-  lapply(out, function(o) o$accept.pct)
    # rm(out)
    call <- formals(WPSA)
    call[names(this.call)] <- this.call
    output <-list(final = lapply(out, function(o) o$final),
                  optimal = lapply(out, function(o) o$optimal),
                  force = force,
                  history = lapply(out, function(o) o$history),
                  temps = lapply(out, function(o) o$temps),
                  accept.pct = lapply(out, function(o) o$accept.pct),
                  call = this.call,
                  message = message,
                  method = meth)
    rm(out)
  }
  
  class(output) <- c("WpProj","annealing")
  
  if(get.theta == TRUE){
    extract       <- extractTheta(output, theta_)
    output$theta  <- extract$theta
    output$nzero  <- extract$nzero
    output$eta    <- lapply(output$theta, function(tt) X %*% tt)
  } else {
    output$theta  <- NULL
    output$nzero  <- nvars
    output$eta    <- NULL
  }
  
  return(output)
}

dist.fun <- function(X=NULL, active.idx=NULL, theta = NULL, Y = NULL, xtx=NULL, xty = NULL, OToptions = NULL, obs.direction ="colwise", 
                     p = 2, ground_p = 2){
  if(is.null(OToptions)) {
    OToptions <- list(same = TRUE,
                      method = "selection.variable",
                      transport.method = "hilbert",
                      epsilon = 0.05,
                      niter = 100)
  } else {
    if(is.null(OToptions$same)) OToptions$same <- FALSE
    if(is.null(OToptions$method)) OToptions$method <- "selection.variable"
    if(is.null(OToptions$transport.method)) OToptions$transport.method <- "hilbert"
    if(is.null(OToptions$epsilon)) OToptions$epsilon <- 0.05
    if(is.null(OToptions$niter)) OToptions$niter <- 100
  }
  beta <- calc.beta(xtx, xty, active.idx = active.idx, 
                    method = OToptions$method, OToptions = OToptions,
                    x = X, theta =theta, Y = Y)
  if(OToptions$method == "projection") {
    mu <- crossprod(X, beta)
    # } else if (method == "selection.variable") {
    #   # wp <- WpProj::wasserstein(mu, Y, p = p, ground_p = p, "colwise", "exact")
    #   mu <- crossprod(X[active.idx, , drop = FALSE], theta[active.idx,, drop = FALSE])
  } else {
    mu <- selVarMeanGen(X,theta,beta)
  }
  shortcut <- (OToptions$method == "projection") & (OToptions$transport.method == "exact")
  wp <- if(shortcut){
      (((sum((mu - Y)^ground_p)^(1/ground_p))^p)^(1/p))/ncol(Y)
    } else {
      WpProj::wasserstein(mu, Y, p = p, ground_p = ground_p, obs.direction, OToptions$transport.method)
    }
  return(wp)
}

energy.fun <- function(temp, proposed.dist, old.dist, method="boltzman") {
  dist <- proposed.dist - old.dist
  if(method == "boltzman"){
    return(-dist/temp)
  }
  if(method == "bose-einstein") {
    return(log(expm1(old.dist/temp)) - log(expm1(proposed.dist/temp)))
    # return(log(expm1(dist/temp)))
  }
}


proposal.fun <- function(xty, cur, idx, force, d, method = "covariance") {
  forced <- idx %in% force
  
  choice.new <- !( forced | (idx %in% cur) )
  choice.old <- cur[!(cur %in% force)]
  
  not.act.idx <- idx[choice.new]
  act.idx <- idx[choice.old]
  
  n.new <- length(not.act.idx)
  n.old <- length(act.idx) + sum(forced)
  
  if (method == "covariance") {
    # xty_new <- xty_old <- 
    potentials <- abs(xty)
    # xty_new[choice] <- 0
    # xty_old[!choice | forced] <- 0
    # if(!all(is.finite(probs_rem))) browser()
    # if(!all(is.finite(probs_add))) browser()
    # if(length(probs_rem)==0) browser()
  }
  if (method == "random") {
    potentials <- rep(1, n.old + n.new)
    # probs_rem <- probs_rev_rem <- rep(1/n.old, n.old)
    # probs_add <- probs_rev_add <- rep(1/n.new, n.new)
    
  }
  probs_add <- potentials[not.act.idx]/sum(potentials[not.act.idx])
  probs_rem <- 1-potentials[act.idx]/sum(potentials[act.idx])
  
  if(all(probs_rem == 0)) probs_rem <- 1/n.old
  if(all(probs_add == 0)) probs_add <- 1/n.new
  
  proposed.removal  <- which(stats::rmultinom(n=1, size=1, prob=probs_rem)==1)
  proposed.addition <- which(stats::rmultinom(n=1, size=1, prob=probs_add)==1)
  
  new.act     <- c(not.act.idx[proposed.addition], act.idx[-(proposed.removal)])
  new.not.act <- c(act.idx[proposed.removal], not.act.idx[-(proposed.addition)])
  
  probs_rev_rem <-  1-potentials[new.act]/sum(potentials[new.act])
  probs_rev_add <-  potentials[new.not.act]/sum(potentials[new.not.act])
  
  if(all(probs_rev_rem == 0)) probs_rev_rem <- rep(1/n.old, n.old)
  if(all(probs_rev_add == 0)) probs_rev_add <-rep(1/n.new, n.new)
  
  
  new.probs <- probs_add[proposed.addition] * probs_rem[proposed.removal]
  old.probs <- probs_rev_rem[1] * probs_rev_add[1] #make sure this makes sense
  
  new.act.idx <- sort(c(new.act, force)) #c(new.act, force) # 
  new.not.act.idx <- sort(new.not.act) # new.not.act #
  
  return(list(proposal=list(active=new.act.idx, not.active=new.not.act.idx),
              probs=c(new.probs, old.probs)))
}


coolingSchedule <- function(t,c, method="Geman-Geman") {
  
  if(method == "Geman-Geman"){
    return(c/log(t+1))
  }
  if(method == "exponential") {
    Ti <- c/log(2)
    Tf <- c/log(.Machine$double.xmax)
    alpha <- -(log(Tf) - log(Ti))/length(t)
    return(exp(-(t-1) *alpha) *  Ti)
  }
}

calc_key <- function(b){
  a <- log(sort(b) + 1)
  s <- length(a)
  val <- 2^a + pi * a
  out <- sum(val)
  return(out)
}
