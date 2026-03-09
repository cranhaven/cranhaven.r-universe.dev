#' Functions to estimate parameters of probability distributions by fitting the distributions using optim()
#'
#' @param x Vector containing the discrete observations
#' @param type Keyword for the probability distribution the data is to be fitted
#'     against. Possible values are ("pois", "nb", "del", pig", "pb", "pois2", "nb2", "del2", "pig2"
#'      "pb2", "zipois", "zinb", "zidel", "zipg", zipb", "zipois2", "zinb2", "zidel2", "zipig2", zipb2")
#' @param optim_control List of options to override presets in
#'     the optim function; Set to list(maxit = 1000) by default.
#'     For more details, please refer to the 'control' parameter in the
#'     standard 'optim' function in package 'stats'.
#' @keywords parameter estimation
#' @name fit_params
#' @importFrom stats kmeans optim runif
#' @importFrom gamlss.dist dPIG dZIPIG rPIG rZIPIG dDEL rDEL
#' @export
#' @examples
#' x1 <- rnbinom(100, size = 13, mu = 9)
#' p1 <- fit_params(x1, "nb")
#' s <- sample(x = c(0,1), size = 100, replace = TRUE, prob = c(0.3,0.7))
#' x2 <- s*x1 + (1-s) * rnbinom(100, size = 15, mu = 53)
#' p2 <- fit_params(x2, "nb2")
fit_params <- function(x, type, optim_control = list(maxit = 1000)) {
  max_iter <- 20
  x_mean = mean(x)
  ################################  base models  ######################################
  if (type == "pois") {
    t <- system.time(o <- optim(par = x_mean, fn = nlogL_pois, data = x, method = "Brent", lower = x_mean-100, upper = x_mean+100, control = optim_control))
  }
  else if (type == "nb") {
    p <- c(1, 1)
    t <- system.time(o <- optim(par = p, fn = nlogL_nb, data = x, control = optim_control))
  }
  else if (type == "del") {
    p <- c(1, 1, 0.5)
    t <- system.time(o <- optim(par = p, fn = nlogL_del, data = x, control = optim_control))
  }
  else if (type == "pig") {
      p <- c(1, 1)
      t <- system.time(o <- optim(par = p, fn = nlogL_pig, data = x, control = optim_control))
  }
  else if (type == "pb") {
    par <- estimate_pb_optim_init_restarts(x)
    t <- system.time(o <- optim(par = par, fn = nlogL_pb, data = x, control = optim_control))
  }
  ################################ Zero-inflated models ###############################
  ######################################################
  else if (type == "zipois") {
    optim_restarts <- list()
    optim_times <- list()
    for(i in 1:max_iter) {
      t <- system.time(o <- optim(par = runif(2), fn = nlogL_zipois, data = x, control = optim_control))
      optim_restarts[[i]] <- o
      optim_times[[i]] <- t
    }
    t <- system.time(o <- optim(par = c(0, x_mean), fn = nlogL_zipois, data = x, control = optim_control))
    optim_restarts[[i+1]] <- o
    optim_times[[i+1]] <- t
    best_optim <- which.min(unlist(lapply(optim_restarts, function(x) x$value)))
    o <- optim_restarts[[best_optim]]
    t <- optim_times[[best_optim]]
  }
  ######################################################
  else if (type == "zinb") {
    optim_restarts <- list()
    optim_times <- list()
    for(i in 1:max_iter) {
      t <- system.time(o <- optim(par = runif(3), fn = nlogL_zinb, data = x, control = optim_control))
      optim_restarts[[i]] <- o
      optim_times[[i]] <- t
    }
    p <- c(0, fit_params(x, "nb")$par)
    t <- system.time(o <- optim(par = p, fn = nlogL_zinb, data = x, control = optim_control))
    optim_restarts[[i+1]] <- o
    optim_times[[i+1]] <- t
    best_optim <- which.min(unlist(lapply(optim_restarts, function(x) x$value)))
    o <- optim_restarts[[best_optim]]
    t <- optim_times[[best_optim]]
  }
  ######################################################
  else if (type == "zidel") {
    optim_restarts <- list()
    optim_times <- list()
    for(i in 1:max_iter) {
      t <- system.time(o <- optim(par = runif(4), fn = nlogL_zidel, data = x, control = optim_control))
      optim_restarts[[i]] <- o
      optim_times[[i]] <- t
    }
    p <- c(0.000001, fit_params(x, "del")$par)
    t <- system.time(o <- optim(par = p, fn = nlogL_zidel, data = x, control = optim_control))
    optim_restarts[[i+1]] <- o
    optim_times[[i+1]] <- t
    best_optim <- which.min(unlist(lapply(optim_restarts, function(x) x$value)))
    o <- optim_restarts[[best_optim]]
    t <- optim_times[[best_optim]]
  }
  ######################################################
  else if (type == "zipig") {
      optim_restarts <- list()
      optim_times <- list()
      for(i in 1:max_iter) {
          t <- system.time(o <- optim(par = runif(3), fn = nlogL_zipig, data = x, control = optim_control))
          optim_restarts[[i]] <- o
          optim_times[[i]] <- t
      }
      p <- c(0.000001, fit_params(x, "pig")$par)
      t <- system.time(o <- optim(par = p, fn = nlogL_zipig, data = x, control = optim_control))
      optim_restarts[[i+1]] <- o
      optim_times[[i+1]] <- t
      best_optim <- which.min(unlist(lapply(optim_restarts, function(x) x$value)))
      o <- optim_restarts[[best_optim]]
      t <- optim_times[[best_optim]]
  }
  ######################################################
  else if (type == "zipb") {
    p1 <- c(0, estimate_pb_optim_init_restarts(x))
    p2 <- c(get_0inf_parameter(x), estimate_pb_optim_init_restarts(x))
    nl1 <- nlogL_zipb(x, p1)
    nl2 <- nlogL_zipb(x, p2)

    par <- if (nl1 < nl2) p1 else p2

    t <- system.time(o <- optim(par = par, fn = nlogL_zipb, data = x, control = optim_control))
  }
  ######################################################
  ################################ 2pop ###############################################
  ######################################################
  else if (type == "pois2") {
    optim_restarts <- list()
    optim_times <- list()
    for(i in 1:max_iter) {
      t <- system.time(o <- optim(par = runif(3), fn = nlogL_pois2, data = x, control = optim_control))
      optim_restarts[[i]] <- o
      optim_times[[i]] <- t
    }
    t <- system.time(o <- optim(par = c(1, x_mean, x_mean), fn = nlogL_pois2, data = x, control = optim_control))
    optim_restarts[[i+1]] <- o
    optim_times[[i+1]] <- t
    best_optim <- which.min(unlist(lapply(optim_restarts, function(x) x$value)))
    o <- optim_restarts[[best_optim]]
    t <- optim_times[[best_optim]]
  }
  ######################################################
  else if (type == "nb2") {
    optim_restarts <- list()
    optim_times <- list()
    for(i in 1:max_iter) {
      t <- system.time(o <- optim(par = runif(5), fn = nlogL_nb2, data = x, control = optim_control))
      optim_restarts[[i]] <- o
      optim_times[[i]] <- t
    }
    t1 <- fit_params(x, "nb")$par
    t <- system.time(o <- optim(par = c(1, t1, t1), fn = nlogL_nb2, data = x, control = optim_control))
    optim_restarts[[i+1]] <- o
    optim_times[[i+1]] <- t
    best_optim <- which.min(unlist(lapply(optim_restarts, function(x) x$value)))
    o <- optim_restarts[[best_optim]]
    t <- optim_times[[best_optim]]
  }
  ######################################################
  else if (type == "del2") {
    optim_restarts <- list()
    optim_times <- list()
    for(i in 1:max_iter) {
      t <- system.time(o <- optim(par = runif(7), fn = nlogL_del2, data = x, control = optim_control))
      optim_restarts[[i]] <- o
      optim_times[[i]] <- t
    }
    t1 <- fit_params(x, "del")$par
    t <- system.time(o <- optim(par = c(1, t1, t1), fn = nlogL_del2, data = x, control = optim_control))
    optim_restarts[[i+1]] <- o
    optim_times[[i+1]] <- t
    best_optim <- which.min(unlist(lapply(optim_restarts, function(x) x$value)))
    o <- optim_restarts[[best_optim]]
    t <- optim_times[[best_optim]]
  }
  ######################################################
  else if (type == "pig2") {
      optim_restarts <- list()
      optim_times <- list()
      for(i in 1:max_iter) {
          t <- system.time(o <- optim(par = runif(5), fn = nlogL_pig2, data = x, control = optim_control))
          optim_restarts[[i]] <- o
          optim_times[[i]] <- t
      }
      t1 <- fit_params(x, "pig")$par
      t <- system.time(o <- optim(par = c(1, t1, t1), fn = nlogL_pig2, data = x, control = optim_control))
      optim_restarts[[i+1]] <- o
      optim_times[[i+1]] <- t
      best_optim <- which.min(unlist(lapply(optim_restarts, function(x) x$value)))
      o <- optim_restarts[[best_optim]]
      t <- optim_times[[best_optim]]
  }
  ######################################################
  else if (type == "pb2") {
    t1<-estimate_pb_optim_init_restarts(x)
    p1 <- c(1, t1, t1)
    nl1 <- nlogL_pb2(x, p1)
    p2 <- estimate_pb2_optim_init_kmeans(x)
    nl2 <- nlogL_pb2(x, p2)
    if(is.na(nl2)) {
      par <- p1
    } else {
      par <- if (nl1 < nl2) p1 else p2
    }
    t <- system.time(o <- optim(par = par, fn = nlogL_pb2, data = x, control = optim_control))
  }
  ################################ zi2  ###############################################
  ######################################################
  else if (type == "zipois2"){
    optim_restarts <- list()
    optim_times <- list()
    for(i in 1:max_iter) {
      t <- system.time(o <- optim(par = c(runif(2)/2, runif(2)), fn = nlogL_zipois2, data = x, control = optim_control))
      optim_restarts[[i]] <- o
      optim_times[[i]] <- t
    }
    t <- system.time(o <- optim(par = c(0, 1, x_mean, x_mean), fn = nlogL_zipois2, data = x, control = optim_control))
    optim_restarts[[i+1]] <- o
    optim_times[[i+1]] <- t
    best_optim <- which.min(unlist(lapply(optim_restarts, function(x) x$value)))
    o <- optim_restarts[[best_optim]]
    t <- optim_times[[best_optim]]
  }
  ######################################################
  else if (type == "zinb2") {
    optim_restarts <- list()
    optim_times <- list()
    for(i in 1:max_iter) {
      t <- system.time(o <- optim(par = c(runif(2)/2, runif(4)), fn = nlogL_zinb2, data = x, control = optim_control))
      optim_restarts[[i]] <- o
      optim_times[[i]] <- t
    }
    t1 <- fit_params(x, "nb")$par
    t <- system.time(o <- optim(par = c(0, 1, t1, t1), fn = nlogL_zinb2, data = x, control = optim_control))
    optim_restarts[[i+1]] <- o
    optim_times[[i+1]] <- t
    best_optim <- which.min(unlist(lapply(optim_restarts, function(x) x$value)))
    o <- optim_restarts[[best_optim]]
    t <- optim_times[[best_optim]]
  }
  ######################################################
  else if (type == "zidel2") {
    optim_restarts <- list()
    optim_times <- list()
    for(i in 1:max_iter) {
      t <- system.time(o <- optim(par = c(runif(2)/2, runif(6)), fn = nlogL_zidel2, data = x, control = optim_control))
      optim_restarts[[i]] <- o
      optim_times[[i]] <- t
    }
    t1 <- fit_params(x, "del")$par
    t <- system.time(o <- optim(par = c(0.0001, 0.0098, t1, t1), fn = nlogL_zidel2, data = x, control = optim_control))
    optim_restarts[[i+1]] <- o
    optim_times[[i+1]] <- t
    best_optim <- which.min(unlist(lapply(optim_restarts, function(x) x$value)))
    o <- optim_restarts[[best_optim]]
    t <- optim_times[[best_optim]]
  }
  ######################################################
  else if (type == "zipig2") {
      optim_restarts <- list()
      optim_times <- list()
      for(i in 1:max_iter) {
          t <- system.time(o <- optim(par = c(runif(2)/2, runif(4)), fn = nlogL_zipig2, data = x, control = optim_control))
          optim_restarts[[i]] <- o
          optim_times[[i]] <- t
      }
      t1 <- fit_params(x, "pig")$par
      t <- system.time(o <- optim(par = c(0.0001, 0.0098, t1, t1), fn = nlogL_zipig2, data = x, control = optim_control))
      optim_restarts[[i+1]] <- o
      optim_times[[i+1]] <- t
      best_optim <- which.min(unlist(lapply(optim_restarts, function(x) x$value)))
      o <- optim_restarts[[best_optim]]
      t <- optim_times[[best_optim]]
  }
  ######################################################
  else if (type == "zipb2") {
    t1<- estimate_pb_optim_init_restarts(x)
    p1 <- c(0, 1, t1, t1)
    nl1 <- nlogL_zipb2(x, p1)
    p2 <- estimate_zipb2_optim_init_kmeans(x)
    nl2 <- nlogL_zipb2(x, p2)
    if(is.na(nl2)) {
      par <- p1
    } else {
      par <- if (nl1 < nl2) p1 else p2
    }
    t <- system.time(o <- optim(par = par, fn = nlogL_zipb2, data = x, control = optim_control))
  }
  ######################################################
  else {
    warning("Invalid distribution type.")
    return(NULL)
  }
  fit_param <- o
  fit_param$time <- t
  fit_param$AIC <- 2 * length(o$par) + 2 * o$value
  fit_param$BIC <- log(length(x)) * length(o$par) + 2 * o$value
  return(fit_param)
}


estimate_pb_optim_init <- function(x, iter = 200) {
  sampled_params <- c()
  n <- length(x)
  for (i in 1:iter) {
    d1 <- sample(x, n, replace = TRUE)
    e1 <- mean(d1)
    e2 <- mean(d1 * ( d1 - 1 ))
    e3 <- mean(d1 * (d1 - 1) * (d1 - 2))
    r1 <- e1
    r2 <- e2 / e1
    r3 <- e3 / e2
    x1 <- r1 * r2 - 2 * r1 * r3 + r2 * r3
    x2 <- r1 - 2 * r2 + r3
    alpha <- 2 * r1 * (r3 - r2) / x1
    if(alpha < 0 || is.infinite(alpha) || is.na(alpha))
      alpha <- runif(1)
    cm <- c(alpha, 0, max(d1))
    cm[2] <- (function(a, c, m) a * c / m - a)(cm[1], cm[3], mean(d1))
    sampled_params <- rbind(sampled_params, cm)
  }
  return(colMeans(sampled_params))
}

estimate_pb2_optim_init_kmeans <- function(x) {
  k <- kmeans(x = x, centers = 2)
  c1 <- x[which(k$cluster == 1)]
  c2 <- x[which(k$cluster == 2)]
  t1 <- tryCatch(
    estimate_pb_optim_init(c1),
    error = function(err) {
      return(runif(3, 1, 100))
    }
  )
  t2 <- tryCatch(
    estimate_pb_optim_init(c2),
    error = function(err) {
      return(runif(3, 1, 100))
    }
  )
  p <- length(c1)/length(x)
  par <- c(p,t1, t2)
  return(par)
}

estimate_zipb2_optim_init_kmeans <- function(x) {
  k <- kmeans(x = x, centers = 2)
  c1 <- x[which(k$cluster == 1)]
  c2 <- x[which(k$cluster == 2)]
  t1 <- tryCatch(
    estimate_pb_optim_init(c1),
    error = function(err) {
      return(runif(3, 1, 100))
    }
  )
  t2 <- tryCatch(
    estimate_pb_optim_init(c2),
    error = function(err) {
      return(runif(3, 1, 100))
    }
  )
  p <- length(c1)/length(x)
  par <- c(get_0inf_parameter(x), p,t1, t2)
  return(par)
}

estimate_pb_optim_init_restarts <- function(x, n = 10) {
  p <- estimate_pb_optim_init(x)
  val <- nlogL_pb(x, p)

  for (i in 1:n) {
    p_temp <- estimate_pb_optim_init(x)
    val_temp <- nlogL_pb(x, p_temp)
    if(val_temp < val) {
      p <- p_temp
      val <- val_temp
    }
  }
  return(p)
}


get_0inf_parameter <- function(x) length(c(which(x == 0))) / length(x)
