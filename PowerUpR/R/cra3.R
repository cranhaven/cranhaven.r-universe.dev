mdes.cra3r3 <- function(power=.80, alpha=.05, two.tailed=TRUE,
                             rho2, rho3, p=.50, g3=0, r21=0, r22=0, r23=0,
                             n, J, K){

  user.parms <- as.list(match.call())
  .error.handler(user.parms)

  df <- K - g3 - 2
  SSE <- sqrt(rho3*(1-r23)/(p*(1-p)*K) +
                rho2*(1-r22)/(p*(1-p)*J*K) +
                (1-rho3-rho2)*(1-r21)/(p*(1-p)*J*K*n))

  mdes <- .mdes.fun(power = power, alpha = alpha, sse = SSE, df = df, two.tailed = two.tailed)
  .summ.mdes(effect = "main", power = power, alpha = alpha, sse = SSE, df = df, two.tailed = two.tailed, mdes = mdes)
  mdes.out <- list(fun = "mdes.cra3r3",
                   parms = list(power=power, alpha=alpha, two.tailed=two.tailed,
                                rho2=rho2, rho3=rho3,
                                g3=g3, r21=r21, r22=r22, r23=r23,
                                p=p, n=n, J=J, K=K),
                   df = df,
                   ncp = mdes[1]/SSE,
                   mdes = mdes)
  class(mdes.out) <- c("main", "mdes")
  return(invisible(mdes.out))
}
# example
# mdes.cra3r3(rho3=.06, rho2=.17, n=15, J=3, K=60)
mdes.cra3 <- mdes.cra3r3

mdesd.mod331 <- mdes.mod331 <- function(power=.80, alpha=.05, two.tailed=TRUE,
                        rho2, rho3, omegam2, omegam3,
                        r21=0, r2m3=0,
                        p=.50, q=NULL, n, J, K){

  user.parms <- as.list(match.call())
  .error.handler(user.parms)

  if(omegam2 == 0) {
    df <- n*J*K - J*K - K - 2
    if(omegam3 != 0 || r2m3 != 1) {
      omegam3 <- 0
      r2m3 <- 1
      message("Arguments 'omegam3' and/or 'r2m3' are ignored")
    }
    cat("\nModerator effect: Non-randomly varying (level 2 and 3) \nModerator type:",
        ifelse(is.null(q), "Continuous\n", "Binary\n"))
  } else if(omegam3 == 0 || r2m3 == 1) {
    df <- J*K - K - 2
    cat("\nModerator effect: Non-randomly varying (level 3) \nModerator type:",
        ifelse(is.null(q), "Continuous\n", "Binary\n"))
  } else {
    df <- K - 2
    cat("\nModerator effect: Randomly varying \nModerator type:",
        ifelse(is.null(q), "Continuous\n", "Binary\n"))
  }

  SSE <- ifelse(is.null(q),
                sqrt(rho3*omegam3*(1-r2m3)/(p*(1-p)*K) +
                       rho2*omegam2/(p*(1-p)*K*J) +
                       (1-rho2-rho3)*(1-r21)/(p*(1-p)*K*J*n)), # continuous mod
                sqrt(rho3*omegam3*(1-r2m3)/(p*(1-p)*K) +
                       rho2*omegam2/(p*(1-p)*K*J) +
                       (1-rho2-rho3)*(1-r21)/(p*(1-p)*q*(1-q)*K*J*n)) # binary mod
  )

  mdes <- .mdes.fun(power = power, alpha = alpha, sse = SSE, df = df, two.tailed = two.tailed)
  .summ.mdes(effect = "mod", power = power, alpha = alpha, sse = SSE, df = df, two.tailed = two.tailed, mdes = mdes)

  mdes.out <- list(fun = "mdes.mod331",
                   parms = list(power=power, alpha=alpha, two.tailed=two.tailed,
                                rho2=rho2, rho3=rho3, omegam2=omegam2, omegam3=omegam3,
                                r21=r21, r2m3=r2m3,
                                p=p, q=q, n=n, J=J, K=K),
                   df = df,
                   ncp = mdes[1]/SSE,
                   mdes = mdes)
  class(mdes.out) <- c("mod331", "mdes")
  return(invisible(mdes.out))
}
# examples
# mdes.mod331(rho3=.05, rho2=.12, omegam2=.08, omegam3=.07, p=.4, q=.7, r21=.20, r2m3=0,  n=20, J=4, K=60)
# mdes.mod331(rho3=.05, rho2=.12, omegam2=.08, omegam3=.07, p=.4, r21=.20, r2m3=0,  n=20, J=4, K=60)
# mdes.mod331(rho3=.05, rho2=.12, omegam2=0.1, omegam3=0, p=.4, q=.7, r21=.20, r2m3=0,  n=20, J=4, K=60)
# mdes.mod331(rho3=.05, rho2=.12, omegam2=0, omegam3=0, p=.4, r21=.20, r2m3=1,  n=20, J=4, K=60)

mdesd.mod332 <- mdes.mod332 <- function(power=.80, alpha=.05, two.tailed=TRUE,
                              rho2, rho3, omegam3, r21=0, r22=0, r2m3=0,
                              p=.50, q=NULL, n, J, K){

  user.parms <- as.list(match.call())
  .error.handler(user.parms)

  if(omegam3 == 0 || r2m3 == 1) {
    df <- J*K - K - 2
    cat("\nModerator effect: Non-randomly varying \nModerator type:",
        ifelse(is.null(q), "Continuous\n", "Binary\n"))
  } else {
    df <- K - 2
    cat("\nModerator effect: Randomly varying \nModerator type:",
        ifelse(is.null(q), "Continuous\n", "Binary\n"))
  }

  SSE <- ifelse(is.null(q),
                sqrt(rho3*omegam3*(1-r2m3)/(p*(1-p)*K) +
                       rho2*(1-r22)/(p*(1-p)*K*J) +
                       (1-rho2-rho3)*(1-r21)/(p*(1-p)*K*J*n)), # continuous mod
                sqrt(rho3*omegam3*(1-r2m3)/(p*(1-p)*K) +
                       rho2*(1-r22)/(p*(1-p)*q*(1-q)*K*J) +
                       (1-rho2-rho3)*(1-r21)/(p*(1-p)*q*(1-q)*K*J*n)) # binary mod
  )

  mdes <- .mdes.fun(power = power, alpha = alpha, sse = SSE, df = df, two.tailed = two.tailed)
  .summ.mdes(effect = "mod", power = power, alpha = alpha, sse = SSE, df = df, two.tailed = two.tailed, mdes = mdes)

  mdes.out <- list(fun = "mdes.mod332",
                   parms = list(power=power, alpha=alpha, two.tailed=two.tailed,
                                rho2=rho2, rho3=rho3, omegam3=omegam3,
                                r21=r21, r22=r22, r2m3=r2m3,
                                p=p, q=q, n=n, J=J, K=K),
                   df = df,
                   ncp = mdes[1]/SSE,
                   mdes = mdes)
  class(mdes.out) <- c("mod332", "mdes")
  return(invisible(mdes.out))
}
# examples
# mdes.mod332(rho3=.1, rho2=.1, omegam3=.05, q=.5, r21=.30, r22=.4, r2m3=0,  n=20, J=4, K=60)
# mdes.mod332(rho3=.1, rho2=.1, omegam3=.05, r21=.30, r22=.4, r2m3=0,  n=20, J=4, K=60)
# mdes.mod332(rho3=.1, rho2=.1, omegam3=0, q=.5, r21=.30, r22=.4, r2m3=0,  n=20, J=4, K=60)
# mdes.mod332(rho3=.1, rho2=.1, omegam3=0, r21=.30, r22=.4, r2m3=0,  n=20, J=4, K=60)

mdesd.mod333 <- mdes.mod333 <- function(power=.80, alpha=.05, two.tailed=TRUE,
                             rho2, rho3, r21=0, r22=0, r23=0,
                             p=.50, q=NULL, n, J, K){

  user.parms <- as.list(match.call())
  .error.handler(user.parms)

  cat(ifelse(is.null(q), "\nModerator type: Continuous\n", "\nModerator type: Binary\n"))

  df <- K - 5
  SSE <- ifelse(is.null(q),
                sqrt(rho3*(1-r23)/(p*(1-p)*(K-5)) +
                       rho2*(1-r22)/(p*(1-p)*J*(K-5)) +
                       (1-rho3-rho2)*(1-r21)/(p*(1-p)*J*(K-5)*n)), # continuous mod
                sqrt(rho3*(1-r23)/(p*(1-p)*q*(1-q)*(K-5)) +
                       rho2*(1-r22)/(p*(1-p)*q*(1-q)*J*(K-5)) +
                       (1-rho3-rho2)*(1-r21)/(p*(1-p)*q*(1-q)*J*(K-5)*n)) # binary mod
  )

  mdes <- .mdes.fun(power = power, alpha = alpha, sse = SSE, df = df, two.tailed = two.tailed)
  .summ.mdes(effect = "mod", power = power, alpha = alpha, sse = SSE, df = df, two.tailed = two.tailed, mdes = mdes)

  mdes.out <- list(fun = "mdes.mod333",
                   parms = list(power=power, alpha=alpha, two.tailed=two.tailed,
                                rho2=rho2, rho3=rho3, r21=r21, r22=r22, r23=r23,
                                p=p, q=q, n=n, J=J, K=K),
                   df = df,
                   ncp = mdes[1]/SSE,
                   mdes = mdes)
  class(mdes.out) <- c("mod333", "mdes")
  return(invisible(mdes.out))
}
# examples
# mdes.mod333(rho3=.1, rho2=.1, q=.5, r21=.3, r22=.4, r23=.5, n=20, J=4, K=60)
# mdes.mod333(rho3=.1, rho2=.1, r21=.3, r22=.4, r23=.5, n=20, J=4, K=60)

power.cra3r3 <- function(es=.25, alpha=.05, two.tailed=TRUE,
                         rho2, rho3, g3=0, r21=0, r22=0, r23=0,
                         p=.50, n, J, K){

  user.parms <- as.list(match.call())
  .error.handler(user.parms)

  df <- K - g3 - 2
  SSE <- sqrt(rho3*(1-r23)/(p*(1-p)*K) +
                rho2*(1-r22)/(p*(1-p)*J*K) +
                (1-rho3-rho2)*(1-r21)/(p*(1-p)*J*K*n))

  power <- .power.fun(es = es, alpha = alpha, sse = SSE, df = df, two.tailed = two.tailed)
  .summ.power(power = power, alpha = alpha, sse = SSE, df = df, two.tailed = two.tailed, es = es)
  power.out <-  list(fun = "power.cra3r3",
                     parms = list(es=es, alpha=alpha, two.tailed=two.tailed,
                                  rho2=rho2, rho3=rho3,
                                  g3=g3, r21=r21, r22=r22, r23=r23,
                                  p=p, n=n, J=J, K=K),
                     df = df,
                     ncp = es/SSE,
                     power = power)
  class(power.out) <- c("main", "power")
  return(invisible(power.out))
}
# example
# power.cra3r3(es=.269, rho3=.06, rho2=.17, n=15, J=3, K=60)
power.cra3 <- power.cra3r3

power.mod331 <- function(es=.25, alpha=.05, two.tailed=TRUE,
                               rho2, rho3, omegam2, omegam3,
                               r21=0, r2m3=0,
                               p=.50, q=NULL, n, J, K){

  user.parms <- as.list(match.call())
  .error.handler(user.parms)

  if(omegam2 == 0) {
    df <- n*J*K - J*K - K - 2
    if(omegam3 != 0 || r2m3 != 1) {
      omegam3 <- 0
      r2m3 <- 1
      message("Arguments 'omegam3' and/or 'r2m3' are ignored")
    }
    cat("\nModerator effect: Non-randomly varying (level 2 and 3) \nModerator type:",
        ifelse(is.null(q), "Continuous\n", "Binary\n"))
  } else if(omegam3 == 0 || r2m3 == 1) {
    df <- J*K - K - 2
    cat("\nModerator effect: Non-randomly varying (level 3) \nModerator type:",
        ifelse(is.null(q), "Continuous\n", "Binary\n"))
  } else {
    df <- K - 2
    cat("\nModerator effect: Randomly varying \nModerator type:",
        ifelse(is.null(q), "Continuous\n", "Binary\n"))
  }

  SSE <- ifelse(is.null(q),
                sqrt(rho3*omegam3*(1-r2m3)/(p*(1-p)*K) +
                       rho2*omegam2/(p*(1-p)*K*J) +
                       (1-rho2-rho3)*(1-r21)/(p*(1-p)*K*J*n)), # continuous mod
                sqrt(rho3*omegam3*(1-r2m3)/(p*(1-p)*K) +
                       rho2*omegam2/(p*(1-p)*K*J) +
                       (1-rho2-rho3)*(1-r21)/(p*(1-p)*q*(1-q)*K*J*n)) # binary mod
  )

  power <- .power.fun(es = es, alpha = alpha, sse = SSE, df = df, two.tailed = two.tailed)
  .summ.power(power = power, alpha = alpha, sse = SSE, df = df, two.tailed = two.tailed, es = es)

  power.out <- list(fun = "power.mod331",
                   parms = list(es=es, alpha=alpha, two.tailed=two.tailed,
                                rho2=rho2, rho3=rho3, omegam2=omegam2, omegam3=omegam3,
                                r21=r21, r2m3=r2m3,
                                p=p, q=q, n=n, J=J, K=K),
                   df = df,
                   ncp = es/SSE,
                   power = power)
  class(power.out) <- c("mod331", "power")
  return(invisible(power.out))
}
# examples
# power.mod331(es=.16, rho3=.05, rho2=.12, omegam2=.08, omegam3=.07, p=.4, q=.7, r21=.20, r2m3=0,  n=20, J=4, K=60)
# power.mod331(es=.09, rho3=.05, rho2=.12, omegam2=.08, omegam3=.07, p=.4, r21=.20, r2m3=0,  n=20, J=4, K=60)
# power.mod331(es=.16, rho3=.05, rho2=.12, omegam2=0, omegam3=.07, p=.4, q=.7, r21=.20, r2m3=0,  n=20, J=4, K=60)
# power.mod331(es=.09, rho3=.05, rho2=.12, omegam2=0, omegam3=.07, p=.4, r21=.20, r2m3=0,  n=20, J=4, K=60)
# power.mod331(es=.16, rho3=.05, rho2=.12, omegam2=.08, omegam3=0, p=.4, q=.7, r21=.20, r2m3=0,  n=20, J=4, K=60)
# power.mod331(es=.09, rho3=.05, rho2=.12, omegam2=.08, omegam3=0, p=.4, r21=.20, r2m3=0,  n=20, J=4, K=60)

power.mod332 <- function(es=.25, alpha=.05, two.tailed=TRUE,
                               rho2, rho3, omegam3, r21=0, r22=0, r2m3=0,
                               p=.50, q=NULL, n, J, K){

  user.parms <- as.list(match.call())
  .error.handler(user.parms)

  if(omegam3 == 0 || r2m3 == 1) {
    df <- J*K - K - 2
    cat("\nModerator effect: Non-randomly varying \nModerator type:",
        ifelse(is.null(q), "Continuous\n", "Binary\n"))
  } else {
    df <- K - 2
    cat("\nModerator effect: Randomly varying \nModerator type:",
        ifelse(is.null(q), "Continuous\n", "Binary\n"))
  }

  SSE <- ifelse(is.null(q),
                sqrt(rho3*omegam3*(1-r2m3)/(p*(1-p)*K) +
                       rho2*(1-r22)/(p*(1-p)*K*J) +
                       (1-rho2-rho3)*(1-r21)/(p*(1-p)*K*J*n)), # continuous mod
                sqrt(rho3*omegam3*(1-r2m3)/(p*(1-p)*K) +
                       rho2*(1-r22)/(p*(1-p)*q*(1-q)*K*J) +
                       (1-rho2-rho3)*(1-r21)/(p*(1-p)*q*(1-q)*K*J*n)) # binary mod
  )

  power <- .power.fun(es = es, alpha = alpha, sse = SSE, df = df, two.tailed = two.tailed)
  .summ.power(power = power, alpha = alpha, sse = SSE, df = df, two.tailed = two.tailed, es = es)

  power.out <- list(fun = "power.mod332",
                   parms = list(es=es, alpha=alpha, two.tailed=two.tailed,
                                rho2=rho2, rho3=rho3, omegam3=omegam3,
                                r21=r21, r22=r22, r2m3=r2m3,
                                p=p, q=q, n=n, J=J, K=K),
                   df = df,
                   ncp = es/SSE,
                   power = power)
  class(power.out) <- c("mod332", "power")
  return(invisible(power.out))
}
# examples
# power.mod332(es=.22, rho3=.1, rho2=.1, omegam3=.05, q=.5, r21=.30, r22=.4, r2m3=0,  n=20, J=4, K=60)
# power.mod332(es=.11, rho3=.1, rho2=.1, omegam3=.05, r21=.30, r22=.4, r2m3=0,  n=20, J=4, K=60)
# power.mod332(es=.22, rho3=.1, rho2=.1, omegam3=0, q=.5, r21=.30, r22=.4, r2m3=0,  n=20, J=4, K=60)
# power.mod332(es=.11, rho3=.1, rho2=.1, omegam3=0, r21=.30, r22=.4, r2m3=0,  n=20, J=4, K=60)

power.mod333 <- function(es=.25, alpha=.05, two.tailed=TRUE,
                              rho2, rho3, r21=0, r22=0, r23=0,
                              p=.50, q=NULL, n, J, K){

  user.parms <- as.list(match.call())
  .error.handler(user.parms)

  cat(ifelse(is.null(q), "\nModerator type: Continuous\n", "\nModerator type: Binary\n"))

  df <- K-5
  SSE <- ifelse(is.null(q),
                sqrt(rho3*(1-r23)/(p*(1-p)*(K-5)) +
                       rho2*(1-r22)/(p*(1-p)*J*(K-5)) +
                       (1-rho3-rho2)*(1-r21)/(p*(1-p)*J*(K-5)*n)), # continuous mod
                sqrt(rho3*(1-r23)/(p*(1-p)*q*(1-q)*(K-5)) +
                       rho2*(1-r22)/(p*(1-p)*q*(1-q)*J*(K-5)) +
                       (1-rho3-rho2)*(1-r21)/(p*(1-p)*q*(1-q)*J*(K-5)*n)) # binary mod
  )

  power <- .power.fun(es = es, alpha = alpha, sse = SSE, df = df, two.tailed = two.tailed)
  .summ.power(power = power, alpha = alpha, sse = SSE, df = df, two.tailed = two.tailed, es = es)

  power.out <- list(fun = "power.mod333",
                   parms = list(es=es, alpha=alpha, two.tailed=two.tailed,
                                rho2=rho2, rho3=rho3,
                                r21=r21, r22=r22, r23=r23,
                                p=p, q=q, n=n, J=J, K=K),
                   df = df,
                   ncp = es/SSE,
                   power = power)
  class(power.out) <- c("mod333", "power")
  return(invisible(power.out))
}
# examples
# power.mod333(es=.30, rho3=.1, rho2=.1, q=.5, r21=.3, r22=.4, r23=.5, n=20, J=4, K=60)
# power.mod333(es=.15, rho3=.1, rho2=.1, r21=.3, r22=.4, r23=.5, n=20, J=4, K=60)

power.med331 <- function(esa, esB, two.tailed = TRUE, alpha = .05,
                         mc = TRUE, nsims = 1000, ndraws = 1000,
                         rho2, rho3, gm3 = 4, r2m3 = 0,
                         r21 = 0, r22 = 0, g3 = 5, r23 = 0,
                         p = .50, n, J, K) {

  user.parms <- as.list(match.call())
  .error.handler(user.parms)

  # standard errors for 3-3-1 mediation
  .se.a331 <- function(r2m3, gm3, p, K) {
    sig2m <- 1
    var.a331 <- sig2m * (1 - r2m3) /
      (p * (1 - p) * (K - gm3 - 1))
    if(is.nan(var.a331) | var.a331 <= 0) {
      stop("Design is not feasible", call. = FALSE)
    }
    return(invisible(sqrt(var.a331)))
  }

  .se.B331 <- function(rho2, rho3, r2m3, r21, r22, g3, r23, n, J, K) {
    sig2m <- 1
    var.B331 <- (rho3 * (1 - r23) + rho2 * (1 - r22) / J + (1 - rho3 - rho2) * (1 - r21) / (n * J)) /
      ((K - g3 - 1) * sig2m * (1 - r2m3))

    if(is.nan(var.B331) | var.B331 <= 0) {
      stop("Design is not feasible", call. = FALSE)
    }
    return(invisible(sqrt(var.B331)))
  }

  dfa <- K - gm3 - 1
  dfB <- K - g3 - 1
  df <- rbind(dfa, dfB)
  colnames(df) <- "df"
  rownames(df) <- c("a", "B")

  sea331 <- .se.a331(r2m3 = r2m3, gm3 = gm3, p = p, K = K)
  seB331 <- .se.B331(rho2 = rho2, rho3 = rho3, r2m3 = r2m3,
                     r21 = r21, r22 = r22, g3 = g3, r23 = r23,
                     n = n, J = J, K = K)
  ncpa <- esa/sea331
  ncpB <- esB/seB331
  ncp <- rbind(ncpa, ncpB)
  colnames(ncp) <- "ncp"
  rownames(ncp) <- c("a", "B")

  powera <- .power.fun(es = esa, alpha = alpha, sse = sea331, two.tailed = two.tailed, df = dfa)
  powerB <- .power.fun(es = esB, alpha = alpha, sse = seB331, two.tailed = two.tailed, df = dfB)
  power.sobel.aB <- .power.sobel(x = esa, y = esB, sex = sea331, sey = seB331, alpha = alpha, two.tailed = two.tailed)
  power.joint.aB <- .power.jt(x = esa, y = esB, sex = sea331, sey = seB331, alpha = alpha, dfx = dfa, dfy = dfB, two.tailed = two.tailed)
  power.mc.aB <- ifelse(mc, .power.mc(nsims = nsims, ndraws = ndraws, x = esa, y = esB, sex = sea331, sey = seB331, alpha = alpha, two.tailed = two.tailed), NA)

  power <- rbind(
    c(round(powera, 3), NA, NA, NA),
    c(round(powerB, 3), NA, NA, NA),
    c(NA, round(power.sobel.aB, 3), round(power.joint.aB, 3), round(power.mc.aB, 3))
  )
  colnames(power) <- c("t", "sobel", "joint", "mc")
  rownames(power) <- c("a", "B", "aB")

  power.out <- list(fun = "power.med331",
                    parms = list(esa = esa, esB = esB,
                                 two.tailed = two.tailed, alpha = alpha,
                                 mc = mc, nsims = nsims, ndraws = ndraws,
                                 rho2 = rho2, rho3 = rho3, gm3 = gm3, r2m3 = r2m3,
                                 r21 = r21, r22 = r22, g3 = g3, r23 = r23,
                                 p = p, n = n, J = J, K = K),
                    df = df,
                    ncp = ncp,
                    power = round(power, 3))
  cat("Statistical power: \n")
  cat("------------------------------------ \n")
  print(power)
  cat("------------------------------------ \n")
  cat("Degrees of freedom for path a:", dfa,
      "\nDegrees of freedom for path B:", dfB,
      "\nStandardized standard error for path a:", round(sea331, 3),
      "\nStandardized standard error for path B:", round(seB331, 3),
      "\nType I error rate:", alpha,
      "\nTwo-tailed test:", two.tailed, "\n")
  class(power.out) <- c("power", "med331")
  return(invisible(power.out))
}
# example
# power.med331(esa= .5, esB = .3, rho2 = .15, rho3 = .15,
#              r21 = .2, r22 = .2, g3 = 4,
#              n = 20, J = 4, K = 80, p = .5, mc = TRUE)

power.med321 <- function(esa, esB, two.tailed = TRUE, alpha = .05,
                         mc = TRUE, nsims = 1000, ndraws = 1000,
                         rhom3, rho2, rho3,
                         r2m2 = 0, gm3 = 4, r2m3 = 0,
                         r21 = 0, r22 = 0, g3 = 5, r23 = 0,
                         p = .50, n, J, K) {

  user.parms <- as.list(match.call())
  .error.handler(user.parms)

  # standard errors for 3-2-1 mediation
  .se.a321 <- function(rhom3, r2m2, r2m3, gm3, p, J, K) {
    var.a321 <- (rhom3 * (1 - r2m3) + (1 - rhom3) * (1 - r2m2) / J) /
      (p * (1 - p) * (K - gm3 - 1))
    if(is.nan(var.a321) | var.a321 <= 0) {
      stop("Design is not feasible", call. = FALSE)
    }
    return(invisible(sqrt(var.a321)))
  }

  .se.B321 <- function(rhom3, rho2, rho3, r2m2, r2m3, r21, r22, g3, r23, n, J, K) {
    var.B321 <- (rho3 * (1 - r23) + rho2 * (1 - r22) / J + (1 - rho3 - rho2) * (1 - r21) / (n * J)) /
      ((K - g3 - 1) * (rhom3 * (1 - r2m3) + (1 - rhom3) * (1 - r2m2) / J))
    if(is.nan(var.B321) | var.B321 <= 0) {
      stop("Design is not feasible", call. = FALSE)
    }
    return(invisible(sqrt(var.B321)))
  }


  dfa <- K - gm3 - 1
  dfB <- K - g3 - 1
  df <- rbind(dfa, dfB)
  colnames(df) <- "df"
  rownames(df) <- c("a", "B")

  sea321 <- .se.a321(rhom3 = rhom3, r2m2 = r2m2, gm3 = gm3, r2m3 = r2m3, p = p, J = J, K = K)
  seB321 <- .se.B321(rhom3 = rhom3, rho2 = rho2, rho3 = rho3, r2m2 = r2m2, g3 = g3, r2m3 = r2m3,
                     r21 = r21, r22 = r22, r23 = r23, n = n, J = J, K = K)
  ncpa <- esa/sea321
  ncpB <- esB/seB321
  ncp <- rbind(ncpa, ncpB)
  colnames(ncp) <- "ncp"
  rownames(ncp) <- c("a", "B")

  powera <- .power.fun(es = esa, alpha = alpha, sse = sea321, two.tailed = two.tailed, df = dfa)
  powerB <- .power.fun(es = esB, alpha = alpha, sse = seB321, two.tailed = two.tailed, df = dfB)
  power.sobel.aB <- .power.sobel(x = esa, y = esB, sex = sea321, sey = seB321, alpha = alpha, two.tailed = two.tailed)
  power.joint.aB <- .power.jt(x = esa, y = esB, sex = sea321, sey = seB321, alpha = alpha, dfx = dfa, dfy = dfB, two.tailed = two.tailed)
  power.mc.aB <- ifelse(mc, .power.mc(nsims = nsims, ndraws = ndraws, x = esa, y =esB, sex = sea321, sey = seB321, alpha = alpha, two.tailed = two.tailed), NA)

  power <- rbind(
    c(round(powera, 3), NA, NA, NA),
    c(round(powerB, 3), NA, NA, NA),
    c(NA, round(power.sobel.aB, 3), round(power.joint.aB, 3), round(power.mc.aB, 3))
  )
  colnames(power) <- c("t", "sobel", "joint", "mc")
  rownames(power) <- c("a", "B", "aB")

  power.out <- list(fun = "power.med321",
                    parms = list(esa = esa, esB = esB,
                                 two.tailed = two.tailed, alpha = alpha,
                                 mc = mc, nsims = nsims, ndraws = ndraws,
                                 rhom3 = rhom3, rho2 = rho2, rho3 = rho3,
                                 r2m2 = r2m2, gm3 = gm3, r2m3 = r2m3,
                                 r21 = r21, r22 = r22, g3 = g3, r23 = r23,
                                 p = p, n = n, J = J, K = K),
                    df = df,
                    ncp = ncp,
                    power = round(power, 3))
  cat("Statistical power: \n")
  cat("------------------------------------ \n")
  print(power)
  cat("------------------------------------ \n")
  cat("Degrees of freedom for path a:", dfa,
      "\nDegrees of freedom for path B:", dfB,
      "\nStandardized standard error for path a:", round(sea321, 3),
      "\nStandardized standard error for path B:", round(seB321, 3),
      "\nType I error rate:", alpha,
      "\nTwo-tailed test:", two.tailed, "\n")
  class(power.out) <- c("power", "med321")
  return(invisible(power.out))
}
# example
# power.med321(esa= .49, esB = .30, rhom3 = 0.26, rho2 = .15, rho3 = .20,
#              r2m2 = .07, r2m3 = .17, r21 = .02, r22 = .41, r23 = .38,
#              p = .50, n = 20, J = 4, K = 30)

power.med311 <- function(esa, esB, two.tailed = TRUE, alpha = .05,
                         mc = TRUE, nsims = 1000, ndraws = 1000,
                         rhom2, rhom3, rho2, rho3,
                         r2m1 = 0, r2m2 = 0, gm3 = 4, r2m3 = 0,
                         r21 = 0, r22 = 0, g3 = 5, r23 = 0,
                         p = .50, n, J, K) {

  user.parms <- as.list(match.call())
  .error.handler(user.parms)

  # standard errors for 3-1-1 mediation
  .se.a311 <- function(rhom2, rhom3, r2m1, gm3, r2m2, r2m3, p, n, J, K) { # r2m3z,
    var.a311 <- (rhom3 * (1 - r2m3) + rhom2 * (1 - r2m2) / J + (1 - rhom3 - rhom2) * (1 - r2m1) / (n*J)) /
      (p * (1 - p) * (K - gm3 - 1))
    if(is.nan(var.a311) | var.a311 <= 0) {
      stop("Design is not feasible", call. = FALSE)
    }
    return(invisible(sqrt(var.a311)))
  }

  .se.B311 <- function(rho2, rho3, rhom2, rhom3,
                       r2m1, r2m2, r2m3, r21, r22, g3, r23,
                       n, J, K) {
    var.B311 <- (rho3 * (1 - r23) + rho2 * (1 - r22) / J + (1 - rho3 - rho2) * (1 - r21) / (n * J)) /
      ((K - g3 - 1) * (rhom3 * (1 - r2m3) + rhom2 * (1 - r2m2) / J + (1 - rhom3 - rhom2) * (1 - r2m3) / (n*J)))
    if(is.nan(var.B311) | var.B311 <= 0) {
      stop("Design is not feasible", call. = FALSE)
    }
    return(invisible(sqrt(var.B311)))
  }

  dfa <- K - gm3 - 1
  dfB <- K - g3 - 1
  df <- rbind(dfa, dfB)
  colnames(df) <- "df"
  rownames(df) <- c("a", "B")

  sea311 <- .se.a311(rhom2 = rhom2, rhom3 = rhom3, r2m1 = r2m1, r2m2 = r2m2, r2m3 = r2m3,
                     gm3 = gm3, p = p, n = n, J = J, K = K)
  seB311 <- .se.B311(rho2 = rho2, rho3 = rho3, rhom2 = rhom2, rhom3 = rhom3,
                     r2m1 = r2m1, r2m2 = r2m2, r2m3 = r2m3, r21 = r21, r22 = r22, g3 = g3, r23 = r23,
                     n = n, J = J, K = K)
  ncpa <- esa/sea311
  ncpB <- esB/seB311
  ncp <- rbind(ncpa, ncpB)
  colnames(ncp) <- "ncp"
  rownames(ncp) <- c("a", "B")


  powera <- .power.fun(es = esa, alpha = alpha, sse = sea311, two.tailed = two.tailed, df = dfa)
  powerB <- .power.fun(es = esB, alpha = alpha, sse = seB311, two.tailed = two.tailed, df = dfB)
  power.sobel.aB <- .power.sobel(x = esa, y = esB, sex = sea311, sey = seB311, alpha = alpha, two.tailed = two.tailed)
  power.joint.aB <- .power.jt(x = esa, y = esB, sex = sea311, sey = seB311, alpha = alpha, dfx = dfa, dfy = dfB, two.tailed = two.tailed)
  power.mc.aB <- ifelse(mc, .power.mc(nsims = nsims, ndraws = ndraws, x = esa, y = esB, sex = sea311, sey = seB311, alpha = alpha, two.tailed = two.tailed), NA)

  power <- rbind(
    c(round(powera, 3), NA, NA, NA),
    c(round(powerB, 3), NA, NA, NA),
    c(NA, round(power.sobel.aB, 3), round(power.joint.aB, 3), round(power.mc.aB, 3))
  )
  colnames(power) <- c("t", "sobel", "joint", "mc")
  rownames(power) <- c("a", "B", "aB")

  power.out <- list(fun = "power.med311",
                    parms = list(esa = esa, esB = esB,
                                 two.tailed = two.tailed, alpha = alpha,
                                 mc = mc, nsims = nsims, ndraws = ndraws,
                                 rhom2 = rhom2, rhom3 = rhom3, rho2 = rho2, rho3 = rho3,
                                 r2m1 = r2m1, r2m2 = r2m2, r2m3 = r2m3, gm3 = gm3,
                                 r21 = r21, r22 = r22, r23 = r23, g3 = g3,
                                 p = p, n = n, J = J, K = K),
                    df = df,
                    ncp = ncp,
                    power = round(power, 3))
  cat("Statistical power: \n")
  cat("------------------------------------ \n")
  print(power)
  cat("------------------------------------ \n")
  cat("Degrees of freedom for path a:", dfa,
      "\nDegrees of freedom for path B:", dfB,
      "\nStandardized standard error for path a:", round(sea311, 3),
      "\nStandardized standard error for path B:", round(seB311, 3),
      "\nType I error rate:", alpha,
      "\nTwo-tailed test:", two.tailed, "\n")
  class(power.out) <- c("power", "med311")
  return(invisible(power.out))
}

# example
# power.med311(esa= .49 , esB = .30, two.tailed = TRUE, alpha = .05,
#              mc = TRUE, nsims = 1000, ndraws = 1000,
#              rhom2 = .05, rhom3 = .26, rho2 = .15, rho3 = .20,
#              r2m1 = .10, r2m2 = .07, r2m3 = .17, r21 = .02, r22 = .41, r23 = .38,
#              p = .50, n = 20, J = 4, K = 30)

mrss.cra3r3 <- function(es=.25, power=.80, alpha=.05, two.tailed=TRUE,
                        n, J, K0=10, tol=.10,
                        rho2, rho3, p=.50, g3=0, r21=0, r22=0, r23=0){

  user.parms <- as.list(match.call())
  .error.handler(user.parms)

  i <- 0
  conv <- FALSE
  while(i<=100 & conv==FALSE){
    df <- K0-g3-2
    if(df<= 0 | is.infinite(df)){break}
    T1 <- ifelse(two.tailed==TRUE,abs(qt(alpha/2,df)),abs(qt(alpha,df)))
    T2 <- abs(qt(power,df))
    M <- ifelse(power>=.5,T1+T2,T1-T2)
    K1 <- (M/es)^2 * (rho3*(1-r23)/(p*(1-p)) +
                          rho2*(1-r22)/(p*(1-p)*J) +
                          (1-rho3-rho2)*(1-r21)/(p*(1-p)*J*n))
    K0 <- (K1+K0)/2
    i <- i+1
  }
  K <- ifelse(df>0,round(K0),NA)

  mrss.out <-  list(fun = "mrss.cra3r3",
                    parms = list(es=es, power=power, alpha=alpha, two.tailed=two.tailed,
                                 n=n, J=J, K0=K0, tol=tol,
                                 rho2=rho2, rho3=rho3,
                                 p=p, r21=r21, r22=r22, r23=r23, g3=g3),
                    df = df,
                    ncp = M,
                    K = K)
  class(mrss.out) <- c("main", "mrss")
  cat("K =", K, "\n")
  return(invisible(mrss.out))
}
# example
# mrss.cra3r3(rho3=.06, rho2=.17, n=15, J=3)
mrss.cra3 <- mrss.cra3r3

mrss.mod331 <- function(es=.25, power=.80, alpha=.05, two.tailed=TRUE,
                              rho2, rho3, omegam2, omegam3,
                              r21=0, r2m3=0,
                              p=.50, q=NULL, n, J, K0=10, tol=.10){

  user.parms <- as.list(match.call())
  .error.handler(user.parms)

  i <- 0
  conv <- FALSE
  while(i<=100 & conv==FALSE){
    if(omegam2 == 0) {
      df <- n*J*K0 - J*K0 - K0 - 2
      if(omegam3 != 0 || r2m3 != 1) {
        omegam3 <- 0
        r2m3 <- 1
      }
    } else if(omegam3 == 0 || r2m3 == 1) {
      df <- J*K0 - K0 - 2
    } else {
      df <- K0 - 2
    }
    if(df<= 0 | is.infinite(df)){break}
    T1 <- ifelse(two.tailed==TRUE,abs(qt(alpha/2,df)),abs(qt(alpha,df)))
    T2 <- abs(qt(power,df))
    M <- ifelse(power>=.5,T1+T2,T1-T2)
    K1 <- ifelse(is.null(q),
                 (M/es)^2 * (rho3*omegam3*(1-r2m3)/(p*(1-p)) +
                               rho2*omegam2/(p*(1-p)*J) +
                               (1-rho2-rho3)*(1-r21)/(p*(1-p)*J*n)), # continuous mod
                 (M/es)^2 * (rho3*omegam3*(1-r2m3)/(p*(1-p)) +
                               rho2*omegam2/(p*(1-p)*J) +
                               (1-rho2-rho3)*(1-r21)/(p*(1-p)*q*(1-q)*J*n))  # binary mod
    )
    K0 <- (K1+K0)/2
    i <- i+1
  }
  K <- ifelse(df>0,round(K0),NA)

  mrss.out <- list(fun = "mrss.mod331",
                   parms = list(es=es, power=power, alpha=alpha, two.tailed=two.tailed,
                                rho2=rho2, rho3=rho3, omegam2=omegam2, omegam3=omegam3,
                                r21=r21, r2m3=r2m3,
                                p=p, q=q, n=n, J=J, K0=K0, tol=tol),
                   df = df,
                   ncp = M,
                   K = K)

  if(omegam2 == 0) {
    if(omegam3 != 0 || r2m3 != 1) {
      message("Arguments 'omegam3' and/or 'r2m3' are ignored")
    }
    cat("\nModerator effect: Non-randomly varying (level 2 and 3) \nModerator type:",
        ifelse(is.null(q), "Continuous\n", "Binary\n"),
        "\nK =", K)
  } else if(omegam3 == 0 || r2m3 == 1) {
    cat("\nModerator effect: Non-randomly varying (level 3) \nModerator type:",
        ifelse(is.null(q), "Continuous\n", "Binary\n"),
        "\nK =", K)
  } else {
    cat("\nModerator effect: Randomly varying \nModerator type:",
        ifelse(is.null(q), "Continuous\n", "Binary\n"),
        "\nK =", K)
  }


  class(mrss.out) <- c("mod331", "mrss")
  return(invisible(mrss.out))
}
# examples
# mrss.mod331(es=.16, rho3=.05, rho2=.12, omegam2=.08, omegam3=.07, p=.4, q=.7, r21=.20, r2m3=0,  n=20, J=4)
# mrss.mod331(es=.09, rho3=.05, rho2=.12, omegam2=.08, omegam3=.07, p=.4, r21=.20, r2m3=0,  n=20, J=4)
# mrss.mod331(es=.16, rho3=.05, rho2=.12, omegam2=0, omegam3=.07, p=.4, q=.7, r21=.20, r2m3=0,  n=20, J=4)
# mrss.mod331(es=.09, rho3=.05, rho2=.12, omegam2=0, omegam3=.07, p=.4, r21=.20, r2m3=0,  n=20, J=4)
# mrss.mod331(es=.16, rho3=.05, rho2=.12, omegam2=.08, omegam3=0, p=.4, q=.7, r21=.20, r2m3=0,  n=20, J=4)
# mrss.mod331(es=.09, rho3=.05, rho2=.12, omegam2=.08, omegam3=0, p=.4, r21=.20, r2m3=0,  n=20, J=4)

mrss.mod332 <- function(es=.25, power=.80, alpha=.05, two.tailed=TRUE,
                              rho2, rho3, omegam3, r21=0, r22=0, r2m3=0,
                              p=.50, q=NULL, n, J, K0=10, tol=.10){

  user.parms <- as.list(match.call())
  .error.handler(user.parms)

  i <- 0
  conv <- FALSE
  while(i<=100 & conv==FALSE){
    if(omegam3 == 0 || r2m3 == 1) {
      df <- J*K0 - K0 - 2
    } else {
      df <- K0 - 2
    }
    if(df<= 0 | is.infinite(df)){break}
    T1 <- ifelse(two.tailed==TRUE,abs(qt(alpha/2,df)),abs(qt(alpha,df)))
    T2 <- abs(qt(power,df))
    M <- ifelse(power>=.5,T1+T2,T1-T2)
    K1 <- ifelse(is.null(q),
                 (M/es)^2 * (rho3*omegam3*(1-r2m3)/(p*(1-p)) +
                               rho2*(1-r22)/(p*(1-p)*J) +
                               (1-rho2-rho3)*(1-r21)/(p*(1-p)*J*n)), # continuous mod
                 (M/es)^2 * (rho3*omegam3*(1-r2m3)/(p*(1-p)) +
                               rho2*(1-r22)/(p*(1-p)*q*(1-q)*J) +
                               (1-rho2-rho3)*(1-r21)/(p*(1-p)*q*(1-q)*J*n)) # binary mod
    )
    K0 <- (K1+K0)/2
    i <- i+1
  }
  K <- ifelse(df>0,round(K0),NA)

  mrss.out <- list(fun = "mrss.mod332",
                   parms = list(es=es, power=power, alpha=alpha, two.tailed=two.tailed,
                                rho2=rho2, rho3=rho3, omegam3=omegam3,
                                r21=r21, r22=r22, r2m3=r2m3,
                                p=p, q=q, n=n, J=J, K0=K0, tol=tol),
                   df = df,
                   ncp = M,
                   K = K)

  if(omegam3 == 0 || r2m3 == 1) {
    cat("\nModerator effect: Non-randomly varying \nModerator type:",
        ifelse(is.null(q), "Continuous\n", "Binary\n"),
        "\nK =", K)
  } else {
    cat("\nModerator effect: Randomly varying \nModerator type:",
        ifelse(is.null(q), "Continuous\n", "Binary\n"),
        "\nK =", K)
  }

  class(mrss.out) <- c("mod332", "mrss")
  return(invisible(mrss.out))
}
# examples
# mrss.mod332(es=.22, rho3=.1, rho2=.1, omegam3=.05, q=.5, r21=.30, r22=.4, r2m3=0,  n=20, J=4)
# mrss.mod332(es=.11, rho3=.1, rho2=.1, omegam3=.05, r21=.30, r22=.4, r2m3=0,  n=20, J=4)
# mrss.mod332(es=.22, rho3=.1, rho2=.1, omegam3=0, q=.2, r21=.30, r22=.4, r2m3=0,  n=20, J=4)
# mrss.mod332(es=.11, rho3=.1, rho2=.1, omegam3=0, r21=.30, r22=.4, r2m3=0,  n=20, J=4)

mrss.mod333 <- function(es=.25, power=.80, alpha=.05, two.tailed=TRUE,
                             rho2, rho3, r21=0, r22=0, r23=0,
                             p=.50, q=NULL, n, J, K0=10, tol=.10){

  user.parms <- as.list(match.call())
  .error.handler(user.parms)

  i <- 0
  conv <- FALSE
  while(i<=100 & conv==FALSE){
    df <- K0 - 5
    if(df<= 0 | is.infinite(df)){break}
    T1 <- ifelse(two.tailed==TRUE,abs(qt(alpha/2,df)),abs(qt(alpha,df)))
    T2 <- abs(qt(power,df))
    M <- ifelse(power>=.5,T1+T2,T1-T2)
    K1 <- ifelse(is.null(q),
                 5 + (M/es)^2 * (rho3*(1-r23)/(p*(1-p)) +
                                        rho2*(1-r22)/(p*(1-p)*J) +
                                        (1-rho3-rho2)*(1-r21)/(p*(1-p)*J*n)), # continuous mod
                 5 + (M/es)^2 * (rho3*(1-r23)/(p*(1-p)*q*(1-q)) +
                                        rho2*(1-r22)/(p*(1-p)*q*(1-q)*J) +
                                        (1-rho3-rho2)*(1-r21)/(p*(1-p)*q*(1-q)*J*n)) # binary mod
    )
    K0 <- (K1+K0)/2
    i <- i+1
  }
  K <- ifelse(df>0,round(K0),NA)

  mrss.out <- list(fun = "mrss.mod333",
                   parms = list(es=es, power=power, alpha=alpha, two.tailed=two.tailed,
                                rho2=rho2, rho3=rho3,
                                r21=r21, r22=r22, r23=r23,
                                p=p, q=q, n=n, J=J, K0=K0, tol=tol),
                   df = df,
                   ncp = M,
                   K = K)
  cat(ifelse(is.null(q), "\nModerator type: Continuous\n", "\nModerator type: Binary\n"),
      "\nK =", K)
  class(mrss.out) <- c("mod333", "mrss")
  return(invisible(mrss.out))
}
# examples
# mrss.mod333(es=.30, rho3=.1, rho2=.1, q=.5, r21=.3, r22=.4, r23=.5, n=20, J=4)
# mrss.mod333(es=.15, rho3=.1, rho2=.1, r21=.3, r22=.4, r23=.5, n=20, J=4)

mdes.bcra4f3 <- function(power=.80, alpha=.05, two.tailed=TRUE,
                         rho2, rho3, p=.50, r21=0, r22=0, r23=0, g3=0,
                         n, J, K, L){

  user.parms <- as.list(match.call())
  .error.handler(user.parms)

  df <- L * (K - 2) - g3
  SSE <- sqrt(rho3*(1-r23)/(p*(1-p)*K*L) +
                rho2*(1-r22)/(p*(1-p)*J*K*L) +
                (1-rho3-rho2)*(1-r21)/(p*(1-p)*J*K*L*n))

  mdes <- .mdes.fun(power = power, alpha = alpha, sse = SSE, df = df, two.tailed = two.tailed)
  .summ.mdes(effect = "main", power = power, alpha = alpha, sse = SSE, df = df, two.tailed = two.tailed, mdes = mdes)
  mdes.out <- list(fun = "mdes.bcra4f3",
                   parms = list(power=power, alpha=alpha, two.tailed=two.tailed,
                                rho2=rho2, rho3=rho3,
                                p=p, r21=r21, r22=r22, r23=r23, g3=g3,
                                n=n, J=J, K=K, L=L),
                   df=df,
                   ncp = mdes[1]/SSE,
                   mdes = mdes)
  class(mdes.out) <- c("main", "mdes")
  return(invisible(mdes.out))
}
# example
# mdes.bcra4f3(alpha=.05, two.tailed=TRUE, power=.80, rho3=.15, rho2=.15, n=10, J=4, K=4, L=15)

power.bcra4f3 <- function(es=.25, alpha=.05, two.tailed=TRUE,
                          rho2, rho3, p=.50, r21=0, r22=0, r23=0, g3=0,
                          n, J, K, L){

  user.parms <- as.list(match.call())
  .error.handler(user.parms)

  df <- L * (K - 2) - g3
  SSE <- sqrt(rho3*(1-r23)/(p*(1-p)*K*L) +
                rho2*(1-r22)/(p*(1-p)*J*K*L) +
                (1-rho3-rho2)*(1-r21)/(p*(1-p)*J*K*L*n))

  power <- .power.fun(es = es, alpha = alpha, sse = SSE, df = df, two.tailed = two.tailed)
  .summ.power(power = power, alpha = alpha, sse = SSE, df = df, two.tailed = two.tailed, es = es)
  power.out <-  list(fun = "power.bcra4f3",
                     parms = list(es=es, alpha=alpha, two.tailed=two.tailed,
                                  rho2=rho2, rho3=rho3,
                                  p=p, r21=r21, r22=r22, r23=r23, g3=g3,
                                  n=n, J=J, K=K, L=L),
                     df=df,
                     ncp = es/SSE,
                     power = power)
  class(power.out) <- c("main", "power")
  return(invisible(power.out))
}
# example
# power.bcra4f3(es=0.24, alpha=.05, two.tailed=TRUE, rho3=.15, rho2=.15, n=10, J=4, K=4, L=15)

mrss.bcra4f3 <- function(es=.25, power=.80, alpha=.05, two.tailed=TRUE,
                         n, J, L, K0=10, tol=.10,
                         rho2, rho3, p=.50, g3=0, r21=0, r22=0, r23=0){

  user.parms <- as.list(match.call())
  .error.handler(user.parms)

  i <- 0
  conv <- FALSE
  while(i<=100 & conv==FALSE){
    df <- L*(K0-2)-g3
    if(df<=0 | is.infinite(df)){break}
    T1 <- ifelse(two.tailed==TRUE,abs(qt(alpha/2,df)),abs(qt(alpha,df)))
    T2 <- abs(qt(power,df))
    M <- ifelse(power>=.5,T1+T2,T1-T2)
    K1 <- (M/es)^2 * (rho3*(1-r23)/(p*(1-p)*L) +
                        rho2*(1-r22)/(p*(1-p)*L*J) +
                        (1-rho3-rho2)*(1-r21)/(p*(1-p)*J*L*n))
    if(abs(K1-K0)<tol){conv <- TRUE}
    K0 <- (K1+K0)/2
    i <- i+1
  }
  K <- ifelse(df>0,round(K0),NA)

  mrss.out <-  list(fun = "mrss.bcra4f3",
                    parms = list(es=es, power=power, alpha=alpha, two.tailed=two.tailed,
                                 n=n, J=J, L=L, K0=K0, tol=tol,
                                 rho2=rho2, rho3=rho3,
                                 p=p, r21=r21, r22=r22, r23=r23, g3=g3),
                    df=df,
                    ncp = M,
                    K = K)
  class(mrss.out) <- c("main", "mrss")
  cat("K =", K, "(per block)\n")
  return(invisible(mrss.out))
}
# example
# mrss.bcra4f3(rho3=.15, rho2=.15, g3=1, p=.5, r23=.5, r22=.5, r21=.5, n=10, J=4, L=4)
