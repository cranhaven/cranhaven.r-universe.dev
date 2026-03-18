# esa: Standardized mean difference in mediator values between treatment and control groups (esa = esa_trt - esa_ctrl)
# esB: Standardized regression coefficient for mediator - outcome path, capturing level 1 (within intervention clusters) and level 2 (between intervention clusters) effects (esaB = esab1 + esab2)
# esb1: Standardized regression coefficient for mediator - outcome path at level 1 in the treatment group (within intervention clusters)
# rhom_trt: Proportion of unconditional mediator variance between level 2 units (intervention clusters) in the treatment group
# rho_trt: Proportion of unconditional outcome variance between level 2 units (intervention clusters) in the control group
# r2m1_ctrl: Proportion of variance in the mediator explained by the covariates in the control group (or waiting list)
# r2m1_trt: Proportion of variance in the level 1 mediator explained by level 1 covariates in the treatment group
# r2m2_trt: Proportion of variance in the aggregate mediator (at level 2) explained by level 2 covariates in the treatment group
# r2y1z_trt: Proportion of variance in the level 1 outcome explained by level 1 covariates in the treatment group
# r2y2z_trt: Proportion of variance in the aggregate outcome (at level 2) explained by level 2 covariates in the treatment group
# r2y1_trt: Proportion of variance in the level 1 outcome explained by level 1 predictors (including mediator) in the treatment group
# r2y2_trt: Proportion of variance in the aggregate outcome (at level 2) explained by level 2 predictors (including aggregate mediator) in the treatment group
# g1_ctrl: Number of covariates introduced at level 1 in the control group
# n_ctrl: Total number of subjects in the control group
# g2_trt: Number of covariates introduced at level 2 (intervention clusters) in the treatment group
# n_trt: Number of subjects per level 2 unit (intervention cluster)
# J_trt: Number of level 2 units (intervention clusters)

power.med_pn21 <- function(esa = .50, esB = .50, esb1 = .10,
                           two.tailed = TRUE, alpha = .05,
                           mc = TRUE, nsims = 1000, ndraws = 1000,
                           rhom_trt = .20, rho_trt = .20,
                           r2m1_ctrl = .20, r2m1_trt = .20, r2m2_trt = .20,
                           r2y1z_trt = 0, r2y2z_trt = 0,
                           r2y1_trt = NULL, r2y2_trt = NULL,
                           g1_ctrl = 0, n_ctrl =  20,
                           g2_trt = 0, n_trt = 30, J_trt = 20) {

  user.parms <- as.list(match.call())
  .error.handler(user.parms)
  # warning("Experimental function. \nArguments are not controlled for possible errors.", call. = FALSE)

  # standard errors for 2/1 partially nested mediation
  .se.a21_trt <- function(tau2m_trt, sig2m_trt,
                          r2m2_trt, r2m1_trt,
                          g2_trt, J_trt, n_trt) {
    var.a21_trt <- (tau2m_trt * (1 - r2m2_trt) + sig2m_trt * (1 - r2m1_trt) / n_trt) /
      (J_trt - g2_trt - 1)
    if(is.nan(var.a21_trt) |  var.a21_trt  <= 0) {
      stop("Design is not feasible", call. = FALSE)
    }
    return(invisible(sqrt(var.a21_trt)))
  }

  .se.a21_ctrl <- function(sig2m_ctrl, r2m1_ctrl,
                           g1_ctrl, n_ctrl) {
    var.a21_ctrl <- sig2m_ctrl * (1 - r2m1_ctrl)  / (n_ctrl - g1_ctrl - 1)
    if(is.nan(var.a21_ctrl) |  var.a21_ctrl  <= 0) {
      stop("Design is not feasible", call. = FALSE)
    }
    return(invisible(sqrt(var.a21_ctrl)))
  }

  .se.B21_trt <- function(tau2y_trt, sig2y_trt, tau2m_trt, sig2m_trt,
                          r2y2_trt, r2y1_trt, r2m2_trt, r2m1_trt,
                          g2_trt, n_trt, J_trt) {
    var.B21_trt <- (tau2y_trt * (1 - r2y2_trt) + sig2y_trt * (1 - r2y1_trt) / n_trt) /
      ((J_trt - g2_trt - 1) * (tau2m_trt * (1 - r2m2_trt) + sig2m_trt * (1 - r2m1_trt) / n_trt))
    if(is.nan(var.B21_trt) | var.B21_trt <= 0) {
      stop("Design is not feasible", call. = FALSE)
    }
    return(invisible(sqrt(var.B21_trt)))
  }

  # p <- .50 # expressions assume balanced allocation?
  # n_ctrl <- n * (1 - p)
  # J_trt <- (n * p) / n_trt

  tau2m_trt <- rhom_trt
  sig2m_trt <- 1 - rhom_trt
  sig2m_ctrl <- sig2m_trt # assumes unconditional mediator variance at level 1 does not change between treatment and control groups
  tau2y_trt <- rho_trt
  sig2y_trt <- 1 - rho_trt

  ifelse(is.null(r2y1_trt),
    r2y1_trt <- r2y1z_trt + sig2m_trt * (esb1)^2 * (1 - r2m1_trt) / sig2y_trt,
    warning("Ignoring `r2y1z_trt`", call. = FALSE))
  ifelse(is.null(r2y2_trt),
    r2y2_trt <- r2y2z_trt + (tau2m_trt + sig2m_trt / n_trt) * (esB)^2 * (1 - r2m2_trt) / tau2y_trt,
    warning("Ignoring `r2y2z_trt`", call. = FALSE))

  dfa <- J_trt - g2_trt - 1
  dfB <- J_trt - g2_trt - 1
  df <- rbind(dfa, dfB)
  colnames(df) <- "df"
  rownames(df) <- c("a", "B")

  se.a21_trt <- .se.a21_trt(tau2m_trt = tau2m_trt, sig2m_trt = sig2m_trt,
                            r2m2_trt = r2m2_trt, r2m1_trt = r2m1_trt,
                            g2_trt = g2_trt, J_trt = J_trt, n_trt = n_trt)
  se.a21_ctrl <- .se.a21_ctrl(sig2m_ctrl = sig2m_ctrl, r2m1_ctrl = r2m1_ctrl,
                              g1_ctrl = g1_ctrl, n_ctrl = n_ctrl)
  se.B21_trt <- .se.B21_trt(tau2y_trt = tau2y_trt, sig2y_trt =  sig2y_trt,
                            tau2m_trt = tau2m_trt, sig2m_trt = sig2m_trt,
                            r2y2_trt = r2y2_trt, r2y1_trt =  r2y1_trt,
                            r2m2_trt = r2m2_trt, r2m1_trt = r2m1_trt,
                            g2_trt = g2_trt, n_trt =  n_trt, J_trt = J_trt)
  se.a21_diff <- sqrt(se.a21_trt^2 + se.a21_ctrl^2)

  ncp.a21_diff <- esa / se.a21_diff
  ncp.B21_trt <- esB / se.B21_trt
  ncp <- rbind(ncp.a21_diff, ncp.B21_trt)
  colnames(ncp) <- "ncp"
  rownames(ncp) <- c("esa","esB")

  power.a21_diff <- .power.fun(es = esa, alpha = alpha, sse = se.a21_diff, two.tailed = two.tailed, df = dfa)
  power.B21_trt <- .power.fun(es = esB, alpha = alpha, sse = se.B21_trt,  two.tailed = two.tailed, df = dfB)
  power.sobel.aB <- .power.sobel(x = esa, y = esB, sex = se.a21_diff, sey = se.B21_trt, alpha = alpha, two.tailed = two.tailed)
  power.joint.aB <- .power.jt(x = esa, y = esB, sex = se.a21_diff, sey = se.B21_trt, alpha = alpha, dfx = dfa, dfy = dfB, two.tailed = two.tailed)
  power.mc.aB <- ifelse(mc, .power.mc(x = esa, y = esB, sex = se.a21_diff, sey = se.B21_trt, nsims = nsims, ndraws = ndraws, alpha = alpha, two.tailed = two.tailed), NA)

  power <- rbind(
    c(round(power.a21_diff, 3), NA, NA, NA),
    c(round(power.B21_trt, 3), NA, NA, NA),
    c(NA, round(power.sobel.aB, 3), round(power.joint.aB, 3), round(power.mc.aB, 3))
  )
  colnames(power) <- c("t", "sobel", "joint", "mc")
  rownames(power) <- c("a", "B", "aB")

  power.out <- list(fun = "power.med_pn21",
                    parms = list(esa = esa, esB = esB, esb1 = esb1,
                                 two.tailed = two.tailed, alpha = alpha,
                                 mc = mc, nsims = nsims, ndraws = ndraws,
                                 rhom_trt = rhom_trt, rho_trt = rho_trt,
                                 r2m1_ctrl = r2m1_ctrl, r2m1_trt = r2m1_trt, r2m2_trt = r2m2_trt,
                                 r2y1z_trt = r2y1z_trt, r2y2z_trt = r2y2z_trt,
                                 r2y1_trt = r2y1_trt, r2y2_trt = r2y2_trt,
                                 n_trt = n_trt, n_ctrl = n_ctrl, J_trt = J_trt),
                    df = df,
                    ncp = ncp,
                    power = round(power, 3))
  cat("Statistical power: \n")
  cat("------------------------------------ \n")
  print(power)
  cat("------------------------------------ \n")
  cat("Degrees of freedom for path a:", dfa,
      "\nDegrees of freedom for path B:", dfB,
      "\nStandardized standard error for path a:", round(se.a21_diff, 3),
      "\nStandardized standard error for path B:", round(se.B21_trt, 3),
      "\nType I error rate:", alpha,
      "\nTwo-tailed test:", two.tailed, "\n")
  class(power.out) <- c("power", "med_pn21")
  return(invisible(power.out))
}


# example
# power.med_pn21(esa = .40, esB = .40, esb1 = .40,
#                two.tailed = TRUE, alpha = .05,
#                rhom_trt = .20, rho_trt = .20,
#                r2m1_ctrl = .60, r2m1_trt = .60, r2m2_trt = .60,
#                r2y1_trt = .50, r2y2_trt = .50,
#                n_ctrl = 50, n_trt =  6, J_trt = 50)


# esa: Standardized mean difference in mediator values between treatment and control groups (esa = esa_trt - esa_ctrl)
# esB: Standardized regression coefficient for mediator - outcome path, capturing level 1 and level 2 (between intervention clusters) effects (esaB = esab1 + esab2)
# esb1: Standardized regression coefficient for mediator - outcome path at level 1 in the treatment group (within first intervention clusters)
# esb2: Standardized regression coefficient for aggregate mediator - outcome path at level 2 in the treatment group (between first intervention clusters)
# rhom3_trt: Proportion of unconditional mediator variance between level 3 units (second intervention clusters) in the treatment group
# rhom2_trt: Proportion of unconditional mediator variance between level 2 units (first intervention clusters) in the treatment group
# rho3_trt: Proportion of unconditional outcome variance between level 3 units (second intervention clusters) in the treatment group
# rho2_trt: Proportion of unconditional outcome variance between level 2 units (first intervention clusters) in the treatment group
# r2m1_ctrl: Proportion of variance in the mediator explained by covariates in the control group (or waiting list)
# r2m1_trt: Proportion of variance in the level 1 mediator explained by level 1 covariates in the treatment group
# r2m2_trt: Proportion of variance in the aggregate mediator (at level 2) explained by level 2 covariates in the treatment group
# r2m3_trt: Proportion of variance in the aggregate mediator (at level 3) explained by level 3 covariates in the treatment group
# r2y1z_trt: Proportion of variance in the level 1 outcome explained by level 1 covariates in the treatment group
# r2y2z_trt: Proportion of variance in the aggregate outcome (at level 2) explained by level 2 covariates in the treatment group
# r2y3z_trt: Proportion of variance in the aggregate outcome (at level 3) explained by level 3 covariates in the treatment group
# r2y1_trt: Proportion of variance in the level 1 outcome explained by level 1 predictors (including mediator) in the treatment group
# r2y2_trt: Proportion of variance in the aggregate outcome (level 2) explained by level 2 predictors (including aggregate mediator) in the treatment group
# r2y3_trt: Proportion of variance in the aggregate outcome (level 3) explained by level 3 predictors (including aggregate mediator) in the treatment group
# g1_ctrl: Number of covariates introduced at level 1 in the control group
# n_ctrl: Total number of subjects in the control group
# g3_trt: Number of covariates introduced at level 3 (second intervention clusters) in the treatment group
# n_trt: Number of subjects per level 2 unit (first intervention cluster)
# J_trt: Number of level 2 units per level 3 unit (second intervention cluster)
# K_trt: Number of level 3 units (second intervention clusters)

power.med_pn31 <- function(esa = .50, esB = .50, esb1 = .10,  esb2 = .10,
                           two.tailed = TRUE, alpha = .05,
                           mc = TRUE, nsims = 1000, ndraws = 1000,
                           rhom3_trt = .20, rhom2_trt = .20,
                           rho3_trt = .20, rho2_trt = .20,
                           r2m1_ctrl = .20, r2m1_trt = .20, r2m2_trt = .20, r2m3_trt = .20,
                           r2y1z_trt = 0, r2y2z_trt = 0, r2y3z_trt = 0,
                           r2y1_trt = NULL, r2y2_trt = NULL, r2y3_trt = NULL,
                           g1_ctrl = 0, n_ctrl =  20,
                           g3_trt = 0, n_trt = 30, J_trt = 20, K_trt = 20) {

  user.parms <- as.list(match.call())
  .error.handler(user.parms)
  # warning("Experimental function. \nArguments are not controlled for possible errors.", call. = FALSE)

  # standard errors for 3/1 partially nested mediation
  .se.a31_trt <- function(tau2m3_trt, tau2m2_trt, sig2m_trt,
                          r2m3_trt, r2m2_trt, r2m1_trt,
                          g3_trt, J_trt, K_trt, n_trt) {
    var.a31_trt <- (tau2m3_trt * (1 - r2m3_trt) + tau2m2_trt * (1 - r2m2_trt) /
                      J_trt + sig2m_trt * (1 - r2m1_trt) / (J_trt * n_trt)) /
      (K_trt - g3_trt - 1)
    if(is.nan(var.a31_trt) |  var.a31_trt  <= 0) {
      stop("Design is not feasible", call. = FALSE)
    }
    return(invisible(sqrt(var.a31_trt)))
  }

  .se.a31_ctrl <- function(sig2m_ctrl, r2m1_ctrl,
                           g1_ctrl, n_ctrl) {
    var.a31_ctrl <- sig2m_ctrl * (1 - r2m1_ctrl)  / (n_ctrl - g1_ctrl - 1)
    if(is.nan(var.a31_ctrl) |  var.a31_ctrl  <= 0) {
      stop("Design is not feasible", call. = FALSE)
    }
    return(invisible(sqrt(var.a31_ctrl)))
  }

  .se.B31_trt <- function(tau2y3_trt, tau2y2_trt, sig2y_trt,
                          tau2m3_trt, tau2m2_trt, sig2m_trt,
                          r2y3_trt, r2y2_trt, r2y1_trt,
                          r2m3_trt, r2m2_trt, r2m1_trt,
                          g3_trt, n_trt, J_trt, K_trt) {
    var.B31_trt <- (tau2y3_trt * (1 - r2y3_trt) + tau2y2_trt * (1 - r2y2_trt) / J_trt + sig2y_trt * (1 - r2y1_trt) / (n_trt * J_trt)) /
      ((K_trt - g3_trt - 1) * (tau2m3_trt * (1 - r2m3_trt) + tau2m2_trt * (1 - r2m2_trt) / J_trt + sig2m_trt * (1 - r2m1_trt) / (J_trt * n_trt)))
    if(is.nan(var.B31_trt) | var.B31_trt <= 0) {
      stop("Design is not feasible", call. = FALSE)
    }
    return(invisible(sqrt(var.B31_trt)))
  }

  tau2m3_trt <- rhom3_trt
  tau2m2_trt <- rhom2_trt
  sig2m_trt <- 1 - rhom3_trt - rhom2_trt
  sig2m_ctrl <- sig2m_trt # assumes unconditional mediator variance at level 1 does not change between treatment and control groups
  tau2y3_trt <- rho3_trt
  tau2y2_trt <- rho2_trt
  sig2y_trt <- 1 - rho3_trt - rho2_trt

  ifelse(is.null(r2y1_trt),
         r2y1_trt <- r2y1z_trt + (sig2m_trt * (1 - 1 / n_trt)) * (esb1)^2 * (1 - r2m1_trt) / sig2y_trt,
         warning("Ignoring `r2y1z_trt`", call. = FALSE))
  ifelse(is.null(r2y2_trt),
         r2y2_trt <- r2y2z_trt + ((tau2m2_trt + sig2m_trt / n_trt) * (1 - 1 / J_trt)) * (esb2)^2 * (1 - r2m2_trt) / tau2y2_trt,
         warning("Ignoring `r2y2z_trt`", call. = FALSE))
  ifelse(is.null(r2y3_trt),
         r2y3_trt <- r2y3z_trt + (tau2m3_trt + tau2m2_trt / J_trt + sig2m_trt / (n_trt * J_trt)) * (esB)^2 * (1 - r2m3_trt) / tau2y3_trt,
         warning("Ignoring `r2y3z_trt`", call. = FALSE))

  dfa <- K_trt - g3_trt - 1
  dfB <- K_trt - g3_trt - 1
  df <- rbind(dfa, dfB)
  colnames(df) <- "df"
  rownames(df) <- c("a", "B")

  se.a31_trt <- .se.a31_trt(tau2m3_trt = tau2m3_trt, tau2m2_trt = tau2m2_trt, sig2m_trt = sig2m_trt,
                            r2m3_trt = r2m3_trt, r2m2_trt = r2m2_trt, r2m1_trt = r2m1_trt,
                            g3_trt = g3_trt, J_trt = J_trt, K_trt = K_trt, n_trt = n_trt)
  se.a31_ctrl <- .se.a31_ctrl(sig2m_ctrl = sig2m_ctrl, r2m1_ctrl =  r2m1_ctrl,
                              g1_ctrl = g1_ctrl, n_ctrl = n_ctrl)
  se.B31_trt <- .se.B31_trt(tau2y3_trt = tau2y3_trt, tau2y2_trt = tau2y2_trt, sig2y_trt = sig2y_trt,
                            tau2m3_trt = tau2m3_trt, tau2m2_trt = tau2m2_trt, sig2m_trt = sig2m_trt,
                            r2y3_trt = r2y3_trt, r2y2_trt = r2y2_trt, r2y1_trt = r2y1_trt,
                            r2m3_trt = r2m3_trt, r2m2_trt = r2m2_trt, r2m1_trt = r2m1_trt,
                            g3_trt = g3_trt, n_trt = n_trt, J_trt = J_trt, K_trt = K_trt)

  se.a31_diff <- sqrt(se.a31_trt^2 + se.a31_ctrl^2)

  ncp.a31_diff <- esa / se.a31_diff
  ncp.B31_trt <- esB / se.B31_trt
  ncp <- rbind(ncp.a31_diff, ncp.B31_trt)
  colnames(ncp) <- "ncp"
  rownames(ncp) <- c("esa","esB")

  power.a31_diff <- .power.fun(es = esa, alpha = alpha, sse = se.a31_diff, two.tailed = two.tailed, df = dfa)
  power.B31_trt <- .power.fun(es = esB, alpha = alpha, sse = se.B31_trt,  two.tailed = two.tailed, df = dfB)
  power.sobel.aB <- .power.sobel(x = esa, y = esB, sex = se.a31_diff, sey = se.B31_trt, alpha = alpha, two.tailed = two.tailed)
  power.joint.aB <- .power.jt(x = esa, y = esB, sex = se.a31_diff, sey = se.B31_trt, alpha = alpha, dfx = dfa, dfy = dfB, two.tailed = two.tailed)
  power.mc.aB <- ifelse(mc, .power.mc(x = esa, y = esB, sex = se.a31_diff, sey = se.B31_trt, nsims = nsims, ndraws = ndraws, alpha = alpha, two.tailed = two.tailed), NA)

  power <- rbind(
    c(round(power.a31_diff, 3), NA, NA, NA),
    c(round(power.B31_trt, 3), NA, NA, NA),
    c(NA, round(power.sobel.aB, 3), round(power.joint.aB, 3), round(power.mc.aB, 3))
  )
  colnames(power) <- c("t", "sobel", "joint", "mc")
  rownames(power) <- c("a", "B", "aB")

  power.out <- list(fun = "power.med_pn31",
                    parms = list(esa = esa, esB = esB, esb1 = esb1, esb2 = esb2,
                                 two.tailed = two.tailed, alpha = alpha,
                                 mc = mc, nsims = nsims, ndraws = ndraws,
                                 rhom3_trt = rhom3_trt, rhom2_trt = rhom2_trt,
                                 rho3_trt = rho3_trt, rho2_trt = rho2_trt,
                                 r2m1_ctrl = r2m1_ctrl, r2m1_trt =  r2m1_trt,
                                 r2m2_trt = r2m2_trt, r2m3_trt = r2m3_trt,
                                 r2y1_trt = r2y1_trt, r2y2_trt = r2y2_trt, r2y3_trt = r2y3_trt,
                                 r2y1z_trt = r2y1z_trt, r2y2z_trt = r2y2z_trt, r2y3z_trt = r2y3z_trt,
                                 g1_ctrl = g1_ctrl, g3_trt = g3_trt,
                                 n_ctrl = n_ctrl, n_trt = n_trt, J_trt = J_trt, K_trt = K_trt),
                    df = df,
                    ncp = ncp,
                    power = round(power, 3))
  cat("Statistical power: \n")
  cat("------------------------------------ \n")
  print(power)
  cat("------------------------------------ \n")
  cat("Degrees of freedom for path a:", dfa,
      "\nDegrees of freedom for path B:", dfB,
      "\nStandardized standard error for path a:", round(se.a31_diff, 3),
      "\nStandardized standard error for path B:", round(se.B31_trt, 3),
      "\nType I error rate:", alpha,
      "\nTwo-tailed test:", two.tailed, "\n")
  class(power.out) <- c("power", "med_pn31")
  return(invisible(power.out))
}


# example
# power.med_pn31(esa = .50, esB = .50, esb1 = .10, esb2 = .10,
#                rhom3_trt = .10, rhom2_trt = .20, rho3_trt = .10, rho2_trt = .20,
#                r2m1_ctrl = .20, r2m1_trt = .20, r2m2_trt = .20, r2m3_trt = .20,
#                r2y1_trt = .20, r2y2_trt = .20, r2y3_trt = .20,
#                n_ctrl = 60, n_trt =  20, J_trt = 10, K_trt = 60)


# rho3_trt: Proportion of unconditional outcome variance between level 3 units (second intervention clusters) in the treatment group
# rho2_trt: Proportion of unconditional outcome variance between level 2 units (first intervention clusters) in the treatment group
# r2m1_ctrl: Proportion of variance in the mediator explained by level 1 covariates in the control group
# r2m2_ctrl: Proportion of variance in the aggregate mediator explained by level 2 covariates in the control group
# r2m1_trt: Proportion of variance in the level 1 mediator explained by level 1 covariates in the treatment group
# r2m2_trt: Proportion of variance in the aggregate mediator (at level 2) explained by level 2 covariates in the treatment group
# r2m3_trt: Proportion of variance in the aggregate mediator (at level 3) explained by level 3 covariates in the treatment group
# r2y1z_trt: Proportion of variance in the level 1 outcome explained by level 1 covariates in the treatment group
# r2y2z_trt: Proportion of variance in the aggregate outcome (at level 2) explained by level 2 covariates in the treatment group
# r2y3z_trt: Proportion of variance in the aggregate outcome (at level 3) explained by level 3 covariates in the treatment group
# r2y1_trt: Proportion of variance in the level 1 outcome explained by level 1 predictors (including mediator) in the treatment group
# r2y2_trt: Proportion of variance in the aggregate outcome (level 2) explained by level 2 predictors (including aggregate mediator) in the treatment group
# r2y3_trt: Proportion of variance in the aggregate outcome (level 3) explained by level 3 predictors (including aggregate mediator) in the treatment group
# g2_ctrl: Number of covariates introduced at level 2 (intervention clusters) in the control group
# n_ctrl: Number of subjects per level 2 unit (intervention cluster) in the control group
# J_ctrl: Number of level 2 units (intervention clusters) in the control group
# g3_trt: Number of covariates introduced at level 3 (second intervention clusters) in the treatment group
# n_trt: Number of subjects per level 2 unit (first intervention cluster)
# J_trt: Number of level 2 units per level 3 unit (second intervention cluster)
# K_trt: Number of level 3 units (second intervention clusters)


power.med_pn32 <- function(esa = .50, esB = .50, esb1 = .10,  esb2 = .10,
                           two.tailed = TRUE, alpha = .05,
                           mc = TRUE, nsims = 1000, ndraws = 1000,
                           rhom2_ctrl = .20, rhom3_trt = .20, rhom2_trt = .20,
                           rho3_trt = .20, rho2_trt = .20,
                           r2m1_ctrl = .20, r2m2_ctrl = .20,
                           r2m1_trt = .20, r2m2_trt = .20, r2m3_trt = .20,
                           r2y1z_trt = 0, r2y2z_trt = 0, r2y3z_trt = 0,
                           r2y1_trt = NULL, r2y2_trt = NULL, r2y3_trt = NULL,
                           g2_ctrl = 0, n_ctrl = 30, J_ctrl = 20,
                           g3_trt = 0, n_trt =  20, J_trt = 20,  K_trt = 20) {

  user.parms <- as.list(match.call())
  .error.handler(user.parms)
  # warning("Experimental function. \nArguments are not controlled for possible errors.", call. = FALSE)

  # standard errors for 3/2 partially nested mediation
  .se.a32_trt <- function(tau2m3_trt, tau2m2_trt, sig2m_trt,
                          r2m3_trt, r2m2_trt, r2m1_trt,
                          g3_trt, J_trt, K_trt, n_trt) {
    var.a32_trt <- (tau2m3_trt * (1 - r2m3_trt) + tau2m2_trt * (1 - r2m2_trt) /
                      J_trt + sig2m_trt * (1 - r2m1_trt) / (J_trt * n_trt)) /
      (K_trt - g3_trt - 1)
    if(is.nan(var.a32_trt) |  var.a32_trt  <= 0) {
      stop("Design is not feasible", call. = FALSE)
    }
    return(invisible(sqrt(var.a32_trt)))
  }

  .se.a32_ctrl <- function(tau2m_ctrl, sig2m_ctrl,
                           r2m1_ctrl, r2m2_ctrl,
                           g2_ctrl, n_ctrl, J_ctrl) {
    var.a32_ctrl <- (tau2m_ctrl * (1 - r2m2_ctrl) + sig2m_ctrl * (1 - r2m1_ctrl) / (J_ctrl * n_ctrl))  / (J_ctrl - g2_ctrl - 1)
    if(is.nan(var.a32_ctrl) |  var.a32_ctrl  <= 0) {
      stop("Design is not feasible", call. = FALSE)
    }
    return(invisible(sqrt(var.a32_ctrl)))
  }

  .se.B32_trt <- function(tau2y3_trt, tau2y2_trt, sig2y_trt,
                          tau2m3_trt, tau2m2_trt, sig2m_trt,
                          r2y3_trt, r2y2_trt, r2y1_trt,
                          r2m3_trt, r2m2_trt, r2m1_trt,
                          g3_trt, n_trt, J_trt, K_trt) {
    var.B32_trt <- (tau2y3_trt * (1 - r2y3_trt) + tau2y2_trt * (1 - r2y2_trt) / J_trt + sig2y_trt * (1 - r2y1_trt) / (n_trt * J_trt)) /
      ((K_trt - g3_trt - 1) * (tau2m3_trt * (1 - r2m3_trt) + tau2m2_trt * (1 - r2m2_trt) / J_trt + sig2m_trt * (1 - r2m1_trt) / (J_trt * n_trt)))
    if(is.nan(var.B32_trt) | var.B32_trt <= 0) {
      stop("Design is not feasible", call. = FALSE)
    }
    return(invisible(sqrt(var.B32_trt)))
  }

  tau2m3_trt <- rhom3_trt
  tau2m2_trt <- rhom2_trt
  sig2m_trt <- 1 - rhom3_trt - rhom2_trt

  tau2m_ctrl <- rhom2_ctrl
  sig2m_ctrl <- 1 - rhom2_ctrl

  #tau2y_ctrl <- rho2_ctrl
  #sig2y_ctrl <- 1 - rho2_ctrl

  tau2y3_trt <- rho3_trt
  tau2y2_trt <- rho2_trt
  sig2y_trt <- 1 - rho3_trt - rho2_trt

  ifelse(is.null(r2y1_trt),
     r2y1_trt <- r2y1z_trt + (sig2m_trt * (1 - 1 / n_trt)) * (esb1)^2 * (1 - r2m1_trt) / sig2y_trt,
     warning("Ignoring `r2y1z_trt`", call. = FALSE))
  ifelse(is.null(r2y2_trt),
     r2y2_trt <- r2y2z_trt + ((tau2m2_trt + sig2m_trt / n_trt) * (1 - 1 / J_trt)) * (esb2)^2 * (1 - r2m2_trt) / tau2y2_trt,
     warning("Ignoring `r2y2z_trt`", call. = FALSE))
  ifelse(is.null(r2y3_trt),
     r2y3_trt <- r2y3z_trt + (tau2m3_trt + tau2m2_trt / J_trt + sig2m_trt / (n_trt * J_trt)) * (esB)^2 * (1 - r2m3_trt) / tau2y3_trt,
     warning("Ignoring `r2y3z_trt`", call. = FALSE))

  dfa <- J_ctrl - g2_ctrl - 1
  dfB <- K_trt - g3_trt - 1
  df <- rbind(dfa, dfB)
  colnames(df) <- "df"
  rownames(df) <- c("a", "B")

  se.a32_trt <- .se.a32_trt(tau2m3_trt = tau2m3_trt, tau2m2_trt = tau2m2_trt, sig2m_trt = sig2m_trt,
                            r2m3_trt = r2m3_trt, r2m2_trt = r2m2_trt, r2m1_trt = r2m1_trt,
                            g3_trt = g3_trt, J_trt = J_trt, K_trt = K_trt, n_trt = n_trt)
  se.a32_ctrl <- .se.a32_ctrl(tau2m_ctrl = tau2m_ctrl, sig2m_ctrl = sig2m_ctrl,
                              r2m1_ctrl =  r2m1_ctrl, r2m2_ctrl = r2m2_ctrl,
                              g2_ctrl = g2_ctrl, n_ctrl = n_ctrl, J_ctrl = J_ctrl)
  se.B32_trt <- .se.B32_trt(tau2y3_trt = tau2y3_trt, tau2y2_trt = tau2y2_trt, sig2y_trt = sig2y_trt,
                            tau2m3_trt = tau2m3_trt, tau2m2_trt = tau2m2_trt, sig2m_trt = sig2m_trt,
                            r2y3_trt = r2y3_trt, r2y2_trt = r2y2_trt, r2y1_trt = r2y1_trt,
                            r2m3_trt = r2m3_trt, r2m2_trt = r2m2_trt, r2m1_trt = r2m1_trt,
                            g3_trt = g3_trt, n_trt = n_trt, J_trt = J_trt, K_trt = K_trt)

  se.a32_diff <- sqrt(se.a32_trt^2 + se.a32_ctrl^2)

  ncp.a32_diff <- esa / se.a32_diff
  ncp.B32_trt <- esB / se.B32_trt
  ncp <- rbind(ncp.a32_diff, ncp.B32_trt)
  colnames(ncp) <- "ncp"
  rownames(ncp) <- c("esa","esB")

  power.a32_diff <- .power.fun(es = esa, alpha = alpha, sse = se.a32_diff, two.tailed = two.tailed, df = dfa)
  power.B32_trt <- .power.fun(es = esB, alpha = alpha, sse = se.B32_trt,  two.tailed = two.tailed, df = dfB)
  power.sobel.aB <- .power.sobel(x = esa, y = esB, sex = se.a32_diff, sey = se.B32_trt, alpha = alpha, two.tailed = two.tailed)
  power.joint.aB <- .power.jt(x = esa, y = esB, sex = se.a32_diff, sey = se.B32_trt, alpha = alpha, dfx = dfa, dfy = dfB, two.tailed = two.tailed)
  power.mc.aB <- ifelse(mc, .power.mc(x = esa, y = esB, sex = se.a32_diff, sey = se.B32_trt, nsims = nsims, ndraws = ndraws, alpha = alpha, two.tailed = two.tailed), NA)

  power <- rbind(
    c(round(power.a32_diff, 3), NA, NA, NA),
    c(round(power.B32_trt, 3), NA, NA, NA),
    c(NA, round(power.sobel.aB, 3), round(power.joint.aB, 3), round(power.mc.aB, 3))
  )
  colnames(power) <- c("t", "sobel", "joint", "mc")
  rownames(power) <- c("a", "B", "aB")

  power.out <- list(fun = "power.med_pn32",
                    parms = list(esa = esa, esB = esB, esb1 = esb1, esb2 = esb2,
                                 two.tailed = two.tailed, alpha = alpha,
                                 mc = mc, nsims = nsims, ndraws = ndraws,
                                 rhom2_ctrl = rhom2_ctrl, rhom3_trt = rhom3_trt, rhom2_trt = rhom2_trt,
                                 rho3_trt = rho3_trt, rho2_trt = rho2_trt,
                                 r2m1_ctrl = r2m1_ctrl, r2m2_ctrl = r2m2_ctrl,
                                 r2m1_trt =  r2m1_trt, r2m2_trt = r2m2_trt, r2m3_trt = r2m3_trt,
                                 r2y1_trt = r2y1_trt, r2y2_trt = r2y2_trt, r2y3_trt = r2y3_trt,
                                 r2y1z_trt = r2y1z_trt, r2y2z_trt = r2y2z_trt, r2y3z_trt = r2y3z_trt,
                                 g2_ctrl = g2_ctrl, g3_trt = g3_trt,
                                 n_ctrl = n_ctrl, n_trt = n_trt, J_ctrl = J_ctrl, J_trt = J_trt, K_trt = K_trt),
                    df = df,
                    ncp = ncp,
                    power = round(power, 3))
  cat("Statistical power: \n")
  cat("------------------------------------ \n")
  print(power)
  cat("------------------------------------ \n")
  cat("Degrees of freedom for path a:", dfa,
      "\nDegrees of freedom for path B:", dfB,
      "\nStandardized standard error for path a:", round(se.a32_diff, 3),
      "\nStandardized standard error for path B:", round(se.B32_trt, 3),
      "\nType I error rate:", alpha,
      "\nTwo-tailed test:", two.tailed, "\n")
  class(power.out) <- c("power", "med_pn32")
  return(invisible(power.out))
}


# example
# power.med_pn32(esa = .50, esB = .50, esb1 = .10, esb2 = .10,
#                rhom2_ctrl = .20, rhom3_trt = .10, rhom2_trt = .20,
#                rho3_trt = .10, rho2_trt = .20,
#                r2m1_ctrl = .20, r2m2_ctrl = .20,
#                r2m1_trt = .20, r2m2_trt = .20, r2m3_trt = .20,
#                r2y1_trt = .20, r2y2_trt = .20, r2y3_trt = .20,
#                n_ctrl = 24, n_trt =  24, J_trt = 12, J_ctrl = 60, K_trt = 60)

