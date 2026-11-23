# Helper functions

#' @import dplyr
#' @import rjags
#' @import mvtnorm
#' @import MatchIt
#' @import ggplot2
#' @import survival
#' @import futile.logger
#' @import doParallel
#' @import foreach
#' @importFrom data.table data.table :=
#' @importFrom methods new slot
#' @importFrom stats as.formula binomial glm qnorm rbinom rexp rnorm rweibull time update var
#' @importFrom grDevices png dev.off

`%notin%` <- Negate(`%in%`)

# The piecewise exponential distribution useful for conditions where failure rates change or simulations with a delayed or changing treatment effect

rpwexp <- function(n, rate=1, intervals=NULL, cumulative=FALSE){
  if(is.null(intervals)){
    if (cumulative){return(cumsum(rexp(n,rate[1])))}else
      return(rexp(n,rate[1]))}
  k <- length(rate)
  if (k==1){
    if(cumulative){return(cumsum(rexp(n,rate)))}else
      return(rexp(n,rate))
  }
  if (length(intervals) < k-1) stop("length(intervals) must be at least length(rate) - 1")
  tx <- 0
  j <- 1
  times <- array(0,n)
  timex <- cumsum(intervals)
  indx <- array(TRUE,n)
  for(i in 1:k){
    nindx <- sum(indx)
    if (nindx==0) break
    increment <- rexp(nindx,rate[i])
    if (cumulative) times[indx] <- tx + cumsum(increment)
    else times[indx] <- tx + increment
    if (i<k){
      tx <- timex[i]
      indx <- (times > timex[i])
    }
  }
  return(times)
}


# Specify sample size
s_trt = function(ssC, ssE, ssExt){
  ext = cbind("ext" = rep(1, ssExt), "trt" = rep(0, ssExt))
  # int = cbind("ext" = rep(0, ssC + ssE), "trt" = sample(c(rep(1, ssE), c(rep(0, ssC)))))
  int = cbind("ext" = rep(0, ssC + ssE), "trt" = c(rep(1, ssE), c(rep(0, ssC))))
  rbind(ext, int)
}

# Simulate covariates
s_cov = function(ext, dt, n_cat, n_cont, mu, var, cov, prob){
  n = sum(dt[,'ext'] == ext)
  flog.debug(paste("Ext =", ext, "trial has sample size =", n, "\n"))
  cov_st = sum(grepl("cov", colnames(dt))) + 1

  covariate <- NULL
  n_cov = sum(n_cont, n_cat)
  flog.debug(paste("[s_cov] Total number of covariates to add this time =", n_cov, "\n"))

  if (n_cat == 0 & n_cont == 1){
    covariate <- as.matrix(rnorm(n, mean = mu, sd = sqrt(var)), ncol = 1)
    flog.debug(paste("User chose to simulate one continuous variable. \n"))
  } else if (n_cont == 0 & n_cat > 0 & sum(cov) == 0){ # if user want to simulate independent binary variables only
    covariate = sapply(1:n_cat, function(i){
      flog.debug(paste("User chose to simulate independent binary variable only. \n"))
      rbinom(n, 1, prob[i])
    })
  } else {
    sig <- diag(var)
    sig[lower.tri(sig)] <- cov
    sig[upper.tri(sig)] = t(sig)[upper.tri(sig)] # create a symmetrical matrix

    covariate <- rmvnorm(n, mean = mu, sigma = sig)
    # flog.debug(paste("User chose to simulate continuous covariate. \n"))
    if (n_cat > 0) {
      covariate[,1:n_cat] = sapply(1:n_cat, function(i){
        cut_val = qnorm(prob[i], mean = mu[i], sd = sqrt(var[i]))
        flog.debug(cat("User chose to simulate", n_cov, "correlated binary covariate(s) with cut_val =", cut_val, "\n"))

        (covariate[, i] < cut_val)
      })
    }
  }

  colnames(covariate) = paste0("cov",cov_st:(cov_st + n_cov - 1))
  covariate
}

# raise covariates to power
# dt <- sample_cov
# change = list(c("cov1", "^", "2"),c("cov1", "*", "cov3"), c("cov1", "*", "cov3"), c("cov1", "*", "cov4"))
# keep = "cov1^2"
c_cov = function(dt, change, keep){
  dt2 <- dt
  new_cov_name = NULL
  for (i in 1:length(change)){
    if (change[[i]][2] == "^") {
      new_cov <- dt[, change[[i]][1]] ^ as.numeric(change[[i]][3])
      flog.debug("^")
    } else if (change[[i]][2] == "*") {
      new_cov <- dt[, change[[i]][1]] * dt[, change[[i]][3]]
      flog.debug("*")
    } else if (change[[i]][2] == "+") {
      new_cov <- dt[, change[[i]][1]] + dt[, change[[i]][3]]
      flog.debug("+")
    }
    dt2 <- cbind(new_cov, dt2)
    colnames(dt2)[1]<- paste(change[[i]],collapse="")
    new_cov_name = c(new_cov_name, paste(change[[i]],collapse=""))
  }
  flog.debug(cat("[c_cov] Original covariates to keep include", keep, "new covariates to keep include", new_cov_name, "\n"))
  flog.debug(cat("[c_cov] Dataset has variables", colnames(dt2), "\n"))
  dt3 <- dt2[, c(keep, new_cov_name, "trt"), drop=FALSE]
  flog.debug(cat("[c_cov] Simulate failure time based on", colnames(dt3), "\n"))
  dt3
}


# match individuals from the external dataset to the internal tx group
m_cov <- function(dt, match.formula){
  dt2 <- data.frame(dt)
  dt2$int = 1 - dt2$ext

  m.out <- matchit(as.formula(match.formula), method = "nearest", ratio = 1, caliper = 0.2, data = dt2)
  #matchit(ext ~ cov1 + cov2, method = "nearest", ratio = 1, caliper = 0.2, data = sample_cov.df) # match on all covariates

  dt.match <- match.data(m.out)

  ps_message(nrow(dt2[dt2$ext == 1,]) - nrow(dt.match[dt.match$ext == 1,]), " patients from the external arm are excluded. ",
          nrow(dt2[dt2$int == 1,]) - nrow(dt.match[dt.match$int == 1,]), " patients from the internal trial are excluded.")

  dt.match_ext <- dt.match[dt.match$ext == 1, colnames(dt.match) %notin% c("distance", "weights", "subclass")]

  dt_int = dt2[dt2$ext == 0,]
  match.all <- as.matrix(rbind(dt.match_ext, dt_int) %>% select(-"int"))
  ps_message("After matching, we have ", nrow(match.all), " patients in total.")
  return(match.all)
}

# Simulate time-to-events following weibull distribution
s_wb_t = function(dt, lambda, HR, beta, shape){
  hr = unique(HR)
  flog.debug(paste("[s_wb_t] HR =", hr, "\n"))

  n = nrow(dt)
  n_cov = sum(grepl("cov", colnames(dt))) #check if the data include any covariate
  cov = dt[, colnames(dt) %notin% c("trt", "ext"), drop = FALSE]
  flog.debug(cat("[s_wb_t] Simulate time using weibull distribution with sample size =", n, "number of covariates =", n_cov, ":", colnames(cov), "\n"))
  flog.debug(cat("[s_wb_t] Dimension of covariates", dim(cov), "length of beta", length(beta), "\n"))
  if (n_cov == 0) {
    log_k = - (log(lambda) + log(hr) * dt[, "trt"])/shape
  } else {
    log_k = - (log(lambda) + log(hr) * dt[, "trt"] + cov %*% beta)/shape #log(k), k is the scale
  }
  # print(paste("length of log of scale for the weibull distribution =", length(log_k)))
  sapply(log_k, function(i){rweibull(1, shape, exp(i))})
}


# Simulate time-to-events following piecewise-exponential distribution
s_exp_t = function(dt, lambda, HR, beta, t_itv){
  hr = unique(HR)
  flog.debug(paste("[s_wb_t] HR =", hr, "\n"))
  flog.debug(cat("[s_exp_t] Simulate time using piece-wise exponential distribution with baseline hazard rate of control arm =", lambda, "\n"))

  n = nrow(dt)
  n_cov = sum(grepl("cov", colnames(dt))) #check if the data include any covariate

  if (n_cov == 0) lambda_x =  exp(log(hr) * dt[, "trt"]) %*% t(lambda)
  else lambda_x = exp(log(hr) * dt[, "trt"] + dt[, colnames(dt) %notin% c("driftHR", "HR", "trt", "ext")] %*% beta) %*% t(lambda)
  flog.debug(cat("Baseline hazard rate has dimension of", dim(lambda_x), "\n"))

  apply(lambda_x, 1, function(i) rpwexp(1, rate = i, intervals = t_itv, cumulative=FALSE))
}


# Simulate enrollment time following piecewise-exponential distribution
s_exp_e = function(n, gamma, e_itv){

  aR <- e_itv
  if (n-sum(gamma[1:length(gamma)-1]*e_itv[1:length(gamma)-1])<=0) {
    stop ("[s_exp_e] User requested to keep the enrollment rate fixed but the total enrollment is already greater than the specified sample size prior to the last interval of R.")
  } else{

    aR[length(gamma)] <- (n - sum(gamma[1:length(gamma)-1]*e_itv[1:length(gamma)-1]))/gamma[length(gamma)]
    flog.debug(cat("[s_exp_e] Updated enrollment intervals", aR, "\n"))
    # Using the cumulative=TRUE option, enrollment times that piecewise constant over time can be generated.
    sample(rpwexp(n, rate=gamma, intervals = e_itv[1:length(gamma)-1], cumulative=TRUE))
  }

}


# Simulate censored survival times
g_one_t <- function(ext, dt,
                    gamma, e_itv, # enroll
                    event, lambda, HR, beta, shape, t_itv, # event
                    etaC, etaE, d_itv, # drop out
                    CCOD, CCOD_t, # CCOD
                    change, keep){

  # set new variables to NULL to avoid CRAN warnings
  time0 <- time1 <- cnsr <- cnsr1 <- ct <- enterT <- ltfuT <- NULL

  # print("lambda");print(lambda)
  d <- dt[dt[, "ext"] == ext,] # matrix
  flog.debug(cat("[g_one_t] We are simulating covariates for ext =", ext, "All covariates include", colnames(d), "\n"))

  x <- data.table(d)
  n = nrow(x)
  ssC = sum(x$trt == 0);  ssE = sum(x$trt == 1)
  flog.debug(paste("[g_one_t] Simulate time with sample size =", n, "number of control =", ssC, "number of treatment =", ssE, "\n"))

  x[, enterT := s_exp_e(n, gamma, e_itv)] #duration

  # update matrix



  d_change = d[, colnames(d) %notin% c("driftHR", "HR", "ext"), drop = FALSE]
  if (!is.null(change)) d_change = c_cov(d[, colnames(d) %notin% c("driftHR", "HR", "ext"), drop = FALSE], change, keep) #update the matrix

  flog.debug(cat("[g_one_t] Covariates for simulating failure times include", colnames(d_change), "\n"))
  flog.debug(paste("[g_one_t] HR =", HR, "\n"))

  # survival time
  if (event  == "weibull") x[, time0 := s_wb_t(d_change, lambda, HR, beta, shape)]
  else x[, time0 := s_exp_t(d_change, lambda, HR, beta, t_itv)]

  # adding LTFU
  x[x$"trt" == 0, ltfuT := rpwexp(ssC, etaC, intervals = d_itv, cumulative=FALSE)]
  x[x$"trt" == 1, ltfuT := rpwexp(ssE, etaE, intervals = d_itv, cumulative=FALSE)]
  x[, time1:= ifelse(time0 > ltfuT, ltfuT, time0)]
  x[, cnsr1 := ifelse(time0 > ltfuT, 1, 0)]
  x[, ct := enterT + time1] # ct: calendar time a subject had evt/censoring

  # add analysis start time
  if (CCOD == "fixed-first") { #pre-fixed, calculate from enrollment of first patients
    x[, time := ifelse(ct > CCOD_t, CCOD_t - enterT, x$time1)]
    x[, cnsr := ifelse(ct > CCOD_t, 1, x$cnsr1)]
    ps_message(paste(sum(x$time < 0), "patients enrolled after", CCOD_t,
                  "days after the study started were excluded from the analysis."))
    x = x[x$time >= 0, ]
  } else if (CCOD == "fixed-last") {
    st = max(x$enterT) + CCOD_t
    x[, time := ifelse(ct > st, st - enterT, x$time1)]
    x[, cnsr := ifelse(ct > st, 1, x$cnsr1)]
    x[, cnsr := ifelse(ct > CCOD_t, 1, x$cnsr1)]
    ps_message(paste(sum(x$time < 0), "patients enrolled after", CCOD_t,
                  "days after the enrollment of last patient were excluded from the analysis."))
  }
  else { # analysis starts when st events have been observed
    st = sort(x$ct[x$cnsr1 == 0])[pmin(sum(x$cnsr1 == 0), CCOD_t)] # compete leaving with event
    if (identical(st, numeric(0))) stop ("Please decrease the number of events required to start the analysis (CCOD_t)")
    x[, time := ifelse(ct > st, st - enterT, x$time1)]

    x[, cnsr := ifelse(ct > st, 1, x$cnsr1)]

    ps_message("User chooses to let number of events drive the CCOD", sum(x$time < 0),
            "patients were enrolled in the trial after ", st,
            "days and were excluded from the analysis")

    x = x[x$time >= 0, ]
  }
  x_fin <- x[, -c("enterT", "time0", "ltfuT", "time1", "cnsr1", "ct")]
  as.matrix(sapply(x_fin, as.numeric))
}


# Calculate propensity score
# Function r_post below calls this
c_ps <- function(dt, cov_name){
  flog.debug(cat("[c_ps] cov_name =", cov_name, "\n"))
  dat = as.data.frame(dt,stringsAsFactors=FALSE)
  dat$int = 1 - dat$ext
  var_col = c(cov_name, "int")
  dat2 = dat[, var_col]
  mod <- glm(int ~ ., family = binomial(), data = dat2)
  cbind(dt, "wgt" = mod$fitted.values)
}


# Generate posterior sample
r_post <- function(dt,
                   prior, pred,
                   r0, alpha, sigma,
                   n.chains, n.adapt, n.burn, n.iter, seed){
  flog.debug(cat("[r_post] dt =", colnames(dt), "\n"))
  cov_name = colnames(dt)[colnames(dt) %notin% c("driftHR", "HR", "trt", "ext", "time", "cnsr", "wgt")]
  flog.debug(cat("[r_post] cov_name =", cov_name, "\n"))

  # pred = c("ps", "cov1")
  # dt is a matrix
  if (sum(grepl("none", pred)) > 0) {
    x = dt[,c("trt"),drop=FALSE]
    n_cov = 0
    flog.debug("No covariates are used as predictors in the weibull distribution. Any other input is disregarded.\n")

  } else if (sum(grepl("all", pred)) > 0) {
    x = dt[,c("trt", cov_name)]
    n_cov = length(cov_name)
    flog.debug(cat("All covariates", cov_name, "are used as predictors in the weibull distribution. Any other input is disregarded. \n"))
  } else if (sum(grepl("ps", pred)) == 0) {
    x = dt[,c("trt", pred)]
    n_cov = length(pred)
    flog.debug(cat("Selected covariate(s)", pred, "are used as predictors in the weibull distribution. Any other input is disregarded. \n"))
  } else if (sum(grepl("ps", pred)) > 0 & ("wgt" %in% colnames(dt))) { # column wgt already existed
    x = dt[,c("trt", "wgt")]
    n_cov = 1
    flog.debug("Weight already provided in the data. It is used as predictor directly. \n")
  } else if (sum(grepl("ps", pred)) > 0 & length(pred) == 1){ # column wgt does not exist
    dt_ps <- c_ps(dt, cov_name)
    x = dt_ps[,c("trt", "wgt")]
    n_cov = 1
    flog.debug(cat("Propensity score is calculated based on all coariates", cov_name, "are used as predictors in the weibull distribution.\n"))
  } else if (sum(grepl("ps", pred)) > 0 & length(pred) > 1){
    dt_ps <- c_ps(dt, pred[pred != "ps"])
    x = dt_ps[,c("trt", "wgt")]
    n_cov = 1
    flog.debug(cat("Propensity score calculated based selected covariate(s)", pred[pred != "ps"],
                   "ise used as predictors in the weibull distribution.\n"))
  }

  data_list = list(N = nrow(dt), timev = dt[,'time'], event = 1-dt[,'cnsr'], ext = dt[,'ext'] +1, n_cov = n_cov, x = x)
  inits_list = list(.RNG.name="base::Super-Duper", .RNG.seed = seed, r0 = r0, alpha = alpha, beta = rep(0, n_cov + 1)) #.RNG.seed = 1,

  out_list = c("alpha", "beta", "r0", "HR_trt_cc", "HR_cc_hc")

  if (prior == "full_ext") {
    prior = "no_ext"
    data_list = list(N = nrow(dt), timev = dt[,'time'], event = 1-dt[,'cnsr'], n_cov = n_cov, x = x)

  } else if (prior == "no_ext") { # no borrow
    dt2 = dt[dt[, "ext"] == 0,] #remove external trial data

    if (sum(grepl("none", pred)) > 0) {
      x = dt2[,c("trt"),drop=FALSE]
    } else if (sum(grepl("all", pred)) > 0) {
      x = dt2[,c("trt", cov_name)]
    } else {
      x = dt2[,c("trt", pred)]
    }

    data_list = list(N = nrow(dt2), timev = dt2[,'time'], event = 1-dt2[,'cnsr'], n_cov = n_cov, x = x)

  } else if (prior == "cauchy" | prior == "unif") {
    inits_list = list(.RNG.name = "base::Super-Duper", .RNG.seed = seed, r0 = r0, alpha = alpha, beta = rep(0, n_cov + 1), sigma = sigma) #.RNG.seed = 1,
    out_list = c("alpha", "beta",  "r0", "prec", "HR_trt_cc", "HR_cc_hc")

  } else if (prior == "gamma") {
    out_list = c("alpha", "beta", "r0", "tau", "HR_trt_cc", "HR_cc_hc")
  }

  prior_txt = system.file("model", paste0(prior, ".txt"), package = "psborrow")
  # prior_txt = paste0("inst/model/", prior, ".txt")


  flog.debug(cat("[r_post] N =", data_list[['N']], "event =", sum(data_list[['event']] == 1),
                 "ext =", ifelse(is.null(data_list[['ext']]), NA, sum(data_list[['ext']] == 2)),
                 "n_cov =", data_list[['n_cov']], "x =", colnames(data_list[['x']]), "\n"
  ))
  flog.debug(cat("[r_post] r0 =", inits_list[['r0']],
                 "alpha =", inits_list[['alpha']], "beta =", inits_list[['beta']],
                 "sigma =", ifelse(is.null(inits_list[['sigma']]), NA, inits_list[['sigma']]),
                 ".RNG.seed = ", seed, "\n"
  ))

  jags.out <- jags.model(
    prior_txt,
    data = data_list,
    inits = inits_list,
    n.chains = n.chains,
    n.adapt = n.adapt,
    quiet = TRUE
  )

  update(jags.out, n.burn = n.burn)

  # If user has asked for psborrow to be quiet then we set progress bar to be
  # "none" instead of using the default JAGS argument specified by "jags.pb"
  progress_bar <- ifelse(
    options("psborrow.quiet")[[1]],
    "none",
    options("jags.pb")[[1]]
  )

  jags.sample <- coda.samples(
    jags.out,
    out_list,
    n.iter = n.iter,
    progress.bar = progress_bar
  )

  jags.sample
}



#' Conditional Message
#'
#' Simple wrapper function around [message()] that will supress
#' printing messages if the option `psborrow.quiet` is set to `TRUE`
#' i.e.
#' ```
#' options("psborrow.quiet" = TRUE)
#' ```
#'
#' @param ... Values passed onto [message()]
ps_message <- function(...){
  if (!getOption("psborrow.quiet")) {
    message(...)
  }
}



#' Check if user is in psborrow development environment
#'
#' Simple function which leverages the DESCRIPTION file
#' to check if the user is in a development environment
#' for psborrow.
#'
#' @return TRUE/FALSE flag (TRUE = in development environment)
is_psborrow_dev <- function() {
  desc <- file.path("DESCRIPTION")
  if (file.exists(desc)) {
    content <- read.dcf(desc)
    pkg <- content[1, ][["Package"]]
    return(pkg == "psborrow")
  }
  return(FALSE)
}
