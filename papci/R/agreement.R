#######################################################################################%
##### Main Function #####
#######################################################################################%
agreement <- function(x=NULL, y=NULL, m=NULL, n=NULL,
                      tb=NULL, baseline=NULL, comparator=NULL,
                      prev=NULL, conf.level=0.95,
                      alternative = "two.sided",
                      methods_pa = "all", methods_pv="all",
                      times = 1000, ...){

  method <- PPV_l <- PPV_u <- lower <- upper <- NPV_l <- NPV_u <- NULL
  ### Checking for Valid Inputs and transform to (x, y, m, n)
  if (any(is.null(c(x, y, m, n)))){

    if (!is.null(tb)){
      x = tb[1, 1]
      y = tb[1, 2]
      m = sum(tb[, 1])
      n = sum(tb[, 2])
    } else if (!is.null(baseline) & !is.null(comparator)) {

      # check missing
      if(any(is.na(c(baseline, comparator)))){
        baseline.copy <- baseline
        comparator.copy <- comparator
        na.base <- is.na(baseline)
        na.comp <- is.na(comparator)
        baseline = baseline[!na.base & !na.comp]
        comparator = comparator[!na.base & !na.comp]
        na.remove = length(baseline.copy) - length(baseline)
        print(paste0("Warning:", na.remove, " sample is removed due to NA in either baseline or comparator."))
      }

      # same length checking
      if ( all(unique(c(baseline, comparator)) %in% c(0, 1)) ){
        # 1: positive, 0: negative
        x = sum(baseline == 1 & comparator == 1)
        y = sum(baseline == 0 & comparator == 1)
        m = sum(baseline == 1)
        n = sum(comparator == 1)
      }else{
        stop("Input Error: input format incorrect (1:positive, 0:negative).")
      }
    }else{
      stop("Input Error: please provide valid inputs.")
    }

  }

  if ( any(!is.numeric(c(x, y, m, n))) | m <= 0 | n <= 0 | x < 0 | y < 0){
    stop("Input Error: (x, m, y, n) must be non-negative integers.")
  }

  if (is.null(prev)){
    stop("Input Error: please specify prevalence.")
  }

  valid_methods_pv <- c("Koopman","Katz","Neother","Gart_Nam","Bootstrap","Plug-In", "all")
  if (! methods_pv %in% valid_methods_pv){
    stop("Input Error: please specify appropriate methods for PPV and NPV CIs.",
         call. = FALSE)
  }

  if (alternative == "two.sided"){
    conf.level = 1 - (1 - conf.level) * 2
  }


  ### PPA and NPA
  results <- list()
  results$ppa <-
    binom::binom.confint(x = x, n = m, conf.level = conf.level, methods = methods_pa)
  results$npa <-
    binom::binom.confint(x = n - y, n = n, conf.level = conf.level, methods = methods_pa)

  ### PPV and NPV
  # CI of PPV and NPV
  PV_result <- NULL
  if (methods_pv == "Koopman" | methods_pv == "all"){
    tmp_res <- data.frame(do.call(cbind, Koopman(x, m, y, n, prev, conf.level)))
    tmp_res$method <- "Koopman"
    PV_result <- rbind(PV_result, tmp_res)
  }
  if (methods_pv == "Katz" | methods_pv == "all"){
    tmp_res <- data.frame(do.call(cbind, Katz(x, m, y, n, prev, conf.level)))
    tmp_res$method <- "Katz"
    PV_result <- rbind(PV_result, tmp_res)
  }
  if (methods_pv == "Neother" | methods_pv == "all"){
    tmp_res <- data.frame(do.call(cbind, Neo(x, m, y, n, prev, conf.level)))
    tmp_res$method <- "Neother"
    PV_result <- rbind(PV_result, tmp_res)
  }
  if (methods_pv == "Gart_Nam" | methods_pv == "all"){
    tmp_res <- data.frame(do.call(cbind, GN(x, m, y, n, prev, conf.level)))
    tmp_res$method <- "Gart_Nam"
    PV_result <- rbind(PV_result, tmp_res)
  }
  if (methods_pv == "Bootstrap" | methods_pv == "all"){
    tmp_res <- data.frame(do.call(cbind, Boot(x, m, y, n, prev, conf.level, times=times)))
    tmp_res$method <- "Bootstrap"
    PV_result <- rbind(PV_result, tmp_res)
  }
  if (methods_pv == "Plug-In" | methods_pv == "all"){
    tmp_res <- data.frame(do.call(cbind, PlugIn(x, m, y, n, prev, conf.level)))
    tmp_res$method <- "Plug-In"
    PV_result <- rbind(PV_result, tmp_res)
  }
  results$ppv <-
    PV_result %>%
    select(method, PPV_l, PPV_u) %>%
    mutate(mean = prev*x/m / ( prev*x/m + (1-prev)*y/n) ) %>%
    rename(lower = PPV_l, upper = PPV_u) %>%
    relocate(method, mean, lower, upper)
  results$npv <-
    PV_result %>%
    select(method, NPV_l, NPV_u) %>%
    mutate(mean =  (n-y)/n*(1-prev) / ((n-y)/n*(1-prev) + prev*(m-x)/m) ) %>%
    rename(lower = NPV_l, upper = NPV_u) %>%
    relocate(method, mean, lower, upper)

  return(results)
}


#######################################################################################%
##### Function for PPV/NPV confidence intervals by different method #####
#######################################################################################%

##### (1) Koopman (1984) #####
Koopman <- function(x, m, y, n, prev, conf.level=0.95){
  # CI for (1-NPA)/PPA and PPV
  fit <- PropCIs::riskscoreci(x1 = y, n1 = n, x2 = x, n2 = m, conf.level = conf.level)
  PPV_l <- prev / (prev + (1 - prev) * fit$conf.int[2])
  PPV_u <- prev / (prev + (1 - prev) * fit$conf.int[1])
  # CI for (1-PPA)/NPA and NPV
  fit <- PropCIs::riskscoreci(x1 = m-x, n1 = m, x2 = n-y, n2 = n, conf.level = conf.level)
  NPV_l <- (1 - prev) / (1 - prev + prev * fit$conf.int[2])
  NPV_u <- (1 - prev) / (1 - prev + prev * fit$conf.int[1])
  return(list(PPV_l = PPV_l,PPV_u = PPV_u, NPV_l = NPV_l, NPV_u = NPV_u))
}

##### (2) Katz log method (1978) #####
Katz <- function(x, m, y, n, prev, conf.level = 0.95){
  # CI for risk ratio using Katz log method
  Katz_formula <- function(x, m, y, n, conf.level){
    z = abs(stats::qnorm((1 - conf.level)/2))
    # CI for (x/m)/(y/n)
    tmp = (1 - x/m) / x + (1 - y/n) / y
    RR_l <- log(x/m) - log(y/n) - z * sqrt(tmp)
    RR_u <- log(x/m) - log(y/n) + z * sqrt(tmp)
    return(list(RR_l = RR_l, RR_u = RR_u))
  }

  # CI for (1-NPA)/PPA and PPV
  if(x == 0 & y == 0){ # special cases
    RR_l = 0
    RR_u = Inf
  }else if(x == 0 & y > 0){ # special cases
    fit = Katz_formula(y, n, 0.5, m, conf.level)
    RR_l = exp(fit$RR_l)
    RR_u = exp(fit$RR_u)
  }else if(x > 0 & y == 0){
    fit = Katz_formula(0.5, n, x, m, conf.level)
    RR_l = exp(fit$RR_l)
    RR_u = exp(fit$RR_u)
  }else if(x == m & y == n){
    fit = Katz_formula(y - 0.5, n, x - 0.5, m, conf.level)
    RR_l = exp(fit$RR_l)
    RR_u = exp(fit$RR_u)
  }else{ # General case
    fit = Katz_formula(y, n, x, m, conf.level)
    RR_l = exp(fit$RR_l)
    RR_u = exp(fit$RR_u)
  }
  PPV_l = prev / (prev + (1 - prev) * RR_u)
  PPV_u = prev / (prev + (1 - prev) * RR_l)

  # CI for (1-PPA)/NPA and NPV
  x = m - x
  y = n - y
  if(x==0 & y==0){ # special case
    RR_l = 0
    RR_u = Inf
  }else if(x==0 & y>0){ # special case
    fit = Katz_formula(0.5, m, y, n, conf.level)
    RR_l = exp(fit$RR_l)
    RR_u = exp(fit$RR_u)
  }else if(x > 0 & y == 0){ # special case
    fit = Katz_formula(x, m, 0.5, n, conf.level)
    RR_l = exp(fit$RR_l)
    RR_u = exp(fit$RR_u)
  }else if(x == m & y == n){ # special case
    fit = Katz_formula(x-0.5,m,y-0.5,n, conf.level)
    RR_l = exp(fit$RR_l)
    RR_u = exp(fit$RR_u)
  }else{ # General case
    fit = Katz_formula(x, m, y, n, conf.level)
    RR_l = exp(fit$RR_l)
    RR_u = exp(fit$RR_u)
  }
  NPV_l = (1 - prev) / (1 - prev + prev * RR_u)
  NPV_u = (1 - prev) / (1 - prev + prev * RR_l)

  return(list(PPV_l = PPV_l, PPV_u = PPV_u, NPV_l = NPV_l, NPV_u = NPV_u))
}

##### (3) Neother (1957) #####
Neo <- function(x, m, y, n, prev, conf.level = 0.95){
  Neo_formula <- function(x, m, y, n, conf.level = conf.level){
    # CI for (x/m)/(y/n)
    z = abs(stats::qnorm((1 - conf.level)/2))
    z2 = z * z
    tmp1 = sqrt( (1-x/m)/x + (1-y/n)/y + 0.25*z2*(1/x/x + 4*(1-x/m)/m/x) )
    tmp2 = x / m / (y / n) / (1 + z2 / m)
    RR_l = tmp2 * (1+ 0.5 * z2 / x - z * tmp1)
    RR_u = tmp2 * (1+ 0.5 * z2 / x + z * tmp1)
    return(list(RR_l = RR_l, RR_u = RR_u))
  }

  # CI for (1-NPA)/PPA and PPV
  if(x == 0 & y == 0){ # special case
    RR_l = 0
    RR_u = Inf
  }else if(x == 0 & y > 0){ # special case
    fit = Neo_formula(y, n, 0.5, m, conf.level)
    RR_l = fit$RR_l
    RR_u = fit$RR_u
  }else if(x > 0 & y == 0){ # special case
    fit = Neo_formula(0.5, n, x, m, conf.level)
    RR_l = fit$RR_l
    RR_u = fit$RR_u
  }else if(x == m & y == n){ # special case
    fit = Neo_formula(y - 0.5, n, x - 0.5, m, conf.level)
    RR_l = fit$RR_l
    RR_u = fit$RR_u
  }else{ # General case
    fit = Neo_formula(y, n, x, m, conf.level)
    RR_l = fit$RR_l
    RR_u = fit$RR_u
  }
  PPV_l = prev / (prev + (1 - prev) * RR_u)
  PPV_u = prev / (prev + (1 - prev) * max(0, RR_l))

  # CI for (1-PPA)/NPA and NPV
  x = m - x
  y = n - y
  if(x == 0 & y == 0){ # special case
    RR_l = 0
    RR_u = Inf
  }else if(x == 0 & y > 0){ # special case
    fit = Neo_formula(0.5, m, y, n, conf.level)
    RR_l = fit$RR_l
    RR_u = fit$RR_u
  }else if(x > 0 & y == 0){ # special case
    fit = Neo_formula(x, m, 0.5, n, conf.level)
    RR_l = fit$RR_l
    RR_u = fit$RR_u
  }else if(x == m & y == n){ # special case
    fit = Neo_formula(x - 0.5, m, y - 0.5, n, conf.level)
    RR_l = fit$RR_l
    RR_u = fit$RR_u
  }else{ # General case
    fit = Neo_formula(x, m, y, n, conf.level)
    RR_l = fit$RR_l
    RR_u = fit$RR_u
  }
  NPV_l = (1 - prev) / (1 - prev + prev * RR_u)
  NPV_u = (1 - prev) / (1 - prev + prev * max(RR_l, 0))

  # Output results
  return(list(PPV_l = PPV_l, PPV_u = PPV_u, NPV_l = NPV_l, NPV_u = NPV_u))
}

##### (4) Gart and Nam skewness correction (1988) #####
GN <- function(x, m, y, n, prev, conf.level = 0.95){
  # CI for (1-NPA)/PPA and PPV
  fit = ratesci::scasci(x1 = y, n1 = n, x2 = x, n2 = m, distrib = "bin",
                        contrast = "RR", level = conf.level)
  PPV_l = prev / (prev + (1 - prev) * fit$estimates[3])
  PPV_u = prev / (prev + (1 - prev) * fit$estimates[1])
  # CI for (1-PPA)/NPA and NPV
  fit = ratesci::scasci(x1= m - x, n1 = m, x2 = n - y, n2 = n, distrib = "bin",
                        contrast = "RR", level = conf.level)
  NPV_l=(1 - prev) / (1 - prev + prev * fit$estimates[3])
  NPV_u=(1 - prev) / (1 - prev + prev * fit$estimates[1])
  # Output results
  return(list(PPV_l = PPV_l, PPV_u = PPV_u, NPV_l = NPV_l, NPV_u = NPV_u))
}

##### (5) Bootstrap #####
Boot <-  function(x, m, y, n, prev, conf.level = 0.95, times){
  # CI for (1-NPA)/PPA and PPV
  X = c(rep(1, x), rep(0, m - x))
  Y = c(rep(0, y), rep(1, n - y))
  PPV = NULL
  NPV = NULL
  for(time in 1:times){
    set.seed(time)
    my_X = X[sample(1 : m, m, replace = TRUE)]
    my_Y = Y[sample(1 : n, n, replace = TRUE)]
    my_PPA = sum(my_X) / m
    my_NPA = sum(my_Y) / n
    PPV = c(PPV, prev / (prev + (1 - prev) * (1 - my_NPA) / my_PPA))
    NPV = c(NPV, (1 - prev) / (1 - prev + prev * (1 - my_PPA) / my_NPA))
  }
  low_q = (1 - conf.level) / 2
  up_q = 1 - low_q
  PPV_l = stats::quantile(PPV, low_q)
  PPV_u = stats::quantile(PPV, up_q)
  NPV_l = stats::quantile(NPV, low_q)
  NPV_u = stats::quantile(NPV, up_q)
  # Output Results
  return(list(PPV_l = PPV_l, PPV_u = PPV_u, NPV_l = NPV_l, NPV_u = NPV_u))
}

##### (6) Use PPA and NPA CI to plug in #####
PlugIn <- function(x, m, y, n, prev, conf.level = 0.95){
  # First derive the CI for PPA and NPA
  # Then plug in to PPV, NPV and find most conservative one
  PPA_CI = Hmisc::binconf(x, m, method = "wilson", alpha = 1 - conf.level)[2:3]
  PPA_L = PPA_CI[1]
  PPA_U = PPA_CI[2]
  NPA_CI = Hmisc::binconf(n - y, n, method = "wilson", alpha = 1 - conf.level)[2:3]
  NPA_L = NPA_CI[1]
  NPA_U = NPA_CI[2]
  # PPV CI
  PPV_1 = prev * PPA_L / (PPA_L * prev + (1 - NPA_L) * (1 - prev))
  PPV_2 = prev * PPA_L / (PPA_L * prev + (1 - NPA_U) * (1 - prev))
  PPV_3 = prev * PPA_U / (PPA_U * prev + (1 - NPA_L) * (1 - prev))
  PPV_4 = prev * PPA_U / (PPA_U * prev + (1 - NPA_U) * (1 - prev))
  PPV_l = min(c(PPV_1, PPV_2, PPV_3, PPV_4))
  PPV_u = max(c(PPV_1, PPV_2, PPV_3, PPV_4))
  # NPV CI
  NPV_1 = (1 - prev) * NPA_L / ((1 - PPA_L) * prev + NPA_L * (1 - prev))
  NPV_2 = (1 - prev) * NPA_L / ((1 - PPA_U) * prev + NPA_L * (1 - prev))
  NPV_3 = (1 - prev) * NPA_U / ((1 - PPA_L) * prev + NPA_U * (1 - prev))
  NPV_4 = (1 - prev) * NPA_U / ((1 - PPA_U) * prev + NPA_U * (1 - prev))
  NPV_l = min(c(NPV_1, NPV_2, NPV_3, NPV_4))
  NPV_u = max(c(NPV_1, NPV_2, NPV_3, NPV_4))
  # Output Results
  return(list(PPV_l = PPV_l, PPV_u = PPV_u, NPV_l = NPV_l, NPV_u = NPV_u))
}


