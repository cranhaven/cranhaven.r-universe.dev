
#----------------------------------------------------------------------------
#single trial
single_trial_po = function(or, pro_ctr, n, cf, threshold, method){

  C  = length(pro_ctr)
  #or = rep(or, C-1)
  dat_I = data_gene(or, sd, pro_ctr, n)
  N = n
  # Pick Bayesian or Frequentest method
  if (method == "Bayesian"){
    results = jags_po_model(dat_I)
    post_draws_I = results$Samples[[1]][,1]
    pr_trt_futility_I = mean(post_draws_I < 0)
  }else{
    fit.prior = ordinal::clm(response ~ treatment, data=dat_I, threshold = "flexible")
    mean = as.numeric(fit.prior$beta)
    sd = sqrt(fit.prior$vcov[C, C])
    samples_draws_I = rnorm(10000, mean, sd)
    pr_trt_futility_I = mean(samples_draws_I < 0)
  }

  stop_for_futility = (pr_trt_futility_I <= cf)

  if(stop_for_futility){
    result_string = "early stop due to the futility"
  }else{
    #get the rest of data  into trial
    #dat_II = data_gene(or, sd, pro_ctr, n)
    dat_II = dat_I
    N = N + n
    #combine two stages data
    dat = rbind(dat_I, dat_II)
    if (method == "Bayesian"){
      results = jags_po_model(dat)
      post_draws_II = results$Samples[[1]][,1]
      pr_trt_futility_II = mean(post_draws_II < 0)
    }else{
      fit.prior = ordinal::clm(response ~ treatment, data=dat, threshold = "flexible")
      mean = as.numeric(fit.prior$beta)
      sd = sqrt(fit.prior$vcov[C, C])
      samples_draws_II = rnorm(10000, mean, sd)
      pr_trt_futility_II = mean(samples_draws_II < 0)
    }

    if(pr_trt_futility_II >= threshold)
    {result_string = "success"
    }else if(pr_trt_futility_II < threshold)
    {result_string = "failure"}
  }
  output = c(result_string, N)
  return(output)
}

#----------------------------------------------------------------------------
#multiple trial

multiple_trial_po = function(sim_runs, or, pro_ctr, n, cf, threshold, method){
  preout = apply(or, MARGIN = 1, FUN=single_trial_po,
                 pro_ctr=pro_ctr, n=n, cf=cf, threshold=threshold,
                 method = method)
  prob = round(mean(preout[1,] == "success"), digits = 4)
  prob_et = round(100*mean(preout[1,] == "early stop due to the futility"), digits = 2)
  avgss = round(mean(as.numeric(preout[2,])), digits = 0)
  return(c(prob_et, prob, avgss))
}



#----------------------------------------------------------------------------
#single trial
single_trial_npo = function(or, sd, pro_ctr, U, n, cf, threshold, method){
  C  = length(pro_ctr)
  dat_I = data_gene_npo(or, sd, pro_ctr, n)
  N = n
  fit.error = c()
  if (method == "Bayesian"){
    results = jags_npo_model(dat_I, U)
    pr_trt_futility_I = results$PPUtilities
  }else if(method == "Frequentist"){

    fit = ordinal::clm(response ~ treatment, nominal = ~ treatment,
              data=dat_I, threshold = "flexible")

    message = tryCatch(ordinal::clm(response ~ treatment, nominal = ~ treatment,
                           data=dat_I, threshold = "flexible"),
                       error=function(e) e, warning=function(w) w)

    #utility function
    u = utility_func(fit, U, C)
    #obtain utility from proportions
    pr_trt_futility_I = mean(u[2,] > u[1,])
    if(is(message,"warning")){
      fit.error = fit
      warnings_I = "Yes"
    }else{warnings_I = "No"}

  }
  if(is.na(pr_trt_futility_I)){
    stop_for_futility = TRUE
    warnings_I = "No pr_trt_futility_I"
  }else{stop_for_futility = (pr_trt_futility_I <= cf)}
  # check if warning message exists


  warnings_II = "No"
  if(stop_for_futility){
    result_string = "early stop due to the futility"
  }else{
    #get the rest of data  into trial
    dat_II = dat_I
    N = N + n
    #combine two stages data
    dat = rbind(dat_I, dat_II)
    if (method == "Bayesian"){
      results = jags_npo_model(dat, U)
      pr_trt_futility_II = results$PPUtilities
    }else if(method == "Frequentist"){
      message = tryCatch(ordinal::clm(response ~ treatment, nominal = ~ treatment,
                             data=dat, threshold = "flexible"),
                         error=function(e) e, warning=function(w) w)
      fit = ordinal::clm(response ~ treatment, nominal = ~ treatment,
                data=dat, threshold = "flexible")
      #utility function
      u = utility_func(fit, U, C)
      #obtain utility from proportions
      pr_trt_futility_II = mean(u[2,] > u[1,])
      # check if warning message exists
      if(is(message,"warning")) {
        warnings_II = "Yes"
      }else{warnings_II = "No"}

    }

    if(pr_trt_futility_II >= threshold)
    {
      result_string = "success"
    } else if(pr_trt_futility_II < threshold)
    {
      result_string = "failure"
    }
  }
  #return(fit.error)
  return(c(result_string, N, warnings_I, warnings_II))
}

#----------------------------------------------------------------------------
#multiple trial
multiple_trial_npo = function(or.mat, sd, pro_ctr, U, n, cf, threshold, method){
  preout = apply(or.mat, MARGIN = 1, FUN=single_trial_npo, sd = sd,
                 pro_ctr=pro_ctr, U=U, n=n, cf=cf, threshold = threshold,
                 method = method)
  # remove those results with warning messages
  preout_filter = preout[,which(preout[3,] == "No" & preout[4,] == "No")]
  prob = round(mean(preout_filter[1,] == "success"),digits = 4)
  prob_et = round(100*mean(preout_filter[1,] == "early stop due to the futility"),digits = 2)
  avgss = round(mean(as.numeric(preout_filter[2,])),digits = 0)
  return(c(prob_et, prob, avgss))
}



# 3rd design - switch model
# 3rd design - switch model
single_trial_switch = function(or, sd, pro_ctr, n_po, n_npo, U, cf, threshold,
                               alpha, power, method){

  model_sele = NA;
  #choose larger sample size for stage 1
  if(n_po >= n_npo){
    n = n_po
  }else{n = n_npo}

  C  = length(pro_ctr)
  if(all(or == or[1])){
    dat_I = data_gene(or, sd, pro_ctr, n)
  }else{
    dat_I = data_gene_npo(or, sd, pro_ctr, n)
  }

  N = n
  #1. Stage I select PO or NPO through MCMC
  result_rjmcmc = rjmcmc_inter(or, sd, pro_ctr, n, U)
  prob = result_rjmcmc$result$`Posterior Model Probabilities`
  #print(prob)

  #2. Estimate parameters based on the model selection result (PO/NPO)
  if(prob[1] > prob[2]){
    model_sele = 'PO'
    results = jags_po_model(dat_I)
    post_draws_I = results$Samples[[1]][,1]
    pr_trt_futility_I = mean(post_draws_I < 0)
  }else{
    model_sele = 'NPO'
    results = jags_npo_model(dat_I, U)
    pr_trt_futility_I = results$PPUtilities
  }

  stop_for_futility = (pr_trt_futility_I <= cf)
  warnings = "No"

  #3. Decide if we go/no go to stage 2
  if(stop_for_futility){
    result_string = "early stop due to the futility"
  }else{
    #4. Re-estimate sample size based on the model selection result
    if(prob[1] > prob[2]){

      dat_II = data_gene(or, sd, pro_ctr, n_po)
      N = N + n_po
      dat = rbind(dat_I, dat_II)
      if(method == "Bayesian"){
        results = jags_po_model(dat)
        post_draws_II = results$Samples[[1]][,1]
        pr_trt_futility_II = mean(post_draws_II < 0)
      }else if(method == "Frequentist"){
        fit.prior = ordinal::clm(response ~ treatment, data=dat, threshold = "flexible")
        mean.fit = as.numeric(fit.prior$beta)
        sd.fit = sqrt(fit.prior$vcov[C, C])
        samples_draws_II = rnorm(10000, mean.fit, sd.fit)
        pr_trt_futility_II = mean(samples_draws_II < 0)
      }
    }
    if(prob[2] > prob[1]){

      dat_II = dat_I
      #dat_II = data_gene_npo(or, sd, pro_ctr, n_npo)
      N = N + n_npo
      dat = rbind(dat_I, dat_II)
      if(method == "Bayesian"){
        results = jags_npo_model(dat, U)
        pr_trt_futility_II = results$PPUtilities
      }else if(method == "Frequentist"){
        message = tryCatch(ordinal::clm(response ~ treatment, nominal = ~ treatment,
                                        data=dat, threshold = "flexible"),
                           error=function(e) e, warning=function(w) w)
        fit = ordinal::clm(response ~ treatment, nominal = ~ treatment,
                           data=dat, threshold = "flexible")
        #utility function
        u = utility_func(fit,  U, C)
        #obtain utility from proportions
        pr_trt_futility_II = mean(u[2,] > u[1,])
        # check if warning message exists
        if(is(message,"warning")) {
          warnings = "Yes"
        }else{warnings = "No"}
      }
    }

    if(pr_trt_futility_II >= threshold){
      result_string = "success"
    }else if(pr_trt_futility_II < threshold)
    {result_string = "failure"}
  }
  output = c(result_string, N, warnings, model_sele)
  return(output)
}


multiple_trial_switch = function(or, sim_runs, sd, pro_ctr, n_po, n_npo,
                                 U, cf, threshold, method){
  #nmax: maximum sample size used for type I error control
  #n: sample size for first stage
  if(all(or == or[1])){
    #random generate or
    ors = exp(rnorm(sim_runs, mean = log(or[1]), sd = sd))
    or.mat = matrix(rep(ors, times = 1, each=length(or)), nrow = sim_runs,
                    ncol = length(or), byrow = TRUE)
  }else{
    #directly put into func
    or.mat = matrix(rep(or, sim_runs), nrow = sim_runs,
                    ncol = length(or), byrow = TRUE)
  }
  preout = apply(or.mat, MARGIN = 1, FUN = single_trial_switch, sd=sd,
                 pro_ctr, n_po, n_npo, U, cf = cf, threshold=threshold,
                 method = method)
  preout_filter = preout[,which(preout[3,] == "No")]
  prob = mean(preout_filter[1,] == "success")
  prob_et = 100*mean(preout_filter[1,] == "early stop due to the futility")
  avgss = mean(as.numeric(preout_filter[2,]))
  #m = table(factor(preout_filter[4,], levels = c('PO', 'NPO')))/dim(preout_filter[,which(preout_filter[1,]!= 'early stop due to the futility')])[2]*100
  m = table(factor(preout_filter[4,], levels = c('PO', 'NPO')))/dim(preout_filter)[2]*100
  return(c(prob_et, prob, avgss, m))
}


single_trial_switch_new = function(or, sd, pro_ctr, n, U, cf, threshold,
                               alpha, power, method){

  model_sele = NA;

  C  = length(pro_ctr)
  if(all(or == or[1])){
    dat_I = data_gene(or, sd, pro_ctr, n)
  }else{
    dat_I = data_gene_npo(or, sd, pro_ctr, n)
    }

  N = n
  #1. Stage I select PO or NPO through MCMC
  result_rjmcmc = rjmcmc_inter(or, sd, pro_ctr, n, U)
  prob = result_rjmcmc$result$`Posterior Model Probabilities`
  #print(prob)

  #2. Estimate parameters based on the model selection result (PO/NPO)
  if(prob[1] > prob[2]){
    model_sele = 'PO'
    results = jags_po_model(dat_I)
    post_draws_I = results$Samples[[1]][,1]
    pr_trt_futility_I = mean(post_draws_I < 0)
  }else{
    model_sele = 'NPO'
    results = jags_npo_model(dat_I, U)
    pr_trt_futility_I = results$PPUtilities
  }

  stop_for_futility = (pr_trt_futility_I <= cf)
  warnings = "No"

  #3. Decide if we go/no go to stage 2
  if(stop_for_futility){
    result_string = "early stop due to the futility"
  }else{
    #4. Re-estimate sample size based on the model selection result
    if(prob[1] > prob[2]){

      dat_II = data_gene(or, sd, pro_ctr, n)
      N = N + n
      dat = rbind(dat_I, dat_II)
      if(method == "Bayesian"){
        results = jags_po_model(dat)
        post_draws_II = results$Samples[[1]][,1]
        pr_trt_futility_II = mean(post_draws_II < 0)
      }else if(method == "Frequentist"){
        fit.prior = ordinal::clm(response ~ treatment, data=dat, threshold = "flexible")
        mean.fit = as.numeric(fit.prior$beta)
        sd.fit = sqrt(fit.prior$vcov[C, C])
        samples_draws_II = rnorm(10000, mean.fit, sd.fit)
        pr_trt_futility_II = mean(samples_draws_II < 0)
      }
    }
    if(prob[2] > prob[1]){

      dat_II = dat_I
      #dat_II = data_gene_npo(or, sd, pro_ctr, n_npo)
      N = N + n
      dat = rbind(dat_I, dat_II)
      if(method == "Bayesian"){
        results = jags_npo_model(dat, U)
        pr_trt_futility_II = results$PPUtilities
      }else if(method == "Frequentist"){
        message = tryCatch(ordinal::clm(response ~ treatment, nominal = ~ treatment,
                               data=dat, threshold = "flexible"),
                           error=function(e) e, warning=function(w) w)
        fit = ordinal::clm(response ~ treatment, nominal = ~ treatment,
                  data=dat, threshold = "flexible")
        #utility function
        u = utility_func(fit,  U, C)
        #obtain utility from proportions
        pr_trt_futility_II = mean(u[2,] > u[1,])
        # check if warning message exists
        if(is(message,"warning")) {
          warnings = "Yes"
        }else{warnings = "No"}
      }
    }

    if(pr_trt_futility_II >= threshold){
      result_string = "success"
    }else if(pr_trt_futility_II < threshold)
    {result_string = "failure"}
  }
  output = c(result_string, N, warnings, model_sele)
  return(output)
}


multiple_trial_switch_new = function(or, sim_runs, sd, pro_ctr, n,
                                 U, cf, threshold, method){
  #nmax: maximum sample size used for type I error control
  #n: sample size for first stage
  if(all(or == or[1])){
    #random generate or
    ors = exp(rnorm(sim_runs, mean = log(or[1]), sd = sd))
    or.mat = matrix(rep(ors, times = 1, each=length(or)), nrow = sim_runs,
                    ncol = length(or), byrow = TRUE)
  }else{
    #directly put into func
    or.mat = matrix(rep(or, sim_runs), nrow = sim_runs,
                    ncol = length(or), byrow = TRUE)
  }
  preout = apply(or.mat, MARGIN = 1, FUN = single_trial_switch, sd=sd,
                 pro_ctr, n, U, cf = cf, threshold=threshold,
                 method = method)
  preout_filter = preout[,which(preout[3,] == "No")]
  prob = round(mean(preout_filter[1,] == "success"), digits = 4)
  prob_et = round(100*mean(preout_filter[1,] == "early stop due to the futility"), digits = 2)
  avgss = round(mean(as.numeric(preout_filter[2,])), digits = 0)
  m = table(factor(preout_filter[4,], levels = c('PO', 'NPO')))/dim(preout_filter[,which(preout_filter[1,]!= 'early stop due to the futility')])[2]*100
  return(c(prob_et, prob, avgss, m))
}


mean_u = function(U,p1,p2){
  u1 = t(U)%*%p1
  u2 = t(U)%*%p2
  dif = u2-u1
  return(c(u1, u2, dif))
}


jags_po_model = function(dat){
  dat.ord <- dat
  dat.ord$treatment <- factor(dat.ord$treatment)
  dat.ord$response <- factor(dat.ord$response)
  fit.prior <- ordinal::clm(response ~ treatment, data=dat.ord, threshold = "flexible")

  modelstring = " model {
  for (i in 1:N){
  mu[i] <- x[i]*delta
  logit(Q[i,1]) <- gamma[1] - mu[i]
  prob[i,1] <- Q[i,1]
  for (c in 2:(C-1)){
  logit(Q[i,c]) <- gamma[c] - mu[i]
  prob[i,c] <- Q[i,c] - Q[i,(c-1)]
  }
  prob[i,C] <- 1-Q[i,(C-1)]
  y[i] ~ dcat(prob[i,1:C])
  }

  for (c in 1:(C-1)){
  gamma.star[c] ~ dnorm(0, 0.1)
  }
  gamma[1:(C-1)] <- sort(gamma.star)
  for (j in 1:4){
  z[j] ~ dnorm(0, 1/(sigma^2))
  }
  delta ~ dnorm(b, sd.prior)
  alpha ~ dnorm(0, 1)
}"
  x <- as.numeric(dat.ord$treatment)-1
  y <- as.numeric(dat.ord$response)
  data <- list(x = x, y = y, N = length(x),
               C = length(unique(dat.ord$response)))
  hyper <- list(#gamma1.prior = as.numeric(fit.prior$coefficients[1]),
    #gamma5.prior = as.numeric(fit.prior$coefficients[5]),
    b = as.numeric(fit.prior$beta),
    sd.prior = sqrt(fit.prior$vcov[data$C, data$C]),
    sigma = 1.5
  )
  init = list( gamma.star = rnorm(data$C-1), delta = rnorm(1), z = rnorm(4))
  model = rjags::jags.model(textConnection(modelstring), data = append(data, hyper),
                            n.chains = 3, inits = init, n.adapt = 500)
  update(model, n.iter = 2000)
  output=rjags::coda.samples(model = model, variable.names = c("gamma", "delta", "z"),
                             n.iter = 20000, thin = 1)
  ess = coda::effectiveSize(output)
  estimation = summary(output)$statistics
  SummaryOutput = list(estimation, ess, output)
  names(SummaryOutput) = c("EST", "ESS","Samples")
  return(SummaryOutput)
}


jags_npo_model = function(dat, U){
  dat.ord <- dat
  dat.ord$treatment <- factor(dat.ord$treatment)
  dat.ord$response <- factor(dat.ord$response)
  #fit.prior <- clm(response ~ treatment, nominal = ~ treatment,
  #                 data=dat.ord, threshold = "flexible")
  #print(fit.prior)

  modelstring = " model {
  for (c in 1:(C-1)){
  logit(Q[1,c]) <- gamma[c]
  logit(Q[2,c]) <- gamma[c] - delta[c]
  }
  for (j in 1:2){
  prob[j,1] <- Q[j,1]
  for (c in 2:(C-1)){
  prob[j,c] <- Q[j,c] - Q[j,(c-1)]
  }
  prob[j,C] <- 1- Q[j, (C-1)]
  }
  for (i in 1:N){
  y[i] ~ dcat(prob[x[i],])
  }

  # Prior
  for (c in 1:(C-1)){
  gamma.star[c] ~ dnorm(0, 0.1)
  delta[c] ~ dnorm(b[c], sd.prior[c])
  }
  gamma[1:(C-1)] <- sort(gamma.star)
  # Mean Utilities
  for (j in 1:2){
  u[j] <- inprod(U[1:C], prob[j,1:C])
  }

}"
  x <- as.numeric(dat.ord$treatment)
  y <- as.numeric(dat.ord$response)
  data <- list(x = x, y = y, N = length(x), C = length(unique(dat.ord$response)), U = U)
  #hyper <- list(b = -as.numeric(fit.prior$alpha.mat[2, 1:(data$C-1)]),
  #              sd.prior = as.vector(sqrt(diag(as.matrix(fit.prior$vcov))[data$C:dim(fit.prior$vcov)[1]]))
  #)
  C = length(unique(dat.ord$response))
  hyper <-list(b=rep(0, (C-1)), sd.prior = rep(0.1,(C-1)))
  init = list(delta = rep(0,(C-1)), gamma.star = rnorm(C-1))
  model = rjags::jags.model(textConnection(modelstring), data = append(data, hyper),
                            n.chains = 1, inits = init, n.adapt = 500)
  update(model, n.iter = 2000)
  output=rjags::coda.samples(model = model, variable.names = c( "gamma","delta", "u"),
                             n.iter = 20000, thin = 1)
  ess = coda::effectiveSize(output)
  estimation = summary(output)$statistics
  samples = as.matrix(output)
  pp.u = mean(samples[,dim(samples)[2]] > samples[,(dim(samples)[2]-1)])
  SummaryOutput = list(estimation, ess, output, pp.u)
  names(SummaryOutput) = c("EST", "ESS", "Samples", "PPUtilities")
  return(SummaryOutput)
}

utility_func = function(fit.prior, Uscore, C){
  #delta
  delta = -as.numeric(fit.prior$alpha.mat[2, 1:(C-1)])
  delta.sd = as.vector(sqrt(diag(as.matrix(fit.prior$vcov))[C:dim(fit.prior$vcov)[1]]))
  #gamma
  gamma = as.numeric(fit.prior$alpha.mat[1, 1:(C-1)])
  gamma.sd = as.vector(sqrt(diag(as.matrix(fit.prior$vcov))[1:(C-1)]))

  d = rbind(cbind(delta, delta.sd),cbind(gamma, gamma.sd))

  samples = apply(d, 1, function(x) rnorm(100, mean = x[1], sd = abs(x[2])))
  u = matrix(NA, nrow = 2, ncol = dim(samples)[1])
  #use posterior samples to calculate proportions
  for (i in 1:dim(samples)[1]){
    Q = matrix(NA, nrow = 2, ncol = C-1)
    prob = matrix(NA, nrow = 2, ncol = C)
    sample = samples[i,]
    for (c in 1:(C-1)){
      Q[1,c] = plogis(sample[c+5])
      Q[2,c] = plogis(sample[c+5] - sample[c])
    }
    for (j in 1:2){
      prob[j,1] <- Q[j,1]
      for (c in 2:(C-1)){
        prob[j,c] <- Q[j,c] - Q[j,(c-1)]
      }
      prob[j,C] <- 1- Q[j, (C-1)]
    }
    u[,i] = prob %*% Uscore
  }
  return(u)
}


