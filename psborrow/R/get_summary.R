# Methods for calculating summary statistics for MCMC

`%notin%` <- Negate(`%in%`)

thm <-  theme(axis.title.x = element_blank(),
              legend.position = "none",
              panel.grid.major.x = element_blank(),
              panel.grid.major.y = element_line(linewidth =.1, color="grey"),
              panel.background = element_rect(fill = "transparent",colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA),
              text = element_text(size=15))


#' Generate summary statistics for the MCMC chains
#'
#' @param samples an object of class \code{mcmc.list}
#' @return a vector containing the mean, median, sd, reject rate for the MCMC chains
#'
#' @keywords method
rej_est = function(samples){
  summ = summary(samples)[[1]]
  quantiles <- summary(samples)[[2]]
  reject = quantiles["HR_trt_cc","97.5%"] < 1
  mean_HR  =  summ["HR_trt_cc","Mean"]
  sd_HR = summ["HR_trt_cc","SD"]
  # post_median_hr_trt_cc = quantiles["HR_trt_cc","50%"]
  mean_driftHR  =  summ["HR_cc_hc", "Mean"]
  sd_driftHR = summ["HR_cc_hc","SD"]
  # print(paste("reject", reject,
  #             "mean_HR", mean_HR, "sd_HR", sd_HR,
  #             "mean_driftHR", mean_driftHR,  "sd_driftHR", sd_driftHR))
  c("reject" = reject,
    "mean_HR" = mean_HR, "sd_HR" = sd_HR,
    "mean_driftHR"  = mean_driftHR,  "sd_driftHR"  = sd_driftHR)
}


#' Generate summary statistics of a simulation scenario
#'
#' @param dt a \code{data.frame} containing summary statistics for the posterior samples from each simulation
#' @return a \code{data.frame} containing the mean and sd of posterior HR between treatment and control arm, the posterior mean and sd of HR between internal control and external control arm, reject rate, variance, bias and mse of the simulation set
#'
#' @export
#' @keywords method
get_summary = function(dt){
  # set new variables to NULL to avoid CRAN warnings
  reject <- mean_HR <- sd_HR <- HR <- mean_driftHR <- sd_driftHR <- NULL
  bias <- mean_HR_trt_cc <- mean_reject <- nsim <- NULL

  dt %>% group_by(dt$HR, dt$driftHR, dt$prior, dt$pred) %>%
    summarize(nsim = n(),
              mean_reject = mean(reject),
              mean_HR_trt_cc = mean(mean_HR),
              sd_HR_trt_cc = mean(sd_HR),
              bias =  mean_HR_trt_cc - mean(HR),
              var = (1/(nsim - 1))*sum((mean_HR - mean_HR_trt_cc)^2),
              sd = sqrt(var),
              mse = var + bias^2,
              mean_HR_cc_hc = mean(mean_driftHR),
              sd_HR_cc_hc = mean(sd_driftHR),
              .groups = 'drop') %>%
    rename(HR = 'dt$HR', driftHR = 'dt$driftHR', prior = 'dt$prior', pred = 'dt$pred',
           reject = mean_reject)

}

#' Plot type 1 error
#'
#' @description Plot type 1 error for each prior distribution according to selected simulation parameters
#'
#' @param dt a \code{data.frame} containing summary statistics for the posterior samples from each simulation generated with \code{get_summary()}. Must contain simulations for HR = 1.0.
#' @param driftHR the driftHR between the external and internal control arms for which the type 1 error should be plotted. Must be within \code{unique(dt$driftHR)}.
#' @param pred the predictors used when fitting the exponential distribution in MCMC for which the type 1 error should be plotted. Must be within \code{unique(dt$pred)}.
#'
#' @return a bar plot of class \code{ggplot} containing type 1 error proportions for each prior distribution.
#'
#' @export
#'
#' @keywords method
plot_type1error <- function(dt, driftHR = 1, pred = "none"){

  # Verify inputs are valid
  ## Make sure the output came from get_summary() and not run_mcmc()
  if (!'data.frame' %in% class(dt) |
      !all(colnames(dt) %in% c("HR", "driftHR", "prior", "pred", "nsim", "reject", "mean_HR_trt_cc",
                         "sd_HR_trt_cc", "bias", "var", "sd", "mse", "mean_HR_cc_hc",
                         "sd_HR_cc_hc"))) {
    stop('dt is not a summary data.frame from get_summary()')
  }

  ## Check that HR = 1 is included
  if (1.0 %notin% unique(dt$HR)) {
    stop("dt does not include HR = 1.0, so type 1 error cannot be calculated.
    Ensure that a true HR of 1.0 is included as an option in simu_cov().")
  }

  ## Check that the selected driftHR is included
  if (driftHR %notin% unique(dt$driftHR)) {
    stop(paste0("dt does not include a driftHR of ", driftHR,".
    Acceptable arguments to driftHR are c(", paste0(unique(dt$driftHR),collapse=", "), ").
    Did you include driftHR = ", driftHR, " as an option in simu_cov()?"))
  }

  ## Check that the selected pred is included
  if (pred %notin% unique(dt$pred)) {
    stop(paste0("dt does not include a pred of '", pred,"'.
    Acceptable arguments to pred are c('", paste0(unique(dt$pred), collapse = "', '"), "').
    Did you include pred = '", pred, "' as an option in set_prior()?"))
  }

  # Set factor levels for plotting
  dt$prior <- factor(dt$prior, levels = c("no_ext", "cauchy", "gamma", "full_ext"))

  # Filter results for driftHR and pred, and separate models with no ext
  dt2 = dt[dt$HR == 1 & dt$driftHR == driftHR & dt$pred == pred & dt$prior != "no_ext",]
  ref = dt[dt$HR == 1 & dt$driftHR == driftHR & dt$pred == pred & dt$prior == "no_ext",
           "reject", drop = TRUE]

  p1 <- ggplot(mapping = aes(y = dt2$reject, x = dt2$prior, fill = dt2$prior)) +
    geom_bar(position = position_dodge2(width = 1.2, preserve = "single"), stat="identity") +
    labs(title = "Summarizing posterior distributions: Type 1 Error",
         subtitle = paste0("pre-specified HR = 1, driftHR =", dt2$driftHR)) +
    ylab("Type 1 error") +
    geom_text(mapping = aes(y = dt2$reject, label = round(dt2$reject, 4)), vjust = -0.8) +
    scale_x_discrete(breaks=c("no_ext", "cauchy", "gamma", "full_ext"),
                     labels=c("No borrow", "Half-Cauchy", "Gamma", "Full borrow")) +

    scale_linetype_manual(name = "No Borrowing", values = "solid", labels = "") +
    scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9")) + thm

  if(length(ref) == 0){
    p1
  } else {
    p1 +
      geom_hline(mapping = aes(yintercept = ref, linetype=factor(ref)), colour = "purple") +
      labs(caption = paste0("Horizontal purple line refers to the type 1 error without any external arm (", round(ref,4),")"))+
      theme(plot.caption=element_text(size=10))
  }
}

#' Plot power
#'
#' @description Plot power for each prior distribution according to selected simulation parameters
#'
#' @param dt a \code{data.frame} containing summary statistics for the posterior samples from each simulation generated with \code{get_summary}.
#' @param HR pre-specified HR between treatment and control arm in the internal trial for which the power should be plotted. Must be within \code{unique(dt$HR)}.
#' @param driftHR pre-specified HR between external control arm and internal control arm for which the power should be plotted. Must be within \code{unique(dt$driftHR)}.
#' @param pred predictors to use when fitting exponential distribution in MCMC for which the power should be plotted. Must be within \code{unique(dt$pred)}.
#'
#' @return a bar plot of class \code{ggplot} containing the power for each prior distribution.
#'
#' @export
#'
#' @keywords method
plot_power <- function(dt, HR = 0.67, driftHR = 1, pred = "none") {

  # Verify inputs are valid
  ## Make sure the output came from get_summary() and not run_mcmc()
  if (!'data.frame' %in% class(dt) |
      !all(colnames(dt) %in% c("HR", "driftHR", "prior", "pred", "nsim", "reject", "mean_HR_trt_cc",
                               "sd_HR_trt_cc", "bias", "var", "sd", "mse", "mean_HR_cc_hc",
                               "sd_HR_cc_hc"))) {
    stop('dt is not a summary data.frame from get_summary()')
  }

  ## Check that HR = 1 is not specified (this would just give you type 1 error)
  if (HR == 1) {
    stop("HR = 1 is not a valid input into plot_power(),
         as power is the probability of rejecting HR = 1
         given the true HR is not 1. Did you mean
         to call plot_type1error()?")
  }

  ## Check that the selected HR is included
  if (HR %notin% unique(dt$HR)) {
    stop(paste0("dt does not include an HR of ", HR,".
    Acceptable arguments to HR are c(", paste0(unique(dt$HR),collapse=", "), ").
    Did you include HR = ", HR, " as an option in simu_cov()?"))
  }

  ## Check that the selected driftHR is included
  if (driftHR %notin% unique(dt$driftHR)) {
    stop(paste0("dt does not include a driftHR of ", driftHR,".
    Acceptable arguments to driftHR are c(", paste0(unique(dt$driftHR),collapse=", "), ").
    Did you include driftHR = ", driftHR, " as an option in simu_cov()?"))
  }

  ## Check that the selected pred is included
  if (pred %notin% unique(dt$pred)) {
    stop(paste0("dt does not include a pred of '", pred,"'.
    Acceptable arguments to pred are c('", paste0(unique(dt$pred), collapse = "', '"), "').
    Did you include pred = '", pred, "' as an option in set_prior()?"))
  }

  dt$prior <- factor(dt$prior, levels = c("no_ext", "cauchy", "gamma", "full_ext"))
  dt2 = dt[dt$HR == HR & dt$driftHR == driftHR & dt$pred == pred & dt$prior != "no_ext",]
  ref = dt[dt$HR == HR & dt$driftHR == driftHR & dt$pred == pred & dt$prior == "no_ext",
           "reject", drop = TRUE]

  p1 <- ggplot(mapping = aes(y = dt2$reject, x = dt2$prior, fill = dt2$prior)) +
    geom_bar(position = position_dodge2(width = 1.2, preserve = "single"), stat="identity") +
    labs(title = "Summarizing posterior distributions: power",
         subtitle = paste0("HR = ", HR, ", driftHR = ", driftHR)) +
    ylab("Power") +
    geom_text(mapping = aes(y = dt2$reject, label = round(dt2$reject, 4)), vjust = -0.8) +
    scale_x_discrete(breaks=c("cauchy", "gamma", "full_ext"),
                     labels=c("Half-Cauchy", "Gamma", "Full borrow")) +
    scale_linetype_manual(name = "No Borrowing", values = "solid", labels = "") +
    scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9")) + thm

  if(length(ref) == 0){
    p1
  } else {
    p1 +
      geom_hline(mapping = aes(yintercept = ref, linetype=factor(ref)), colour = "purple") +
      labs(caption = paste0("Horizontal purple line refers to the power without any external arm (", round(ref,4),")"))+
      theme(plot.caption=element_text(size=10))
  }
}

#' Plot mean posterior hazard ratio between treatment and control
#'
#' @description Plot mean posterior hazard ratio between treatment and control
#'
#' @param dt a \code{data.frame} containing summary statistics for the posterior samples from each simulation generated with \code{get_summary}.
#' @param HR pre-specified HR between treatment and control arm in the internal trial for which the mean posterior hazard ratio should be plotted. Must be within \code{unique(dt$HR)}.
#' @param driftHR pre-specified HR between external control arm and internal control arm for which the mean posterior hazard ratio should be plotted. Must be within \code{unique(dt$driftHR)}.
#' @param pred predictors to use when fitting exponential distribution in MCMC for which the mean posterior hazard ratio should be plotted. Must be within \code{unique(dt$pred)}.
#'
#' @return a plot of class \code{ggplot} containing the mean posterior hazard ratio for each prior distribution.
#'
#' @export
#'
#' @keywords method
plot_hr <- function(dt, HR = 0.67, driftHR = 1, pred = "none") {

  # Verify inputs are valid
  ## Make sure the output came from get_summary() and not run_mcmc()
  if (!'data.frame' %in% class(dt) |
      !all(colnames(dt) %in% c("HR", "driftHR", "prior", "pred", "nsim", "reject", "mean_HR_trt_cc",
                               "sd_HR_trt_cc", "bias", "var", "sd", "mse", "mean_HR_cc_hc",
                               "sd_HR_cc_hc"))) {
    stop('dt is not a summary data.frame from get_summary()')
  }

  ## Check that the selected HR is included
  if (HR %notin% unique(dt$HR)) {
    stop(paste0("dt does not include an HR of ", HR,".
    Acceptable arguments to HR are c(", paste0(unique(dt$HR),collapse=", "), ").
    Did you include HR = ", HR, " as an option in simu_cov()?"))
  }

  ## Check that the selected driftHR is included
  if (driftHR %notin% unique(dt$driftHR)) {
    stop(paste0("dt does not include a driftHR of ", driftHR,".
    Acceptable arguments to driftHR are c(", paste0(unique(dt$driftHR),collapse=", "), ").
    Did you include driftHR = ", driftHR, " as an option in simu_cov()?"))
  }

  ## Check that the selected pred is included
  if (pred %notin% unique(dt$pred)) {
    stop(paste0("dt does not include a pred of '", pred,"'.
    Acceptable arguments to pred are c('", paste0(unique(dt$pred), collapse = "', '"), "').
    Did you include pred = '", pred, "' as an option in set_prior()?"))
  }

  dt$prior <- factor(dt$prior, levels = c("no_ext", "cauchy", "gamma", "full_ext"))
  dt2 = dt[dt$HR == HR & dt$driftHR == driftHR & dt$pred == pred & dt$prior != "no_ext",]
  ref = dt[dt$HR == HR & dt$driftHR == driftHR & dt$pred == pred & dt$prior == "no_ext", "mean_HR_trt_cc", drop = TRUE]

  p1 <- ggplot(mapping = aes(y = dt2$mean_HR_trt_cc, x = dt2$prior, fill = dt2$prior)) +
    geom_point() +
    geom_errorbar(mapping=aes(x = dt2$prior,
                              ymin = dt2$mean_HR_trt_cc - dt2$sd_HR_trt_cc,
                              ymax = dt2$mean_HR_trt_cc + dt2$sd_HR_trt_cc),
                  width=0.2,
                  color="orange") +
    labs(title = "Summarizing posterior distributions: mean posterior hazard ratio",
         subtitle = paste0("Pre-specified HR = ", HR, ", driftHR = ", driftHR)) +
    ylab("Posterior HR") +
    geom_text(aes(y = dt2$mean_HR_trt_cc, label = round(dt2$mean_HR_trt_cc, 4)), vjust = -0.8) +
    scale_x_discrete(breaks=c("cauchy", "gamma", "full_ext"),
                     labels=c("Half-Cauchy", "Gamma", "Full borrow")) +

    scale_linetype_manual(name = "No Borrowing", values = "solid", labels = "") +
    scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9")) + thm

  if(length(ref) == 0){
    p1
  } else {
    p1 +
      geom_hline(mapping = aes(yintercept = ref, linetype=factor(ref)), colour = "purple") +
      labs(caption = paste0("Horizontal purple line refers to the posterior hazard ratio without any external arm (", round(ref,4),")"))+
      theme(plot.caption=element_text(size=10))
  }
}


#' Plot bias
#'
#' @description Plot bias for each prior distribution according to selected simulation parameters
#'
#' @param dt a \code{data.frame} containing summary statistics for the posterior samples from each simulation generated with \code{get_summary}.
#' @param HR pre-specified HR between treatment and control arm in the internal trial for which the bias should be plotted. Must be within \code{unique(dt$HR)}.
#' @param driftHR pre-specified HR between external control arm and internal control arm for which the bias should be plotted. Must be within \code{unique(dt$driftHR)}.
#' @param pred predictors to use when fitting exponential distribution in MCMC for which the bias should be plotted. Must be within \code{unique(dt$pred)}.
#'
#' @return a bar plot of class \code{ggplot} containing the bias for each prior distribution.
#'
#' @export
#'
#' @keywords method
plot_bias <- function(dt, HR = 1, driftHR = 1, pred = "none"){

  # Verify inputs are valid
  ## Make sure the output came from get_summary() and not run_mcmc()
  if (!'data.frame' %in% class(dt) |
      !all(colnames(dt) %in% c("HR", "driftHR", "prior", "pred", "nsim", "reject", "mean_HR_trt_cc",
                               "sd_HR_trt_cc", "bias", "var", "sd", "mse", "mean_HR_cc_hc",
                               "sd_HR_cc_hc"))) {
    stop('dt is not a summary data.frame from get_summary()')
  }

  ## Check that the selected HR is included
  if (HR %notin% unique(dt$HR)) {
    stop(paste0("dt does not include an HR of ", HR,".
    Acceptable arguments to HR are c(", paste0(unique(dt$HR),collapse=", "), ").
    Did you include HR = ", HR, " as an option in simu_cov()?"))
  }

  ## Check that the selected driftHR is included
  if (driftHR %notin% unique(dt$driftHR)) {
    stop(paste0("dt does not include a driftHR of ", driftHR,".
    Acceptable arguments to driftHR are c(", paste0(unique(dt$driftHR),collapse=", "), ").
    Did you include driftHR = ", driftHR, " as an option in simu_cov()?"))
  }

  ## Check that the selected pred is included
  if (pred %notin% unique(dt$pred)) {
    stop(paste0("dt does not include a pred of '", pred,"'.
    Acceptable arguments to pred are c('", paste0(unique(dt$pred), collapse = "', '"), "').
    Did you include pred = '", pred, "' as an option in set_prior()?"))
  }

  dt$prior <- factor(dt$prior, levels = c("no_ext", "cauchy", "gamma", "full_ext"))
  dt2 = dt[dt$HR == HR & dt$driftHR == driftHR & dt$pred == pred & dt$prior != "no_ext",]
  ref = dt[dt$HR == HR & dt$driftHR == driftHR & dt$pred == pred & dt$prior == "no_ext",
           "bias", drop = TRUE]

  p1 <- ggplot(mapping = aes(y = dt2$bias, x = dt2$prior, fill = dt2$prior)) +
    geom_bar(position = position_dodge2(width = 1.2, preserve = "single"), stat="identity") +
    labs(title = "Summarizing posterior distributions: bias",
         subtitle = paste0("Pre-specified HR = ", HR, ", driftHR = ", driftHR)) +
    ylab("Bias") +
    geom_text(mapping = aes(y = dt2$bias, label = round(dt2$bias, 4)), vjust = -0.8) +
    scale_x_discrete(breaks=c("cauchy", "gamma", "full_ext"),
                     labels=c("Half-Cauchy", "Gamma", "Full borrow")) +

    scale_linetype_manual(name = "No Borrowing", values = "solid", labels = "") +
    scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9")) + thm

  if(length(ref) == 0){
    p1
  } else {
    p1 +
      geom_hline(mapping = aes(yintercept = ref, linetype=factor(ref)), colour = "purple") +
      labs(caption = paste0("Horizontal purple line refers to the bias without any external arm (", round(ref,4),")"))+
      theme(plot.caption=element_text(size=10))
  }
}

#' Plot mean squared error (MSE)
#'
#' @description Plot mean squared error (MSE) for each prior distribution according to selected simulation parameters
#'
#' @param dt a \code{data.frame} containing summary statistics for the posterior samples from each simulation generated with \code{get_summary}.
#' @param HR pre-specified HR between treatment and control arm in the internal trial for which the MSE should be plotted. Must be within \code{unique(dt$HR)}.
#' @param driftHR pre-specified HR between external control arm and internal control arm for which the MSE should be plotted. Must be within \code{unique(dt$driftHR)}.
#' @param pred predictors to use when fitting exponential distribution in MCMC for which the MSE should be plotted. Must be within \code{unique(dt$pred)}.
#'
#' @return a bar plot of class \code{ggplot} containing the MSE for each prior distribution.
#'
#' @export
#' @keywords method
plot_mse <- function(dt, HR = 1, driftHR = 1, pred = "none"){

  # Verify inputs are valid
  ## Make sure the output came from get_summary() and not run_mcmc()
  if (!'data.frame' %in% class(dt) |
      !all(colnames(dt) %in% c("HR", "driftHR", "prior", "pred", "nsim", "reject", "mean_HR_trt_cc",
                               "sd_HR_trt_cc", "bias", "var", "sd", "mse", "mean_HR_cc_hc",
                               "sd_HR_cc_hc"))) {
    stop('dt is not a summary data.frame from get_summary()')
  }

  ## Check that there are >1 simulations
  if (unique(dt$nsim) == 1) {
    stop(paste0("plot_mse() requires >1 simulation to estimate variance.
         dt only contains results of a single simulation.
         Revisit simu_cov() and specify more simulations."))
  }

  ## Check that the selected HR is included
  if (HR %notin% unique(dt$HR)) {
    stop(paste0("dt does not include an HR of ", HR,".
    Acceptable arguments to HR are c(", paste0(unique(dt$HR),collapse=", "), ").
    Did you include HR = ", HR, " as an option in simu_cov()?"))
  }

  ## Check that the selected driftHR is included
  if (driftHR %notin% unique(dt$driftHR)) {
    stop(paste0("dt does not include a driftHR of ", driftHR,".
    Acceptable arguments to driftHR are c(", paste0(unique(dt$driftHR),collapse=", "), ").
    Did you include driftHR = ", driftHR, " as an option in simu_cov()?"))
  }

  ## Check that the selected pred is included
  if (pred %notin% unique(dt$pred)) {
    stop(paste0("dt does not include a pred of '", pred,"'.
    Acceptable arguments to pred are c('", paste0(unique(dt$pred), collapse = "', '"), "').
    Did you include pred = '", pred, "' as an option in set_prior()?"))
  }

  dt$prior <- factor(dt$prior, levels = c("no_ext", "cauchy", "gamma", "full_ext"))
  dt2 = dt[dt$HR == HR & dt$driftHR == driftHR & dt$pred == pred & dt$prior != "no_ext",]
  ref = dt[dt$HR == HR & dt$driftHR == driftHR & dt$pred == pred & dt$prior == "no_ext",
           "mse", drop = TRUE]
  p1 <- ggplot(mapping = aes(y = dt2$mse, x = dt2$prior, fill = dt2$prior)) +
    geom_bar(position = position_dodge2(width = 1.2, preserve = "single"), stat="identity") +
    labs(title = "Summarizing posterior distributions: MSE",
         subtitle = paste0("Pre-specified HR = ", HR, ", driftHR = ", driftHR)) +
    ylab("MSE") +
    geom_text(mapping = aes(y = dt2$mse, label = round(dt2$mse, 4)), vjust = -0.8) +
    scale_x_discrete(breaks=c("cauchy", "gamma", "full_ext"),
                     labels=c("Half-Cauchy", "Gamma", "Full borrow")) +
    scale_linetype_manual(name = "No Borrowing", values = "solid", labels = "") +
    scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9")) + thm

  if(length(ref) == 0){
    p1
  } else {
    p1 +
      geom_hline(mapping = aes(yintercept = ref, linetype=factor(ref)), colour = "purple") +
      labs(caption = paste0("Horizontal purple line refers to the MSE without any external arm (", round(ref,4),")"))+
      theme(plot.caption=element_text(size=10))
  }
}
