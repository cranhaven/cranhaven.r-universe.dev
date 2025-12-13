if(require("TH.data") & require("survival")) {
  ## base model
  bmod <- survreg(Surv(time, cens) ~ horTh, data = GBSG2, model = TRUE)
  survreg_plot(bmod)
  
  ## partitioned model
  tr <- pmtree(bmod)
  plot(tr, terminal_panel = node_pmterminal(tr, plotfun = survreg_plot, 
                                            confint = TRUE))
  summary(tr)
  summary(tr, node = 1:2)
  
  logLik(bmod)
  logLik(tr)
  
  
  ## Sometimes the number of participant in each treatment group needs to 
  ## be of a certain size. This can be accomplished using converged
  
  ## Each treatment group should have more than 33 observations
  ctrl <- ctree_control(lookahead = TRUE)
  ctrl$converged <- function(mod, data, subset) {
      all(table(data$horTh[subset]) > 33)
  }
  
  tr2 <- pmtree(bmod, control = ctrl)
  plot(tr2, terminal_panel = node_pmterminal(tr, plotfun = survreg_plot,
      confint = TRUE))
  
  summary(tr2[[5]]$data$horTh)
}
