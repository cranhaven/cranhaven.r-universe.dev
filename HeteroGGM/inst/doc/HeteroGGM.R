## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----eval=FALSE---------------------------------------------------------------
#  data(example.data)
#  K <- 6
#  lambda <- genelambda.obo(nlambda1=5,lambda1_max=0.5,lambda1_min=0.1,
#                           nlambda2=15,lambda2_max=1.5,lambda2_min=0.1,
#                           nlambda3=10,lambda3_max=3.5,lambda3_min=0.5)

## ----eval=FALSE---------------------------------------------------------------
#  res <- GGMPF(lambda, example.data$data, K, penalty = "MCP")
#  Theta_hat.list <- res$Theta_hat.list
#  Mu_hat.list <- res$Mu_hat.list
#  opt_num <- res$Opt_num
#  opt_Mu_hat <- Mu_hat.list[[opt_num]]
#  opt_Theta_hat <- Theta_hat.list[[opt_num]]
#  K_hat <- dim(opt_Theta_hat)[3]
#  K_hat  # Output the estimated K0.

## ----eval=FALSE---------------------------------------------------------------
#  summ <- summary_network(opt_Mu_hat, opt_Theta_hat, example.data$data)
#  summ$Theta_summary$overlap
#  va_names <- c("6")
#  linked_node_names(summ, va_names, num_subgroup=1)
#  plot_network(summ, num_subgroup = c(1:K_hat), plot.mfrow=c(1,K_hat))

## ----eval=FALSE---------------------------------------------------------------
#  data(example.data)
#  K <- 3
#  lambda <- genelambda.obo(nlambda1=5,lambda1_max=0.5,lambda1_min=0.1,
#                           nlambda2=15,lambda2_max=1.5,lambda2_min=0.1)

## ----eval=FALSE---------------------------------------------------------------
#  res <- PGGMBC(lambda, example.data$data, K, initial.selection="K-means")
#  Theta_hat.list <- res$Theta_hat.list
#  opt_num <- res$Opt_num
#  opt_Theta_hat <- Theta_hat.list[[opt_num]]

