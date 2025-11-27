## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse  = TRUE,
  comment   = "#>",
  message   = FALSE,
  warning   = FALSE,
  fig.width = 6
)

## ----load_libraries, echo = FALSE---------------------------------------------
library(registr)
library(dplyr)
have_ggplot2 = requireNamespace("ggplot2", quietly = TRUE)
have_cowplot = requireNamespace("cowplot", quietly = TRUE)
if (have_ggplot2 & have_cowplot)  {
  library(ggplot2)
  theme_set(theme_bw())
  library(cowplot)
}

## ----sim_data2----------------------------------------------------------------
registration_data = simulate_unregistered_curves(I = 50, D = 200, seed = 2018)

head(registration_data)


## ----plot_sim2, echo = FALSE, fig.show='hold'---------------------------------
if (have_ggplot2 & have_cowplot) {
  gg1 <- registration_data %>%
    ggplot(aes(index, plogis(latent_mean), group = id)) + theme_bw() + 
    geom_line(alpha = 0.25) + labs(y = "Pr(Y = 1)")
  
  gg2 <- registration_data %>%
    ggplot(aes(t, plogis(latent_mean), group = id)) + theme_bw() + 
    geom_line(alpha = 0.25) + labs(y = "Pr(Y = 1)")
  
  cowplot::plot_grid(gg1, gg2, nrow = 1)
}

## ----sim_data1----------------------------------------------------------------
fpca_data = simulate_functional_data(I = 100, D = 200, seed = 2018)

ls(fpca_data)

head(fpca_data$Y)

## ----plot1_sim1, fig.show='hold', echo = FALSE--------------------------------

Y = fpca_data$Y
pc_df = data.frame(pop_mean = fpca_data$alpha, 
									 psi1 = fpca_data$psi1,
									 psi2 = fpca_data$psi2,
									 index = seq(0, 1, length.out = 200),
									 id = 1)

if (have_ggplot2 & have_cowplot) {
  gg1 <- ggplot(Y, aes(index, latent_mean, group = id)) + theme_bw() +
    geom_line(alpha = 0.25) + geom_line(data = pc_df, aes(y = pop_mean), color = "red") 
  
  gg2 <- ggplot(pc_df, aes(index, psi1)) + theme_bw() + geom_line(color = "blue") 
  gg3 <- ggplot(pc_df, aes(index, psi2)) + theme_bw() + geom_line(color = "blue") 
  
  cowplot::plot_grid(gg1, gg2, gg3, nrow = 1)
}

## ----plot2_sim1, echo = FALSE-------------------------------------------------
if (have_ggplot2) {
  Y %>%
    filter(id == 7) %>%
    ggplot(aes(index, value)) + theme_bw() +
    geom_point(alpha = 0.75, size = 0.25) + geom_line(aes(y = plogis(latent_mean))) +
    labs(y = "Pr(Y = 1)")
}

## ----register_binary, message = FALSE-----------------------------------------
registr_bin = register_fpca(Y = registration_data, family = "binomial", Kt = 8, Kh = 4, npc = 1, verbose = 2)

## ----plot_reg_bin, echo = FALSE, fig.show='hold', fig.width=6-----------------
Y = registr_bin$Y
if (have_ggplot2 & have_cowplot) {
  gg1 <- ggplot(Y, aes(tstar, plogis(latent_mean), group = id)) + theme_bw() + 
    geom_line(alpha = 0.25) + labs(y = "Pr(Y = 1)")
  
  gg2 <- ggplot(Y, aes(t, plogis(latent_mean), group = id)) + theme_bw() + 
    geom_line(alpha = 0.25) + labs(y = "Pr(Y = 1)")
  
  gg3 <- ggplot(Y, aes(t_hat, plogis(latent_mean), group = id)) + theme_bw() + 
    geom_line(alpha = 0.25) + labs(y = "Pr(Y = 1)")
  
  cowplot::plot_grid(gg1, gg2, gg3, nrow = 1)
}


## ----plot_reg_bin_warp, echo = FALSE, fig.show='hold'-------------------------
if (have_ggplot2 & have_cowplot) {
  gg1 <- ggplot(Y, aes(tstar, t, group = id)) + theme_bw() + 
    geom_line(alpha = 0.25)
  
  gg2 <- ggplot(Y, aes(tstar, t_hat, group = id)) + theme_bw() + 
    geom_line(alpha = 0.25)
  
  cowplot::plot_grid(gg1, gg2, nrow = 1)
}

## ----register_gaussian, message = FALSE---------------------------------------
registration_data$value = registration_data$latent_mean
registr_gauss = register_fpca(Y = registration_data, family = "gaussian", npc = 1, Kt = 10)

## ----bfpca--------------------------------------------------------------------
bfpca_object = bfpca(fpca_data$Y, npc = 2, Kt = 8, print.iter = TRUE)

## ----plot_bfpca, echo = FALSE, fig.show='hold'--------------------------------
epc_df = data.frame(index     = bfpca_object$t_vec,
                    psi1_est  = bfpca_object$efunctions[,1],
                    psi2_est  = bfpca_object$efunctions[,2],
                    alpha_est = bfpca_object$alpha %>% as.vector())
if (have_ggplot2 & have_cowplot) {
  gg1 <- ggplot() + geom_line(data = pc_df, aes(index, pop_mean), color = "blue") +
    geom_line(data = epc_df, aes(index, alpha_est), linetype = 2, color = "red") +
    theme_bw()
  
  gg2 <- ggplot() + geom_line(data = pc_df, aes(index, psi1), color = "blue") +
    geom_line(data = epc_df, aes(index, psi2_est), linetype = 2, color = "red") +
    theme_bw()
  
  gg3 <- ggplot() + geom_line(data = pc_df, aes(index, psi2), color = "blue") +
    geom_line(data = epc_df, aes(index, psi1_est), linetype = 2, color = "red") +
    theme_bw()
  
  cowplot::plot_grid(gg1, gg2, gg3, nrow = 1)
}

## ----plot.fpca, fig.show='hold', fig.width=6----------------------------------
if (have_ggplot2 && requireNamespace("cowplot", quietly = TRUE)) {
  registr:::plot.fpca(bfpca_object)
}

## ----registr_function---------------------------------------------------------
data_test_gradient = simulate_unregistered_curves(I = 50, D = 100, seed = 2018)

start_time   = Sys.time()
reg_analytic = registr(Y = data_test_gradient, family = "binomial", gradient = TRUE)
end_time     = Sys.time()

analytic_gradient = as.numeric(round((end_time - start_time), 2))

start_time  = Sys.time()
reg_numeric = registr(Y = data_test_gradient, family = "binomial", gradient = FALSE)
end_time    = Sys.time()

numeric_gradient = as.numeric(round((end_time - start_time), 2))

## ----registr_function gradient large data, include=FALSE----------------------
data_test_gradient = simulate_unregistered_curves(I = 1000, D = 500, seed = 2018)

start_time = Sys.time()
reg_analytic = registr(Y = data_test_gradient, family = "binomial", gradient = TRUE)
end_time = Sys.time()

analytic_gradient_large = as.numeric(round((end_time - start_time), 1))

start_time = Sys.time()
reg_numeric = registr(Y = data_test_gradient, family = "binomial", gradient = FALSE)
end_time = Sys.time()

numeric_gradient_large = as.numeric(round((end_time - start_time), 1))

## ----register_parametric------------------------------------------------------
registration_data = simulate_unregistered_curves(I = 10, D = 50, seed = 2018)

registr_parametric = register_fpca(Y = registration_data, family = "binomial", 
                                   Kt = 8, Kh = 4, npc = 1, gradient = FALSE,
                                   warping = "piecewise_linear2")

## ----register_parametric_plots, echo = FALSE, fig.show='hold', eval = have_ggplot2----
if (have_ggplot2 & have_cowplot) {
  gg1 <- ggplot(registr_gauss$Y, aes(x = tstar, y = t_hat, group = id)) +
    geom_line() + 
    labs(title = "warping = nonparametric")
  
  gg2 <- ggplot(registr_parametric$Y, aes(x = tstar, y = t_hat, group = id)) +
    geom_line() + 
    labs(title = "warping = piecewise_linear2")
  
  cowplot::plot_grid(gg1, gg2, nrow = 1)
}

## ----register_par_priors------------------------------------------------------
registr_par_priors = register_fpca(Y = registration_data, family = "binomial", 
                                   Kt = 8, Kh = 4, npc = 1, gradient = FALSE,
                                   warping = "piecewise_linear2",
                                   priors = TRUE, prior_sd = 0.1)

## ---- register_par_priors_plots, echo = FALSE, fig.show='hold'----------------
registr_par_priors2 = register_fpca(Y = registration_data, family = "binomial", 
                                    Kt = 8, Kh = 4, npc = 1, gradient = FALSE,
																		warping = "piecewise_linear2",
                                    priors = TRUE, prior_sd = 0.01)

if (have_ggplot2 & have_cowplot) {
  gg1 <- ggplot(registr_par_priors$Y, aes(x = tstar, y = t_hat, group = id)) +
    geom_line() + 
    labs(title = "sd for all priors = 0.1")
  
  gg2 <- ggplot(registr_par_priors2$Y, aes(x = tstar, y = t_hat, group = id)) +
    geom_line() + 
    labs(title = "sd for all priors = 0.01")
  
  cowplot::plot_grid(gg1, gg2, nrow = 1)
}

## ----fpca_periodic------------------------------------------------------------
registr_periodic = register_fpca(Y = registration_data, family = "binomial", 
                                 Kt = 8, Kh = 4, npc = 1, gradient = FALSE,
                                 periodic = TRUE)

## ----register_fpca_periodic, echo = FALSE, fig.show='hold'--------------------

registr_non_periodic = register_fpca(Y = registration_data, family = "binomial", 
                                 Kt = 8, Kh = 4, npc = 1, gradient = FALSE,
                                 periodic = FALSE)
if (have_ggplot2 & have_cowplot) {
  gg1 <- tibble(mu = registr_non_periodic$fpca_obj$mu) %>%
    mutate(time = row_number()) %>%
    ggplot(aes(x = time, y = mu)) + 
    theme_bw() + 
    geom_line() + 
    labs(y = "mu (non-periodic)")
  
  gg2 <- tibble(psi1 = registr_non_periodic$fpca_obj$efunctions[,1]) %>%
    mutate(time = row_number()) %>%
    ggplot(aes(x = time, y = psi1)) + 
    theme_bw() + 
    geom_line() + 
    labs(y = "psi1 (non-periodic)")
  
  gg3 <- tibble(mu = registr_periodic$fpca_obj$mu) %>%
    mutate(time = row_number()) %>%
    ggplot(aes(x = time, y = mu)) + 
    theme_bw() + 
    geom_line() + 
    geom_hline(yintercept = registr_periodic$fpca_obj$mu[1], lty = "dotted") + 
    labs(y = "mu (periodic)")
  
  gg4 <- tibble(psi1 = registr_periodic$fpca_obj$efunctions[,1]) %>%
    mutate(time = row_number()) %>%
    ggplot(aes(x = time, y = psi1)) + 
    theme_bw() + 
    geom_line() + 
    geom_hline(yintercept = registr_periodic$fpca_obj$efunctions[1,1], lty = "dotted") + 
    labs(y = "psi1 (periodic)")
  
  cowplot::plot_grid(gg1, gg2, gg3, gg4, nrow = 2)
}

## ----template data sim, echo=F, fig.width=5-----------------------------------
t = seq(0, 1, length.out = 100)
temp_dat = data.frame(index = rep(t, times = 3),
                      value = c(dnorm(t, mean = 0.5, sd = 0.15),
                                dnorm(t, mean = 0.65, sd = 0.185),
                                dnorm(t, mean = 0.7, sd = 0.18)),
                      id    = factor(rep(1:3, each = length(t))))
if (have_ggplot2) {
  ggplot(temp_dat, aes(x = index, y = value)) + 
    geom_line(aes(col = id)) +
    geom_smooth(se = FALSE, col = "black") +
    ggtitle("Simulated data with mean curve in black")
}

## ----temp registration without template, fig.width=5--------------------------
reg1 = registr(Y = temp_dat, family = "gaussian", Kh = 4,
               incompleteness = "trailing", lambda_inc = 0)
if (have_ggplot2) {
  ggplot(reg1$Y, aes(x = index, y = value, col = id)) + 
    geom_line() +
    ggtitle("Registration with overall mean (black) as template function")
}

## ----temp registration with template, fig.width=5-----------------------------
Y_template = temp_dat %>% filter(id == 1)
reg2 = registr(Y = temp_dat, family = "gaussian", Kh = 4, Y_template = Y_template,
               incompleteness = "trailing", lambda_inc = 0)
if (have_ggplot2) {
  ggplot(reg2$Y, aes(x = index, y = value, col = id)) +
    geom_line() +
    ggtitle("Registration with red curve as template")
}

