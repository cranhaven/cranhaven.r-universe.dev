## ---- echo = FALSE, include = TRUE--------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
options(rmarkdown.html_vignette.check_title = FALSE)

## ---- echo = FALSE, include = FALSE, warning = FALSE, results = "hide"--------
df_assurvals <- read.csv("Tables/vig2_assurvals.csv", header = TRUE)
df_assurvals_long <- read.csv("Tables/vig2_assurvals_long.csv", header = TRUE)
df_assurvals_unbalanced <- read.csv("Tables/vig2_assurvals_unbalanced.csv", header = TRUE)
df_assurvals_unbal_costeff <- read.csv("Tables/vig2_assurvals_unbal_costeff.csv", header = TRUE)

## ---- echo = TRUE, include = TRUE, warning = FALSE, results = "hide"----------
library(bayesassurance)

## ---- echo = TRUE, include = TRUE, eval = FALSE-------------------------------
#  n <- seq(100, 250, 5)
#  
#  set.seed(10)
#  assur_vals <- bayes_sim(n, p = 1, u = 1, C = 0.15, Xn = NULL,
#                                          Vbeta_d = 1e-8, Vbeta_a_inv = 0,
#                                          Vn = NULL, sigsq = 0.265,
#                                          mu_beta_d = 0.25, mu_beta_a = 0,
#                                          alt = "greater", alpha = 0.05, mc_iter = 10000)

## ---- echo = TRUE, include = TRUE, warning = FALSE, results=FALSE, eval = FALSE----
#  head(assur_vals$assurance_table)

## ---- echo = FALSE, include = TRUE, warning = FALSE---------------------------
library(knitr)
tab <- df_assurvals
kable(head(tab))

## ---- echo = TRUE, include = TRUE, warning = FALSE, eval = FALSE--------------
#  assur_vals$assurance_plot

## ---- echo = FALSE, out.width = "50%"-----------------------------------------
library(knitr)
knitr::include_graphics("Images/vig2_assurtab_plot.png")

## ---- echo = TRUE, include = TRUE, results = 'hide', eval = FALSE-------------
#  n <- seq(100, 250, 5)
#  y1 <- bayesassurance::pwr_freq(n = n, sigsq = 0.265, alt = "greater", alpha = 0.05,
#                                 theta_0 = 0.15, theta_1 = 0.25)
#  y2 <- assur_vals
#  
#  library(ggplot2)
#  p1 <- ggplot2::ggplot(y1$pwr_table, alpha = 0.5,
#                        aes(x = n, y = Power, color="Power")) +
#    geom_line(lwd=1.2) + geom_point(data = y2$assurance_table, alpha = 0.5,
#    aes(y = y2$assurance_table$Assurance, color="Assurance"),lwd=1.2) +
#    ggtitle("Power Curve and Assurance Values Overlayed") + xlab("Sample Size n") +
#    ylab("Power/Assurance")
#  
#  p1

## ---- echo = FALSE, out.width = "50%"-----------------------------------------
library(knitr)
knitr::include_graphics("Images/bayes_sim_overlay.png")

## ---- echo = TRUE, include = TRUE, eval = FALSE, results = "hide"-------------
#  n = 285
#  p = 4
#  K = 20000 # threshold unit cost
#  C <- 0
#  u <- as.matrix(c(-K, 1, K, -1))
#  sigsq <- 4.04^2
#  
#  ## Assign correlation matrices to analysis and design stage priors
#  Vbeta_a_inv <- matrix(rep(0, p^2), nrow = p, ncol = p)
#  
#  Vbeta_d <- (1 / sigsq) * matrix(c(4, 0, 3, 0, 0, 10^7, 0, 0, 3, 0, 4, 0, 0, 0, 0, 10^7),
#  nrow = 4, ncol = 4)
#  
#  tau1 <- tau2 <- 8700
#  sig <- sqrt(sigsq)
#  Vn <- matrix(0, nrow = n*p, ncol = n*p)
#  Vn[1:n, 1:n] <- diag(n)
#  Vn[(2*n - (n-1)):(2*n), (2*n - (n-1)):(2*n)] <- (tau1 / sig)^2 * diag(n)
#  Vn[(3*n - (n-1)):(3*n), (3*n - (n-1)):(3*n)] <- diag(n)
#  Vn[(4*n - (n-1)):(4*n), (4*n - (n-1)):(4*n)] <- (tau2 / sig)^2 * diag(n)
#  
#  ## Assign mean parameters to analysis and design stage priors
#  mu_beta_d <- as.matrix(c(5, 6000, 6.5, 7200))
#  mu_beta_a <- as.matrix(rep(0, p))
#  
#  set.seed(10)
#  assur_vals <- bayesassurance::bayes_sim(n = 285, p = 4, u = as.matrix(c(-K, 1, K, -1)),
#                                          C = 0, Xn = NULL,
#                                          Vbeta_d = Vbeta_d, Vbeta_a_inv = Vbeta_a_inv,
#                                          Vn = Vn, sigsq = 4.04^2,
#                                          mu_beta_d = as.matrix(c(5, 6000, 6.5, 7200)),
#                                          mu_beta_a = as.matrix(rep(0, p)),
#                                          alt = "greater", alpha = 0.05, mc_iter = 10000)
#  
#  assur_vals$assur_val

## ---- echo = FALSE, include = TRUE--------------------------------------------
print("Assurance: 0.724")

## ---- echo = TRUE, include = TRUE, message = FALSE, eval = FALSE--------------
#  n <- seq(10, 100, 5)
#  ids <- c(1,2)
#  sigsq <- 100
#  Vbeta_a_inv <- matrix(rep(0, 16), nrow = 4, ncol = 4)
#  Vbeta_d <- (1 / sigsq) * matrix(c(4, 0, 3, 0, 0, 6, 0, 0, 3, 0, 4, 0, 0, 0, 0, 6),
#                                  nrow = 4, ncol = 4)
#  
#  assur_out <- bayes_sim(n = n, p = NULL, u = c(1, -1, 1, -1), C = 0, Xn = NULL,
#                         Vbeta_d = Vbeta_d, Vbeta_a_inv = Vbeta_a_inv,
#                         Vn = NULL, sigsq = 100,
#                         mu_beta_d = as.matrix(c(5, 6.5, 62, 84)),
#                         mu_beta_a = as.matrix(rep(0, 4)), mc_iter = 5000,
#                         alt = "two.sided", alpha = 0.05, longitudinal = TRUE, ids = ids,
#                         from = 10, to = 120)
#  

## ---- echo = TRUE, include = TRUE, eval = FALSE, results = "hide"-------------
#  head(assur_out$assurance_table)

## ---- echo = FALSE, include = TRUE--------------------------------------------
library(knitr)
kable(head(df_assurvals_long))

## ---- echo = TRUE, include = TRUE, eval = FALSE, results = "hide"-------------
#  assur_out$assurance_plot

## ---- echo = FALSE, out.width = "50%"-----------------------------------------
library(knitr)
knitr::include_graphics("Images/bayes_sim_long.png")

## ---- echo = TRUE, include = TRUE, message = FALSE, eval = FALSE--------------
#  n1 <- seq(20, 75, 5)
#  n2 <- seq(50, 160, 10)
#  
#  set.seed(100)
#  assur_out <- bayes_sim_unbalanced(n1 = n1, n2 = n2, repeats = 1, u = c(1, -1),
#               C = 0, Xn = NULL, Vbeta_d = matrix(c(50, 0, 0, 10),nrow = 2, ncol = 2),
#               Vbeta_a_inv = matrix(rep(0, 4), nrow = 2, ncol = 2),
#               Vn = NULL, sigsq = 100,  mu_beta_d = c(1.17, 1.25),
#               mu_beta_a = c(0, 0), alt = "two.sided", alpha = 0.05, mc_iter = 5000,
#               surface_plot = TRUE)

## ---- echo = TRUE, include = TRUE, message = FALSE, results = "hide", eval = FALSE----
#  ## Outputs
#  head(assur_out$assurance_table)

## ---- echo = FALSE, include = TRUE--------------------------------------------
## Outputs
library(knitr)
kable(head(df_assurvals_unbalanced))

## ---- echo = TRUE, include = TRUE, message = FALSE, eval = FALSE, results = "hide"----
#  assur_out$contourplot

## ---- echo = FALSE, out.width = "50%"-----------------------------------------
library(knitr)
knitr::include_graphics("Images/vig2_assur_contourplot.png")

## ---- echo = TRUE, include = TRUE, message = FALSE, eval = FALSE--------------
#  n1 <- c(4, 5, 15, 25, 30, 100, 200)
#  n2 <- c(8, 10, 20, 40, 50, 200, 250)
#  
#  mu_beta_d <- as.matrix(c(5, 6000, 6.5, 7200))
#  mu_beta_a <- as.matrix(rep(0, 4))
#  K = 20000 # threshold unit cost
#  C <- 0
#  u <- as.matrix(c(-K, 1, K, -1))
#  sigsq <- 4.04^2
#  Vbeta_a_inv <- matrix(rep(0, 16), nrow = 4, ncol = 4)
#  Vbeta_d <- (1 / sigsq) * matrix(c(4, 0, 3, 0, 0, 10^7, 0, 0, 3, 0, 4, 0, 0, 0, 0, 10^7),
#                                  nrow = 4, ncol = 4)
#  
#  set.seed(12)
#  assur_out <- bayes_sim_unbalanced(n1 = n1, n2 = n2, repeats = 2, u = as.matrix(c(-K, 1, K, -1)),
#                                    C = 0, Xn = NULL, Vbeta_d = Vbeta_d,
#                                    Vbeta_a_inv = Vbeta_a_inv,
#                                    Vn = NULL, sigsq = 4.04^2,
#                                    mu_beta_d = as.matrix(c(5, 6000, 6.5, 7200)),
#                                    mu_beta_a = as.matrix(rep(0, 4)), alt = "greater",
#                                    alpha = 0.05, mc_iter = 5000, surface_plot = TRUE)
#  
#  

## ---- echo = TRUE, include = TRUE, eval = FALSE, results = "hide"-------------
#  head(assur_out$assurance_table)

## ---- echo = FALSE, include = TRUE--------------------------------------------
## Outputs
library(knitr)
kable(head(df_assurvals_unbal_costeff))

## ---- echo = TRUE, include = TRUE, eval = FALSE, results = "hide"-------------
#  assur_out$contourplot

## ---- echo = FALSE, out.width = "50%"-----------------------------------------
library(knitr)
knitr::include_graphics("Images/assur_contourplot_costeff.png")

## ---- echo = TRUE, include = TRUE, eval = FALSE-------------------------------
#  n <- seq(100, 250, 5)
#  assur_vals <- bayesassurance::bayes_sim(n, p = 1, u = 1, C = 0.15, Xn = NULL,
#                                         Vbeta_d = 1e-8, Vbeta_a_inv = 0,
#                                          Vn = NULL, sigsq = 0.265,
#                                          mu_beta_d = 0.25, mu_beta_a = 0,
#                                        alpha = 0.05, mc_iter = 10000)
#  
#  
#  assur_vals <- bayesassurance::bayes_sim_unknownvar(n, p = 1, u = 1,
#                                           C = 0.15, R = 150, Xn = NULL, Vn = NULL,
#                                           Vbeta_d = 1e-8, Vbeta_a_inv = 0,
#                                           mu_beta_d = 0.25, mu_beta_a = 0,
#                                           a_sig_a = 0.1, b_sig_a = 0.1,
#                                           a_sig_d = 0.1, b_sig_d = 0.1,
#                                           alpha = 0.05, mc_iter = 5000)
#  

## ---- echo = TRUE, include = TRUE---------------------------------------------
n <- c(1,2,3,4)
bayesassurance::gen_Xn(n)

## ---- echo = TRUE, include = TRUE---------------------------------------------
n <- 3
p <- 4
bayesassurance::gen_Xn(rep(n, p))

## ---- echo = TRUE, include = TRUE---------------------------------------------
ids <- c(1,2,3,4)
gen_Xn_longitudinal(ids, from = 1, to = 10, num_repeated_measures = 4)

