## ---- echo = FALSE, include = TRUE--------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
options(rmarkdown.html_vignette.check_title = FALSE)

## ---- echo = FALSE, include = FALSE, warning = FALSE, results = "hide"--------
df_out1_power <- read.csv("Tables/vig1_out1_powertab", header = TRUE)
df_out1_assur <- read.csv("Tables/vig1_out1_assurtab", header = TRUE)
df_out2 <- read.csv("Tables/vig1_out2.csv", header = TRUE)

## ---- echo = TRUE, include = TRUE, warning = FALSE, results = "hide"----------
library(bayesassurance)

## ---- echo = TRUE, include = TRUE, warning = FALSE, results = "hide"----------
n <- seq(10, 140, 5)
pwr_vals <- bayesassurance::pwr_freq(n = n, theta_0 = 0.15, theta_1 = 0.35, sigsq = 0.3, 
                                     alt = "greater", alpha = 0.05)

## ---- echo = TRUE, include = TRUE, warning = FALSE, results=FALSE-------------
head(pwr_vals$pwr_table)

## ---- echo = FALSE, include = TRUE, warning = FALSE---------------------------
library(knitr)
tab <- head(pwr_vals$pwr_table)
kable(tab)

## ---- echo = TRUE, include = TRUE, warning = FALSE----------------------------
pwr_vals$pwr_plot

## ---- echo = TRUE, include = TRUE, warning = FALSE----------------------------
n <- 20
pwr_freq(n = n, theta_0 = 0.15, theta_1 = 0.35, sigsq = 0.3, 
                                     alt = "greater", alpha = 0.05)

## ---- echo = TRUE, include = TRUE, warning = FALSE, results = "hide"----------
n <- seq(10, 500, 5)
n_a <- 1e-8
n_d <- 1e+8
theta_0 <- 0.15
theta_1 <- 0.25
sigsq <- 0.104
assur_vals <- assurance_nd_na(n = n, n_a = n_a, n_d = n_d, theta_0 = theta_0, 
                              theta_1 = theta_1, sigsq = sigsq, alt = "greater", 
                              alpha = 0.05)

## ---- echo = TRUE, include = TRUE, warning = FALSE, results = "hide"----------
head(assur_vals$assurance_table)

## ---- echo = FALSE, include = TRUE, warning = FALSE---------------------------
library(knitr)
tab <- head(assur_vals$assurance_table)
kable(tab)

## ---- echo = TRUE, include = TRUE, warning = FALSE, results = "hide"----------
assur_vals$assur_plot

## ---- echo = TRUE, include = TRUE, eval = FALSE-------------------------------
#  n <- seq(100, 300, 10)
#  n_a <- 1e-8
#  n_d <- 1e+8
#  theta_0 <- 0.15
#  theta_1 <- 0.25
#  sigsq <- 0.104
#  alpha <- 0.05
#  
#  out1 <- bayesassurance::pwr_curve(n, n_a, n_d, theta_0, theta_1, sigsq,
#                                    alt = "greater", alpha, bayes_sim = FALSE)
#  

## ---- echo = TRUE, include = TRUE, warning = FALSE, results = "hide", eval = FALSE----
#  head(out1$power_table)

## ---- echo = FALSE, include = TRUE--------------------------------------------
library(knitr)
kable(head(df_out1_power))

## ---- echo = TRUE, include = TRUE, warning = FALSE, results = "hide", eval = FALSE----
#  head(out1$assurance_table)

## ---- echo = FALSE, include = TRUE--------------------------------------------
library(knitr)
kable(head(df_out1_assur))

## ---- echo = TRUE, include = FALSE, results='hide', eval = FALSE--------------
#  out1$plot

## ---- echo = FALSE, out.width = "50%"-----------------------------------------
library(knitr)
knitr::include_graphics("Images/pwr_curves1.png")

## ---- echo = TRUE, include = TRUE, eval = FALSE-------------------------------
#  n <- seq(100, 300, 10)
#  n_a <- 1e-8
#  n_d <- 1e+8
#  theta_0 <- 0.15
#  theta_1 <- 0.25
#  sigsq <- 0.104
#  alpha <- 0.05
#  
#  set.seed(10)
#  out2 <- bayesassurance::pwr_curve(n, n_a, n_d, theta_0, theta_1, sigsq, alt = "greater",
#                                    alpha, bayes_sim = TRUE)

## ---- echo = TRUE, include = TRUE, eval = FALSE-------------------------------
#  head(out2$bayes_sim_table)

## ---- echo = FALSE, include = TRUE, warning = FALSE---------------------------
library(knitr)
kable(head(df_out2))

## ---- echo = TRUE, include = TRUE, results="hide", eval=FALSE-----------------
#  out2$plot

## ---- echo = FALSE, out.width = "50%"-----------------------------------------
library(knitr)
knitr::include_graphics("Images/pwr_curves2.png")

