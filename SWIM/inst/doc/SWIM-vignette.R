## ---- setup, echo=FALSE, cache=FALSE------------------------------------------
options(digits = 2) # auto round to 2 decimals when printed

## ----wrap-hook, echo = FALSE--------------------------------------------------
library(knitr)
hook_error = knit_hooks$get('error')
knit_hooks$set(error = function(x, options) {
  # this hook is used only when the linewidth option is not NULL
  if (!is.null(n <- options$linewidth)) {
    x = knitr:::split_lines(x)
    # any lines wider than n should be wrapped
    if (any(nchar(x) > n)) x = strwrap(x, width = n)
    x = paste(x, collapse = '\n')
  }
  hook_error(x, options)
})

## ---- example1_sim_data, include = TRUE---------------------------------------
set.seed(0)
# number of simulated scenarios
n.sim <- 10 ^ 5
# correlation between Z1 and Z2
r <- 0.5
# simulation of Z1  and Z2
# constructed as a combination of independent standard normals U1, U2
U1 <- rnorm(n.sim)
U2 <- rnorm(n.sim)
Z1 <- 100 + 40 * U1
Z2 <- 100 + 20 * (r * U1 + sqrt(1 - r ^ 2) * U2)
# simulation of Z3
Z3 <- rnorm(n.sim, 100, 20)
# portfolio loss Y
Y <- Z1 + Z2 + Z3
# data of baseline model
dat <- data.frame(Z1, Z2, Z3, Y)

## ---- example1_first_stress, echo = -3, warning = FALSE, message = FALSE------
library(SWIM)
str.mean <- stress(type = "mean", x = dat, k = 1, new_means = 110)
options(digits = 2)
summary(str.mean, base = TRUE)

## ---- example1-cdfs-mean, warning = FALSE, message = FALSE, fig.show='hold', out.width = '50%', fig.cap = "Baseline and stressed empirical distribution functions of model components  $Z_1$ (left) and $Y$ (right), subject to a stress on the mean of $Z_1$."----
# refer to variable of interest by name...
plot_cdf(str.mean, xCol = "Z1", base = TRUE)
# ... or column number
plot_cdf(str.mean, xCol = 4, base = TRUE)

## ---- example1-weights-mean, warning = FALSE, message = FALSE, fig.show='hold', out.width = '50%',fig.cap = "Scenario weights against observations of model components  $Z_1$ (left) and $Y$ (right), subject to a stress on the mean of $Z_1$."----
# parameter n specifies the number of scenario weights plotted
plot_weights(str.mean, xCol = "Z1", n = 1000)
# specifying the limits of the x-axis
plot_weights(str.mean, xCol = "Y", x_limits = c(90, 550), n = 1000)

## ---- example1_second_stress, cache = TRUE, echo = -2, warning = FALSE, message = FALSE----
str.sd <- stress(type = "mean sd", x = dat, k = 1, new_means = 100, new_sd = 50)
options(digits = 2)
summary(str.sd, base = FALSE)

## ---- example1-cdfs-sd, cache = FALSE, warning = FALSE, message = FALSE, fig.show='hold', out.width = '50%', fig.cap = "Baseline and stressed empirical distribution functions of model components  $Z_1$ (left) and $Y$ (right), subject to a stress on the standard deviation of $Z_1$."----
plot_cdf(str.sd, xCol = "Z1", base = TRUE)
plot_cdf(str.sd, xCol = 4, base = TRUE)

## ---- example1-weights-sd, cache = TRUE, warning = FALSE, message = FALSE, fig.show='hold', out.width = '50%',fig.cap = "Scenario weights against observations of model components  $Z_1$ (left) and $Y$ (right), subject to a stress on the standard deviation of $Z_1$."----
plot_weights(str.sd, xCol = "Z1", n = 2000)
plot_weights(str.sd, xCol = "Y", n = 2000)

## ---- example1_third_stress, cache = FALSE, error=TRUE, linewidth = 80--------
stress(type = "mean", x = dat, k = 1, new_means = 300)
max(Z1)

## ----echo=FALSE,message=FALSE,warning=FALSE-----------------------------------
require(knitr)
opts_chunk$set(tidy.opts=list(width.cutoff=70),tidy=TRUE)

## ---- loading-packages, cache = FALSE, include = FALSE------------------------
  library(SWIM)
  library(ggplot2)
  library(ggpubr)

## ---- CM-data-head, echo = -2, cache = TRUE-----------------------------------
data("credit_data")
options(digits = 3)
head(credit_data)

## ---- CM-stress-VaR, cache = FALSE, echo = TRUE, linewidth = 55---------------
stress.credit <- stress(type = "VaR", x = credit_data, k = "L", alpha = 0.9, q_ratio = 1.2)

## ---- CM-stress-VaR-check-ES, cache = FALSE, linewidth = 70-------------------
VaR_stressed(object = stress.credit, alpha =  c(0.75, 0.9, 0.95, 0.99), xCol = "L", wCol = 1, base = TRUE)
ES_stressed(object = stress.credit, alpha = 0.9, xCol = "L", wCol = 1, base = TRUE)

## ---- CM-stress-VaR-ES, cache = FALSE, linewidth = 60-------------------------
stress.credit <- stress(type = "VaR ES", x = stress.credit, k = "L", alpha = 0.9, q_ratio = 1.2, s = 3500)

## ---- CM-summary, echo = -1, cache = FALSE------------------------------------
options(digits = 3)
summary(stress.credit, base = TRUE)

## ---- CM-specs, echo = -1, cache = FALSE--------------------------------------
options(digits = 3)
get_specs(stress.credit)

## ---- credit-weights, warning = FALSE, cache = FALSE, message = FALSE, fig.show='hold', out.width = '50%',fig.cap = "Scenario weights against the portfolio loss $L$ for stressing VaR (left) and stressing both VaR and ES (right).", tidy = FALSE----
plot_weights(stress.credit, xCol = "L", wCol = 1, n = 2000)
# parameter `wCol` specifies the stresses, whose scenario weights are plotted.
plot_weights(stress.credit, xCol = "L", wCol = 2, n = 7000)

## ---- CM-histL, cache = FALSE, fig.cap = "Histogram of the portfolio loss $L$ under the baseline and the two stressed models.", out.width = '80%', fig.align = 'center'----
plot_hist(object = stress.credit, xCol = "L", base = TRUE)

## ---- CM-plot1, cache = FALSE, fig.cap = "Distribution functions and histograms of the subportfolios $L_1, L_2, L_3$ for the stresses on the VaR (stress 1) and on both the VaR and ES (stress 2) of the portfolio loss $L$.", out.width = '100%', fig.align = 'center', warning = FALSE----
pL1.cdf <- plot_cdf(object = stress.credit, xCol = 2, wCol = "all", base = TRUE)
pL2.cdf <- plot_cdf(object = stress.credit, xCol = 3, wCol = "all", base = TRUE)
pL3.cdf <- plot_cdf(object = stress.credit, xCol = 4, wCol = "all", base = TRUE)

pL1.hist <- plot_hist(object = stress.credit, xCol = 2, wCol = "all", base = TRUE)
pL2.hist <- plot_hist(object = stress.credit, xCol = 3, wCol = "all", base = TRUE)
pL3.hist <- plot_hist(object = stress.credit, xCol = 4, wCol = "all", base = TRUE)

ggarrange(pL1.cdf, pL1.hist, pL2.cdf, pL2.hist, pL3.cdf, pL3.hist, ncol = 2, nrow = 3,
          common.legend = TRUE)

## ---- CM-sensitivity0, cache = TRUE-------------------------------------------
sensitivity(object = stress.credit, xCol = 1, wCol = "all", type = "Kolmogorov")

## ---- CM-sensitivity1, cache = TRUE-------------------------------------------
sensitivity(object = stress.credit, xCol = c(2 : 7), wCol = "all", type = "Gamma")

## ---- CM-sensitivity2, cache = TRUE-------------------------------------------
sensitivity(object = stress.credit, type = "Gamma", f = sum, k = c(2, 4), wCol = 1, xCol = NULL)

## ---- CM-rank, cache = TRUE---------------------------------------------------
importance_rank(object = stress.credit, xCol = c(2 : 7), wCol = 1, type = "Gamma")

## ---- CM-stress-fixed-VaR, cache = TRUE, tidy = FALSE-------------------------
# 90% VaR of L under the baseline model
VaR.L <- quantile(x = credit_data[, "L"], prob = 0.9, type = 1) 
# 75th quantile of H2 under the baseline model
q.H2 <- quantile(x = credit_data[, "H2"], prob = 0.75, type = 1) 
# columns to be stressed (L, H2, H2)
k.stressH2 = list(1, 6, 6) 
# functions to be applied to columns
f.stressH2 <- list(
                 # indicator function for L, for stress on VaR
                 function(x)1 * (x <= VaR.L * 1.2), 
                 # mean of H2
                 function(x)x, 
                 # indicator function for 75th quaantile of H2
                 function(x)1 * (x <= q.H2)) 
# new values for the 90% VaR of L, mean of H2, 75th quantile of H2
m.stressH2 = c(0.9, mean(credit_data[, "H2"]), 0.75) 
stress.credit <- stress_moment(x = stress.credit, f = f.stressH2, k = k.stressH2, 
                               m = m.stressH2)

## ---- summary-sens-H2, echo = -1, cache = TRUE--------------------------------
options(digits = 3)
summary(stress.credit, wCol = 3, xCol = 6, base = TRUE)
sensitivity(object = stress.credit, xCol = c(2:4), type = "Gamma")

## ---- CM-joint-stress-VaR, echo = -1, cache = TRUE, tidy = FALSE--------------
options(digits = 3)
# VaR of L2 and L3, respectively
VaR.L2 <- quantile(x = credit_data[, "L2"], prob = 0.9, type = 1) 
VaR.L3 <- quantile(x = credit_data[, "L3"], prob = 0.9, type = 1) 
#stressing VaR of L2 and L3
f.stress <- list(function(x)1 * (x <= VaR.L2 * 1.2), 
                 function(x)1 * (x <= VaR.L3 * 1.2)) 
stress.credit.L2L3 <- stress_moment(x = credit_data, f = f.stress, k = list(3, 4), 
                                    m = c(0.9, 0.9))
#impact on portfolio tail
VaR_stressed(stress.credit.L2L3, alpha = c(0.75, 0.9, 0.95, 0.99), xCol = "L", 
             base = TRUE)

## ---- CM-joint-stress-VaR-2, echo = TRUE, cache = TRUE------------------------
# probability of joint exceendance under the baseline model
mean(1 * (credit_data[, "L2"] > VaR.L2 * 1.2) * (credit_data[, "L3"] > VaR.L3 * 1.2))
# probability of joint exceendance under the stressed model
mean(get_weights(stress.credit.L2L3) * (credit_data[, "L2"] > VaR.L2 * 1.2) * (credit_data[, "L3"] > VaR.L3 * 1.2))

# additionally stress joint exceedance probability of L2 and L3
f.stress.joint <- c(f.stress, function(x)1 * (x[1] > VaR.L2 * 1.2) * (x[2] > VaR.L3 * 1.2)) 
stress.credit.L2L3 <- stress_moment(x = stress.credit.L2L3, f = f.stress.joint, k = list(3,4,c(3, 4)), m = c(0.9, 0.9, 0.06))

## ---- CM-joint-stress-VaR-2-effect, echo = TRUE, cache = FALSE, fig.cap = "Quantiles of the aggregate loss $L$ under the baseline (blue), the stress on the tails of $L_2$ and $L_3$ (red), and the additional stress on the joint tail of $L_2$ and $L_3$ (green).", out.width = '80%', fig.align = 'center'----
plot_quantile(stress.credit.L2L3, xCol = "L", wCol = "all", base = TRUE, x_limits = c(0.75, 1))

## ---- eval=FALSE, tidy = FALSE------------------------------------------------
#    set.seed(1)
#    library(copula)
#    nsim <- 100000
#  
#  # counterparties subportfolio 1, 2 and 3
#    m1 <- 2500
#    m2 <- 5000
#    m3 <- 2500
#  
#    # prob of default for subportfolios 1, 2 and 3
#    p1 <- 0.0004
#    p2 <- 0.0097
#    p3 <- 0.0503
#  
#    # correlation between default probabilities
#    rho1 <- 0.0004
#    rho2 <- 0.0044
#    rho3 <- 0.01328
#  
#  # exposures
#    e1 <- 80
#    e2 <- 25
#    e3 <- 10
#  
#  # loss given default
#    LGD1 <- 0.25
#    LGD2 <- 0.375
#    LGD3 <- 0.5
#  
#  # beta parameters: matching subportfolios default probabilities and correlation
#    alpha1 <- p1 * (1 / rho1 - 1)
#    beta1 <- alpha1 * (1 / p1 - 1)
#  
#    alpha2 <- p2 * (1 / rho2 - 1)
#    beta2 <- alpha2 * (1 / p2 - 1)
#  
#    alpha3 <- p3 * (1 / rho3 - 1)
#    beta3 <- alpha3 * (1 / p3 - 1)
#  
#  # correlations between subportfolios
#    cor12 <- 0.3
#    cor13 <- 0.1
#    cor23 <- 0.4
#  
#  # Gaussian copula structure
#    myCop <- normalCopula(param = c(cor12, cor13, cor23), dim = 3, dispstr = "un")
#  
#  # multivariate beta with given copula
#    myMvd <- copula::mvdc(copula = myCop,
#                  margins = c("beta", "beta", "beta"),
#                  paramMargins = list(list(alpha1, beta1),
#                                      list(alpha2, beta2),
#                                      list(alpha3, beta3)))
#  
#  # simulation from the chosen copula
#    H <- copula::rMvdc(nsim, myMvd)
#  
#  # simulate number of default per subportfolios (binomial distributions)
#    M1 <- rbinom(n = nsim, size = m1, prob = H[, 1])
#    M2 <- rbinom(n = nsim, size = m2, prob = H[, 2])
#    M3 <- rbinom(n = nsim, size = m3, prob = H[, 3])
#  
#  # total loss per subportfolio
#    L1 <- M1 * e1 * LGD1
#    L2 <- M2 * e2 * LGD2
#    L3 <- M3 * e3 * LGD3
#  
#  # aggregate portfolio loss
#    L <- L1 + L2 + L3
#  
#  # the credit data included in SWIM
#    credit_data <- cbind(L, L1, L2, L3, H)
#    colnames(credit_data) <- c("L", "L1", "L2", "L3", "H1", "H2", "H3")

