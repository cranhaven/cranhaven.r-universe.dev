## ---- echo = FALSE, include = TRUE, results = "hide"--------------------------
adcock_table <- read.csv("Tables/vig3_assurvals_adcock.csv", header = TRUE, stringsAsFactors = FALSE)
betabin_table <- read.csv("Tables/vig3_assurvals_betabin.csv", header = TRUE, stringsAsFactors = FALSE)

## ---- echo = TRUE, include = TRUE, warning = FALSE, results = "hide"----------
library(bayesassurance)

## ---- echo = TRUE, include = TRUE, eval = FALSE, results = "hide"-------------
#  library(ggplot2)
#  n <- seq(20, 145, 5)
#  
#  set.seed(20)
#  out <- bayesassurance::bayes_adcock(n = n, d = 0.20, mu_beta_a = 0.64, mu_beta_d = 0.9,
#                                      n_a = 20, n_d = 10, sig_sq = 0.265, alpha = 0.05,
#                                      mc_iter = 10000)

## ---- echo = TRUE, include = TRUE, eval = FALSE, results = "hide"-------------
#  head(out$assurance_table)

## ---- echo = FALSE, include = TRUE--------------------------------------------
library(knitr)
kable(head(adcock_table))

## ---- echo = TRUE, include = TRUE, eval = FALSE, results = "hide"-------------
#  out$assurance_plot

## ---- echo = FALSE, out.width = "50%"-----------------------------------------
library(knitr)
knitr::include_graphics("Images/vig3_adcock_assurancecurve.png")

## ---- echo = TRUE, include = TRUE---------------------------------------------
freq_cond <- function(n, d, sig_sq){
  sigma <- sqrt(sig_sq)
  lhs <- 2 * pnorm(d / (sigma / sqrt(n))) - 1 
  return(lhs)
}

## ---- echo = FALSE, include = TRUE--------------------------------------------
adcock_lhs <- function(n, d, mu_beta_a, mu_beta_d, n_a, n_d, sig_sq){

  count <- 0
  maxiter <- 1000
  lhs <- c()
  
  # Design Stage
  for(i in 1:maxiter){
    var_d <- sig_sq * ((n_d + n) / (n * n_d))
    #var_d <- sig_sq * ((1 / n_d) + (1 / n))
    xbar <- rnorm(n=1, mean = mu_beta_d, sd = sqrt(var_d))

    lambda <- ((n_a * mu_beta_a) + (n * xbar)) / (n_a + n)
    # var_a <- sig_sq * (1 / (n_a + n))
    # theta <- rnorm(n=1, mean = lambda, sd = sqrt(var_a))

    phi_1 <- (sqrt(n_a + n) / sqrt(sig_sq)) * (xbar + d - lambda)
    phi_2 <- (sqrt(n_a + n) / sqrt(sig_sq)) * (xbar - d - lambda)

    lhs <- c(lhs, pnorm(phi_1) - pnorm(phi_2))

  }

  lhs_avg <- mean(lhs)
  return(lhs_avg)

}

## ---- echo = TRUE, include = TRUE, eval = FALSE, results = "hide"-------------
#  library(ggplot2)
#  n <- seq(5, 300, 5)
#  n_a <- 1e-8
#  n_d <- n_a
#  d <- 0.15
#  set.seed(1)
#  sig_sq <- runif(1, 0, 1)
#  mu_beta_d <- runif(1, 0, 1)
#  mu_beta_a <- runif(1, 0, 1)
#  
#  lhs1 <- freq_cond(n, d, sig_sq)
#  lhs2 <- sapply(n, function(i) adcock_lhs(n = i, d, mu_beta_a, mu_beta_d, n_a,
#                                           n_d, sig_sq))
#  
#  df1 <- as.data.frame(cbind(n, lhs1))
#  df2 <- as.data.frame(cbind(n, lhs2))
#  
#  p <- ggplot(df1) + geom_line(data = df1, alpha = 0.5, aes(x = n, y = lhs1,
#       color="Adcock Cond"), lwd = 1.2)
#  
#  p1 <- p + geom_point(data = df2, alpha = 0.5, aes(x = n, y = lhs2,
#        color="Bayesian Sim"),lwd=1.2) + ylab("Probability of Meeting Analysis Objective") +
#        xlab("Sample Size n") + ggtitle("Comparison Between Adcock Conditiion and Bayesian
#                                        Simulation Results")
#  
#  
#  p1  <- p1 + geom_label(
#        label="d = 0.15",
#        x=25,
#        y=0.98,
#        color = "black", size = 3
#        )
#  

## ---- echo = TRUE, include = TRUE, warning = FALSE, results = "hide"----------
library(bayesassurance)

## ---- echo = TRUE, include = TRUE, eval = FALSE-------------------------------
#  n <- seq(600, 1000, 10)
#  
#  set.seed(30)
#  out <- bayesassurance::bayes_sim_betabin(n1 = n, n2 = n, p1 = 0.25, p2 = 0.2,
#                                           alpha_1 = 0.5, beta_1 = 0.5, alpha_2 = 0.5,
#                                           beta_2 = 0.5, sig_level = 0.05, alt = "two.sided")

## ---- echo = TRUE, include = TRUE, eval = FALSE, results = "hide"-------------
#  head(out$assurance_table)

## ---- echo = FALSE, include = TRUE--------------------------------------------
library(knitr)
kable(head(betabin_table))

## ---- echo = TRUE, include = TRUE, eval = FALSE-------------------------------
#  propdiffCI_classic <- function(n, p1, p2, sig_level){
#    p <- p1 - p2
#    power <- pnorm(sqrt(n / ((p1*(1-p1)+p2*(1-p2)) / (p)^2)) - qnorm(1-sig_level/2))
#    return(power)
#  }

## ---- echo = TRUE, include = TRUE, eval = FALSE-------------------------------
#  #########################################################
#  # alpha1 = 0.5, beta1 = 0.5, alpha2 = 0.5, beta2 = 0.5 ##
#  #########################################################
#  n <- seq(40, 1000, 10)
#  
#  power_vals <- sapply(n, function(i) propdiffCI_classic(n=i, 0.25, 0.2, 0.05))
#  df1 <- as.data.frame(cbind(n, power_vals))
#  
#  assurance_out <- bayes_sim_betabin(n1 = n, n2 = n, p1 = 0.25,
#                p2 = 0.2, alpha_1 = 0.5, beta_1 = 0.5, alpha_2 = 0.5, beta_2 = 0.5,
#                sig_level = 0.05, alt = "two.sided")
#  df2 <- assurance_out$assurance_table
#  colnames(df2) <- c("n1", "n2", "Assurance")
#  
#  p1 <- ggplot(df1, alpha = 0.5, aes(x = n, y = power_vals, color="Frequentist Power"))
#  p1 <- p1 + geom_line(alpha = 0.5, aes(x = n, y = power_vals,
#             color="Frequentist Power"), lwd = 1.2)
#  
#  p2 <- p1 + geom_point(data = df2, alpha = 0.5, aes(x = .data$n1, y = .data$Assurance,
#        color = "Bayesian Assurance"), lwd = 1.2) + ylab("Power/Assurance") +
#        xlab(~ paste("Sample Size n = ", "n"[1], " = ", "n"[2])) +
#        ggtitle("Power/Assurance Curves of Difference in Proportions")
#  
#  p2 <- p2 + geom_label(aes(525, 0.6,
#                        #label="~alpha[1] == ~beta[1] == 0.5~and~alpha[2] == ~beta[2] == 0.5"),
#                        label = "~p[1]-p[2] == 0.05"), parse = TRUE,
#                        color = "black", size = 3)
#  

## ---- echo = TRUE, include = TRUE, eval = FALSE-------------------------------
#  
#  propdiffCI_classic <- function(n, p1, p2, alpha_1, beta_1, alpha_2, beta_2, sig_level){
#    set.seed(1)
#    if(is.null(p1) == TRUE & is.null(p2) == TRUE){
#      p1 <- rbeta(n=1, alpha_1, beta_1)
#      p2 <- rbeta(n=1, alpha_2, beta_2)
#    }else if(is.null(p1) == TRUE & is.null(p2) == FALSE){
#      p1 <- rbeta(n=1, alpha_1, beta_1)
#    }else if(is.null(p1) == FALSE & is.null(p2) == TRUE){
#      p2 <- rbeta(n=1, alpha_2, beta_2)
#    }
#    p <- p1 - p2
#  
#    power <- pnorm(sqrt(n / ((p1*(1-p1)+p2*(1-p2)) / (p)^2)) - qnorm(1-sig_level/2))
#    return(power)
#  }

## ---- echo = TRUE, include = TRUE, warning = FALSE, eval = FALSE--------------
#  ################################################
#  # alpha1 = 2, beta1 = 2, alpha2 = 6, beta2 = 6 #
#  ################################################
#  library(ggplot2)
#  
#  power_vals <- propdiffCI_classic(n=n, p1 = NULL, p2 = NULL, 2, 2, 6, 6, 0.05)
#  df1 <- as.data.frame(cbind(n, power_vals))
#  
#  assurance_vals <- bayes_sim_betabin(n1 = n, n2 = n,
#                p1 = NULL, p2 = NULL, alpha_1 = 2, beta_1 = 2, alpha_2 = 6,
#                beta_2 = 6, sig_level = 0.05, alt = "two.sided")
#  df2 <- assurance_vals$assurance_table
#  
#  
#  
#  p1 <- ggplot(df1, aes(x = n, y = power_vals))
#  p1 <- p1 + geom_line(alpha = 0.5, aes(x = n, y = power_vals,
#                                        color="Frequentist Power"), lwd = 1.2)
#  
#  p2 <- p1 + geom_point(data = df2, alpha = 0.5, aes(x = n1, y = Assurance,
#        color = "Bayesian Assurance"), lwd = 1.2) +
#    ylab("Power/Assurance") + xlab(~ paste("Sample Size n = ", "n"[1], " = ", "n"[2])) +
#      ggtitle("Power/Assurance Curves for Difference in Proportions",
#        subtitle = expression(paste(p[1], "~ Beta(", alpha[1], ", ", beta[1], "); ",
#                                        p[2], "~ Beta(", alpha[2], ", ", beta[2], ")")))
#  
#  p2 <- p2 + geom_label(aes(75, 1.03, label="~alpha[1] == ~beta[1] == 0.5~and~alpha[2] == ~beta[2] == 0.5"), parse=TRUE,
#    color = "black", size = 2.5) + ylim(0.45, 1.03) + xlim(40, 350)

