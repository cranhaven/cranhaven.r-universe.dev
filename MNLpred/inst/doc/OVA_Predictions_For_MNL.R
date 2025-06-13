## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
# Required packages
library(magrittr) # for pipes
library(nnet) # for the multinom()-function
library(MASS) # for the multivariate normal distribution

# The package
library(MNLpred)

# Plotting the predicted probabilities:
library(ggplot2)
library(scales)

## ----data, echo=TRUE----------------------------------------------------------
# The data:
data("gles")

## ----model--------------------------------------------------------------------
# Multinomial logit model:
mod1 <- multinom(vote ~ egoposition_immigration + 
                   political_interest + 
                   income + gender + ostwest, 
                 data = gles,
                 Hess = TRUE)

## ----results------------------------------------------------------------------
summary(mod1)

## ----math---------------------------------------------------------------------
summary(gles$egoposition_immigration)

## ----mnl_pred_ova-------------------------------------------------------------
pred1 <- mnl_pred_ova(model = mod1,
                      data = gles,
                      x = "egoposition_immigration",
                      by = 1,
                      seed = "random", # default
                      nsim = 100, # faster
                      probs = c(0.025, 0.975)) # default

## ----return-------------------------------------------------------------------
pred1$plotdata %>% head()

## ----prediction_plot1---------------------------------------------------------
ggplot(data = pred1$plotdata, aes(x = egoposition_immigration, 
                                  y = mean,
                                  ymin = lower, ymax = upper)) +
  geom_ribbon(alpha = 0.1) + # Confidence intervals
  geom_line() + # Mean
  facet_wrap(.~ vote, scales = "free_y", ncol = 2) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) + # % labels
  scale_x_continuous(breaks = c(0:10)) +
  theme_bw() +
  labs(y = "Predicted probabilities",
       x = "Ego-position toward immigration") # Always label your axes ;)

## ----static_fd----------------------------------------------------------------
fdif1 <- mnl_fd2_ova(model = mod1,
                     data = gles,
                     x = "egoposition_immigration",
                     value1 = min(gles$egoposition_immigration),
                     value2 = max(gles$egoposition_immigration),
                     nsim = 100)

## ----static_fd_plot-----------------------------------------------------------
ggplot(fdif1$plotdata_fd, aes(x = categories, 
                              y = mean,
                              ymin = lower, ymax = upper)) +
  geom_pointrange() +
  geom_hline(yintercept = 0) +
  scale_y_continuous(labels = percent_format()) +
  theme_bw() +
  labs(y = "Predicted probabilities",
       x = "Party vote")

## ----first_diffferences_prediction--------------------------------------------
fdif2 <- mnl_fd_ova(model = mod1,
                    data = gles,
                    x = "egoposition_immigration",
                    by = 1,
                    z = "gender",
                    z_values = c(0,1),
                    nsim = 100)

## ----fd_return----------------------------------------------------------------
fdif2$plotdata_fd %>% head()

## ----prediction_plot2---------------------------------------------------------
ggplot(data = fdif2$plotdata, aes(x = egoposition_immigration, 
                                  y = mean,
                                  ymin = lower, ymax = upper,
                                  group = as.factor(gender),
                                  linetype = as.factor(gender))) +
  geom_ribbon(alpha = 0.1) +
  geom_line() +
  facet_wrap(. ~ vote, scales = "free_y", ncol = 2) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) + # % labels
  scale_x_continuous(breaks = c(0:10)) +
  scale_linetype_discrete(name = "Gender",
                          breaks = c(0, 1),
                          labels = c("Male", "Female")) +
  theme_bw() +
  labs(y = "Predicted probabilities",
       x = "Ego-position toward immigration") # Always label your axes ;)

## ----first_differences_plot---------------------------------------------------
ggplot(data = fdif2$plotdata_fd, aes(x = egoposition_immigration, 
                                     y = mean,
                                  ymin = lower, ymax = upper)) +
  geom_ribbon(alpha = 0.1) +
  geom_line() +
  geom_hline(yintercept = 0) +
  facet_wrap(. ~ vote, ncol = 3) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) + # % labels
  scale_x_continuous(breaks = c(0:10)) +
  theme_bw() +
  labs(y = "Predicted probabilities",
       x = "Ego-position toward immigration") # Always label your axes ;)

