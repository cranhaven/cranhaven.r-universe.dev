## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----sim_data_example---------------------------------------------------------
library(iClusterVB)

# sim_data comes with the iClusterVB package.
dat1 <- list(
  gauss_1 = sim_data$continuous1_data[c(1:20, 61:80, 121:140, 181:200), 1:75],
  gauss_2 = sim_data$continuous2_data[c(1:20, 61:80, 121:140, 181:200), 1:75],
  poisson_1 = sim_data$count_data[c(1:20, 61:80, 121:140, 181:200), 1:75],
  multinomial_1 = sim_data$binary_data[c(1:20, 61:80, 121:140, 181:200), 1:75]
)

# We re-code `0`s to `2`s

dat1$multinomial_1[dat1$multinomial_1 == 0] <- 2

dist <- c(
  "gaussian", "gaussian",
  "poisson", "multinomial"
)

## ----model--------------------------------------------------------------------
fit_iClusterVB <- iClusterVB(
  mydata = dat1,
  dist = dist,
  K = 4,
  initial_method = "VarSelLCM",
  VS_method = 1,
  max_iter = 50
)

## ----summary------------------------------------------------------------------
# We can obtain a summary using summary()
summary(fit_iClusterVB)

## ----plots, fig.width=6, fig.height=6-----------------------------------------
plot(fit_iClusterVB)

## ----piplot, fig.width=6, fig.height=6----------------------------------------
# The `piplot` function can be used to visualize the probability of inclusion

piplot(fit_iClusterVB)

## ----chmap, echo = TRUE, fig.width=6, fig.height=6----------------------------
# The `chmap` function can be used to display heat maps for each data view

chmap(fit_iClusterVB, rho = 0,
      cols = c("green", "blue",
               "purple", "red"),
      scale = "none")

