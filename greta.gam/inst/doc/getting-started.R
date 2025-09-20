## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  eval = greta:::check_tf_version("message"),
  fig.height = 5,
  fig.width = 7
)

## ----mgcv-generate-and-fit----------------------------------------------------
library(mgcv)
set.seed(2024 - 12 - 12)

# simulate some data...
dat <- gamSim(1, n = 400, dist = "normal", scale = 0.3)
head(dat)
# fit a model using gam()
mgcv_fit <- gam(y ~ s(x2), data = dat)
mgcv_fit
summary(mgcv_fit)
## show partial residuals
plot(mgcv_fit, scheme = 1, shift = coef(mgcv_fit)[1])

## ----greta-fit----------------------------------------------------------------
library(greta.gam)
set.seed(2024 - 02 - 09)
# setup the linear predictor for the smooth
linear_predictor <- smooths(~ s(x2), data = dat)
linear_predictor

## ----greta-fit-add-distribution-----------------------------------------------
dist_sd <- cauchy(0, 1, truncation = c(0, Inf))
distribution(dat$y) <- normal(mean = linear_predictor, sd = dist_sd)

## ----greta-fit-make-preds-----------------------------------------------------
pred_dat <- data.frame(
  x2 = seq(0, 1, length.out = 100)
  )

head(pred_dat)

## ----greta-fit-eval-preds-----------------------------------------------------
linear_preds <- evaluate_smooths(linear_predictor, newdata = pred_dat)
linear_preds

## ----greta-fit-mcmc-----------------------------------------------------------
# build model
m <- model(linear_preds)
m
# draw from the posterior
draws <- mcmc(m, n_samples = 200, verbose = FALSE)
class(draws)
# 4 chains
length(draws)

# 200 draws, 100 predictors
dim(draws[[1]])

# look at the top corner
draws[[1]][1:5, 1:5]

## ----greta-fit-plot-greta-v-mgcv----------------------------------------------
plot(mgcv_fit, scheme = 1, shift = coef(mgcv_fit)[1])

# add in a line for each posterior sample
apply(draws[[1]], 1, lines, x = pred_dat$x2, 
      col = adjustcolor("firebrick", alpha.f = 0.1))

# plot the data
points(dat$x2, dat$y, pch = 19, cex = 0.2)

