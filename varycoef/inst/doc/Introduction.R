## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE, message = FALSE, 
                      fig.width=7, fig.height=4)
library(knitr)
prop_train <- 0.2

## ----install, eval=FALSE------------------------------------------------------
#  # install from CRAN
#  install.packages("varycoef")
#  
#  # install from Github (make sure that you installed the package "devtools")
#  devtools::install_github("jakobdambon/varycoef")

## ----help and vignettes, warning=FALSE----------------------------------------
# attach package
library(varycoef)

# general package help file
help("varycoef")

# where you find this vignette
vignette("Introduction", package = "varycoef")

## ----synthetic data-----------------------------------------------------------
str(SVCdata)
help(SVCdata)
# number of observations, number of coefficients
n <- nrow(SVCdata$X); p <- ncol(SVCdata$X)

## ----synthetic data train and test--------------------------------------------
# create data frame
df <- with(SVCdata, data.frame(y = y, x = X[, 2], locs = locs))
set.seed(123)
idTrain <- sort(sample(n, n*prop_train))
df_train <- df[idTrain, ]
df_test <- df[-idTrain, ]

## ----synthetic data EDA-------------------------------------------------------
par(mfrow = 1:2)
plot(y ~ x, data = df_train, xlab = "x", ylab = "y", 
     main = "Scatter Plot of Response and Covariate")
plot(y ~ locs, data = df_train, xlab = "s", ylab = "y", 
     main = "Scatter Plot of Response and Locations")
par(mfrow = c(1, 1))

## ----synthetic data linear model----------------------------------------------
fit_lm <- lm(y ~ x, data = df_train)
coef(fit_lm)
# residual plots
par(mfrow = 1:2)
plot(x = fitted(fit_lm), y = resid(fit_lm), 
     xlab = "Fitted Values", ylab = "Residuals", 
     main = "Residuals vs Fitted")
abline(h = 0, lty = 2, col = "grey")
plot(x = df_train$locs, y = resid(fit_lm), xlab = "Location", ylab = "Residuals", 
     main = "Residuals vs Locations")
abline(h = 0, lty = 2, col = "grey")
par(mfrow = c(1, 1))

## ----synthetic data svc model-------------------------------------------------
fit_svc <- SVC_mle(y ~ x, data = df_train, locs = df_train$locs)
coef(fit_svc)

## ----methods------------------------------------------------------------------
# summary output
summary(fit_svc)
# fitted output
head(fitted(fit_svc))
# residual plots
par(mfrow = 1:2)
plot(x = fitted(fit_svc)$y.pred, y = resid(fit_svc), 
     xlab = "Fitted Values", ylab = "Residuals", 
     main = "Residuals vs Fitted")
abline(h = 0, lty = 2, col = "grey")
plot(x = df_train$locs, y = resid(fit_svc), xlab = "Location", ylab = "Residuals", 
     main = "Residuals vs Locations")
abline(h = 0, lty = 2, col = "grey")
par(mfrow = c(1, 1))

## ----synthetic data  model fit------------------------------------------------
kable(data.frame(
  Model = c("linear", "SVC"),
  # using method logLik
  `log Likelihood` = round(as.numeric(c(logLik(fit_lm), logLik(fit_svc))), 2), 
  # using method AIC
  AIC = round(c(AIC(fit_lm), AIC(fit_svc)), 2), 
  # using method BIC
  BIC = round(c(BIC(fit_lm), BIC(fit_svc)), 2)
))

## ----synthetic data fitted----------------------------------------------------
head(fitted(fit_svc))

## ----synthetic data SVC plot--------------------------------------------------
mat_coef <- cbind(
  # constant coefficients from lm
  lin1 = coef(fit_lm)[1], 
  lin2 = coef(fit_lm)[2], 
  # SVCs
  svc1 = coef(fit_svc)[1] + fitted(fit_svc)[, 1], 
  svc2 = coef(fit_svc)[2] + fitted(fit_svc)[, 2]
)
matplot(
  x = df_train$locs, 
  y = mat_coef, pch = c(1, 2, 1, 2), col = c(1, 1, 2, 2), 
  xlab = "Location", ylab = "Beta", main = "Estimated Coefficients")
legend("topright", legend = c("Intercept", "covariate x", "linear model", "SVC model"), 
       pch = c(1, 2, 19, 19), col = c("grey", "grey", "black", "red"))

## ----synthetic data predictive performance------------------------------------
# using method predict with whole data and corresponding locations
df_svc_pred <- predict(fit_svc, newdata = df, newlocs = df$locs)
# combining mean values and deviations
mat_coef_pred <- cbind(
  svc1_pred = coef(fit_svc)[1] + df_svc_pred[, 1], 
  svc2_pred = coef(fit_svc)[2] + df_svc_pred[, 2]
)
# plot
matplot(x = df$locs, y = mat_coef_pred, 
        xlab = "Location", ylab = "Beta", 
        main = "Predicted vs Actual Coefficients",
        col = c(1, 2), lty = c(1, 1), type = "l")
points(x = df$locs, y = SVCdata$beta[, 1], col = 1, pch = ".")
points(x = df$locs, y = SVCdata$beta[, 2], col = 2, pch = ".")
legend("topright", legend = c("Intercept", "Covariate", "Actual"), 
       col = c("black", "red", "black"), 
       pch = c(NA, NA, "."), 
       lty = c(1, 1, NA))

## ----sample location----------------------------------------------------------
(s_id <- sample(n, 1))

## ----synthetic data RMSE------------------------------------------------------
SE_lm <- (predict(fit_lm, newdata = df) - df$y)^2
# df_svc_pred from above
SE_svc <- (df_svc_pred$y.pred - df$y)^2
kable(data.frame(
  model = c("linear", "SVC"), 
  `in-sample RMSE` = round(sqrt(c(mean(SE_lm[idTrain]), mean(SE_svc[idTrain]))), 3),
  `out-of-sample RMSE` = round(sqrt(c(mean(SE_lm[-idTrain]), mean(SE_svc[-idTrain]))), 3)
))

## ----meuse intro--------------------------------------------------------------
library(sp)
# attach sp and load data
data("meuse")

# documentation
help("meuse")

# overview
summary(meuse)
dim(meuse)

## ----meuse data and location of interest--------------------------------------
df_meuse <- meuse[, c("dist", "lime", "elev")]
df_meuse$l_cad <- log(meuse$cadmium)
df_meuse$lime <- as.numeric(as.character(df_meuse$lime))
locs <- as.matrix(meuse[, c("x", "y")])

## ----meuse data spatial plot, echo = FALSE, message=FALSE, warning=FALSE------
# load meuse river outlines
data("meuse.riv")
# create spatial object to create spplot
sp_meuse <- df_meuse
coordinates(sp_meuse) <- ~ locs
# visualize log Cadmium measurements along river
spplot(
  sp_meuse, zcol = "l_cad", main = "Meuse River and Log Cadmium Measurements"
  ) + latticeExtra::layer(panel.lines(meuse.riv))

## ----meuse data pairs---------------------------------------------------------
pairs(df_meuse)

## ----meuse linear model-------------------------------------------------------
fit_lm <- lm(l_cad ~ ., data = df_meuse)
coef(fit_lm)

## ----LM residuals-------------------------------------------------------------
oldpar <- par(mfrow = c(1, 2))
plot(fit_lm, which = 1:2)
par(oldpar)

## ----LM spatial residuals, echo=FALSE-----------------------------------------
# add residuals to spatial object
sp_meuse$res_lm <- resid(fit_lm)
# visualize linear model residuals along river
spplot(sp_meuse, zcol = "res_lm",
     main = "Meuse River and Residuals of Linear Model"
  ) + latticeExtra::layer(panel.lines(meuse.riv))

## ----meuse SVC model, warning=FALSE-------------------------------------------
fit_svc <- SVC_mle(l_cad ~ ., data = df_meuse, locs = locs, 
                   control = SVC_mle_control(
                     profileLik = TRUE, 
                     parscale = TRUE
                   ))
coef(fit_svc)

## ----meuse fixed effects, echo = FALSE----------------------------------------
kable(t(data.frame(
  round(cbind(`linear` = coef(fit_lm), `SVC`= coef(fit_svc)), 3)
)))

## ----varycoef predict locations-----------------------------------------------
# study area
data("meuse.grid")
# prediction
df_svc_pred <- predict(fit_svc, newlocs = as.matrix(meuse.grid[, c("x", "y")]))
colnames(df_svc_pred)[1:4] <- c("Intercept", "dist", "lime", "elev")
head(df_svc_pred)

