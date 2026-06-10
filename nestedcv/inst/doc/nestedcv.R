## ----setup, include = FALSE---------------------------------------------------
knitr::knit_hooks$set(pngquant = knitr::hook_pngquant)
knitr::opts_chunk$set(
  collapse = TRUE,
  warning = FALSE
)
library(nestedcv)
library(pROC)

## ----eval = FALSE-------------------------------------------------------------
# install.packages("nestedcv")
# library(nestedcv)

## -----------------------------------------------------------------------------
## Example binary classification problem with P >> n
x <- matrix(rnorm(150 * 2e+04), 150, 2e+04)  # predictors
y <- factor(rbinom(150, 1, 0.5))  # binary response

## Partition data into 2/3 training set, 1/3 test set
trainSet <- caret::createDataPartition(y, p = 0.66, list = FALSE)

## t-test filter using whole test set
filt <- ttest_filter(y, x, nfilter = 100)
filx <- x[, filt]

## Train glmnet on training set only using filtered predictor matrix
library(glmnet)
fit <- cv.glmnet(filx[trainSet, ], y[trainSet], family = "binomial")

## Predict response on test set
predy <- predict(fit, newx = filx[-trainSet, ], s = "lambda.min", type = "class")
predy <- as.vector(predy)
predyp <- predict(fit, newx = filx[-trainSet, ], s = "lambda.min", type = "response")
predyp <- as.vector(predyp)
output <- data.frame(testy = y[-trainSet], predy = predy, predyp = predyp)

## Results on test set
## shows bias since univariate filtering was applied to whole dataset
predSummary(output)

## Nested CV
fit2 <- nestcv.glmnet(y, x, family = "binomial", alphaSet = 7:10 / 10,
                      filterFUN = ttest_filter,
                      filter_options = list(nfilter = 100))
fit2

testroc <- pROC::roc(output$testy, output$predyp, direction = "<", quiet = TRUE)
inroc <- innercv_roc(fit2)
plot(fit2$roc)
lines(inroc, col = 'blue')
lines(testroc, col = 'red')
legend('bottomright', legend = c("Nested CV", "Left-out inner CV folds", 
                                 "Test partition, non-nested filtering"), 
       col = c("black", "blue", "red"), lty = 1, lwd = 2, bty = "n")

## ----fig.align="center", echo=FALSE-------------------------------------------
knitr::include_graphics("fig1.png")

## ----fig.align="center", echo=FALSE-------------------------------------------
knitr::include_graphics("fig2.png")

## ----eval = FALSE-------------------------------------------------------------
# # Raw RNA-Seq data for this example is located at:
# # https://www.ebi.ac.uk/arrayexpress/experiments/E-MTAB-11611/
# 
# # set up data
# load("/../R4RA_270821.RData")
# 
# index <- r4ra.meta$Outliers_Detected_On_PCA != "outlier" & r4ra.meta$Visit == 3 &
#           !is.na(r4ra.meta$Visit)
# metadata <- r4ra.meta[index, ]
# dim(metadata)  # 133 individuals
# 
# medians <- Rfast::rowMedians(as.matrix(r4ra.vst))
# data <- t(as.matrix(r4ra.vst))
# # remove low expressed genes
# data <- data[index, medians > 6]
# dim(data)  # 16254 genes
# 
# # Rituximab cohort only
# yrtx <- metadata$CDAI.response.status.V7[metadata$Randomised.medication == "Rituximab"]
# yrtx <- factor(yrtx)
# data.rtx <- data[metadata$Randomised.medication == "Rituximab", ]
# 
# # no filter
# res.rtx <- nestcv.glmnet(y = yrtx, x = data.rtx,
#                          family = "binomial", cv.cores = 8,
#                          alphaSet = seq(0.7, 1, 0.05))
# res.rtx

## ----eval = FALSE-------------------------------------------------------------
# # t-test filter
# res.rtx <- nestcv.glmnet(y = yrtx, x = data.rtx, filterFUN = ttest_filter,
#                          filter_options = list(nfilter = 300, p_cutoff = NULL),
#                          family = "binomial", cv.cores = 8,
#                          alphaSet = seq(0.7, 1, 0.05))
# summary(res.rtx)

## ----eval = FALSE-------------------------------------------------------------
# plot_alphas(res.rtx)
# plot_lambdas(res.rtx)

## ----out.width='100%', fig.align="center", echo=FALSE-------------------------
knitr::include_graphics("plot_alpha_lam.png")

## ----eval = FALSE-------------------------------------------------------------
# # Fold 1 line plot
# plot(res.rtx$outer_result[[1]]$cvafit)
# 
# # Scatter plot
# plot(res.rtx$outer_result[[1]]$cvafit, type = 'p')
# 
# # Number of non-zero coefficients
# plot(res.rtx$outer_result[[1]]$cvafit, xaxis = 'nvar')

## ----out.width='100%', fig.align="center", echo=FALSE-------------------------
knitr::include_graphics("plot_cva.png")

## ----eval = FALSE-------------------------------------------------------------
# # Outer CV ROC
# plot(res.rtx$roc, main = "Outer fold ROC", font.main = 1, col = 'blue')
# legend("bottomright", legend = paste0("AUC = ", signif(pROC::auc(res.rtx$roc), 3)), bty = 'n')
# 
# # Inner CV ROC
# rtx.inroc <- innercv_roc(res.rtx)
# plot(rtx.inroc, main = "Inner fold ROC", font.main = 1, col = 'red')
# legend("bottomright", legend = paste0("AUC = ", signif(pROC::auc(rtx.inroc), 3)), bty = 'n')

## ----out.width='100%', fig.align="center", echo=FALSE-------------------------
knitr::include_graphics("roc.png")

## ----eval = FALSE-------------------------------------------------------------
# # Outer LOOCV
# res.rtx <- nestcv.glmnet(y = yrtx, x = data.rtx, min_1se = 0, filterFUN = ttest_filter,
#                          filter_options = list(nfilter = 300, p_cutoff = NULL),
#                          outer_method = "LOOCV",
#                          family = "binomial", cv.cores = 8,
#                          alphaSet = seq(0.7, 1, 0.05))
# summary(res.rtx)

## ----eval = FALSE-------------------------------------------------------------
# # Random forest filter
# res.rtx <- nestcv.glmnet(y = yrtx, x = data.rtx, min_1se = 0.5, filterFUN = rf_filter,
#                          filter_options = list(nfilter = 300),
#                          family = "binomial", cv.cores = 8,
#                          alphaSet = seq(0.7, 1, 0.05))
# summary(res.rtx)
# 
# # ReliefF algorithm filter
# res.rtx <- nestcv.glmnet(y = yrtx, x = data.rtx, min_1se = 0, filterFUN = relieff_filter,
#                          filter_options = list(nfilter = 300),
#                          family = "binomial", cv.cores = 8,
#                          alphaSet = seq(0.7, 1, 0.05))
# summary(res.rtx)

## -----------------------------------------------------------------------------
library(mlbench)
data(BostonHousing2)
dat <- BostonHousing2
y <- dat$cmedv  ## continuous outcome
x <- subset(dat, select = -c(cmedv, medv, town))

stat_filter(y, x, type = "full")

## ----eval = FALSE-------------------------------------------------------------
# filter <- function(y, x, ...) {}

## ----eval=FALSE---------------------------------------------------------------
# # this example requires the missRanger package
# library(missRanger)
# 
# x_na <- generateNA(x)  # insert NA into x
# x_na <- as.matrix(x_na)
# 
# # missRanger requires a dataframe, whereas glmnet requires a matrix
# impute_x <- function(x, ...) {
#   missRanger(as.data.frame(x), num.trees = 50, ...)
# }
# 
# res <- nestcv.glmnet(y, x_na, family = "gaussian",
#                      alphaSet = 1,
#                      n_outer_folds = 3, cv.cores = 2,
#                      modifyX = impute_x,
#                      na.option = "pass")

## ----eval=FALSE---------------------------------------------------------------
# # receives training data from `x` and `y` only
# # returns object with class 'modxy'
# modxy <- function(y, x) {
#   sc <- scale(x)
#   cen <- attr(sc, "scaled:center")
#   sca <- attr(sc, "scaled:scale")
#   out <- cbind(cen, sca)
#   class(out) <- "modxy"
#   out
# }
# 
# # define predict function for class 'modxy'
# # applied independently to train and test folds of `x`
# predict.modxy <- function(object, newdata, ...) {
#   scale(newdata, center = object[,1], scale = object[,2])
# }
# 
# res <- nestcv.glmnet(y, x, family = "gaussian", alphaSet = 1,
#                      n_outer_folds = 3, cv.cores = 3,
#                      modifyX = modxy, modifyX_useY = TRUE)

## -----------------------------------------------------------------------------
## Imbalanced dataset
set.seed(1, "L'Ecuyer-CMRG")
x <- matrix(rnorm(150 * 2e+04), 150, 2e+04)  # predictors
y <- factor(rbinom(150, 1, 0.2))  # imbalanced binary response
table(y)

## first 30 parameters are weak predictors
x[, 1:30] <- rnorm(150 * 30, 0, 1) + as.numeric(y)*0.7

## -----------------------------------------------------------------------------
out <- randomsample(y, x)
y2 <- out$y
x2 <- out$x
table(y2)

## Nested CV glmnet with unnested balancing by random oversampling on
## whole dataset
fit1 <- nestcv.glmnet(y2, x2, family = "binomial", alphaSet = 1,
                      n_outer_folds = 4,
                      cv.cores=2,
                      filterFUN = ttest_filter)
fit1$summary

## -----------------------------------------------------------------------------
out <- randomsample(y, x, minor=1, major=0.4)
y2 <- out$y
x2 <- out$x
table(y2)

## Nested CV glmnet with unnested balancing by random undersampling on
## whole dataset
fit1b <- nestcv.glmnet(y2, x2, family = "binomial", alphaSet = 1,
                       n_outer_folds = 4,
                       cv.cores=2,
                       filterFUN = ttest_filter)
fit1b$summary

## Balance x & y outside of CV loop by SMOTE
out <- smote(y, x)
y2 <- out$y
x2 <- out$x
table(y2)

## Nested CV glmnet with unnested balancing by SMOTE on whole dataset
fit2 <- nestcv.glmnet(y2, x2, family = "binomial", alphaSet = 1,
                      n_outer_folds = 4,
                      cv.cores=2,
                      filterFUN = ttest_filter)
fit2$summary

## -----------------------------------------------------------------------------
## Nested CV glmnet with nested balancing by random oversampling
fit3 <- nestcv.glmnet(y, x, family = "binomial", alphaSet = 1,
                      n_outer_folds = 4,
                      cv.cores=2,
                      balance = "randomsample",
                      filterFUN = ttest_filter)
fit3$summary

## -----------------------------------------------------------------------------
## Nested CV glmnet with weights
w <- weight(y)
table(w)

fit4 <- nestcv.glmnet(y, x, family = "binomial", alphaSet = 1,
                      n_outer_folds = 4,
                      cv.cores=2,
                      weights = w,
                      filterFUN = ttest_filter)
fit4$summary

## ----fig.dim = c(5, 5)--------------------------------------------------------
plot(fit1$roc, col='green')
lines(fit1b$roc, col='red')
lines(fit2$roc, col='blue')
lines(fit3$roc)
lines(fit4$roc, col='purple')
legend('bottomright', legend = c("Unnested random oversampling", 
                                 "Unnested SMOTE",
                                 "Unnested random undersampling",
                                 "Nested random oversampling",
                                 "Nested glmnet with weights"), 
       col = c("green", "blue", "red", "black", "purple"), lty = 1, lwd = 2, bty = "n", cex=0.8)

## ----eval = FALSE-------------------------------------------------------------
# balance <- function(y, x, ...) {
# 
#   return(list(y = y, x = x))
# }

## ----eval = FALSE-------------------------------------------------------------
# # nested CV using caret
# tg <- expand.grid(lambda = exp(seq(log(2e-3), log(1e0), length.out = 100)),
#                   alpha = seq(0.8, 1, 0.1))
# ncv <- nestcv.train(y = yrtx, x = data.rtx,
#                method = "glmnet",
#                savePredictions = "final",
#                filterFUN = ttest_filter, filter_options = list(nfilter = 300),
#                tuneGrid = tg, cv.cores = 8)
# ncv$summary
# 
# # Plot ROC on outer folds
# plot(ncv$roc)
# 
# # Plot ROC on inner LO folds
# inroc <- innercv_roc(ncv)
# plot(inroc)
# pROC::auc(inroc)
# 
# # Extract coefficients of final fitted model
# glmnet_coefs(ncv$final_fit$finalModel, s = ncv$finalTune$lambda)

## ----eval = FALSE-------------------------------------------------------------
# # Example tuning plot for outer fold 1
# plot(ncv$outer_result[[1]]$fit, xTrans = log)
# 
# # ggplot2 version
# ggplot(ncv$outer_result[[1]]$fit) +
#   scale_x_log10()

## ----eval = FALSE-------------------------------------------------------------
# data(iris)
# y <- iris$Species
# x <- iris[, -5]
# 
# out_folds <- caret::createFolds(y, k = 8)
# in_folds <- lapply(out_folds, function(i) {
#   train_y <- y[-i]
#   caret::createFolds(train_y, k = 8)
# })
# 
# res <- nestcv.train(y, x, method = "rf",
#                     cv.cores = 8,
#                     inner_folds = in_folds,
#                     outer_folds = out_folds)
# summary(res)
# res$outer_folds  # show which outer fold indices were used

## -----------------------------------------------------------------------------
library(mlbench)
data(Sonar)
y <- Sonar$Class
x <- Sonar[, -61]

fit1 <- nestcv.glmnet(y, x, family = "binomial", alphaSet = 1,
                      n_outer_folds = 4, cv.cores = 2)

metrics(fit1, extra = TRUE)

## ----fig.dim = c(10, 5)-------------------------------------------------------
fit1$prc <- prc(fit1)

# precision-recall AUC values
fit1$prc$auc

# plot ROC and PR curves
op <- par(mfrow = c(1, 2), mar = c(4, 4, 2, 2) +.1)
plot(fit1$roc, col = "red", main = "ROC", las = 1)

plot(fit1$prc, col = "red", main = "Precision-recall")
par(op)

## ----eval = FALSE-------------------------------------------------------------
# # for nestcv.glmnet object
# preds <- predict(res.rtx, newdata = data.rtx, type = 'response')
# 
# # for nestcv.train object
# preds <- predict(ncv, newdata = data.rtx)

## ----results='hide'-----------------------------------------------------------
data(Sonar)
y <- Sonar$Class
x <- Sonar[, -61]

# single fit
fit <- nestcv.glmnet(y, x, family = "binomial", alphaSet = 1,
                     n_outer_folds = 4, cv.cores = 2)

# repeated nested CV
set.seed(123, "L'Ecuyer-CMRG")
repcv <- nestcv.glmnet(y, x, family = "binomial", alphaSet = 1,
                       n_outer_folds = 4) |>
         repeatcv(8, rep.cores = 2)

## -----------------------------------------------------------------------------
repcv
summary(repcv)

## ----eval=FALSE---------------------------------------------------------------
# folds <- repeatfolds(y, repeats = 3, n_outer_folds = 4)
# 
# repcv <- nestcv.glmnet(y, x, family = "binomial", alphaSet = 1,
#                        n_outer_folds = 4) |>
#          repeatcv(3, repeat_folds = folds, rep.cores = 2)
# repcv

## ----fig.dim = c(10, 5)-------------------------------------------------------
op <- par(mfrow = c(1, 2), mar = c(4, 4, 2, 2) +.1)
plot(fit1$roc, col = "red", las = 1, main = "ROC")  # single nested cv
lines(repcv$roc)  # repeated nested cv

repcv$prc <- prc(repcv)  # calculate precision-recall curve

plot(fit1$prc, col = "red", main = "Precision-recall")  # single nested cv
lines(repcv$prc)  # repeated nested cv
legend("topright", legend = c("single nested cv", "repeat nested cv"),
       col = c("red", "black"), lwd = 2, bty = "n")
par(op)

## ----eval=FALSE---------------------------------------------------------------
# # example parallelisation across outer CV
# res <- nestcv.train(y, x, method = "gbm", cv.cores = 8)
# 
# # same using future
# library(future)
# # on linux/mac
# plan(multicore, workers = 8)
# 
# # on windows or with Rstudio use
# plan(multisession, workers = 8)
# 
# res <- nestcv.train(y, x, method = "gbm", parallel_mode = "future")
# 
# # parallelisation of repeat CV
# res <- nestcv.train(y, x, method = "gbm") |>
#   	repeatcv(16, rep.cores = 8)
# 
# # same using future
# res <- nestcv.train(y, x, method = "gbm") |>
#   	repeatcv(16, rep_parallel = "future")

## ----eval=FALSE---------------------------------------------------------------
# set.seed(123, "L'Ecuyer-CMRG")

## ----eval = FALSE-------------------------------------------------------------
# parallel::detectCores(logical = FALSE)

