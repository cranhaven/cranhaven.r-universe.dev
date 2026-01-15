## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## -----------------------------------------------------------------------------
library(rbooster)
cv_sampler <- function(y, train_proportion) {
  unlist(lapply(unique(y), function(m) sample(which(y==m), round(sum(y==m))*train_proportion)))
}

library(imbalance)
data <- glass0
p <- ncol(data) - 1

x <- data[,1:p]
y <- data[, p + 1]

train_i <- cv_sampler(y, 0.9)
x_train <- x[train_i,]
y_train <- y[train_i]

x_test <- x[-train_i,]
y_test <- y[-train_i]

## ---- fig.width=7, fig.height=6, fig.align='center'---------------------------
m_discrete <- booster(x_train = x_train, 
        y_train = y_train, 
        classifier = "rpart", 
        method = "discrete",
        x_test = x_test,
        y_test = y_test, 
        weighted_bootstrap = FALSE,
        max_iter = 20, 
        lambda = 1, 
        print_detail = TRUE, 
        print_plot = TRUE, 
        bag_frac = 0.8, 
        p_weak = 4)

m_real <- booster(x_train = x_train, 
                      y_train = y_train, 
                      classifier = "rpart", 
                      method = "real",
                      x_test = x_test,
                      y_test = y_test, 
                      weighted_bootstrap = FALSE,
                      max_iter = 20, 
                      lambda = 1, 
                      print_detail = TRUE, 
                      print_plot = TRUE, 
                      bag_frac = 0.8, 
                      p_weak = 4)

## -----------------------------------------------------------------------------
head(m_discrete$test_prediction)
head(m_real$test_prediction)

table(y_test, m_discrete$test_prediction)
table(y_test, m_real$test_prediction)

## -----------------------------------------------------------------------------
pred_discrete <- predict(object = m_discrete, newdata = x_test, type = "pred")
pred_real <- predict(object = m_real, newdata = x_test, type = "pred")

all(pred_discrete == m_discrete$test_prediction)
all(pred_discrete == m_discrete$test_prediction)

## -----------------------------------------------------------------------------
prob_discrete <- predict(object = m_discrete, newdata = x_test, type = "prob")
head(prob_discrete)

## ---- fig.width=7, fig.height=6, fig.align='center'---------------------------

library(mlbench)
data(Glass)
data <- Glass
p <- ncol(data) - 1

x <- data[,1:p]
y <- data[, p + 1]

train_i <- cv_sampler(y, 0.9)
x_train <- x[train_i,]
y_train <- y[train_i]

x_test <- x[-train_i,]
y_test <- y[-train_i]


par(mfrow = c(2,1))
m_discrete <- booster(x_train = x_train, 
                      y_train = y_train, 
                      classifier = "rpart", 
                      method = "discrete",
                      x_test = x_test,
                      y_test = y_test, 
                      weighted_bootstrap = FALSE,
                      max_iter = 20, 
                      lambda = 1, 
                      print_detail = FALSE, 
                      print_plot = TRUE, 
                      bag_frac = 0.8, 
                      p_weak = p)

m_real <- booster(x_train = x_train, 
                  y_train = y_train, 
                  classifier = "rpart", 
                  method = "real",
                  x_test = x_test,
                  y_test = y_test, 
                  weighted_bootstrap = FALSE,
                  max_iter = 20, 
                  lambda = 0.1, 
                  print_detail = FALSE, 
                  print_plot = TRUE, 
                  bag_frac = 1, 
                  p_weak = p)

invisible(dev.off())
pred_discrete <- predict(object = m_discrete, newdata = x_test, type = "pred")
pred_real <- predict(object = m_real, newdata = x_test, type = "pred")

table(y_test, pred_discrete)
table(y_test, pred_real)

## ---- fig.width=7, fig.height=6, fig.align='center'---------------------------
par(mfrow = c(2,1))
m_discrete <- booster(x_train = x_train, 
                      y_train = y_train, 
                      classifier = "dnb", 
                      method = "discrete",
                      x_test = x_test,
                      y_test = y_test, 
                      weighted_bootstrap = FALSE,
                      max_iter = 250, 
                      lambda = 1, 
                      print_detail = FALSE, 
                      print_plot = TRUE, 
                      bag_frac = 0.5, 
                      p_weak = 4)

m_real <- booster(x_train = x_train, 
                  y_train = y_train, 
                  classifier = "dnb", 
                  method = "real",
                  x_test = x_test,
                  y_test = y_test, 
                  weighted_bootstrap = FALSE,
                  max_iter = 250, 
                  lambda = 1e-4, 
                  print_detail = FALSE, 
                  print_plot = TRUE, 
                  bag_frac = 0.2, 
                  p_weak = 4)

invisible(dev.off())
pred_discrete <- predict(object = m_discrete, newdata = x_test, type = "pred")
pred_real <- predict(object = m_real, newdata = x_test, type = "pred")

table(y_test, pred_discrete)
table(y_test, pred_real)

## -----------------------------------------------------------------------------

classifier_lm <- function(x_train, y_train, weights, ...){
  y_train_code <- c(-1,1)
  y_train_coded <- sapply(levels(y_train), function(m) y_train_code[(y_train == m) + 1])
  y_train_coded <- y_train_coded[,1]
  if (is.null(weights)) {
    weights <- rep(1, length(y_train))
  }
  
  model <- lm.wfit(x = as.matrix(cbind(1,x_train)), y = y_train_coded, w = weights)
  return(list(coefficients = model$coefficients,
              levels = levels(y_train)))
}

predictor_lm <- function(model, x_new, type = "pred", ...) {
  coef <- model$coefficients
  levels <- model$levels
  
  fit <- as.matrix(cbind(1, x_new))%*%coef
  probs <- 1/(1 + exp(-fit))
  probs <- data.frame(probs, 1 - probs)
  colnames(probs) <- levels
  
  if (type == "pred") {
    preds <- factor(levels[apply(probs, 1, which.max)], levels = levels, labels = levels)
    return(preds)
  }
  if (type == "prob") {
    return(probs)
  }
}

## ---- fig.width=7, fig.height=6, fig.align='center'---------------------------
data <- glass0
p <- ncol(data) - 1

x <- data[,1:p]
y <- data[, p + 1]

train_i <- cv_sampler(y, 0.9)
x_train <- x[train_i,]
y_train <- y[train_i]

x_test <- x[-train_i,]
y_test <- y[-train_i]

par(mfrow = c(2,1))
m_discrete <- booster(x_train = x_train, 
                      y_train = y_train, 
                      classifier = classifier_lm,
                      predictor = predictor_lm,
                      method = "discrete",
                      x_test = x_test,
                      y_test = y_test, 
                      weighted_bootstrap = FALSE,
                      max_iter = 600, 
                      lambda = 2, 
                      print_detail = FALSE, 
                      print_plot = TRUE, 
                      bag_frac = 0.4, 
                      p_weak = 4)

m_real <- booster(x_train = x_train, 
                  y_train = y_train, 
                  classifier = classifier_lm,
                  predictor = predictor_lm,
                  method = "real",
                  x_test = x_test,
                  y_test = y_test, 
                  weighted_bootstrap = FALSE,
                  max_iter = 200, 
                  lambda = 0.1, 
                  print_detail = FALSE, 
                  print_plot = TRUE, 
                  bag_frac = 1, 
                  p_weak = 4)

invisible(dev.off())
pred_discrete <- predict(object = m_discrete, newdata = x_test, type = "pred")
pred_real <- predict(object = m_real, newdata = x_test, type = "pred")

table(y_test, pred_discrete)
table(y_test, pred_real)

## ---- fig.width=7, fig.height=6, fig.align='center'---------------------------
par(mfrow = c(2,1))
plot(m_discrete)
plot(m_real)
invisible(dev.off())

