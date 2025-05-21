library(SSLR)
#' \donttest{

library(caret)
library(kernlab)
library(proxy)


## Load Wine data set
data(wine)
cls <- which(colnames(wine) == "Wine")
x <- wine[, -cls] # instances without classes
y <- wine[, cls] # the classes
x <- scale(x)


set.seed(20)

# Use 50% of instances for training
tra.idx <- sample(x = length(y), size = ceiling(length(y) * 0.5))
xtrain <- x[tra.idx,]
ytrain <- y[tra.idx]

# Use 70% of train instances as unlabeled set
tra.na.idx <- sample(x = length(tra.idx),size = ceiling(length(tra.idx) * 0.7))
ytrain[tra.na.idx] <- NA


# Use the other 50% of instances for inductive testing
tst.idx <- setdiff(1:length(y), tra.idx)
xitest <- x[tst.idx,] # testing instances
yitest <- y[tst.idx] # classes of instances in xitest
# Use the unlabeled examples for transductive testing
xttest <- x[tra.idx[tra.na.idx],] # transductive testing instances
yttest <- y[tra.idx[tra.na.idx]] # classes of instances in xttest


## Example A:
# Training from a set of instances with
# 1-NN and C-svc (SVM) as base classifiers.

### Define knn base classifier using knn3 from caret package

# learner function
knn <- function(indexes, cls) {
  knn3(x = xtrain[indexes, ], y = cls, k = 1)
}
# function to predict probabilities
knn.prob <- function(model, indexes) {
  predict(model, xtrain[indexes, ])
}

### Define svm base classifier using ksvm from kernlab package

# learner function
svm <- function(indexes, cls) {
  rbf <- function(x, y) {
    sigma <- 0.048
    d <- dist(x, y, method = "Euclidean", by_rows = FALSE)
    exp(-sigma *  d * d)
  }
  class(rbf) <- "kernel"
  ksvm(x = xtrain[indexes, ], y = cls, scaled = FALSE,
       type = "C-svc",  C = 1,
       kernel = rbf, prob.model = TRUE)
}
# function to predict probabilities
svm.prob <- function(model, indexes) {
  predict(model, xtrain[indexes, ], type = "probabilities")
}

### Train

trControl_democraticG1 <- list(gen.learners = list(knn, svm),
                              gen.preds = list(knn.prob, svm.prob))
m1 <- train_generic(ytrain,method = "democraticG", trControl = trControl_democraticG1)

### Predict
# predict labels using each classifier
m1.pred1 <- predict(m1$model[[1]], xitest, type = "class")
m1.pred2 <- predict(m1$model[[2]], xitest)

# combine predictions
m1.pred <- list(m1.pred1, m1.pred2)
cls1 <- democraticCombine(m1.pred, m1$W, m1$classes)


#' }
