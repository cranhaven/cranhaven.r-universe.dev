## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment  = "#>",
  message  = FALSE,
  warning  = FALSE
)

## ----setup--------------------------------------------------------------------
library(SLmetrics)

## -----------------------------------------------------------------------------
# 1) seed
set.seed(1903)

# 2) actual values
actual <- factor(
    x = sample(c("A", "B", "C"), size = 10, replace = TRUE)
)

# 3) predicted values
predicted <- factor(
    x = sample(c("A", "B", "C"), size = 10, replace = TRUE)
)

# 4) sample weights
weights <- runif(
    n = length(actual)
)

## -----------------------------------------------------------------------------
# 1) calculate accuracy
accuracy(
    actual    = actual,
    predicted = predicted
)

## -----------------------------------------------------------------------------
# 1) calculate recall
recall(
    actual    = actual,
    predicted = predicted
)

# 2) calculate sensitivity
sensitivity(
    actual    = actual,
    predicted = predicted
)

# 1) calculate true positive rate
tpr(
    actual    = actual,
    predicted = predicted
)

## -----------------------------------------------------------------------------
# 1) macro average
recall(
    actual    = actual,
    predicted = predicted,
    estimator = 2 # macro average: 2
)

# 2) micro average
recall(
    actual    = actual,
    predicted = predicted,
    estimator = 1 # micro average: 1
)

## -----------------------------------------------------------------------------
# 1) confusion matrix
confusion_matrix <- cmatrix(
    actual    = actual,
    predicted = predicted
)

# 2) summarise confusion matrix
summary(
    confusion_matrix
)

## -----------------------------------------------------------------------------
# 1) calculate accuracy
accuracy(
    confusion_matrix
)

# 2) calculate false positive rate
fpr(
    confusion_matrix
)

## -----------------------------------------------------------------------------
# 1) calculate recall
weighted.recall(
    actual    = actual,
    predicted = predicted,
    w         = weights
)

# 2) calculate sensitivity
weighted.sensitivity(
    actual    = actual,
    predicted = predicted,
    w         = weights
)

# 1) calculate true positive rate
weighted.tpr(
    actual    = actual,
    predicted = predicted,
    w         = weights
)

## -----------------------------------------------------------------------------
# 1) calculate weighted confusion matrix
weighted_confusion_matrix <- weighted.cmatrix(
    actual = actual,
    predicted = predicted,
    w = weights
)

# 2) calculate weighted accuracy
try(
    weighted.accuracy(weighted_confusion_matrix)
)

## -----------------------------------------------------------------------------
accuracy(weighted_confusion_matrix)

## -----------------------------------------------------------------------------
all.equal(
    accuracy(weighted_confusion_matrix),
    weighted.accuracy(actual, predicted, w = weights)
)

