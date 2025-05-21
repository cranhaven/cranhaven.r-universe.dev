## ----eval=FALSE---------------------------------------------------------------
#  
#  library(SSLR)
#  library(tidymodels)
#  library(caret)

## ----include=FALSE------------------------------------------------------------
knitr::opts_chunk$set(
  digits = 3,
  collapse = TRUE,
  comment = "#>"
)
options(digits = 3)

library(SSLR)
library(tidymodels)
library(caret)

## ----wine, results="hide"-----------------------------------------------------
data(wine)

set.seed(1)

#Train and test data
train.index <- createDataPartition(wine$Wine, p = .7, list = FALSE)
train <- wine[ train.index,]
test  <- wine[-train.index,]

cls <- which(colnames(wine) == "Wine")

# 20 % LABELED
labeled.index <- createDataPartition(wine$Wine, p = .2, list = FALSE)
train[-labeled.index,cls] <- NA

## ----fit, results="hide"------------------------------------------------------
m <- SSLRDecisionTree(min_samples_split = round(length(labeled.index) * 0.25),
                      w = 0.3) %>% fit(Wine ~ ., data = train)

## ----testing------------------------------------------------------------------
test_results <- 
    test %>%
    select(Wine) %>%
    as_tibble() %>%
    mutate(
        dt_class = predict(m, test) %>% 
            pull(.pred_class)
    )

test_results


## ----metrics------------------------------------------------------------------
test_results %>% accuracy(truth = Wine, dt_class)

test_results %>% conf_mat(truth = Wine, dt_class)

#Using multiple metrics

multi_metric <- metric_set(accuracy, kap, sens, spec, f_meas )

test_results %>% multi_metric(truth = Wine, estimate = dt_class)

## ----metrics_raw--------------------------------------------------------------
predict(m,test,"raw")

## ----metrics_prob-------------------------------------------------------------
predict(m,test,"prob")

