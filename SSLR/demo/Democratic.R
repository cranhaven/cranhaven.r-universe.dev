library(tidyverse)
library(tidymodels)
library(caret)
library(SSLR)

data(wine)

set.seed(1)
train.index <- createDataPartition(wine$Wine, p = .7, list = FALSE)
train <- wine[ train.index,]
test  <- wine[-train.index,]

cls <- which(colnames(wine) == "Wine")

#% LABELED
labeled.index <- createDataPartition(wine$Wine, p = .2, list = FALSE)
train[-labeled.index,cls] <- NA

#We need a model with probability predictions from parsnip
#https://tidymodels.github.io/parsnip/articles/articles/Models.html
#It should be with mode = classification


rf <-  rand_forest(trees = 100, mode = "classification") %>%
  set_engine("randomForest")


bt <-  boost_tree(trees = 100, mode = "classification") %>%
  set_engine("C5.0")


m <- democratic(learners = list(rf,bt)) %>% fit(Wine ~ ., data = train)

#' \donttest{
#Accuracy
predict(m,test) %>%
  bind_cols(test) %>%
  metrics(truth = "Wine", estimate = .pred_class)


#With schemes
set.seed(1)
m <- democratic(learners = list(rf,bt),
                schemes = list(c("Malic.Acid","Ash"), c("Magnesium","Proline")) ) %>%
  fit(Wine ~ ., data = train)


#Accuracy
predict(m,test) %>%
  bind_cols(test) %>%
  metrics(truth = "Wine", estimate = .pred_class)

#'}
