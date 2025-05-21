library(tidyverse)
library(caret)
library(SSLR)
library(tidymodels)

data(wine)


cls <- which(colnames(wine) == "Wine")

#% LABELED
labeled.index <- createDataPartition(wine$Wine, p = .2, list = FALSE)
wine[-labeled.index,cls] <- NA


m <- GRFClassifierSSLR() %>% fit(Wine ~ ., data = wine)

#Accesing model from RSSL
model <- m$model

#Predictions of unlabeled
preds_unlabeled <- m %>% predictions()
print(preds_unlabeled)

preds_unlabeled <- m %>% predictions(type = "raw")
print(preds_unlabeled)

#Total
y_total <- wine[,cls]
y_total[-labeled.index] <- preds_unlabeled
