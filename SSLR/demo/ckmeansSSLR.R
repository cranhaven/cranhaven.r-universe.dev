library(tidyverse)
library(caret)
library(SSLR)
library(tidymodels)

data <- iris

set.seed(1)
#% LABELED
cls <- which(colnames(iris) == "Species")

labeled.index <- createDataPartition(data$Species, p = .2, list = FALSE)
data[-labeled.index,cls] <- NA


m <- ckmeansSSLR() %>% fit(Species ~ ., data)

#Get labels (assing clusters), type = "raw" return factor
labels <- m %>% cluster_labels()

print(labels)


