library(tidyverse)
library(tidymodels)
library(SSLR)

#' \donttest{
data <- na.omit(airquality)
#data <- na.omit(airquality)

cls <- which(colnames(data) == "Ozone")

colnames(data)[cls]<- "class"

set.seed(1)
train.index  <- sample(nrow(data), round(0.7 * nrow(data)))
train <- data[ train.index,]
test  <- data[-train.index,]


#% LABELED
labeled.index <- sample(nrow(train), round(0.2 * nrow(train)))
train[-labeled.index,cls] <- NA

#We need a model with numeric predictions from parsnip
#https://tidymodels.github.io/parsnip/articles/articles/Models.html
#It should be with mode = regression

m_r <- rand_forest( mode = "regression") %>%
  set_engine("ranger")

m <- coBCReg(learner = m_r, max.iter = 2)
#' }
