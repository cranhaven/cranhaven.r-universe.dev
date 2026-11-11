## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)
LOCAL <- identical(Sys.getenv("LOCAL"), "true")

## ----eval = LOCAL-------------------------------------------------------------
# library(rMIDAS)
# 
# adult <- read.csv("https://raw.githubusercontent.com/MIDASverse/MIDASpy/master/Examples/adult_data.csv",
#                   row.names = 1)[1:1000,]

## ----eval = LOCAL-------------------------------------------------------------
# set.seed(89)
# 
# adult <- add_missingness(adult, prop = 0.1)

## ----eval = LOCAL-------------------------------------------------------------
# 
# adult_cat <- c('workclass','marital_status','relationship','race','education','occupation','native_country')
# adult_bin <- c('sex','class_labels')
# 
# # Apply rMIDAS preprocessing steps
# adult_conv <- convert(adult,
#                       bin_cols = adult_bin,
#                       cat_cols = adult_cat,
#                       minmax_scale = TRUE)

## ----eval = LOCAL-------------------------------------------------------------
# # Train the model for 20 epochs
# adult_train <- train(adult_conv,
#                        training_epochs = 20,
#                        layer_structure = c(128,128),
#                        input_drop = 0.75,
#                        seed = 89)

## ----eval = LOCAL-------------------------------------------------------------
# 
# # Generate 10 imputed datasets
# adult_complete <- complete(adult_train, m = 10)
# 
# # Inspect first imputed dataset:
# head(adult_complete[[1]])

## ----eval = LOCAL-------------------------------------------------------------
# 
# # Estimate logit model on 10 completed datasets (using Rubin's combination rules)
# adult_model <- combine("class_labels ~ hours_per_week + sex",
#                     adult_complete,
#                     family = stats::binomial)
# 
# adult_model

