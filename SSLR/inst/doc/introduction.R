## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ---- eval = FALSE------------------------------------------------------------
#  rf <-  rand_forest(trees = 100, mode = "classification") %>%
#    set_engine("randomForest")
#  
#  m <- selfTraining(learner = rf) %>% fit(Wine ~ ., data = train)

## ---- eval = FALSE------------------------------------------------------------
#  rf <-  rand_forest(trees = 100, mode = "classification") %>%
#    set_engine("randomForest")
#  
#  m <- selfTraining(learner = rf) %>% fit_xy(x = train[,-cls], y = train$Wine)

