## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  message = FALSE,
  eval = FALSE
)

## ---- eval=FALSE--------------------------------------------------------------
#  library("dplyr")
#  library("DALEX")
#  
#  head(titanic_imputed)

## ---- eval=FALSE--------------------------------------------------------------
#  library("ranger")
#  
#  titanic_rf <- ranger(survived ~ .,
#                data = titanic_imputed,
#                probability = TRUE,
#                classification = TRUE)

## ---- eval=FALSE--------------------------------------------------------------
#  library("DALEX")
#  
#  titanic_ex <- explain(
#  	titanic_rf,
#  	y = titanic_imputed$survived,
#  	data = titanic_imputed)

## ---- eval=FALSE--------------------------------------------------------------
#  library("arenar")
#  
#  titanic_ar <- create_arena(live = TRUE) %>%
#  	push_model(titanic_ex)

## ---- eval=FALSE--------------------------------------------------------------
#  run_server(titanic_ar)

## ---- eval=FALSE--------------------------------------------------------------
#  passangers <- data.frame(
#              class = factor(c("1st", "3rd", "1st"), levels = c("1st", "2nd", "3rd", "deck crew",
#                          "engineering crew", "restaurant staff", "victualling crew")),
#              gender = factor(c("male", "male", "female"), levels = c("female", "male")),
#              age = c(8, 42, 12),
#              sibsp = c(0, 0, 0),
#              parch = c(0, 0, 0),
#              fare = c(72, 10, 50),
#              embarked = factor(c("Southampton", "Belfast", "Belfast"), levels = c("Belfast",
#                          "Cherbourg","Queenstown","Southampton")))
#  rownames(passangers) = c("Johny D", "Henry", "Mary")
#  passangers

## ---- eval=FALSE--------------------------------------------------------------
#  titanic_ar <- titanic_ar %>%
#  	push_observations(passangers)

## ---- eval=FALSE--------------------------------------------------------------
#  run_server(titanic_ar)

## ---- eval=FALSE--------------------------------------------------------------
#  titanic_glm <- glm(survived ~ ., data = titanic_imputed, family = "binomial")
#  
#  library("gbm")
#  
#  titanic_gbm <- gbm(survived ~ ., data = titanic_imputed, n.trees = 500)

## ---- eval=FALSE--------------------------------------------------------------
#  titanic_egb <- explain(titanic_gbm,
#      y = titanic_imputed$survived,
#      data = titanic_imputed)
#  titanic_elm <- explain(titanic_glm,
#      y = titanic_imputed$survived,
#      data = titanic_imputed)

## -----------------------------------------------------------------------------
#  titanic_ar <- titanic_ar %>%
#  	push_model(titanic_egb) %>%
#  	push_model(titanic_elm)

## ---- eval=FALSE--------------------------------------------------------------
#  run_server(titanic_ar)

## ---- eval=FALSE--------------------------------------------------------------
#  create_arena() %>%
#  	push_model(titanic_ex) %>%
#  	push_model(titanic_egb) %>%
#  	push_model(titanic_elm) %>%
#  	push_observations(passangers) %>%
#    upload_arena()

