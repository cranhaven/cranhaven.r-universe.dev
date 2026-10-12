## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  message = FALSE,
  eval = FALSE
)

## -----------------------------------------------------------------------------
#  library(arenar)
#  
#  apartments <- DALEX::apartments
#  head(apartments)

## ---- results = "hide"--------------------------------------------------------
#  library(gbm)
#  library(DALEX)
#  library(dplyr)
#  
#  model_gbm100 <- gbm(m2.price ~ ., data = apartments, n.trees = 100)
#  expl_gbm100 <- explain(
#    model_gbm100,
#    data = apartments,
#    y = apartments$m2.price,
#    label = "gbm [100 trees]"
#  )
#  
#  model_gbm500 <- gbm(m2.price ~ ., data = apartments, n.trees = 500)
#  expl_gbm500 <- explain(
#    model_gbm500,
#    data = apartments,
#    y = apartments$m2.price,
#    label = "gbm [500 trees]"
#  )
#  
#  model_glm <- glm(m2.price ~ ., data = apartments)
#  expl_glm <- explain(model_glm, data = apartments, y = apartments$m2.price)

## ---- eval = FALSE------------------------------------------------------------
#  arena <- create_arena(live = TRUE) %>%
#    # Pushing explainers for each models
#    push_model(expl_gbm100) %>%
#    push_model(expl_gbm500) %>%
#    push_model(expl_glm) %>%
#    # Push dataframe of observations
#    push_observations(apartments) %>%
#    # Run server of default port and ip
#    run_server()

## ---- eval = FALSE------------------------------------------------------------
#  apartments2 <- apartments
#  rownames(apartments2) <- paste0(
#    1:nrow(apartments2),
#    ". ",
#    apartments2$surface,
#    "m2 ",
#    apartments2$district
#  )
#  
#  create_arena(live = TRUE) %>%
#    push_model(expl_glm) %>%
#    push_observations(apartments2) %>%
#    run_server()

## ---- results = "hide", eval = FALSE------------------------------------------
#  create_arena(live = TRUE) %>%
#    push_model(expl_glm) %>%
#    run_server(append_data = TRUE)

