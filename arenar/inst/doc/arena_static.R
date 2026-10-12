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

## -----------------------------------------------------------------------------
#  observations <- apartments %>% filter(construction.year >= 2009)
#  # Observations' names are taken from rownames
#  rownames(observations) <- paste0(
#    observations$district,
#    " ",
#    observations$surface,
#    "m2 "
#  )

## ---- eval = FALSE------------------------------------------------------------
#  arena <- create_arena() %>%
#    # Pushing explainers for each models
#    push_model(expl_gbm100) %>%
#    push_model(expl_gbm500) %>%
#    push_model(expl_glm) %>%
#    # Push dataframe of observations
#    push_observations(observations) %>%
#    # Upload calculated arena files to Gist and open Arena in browser
#    upload_arena()

## -----------------------------------------------------------------------------
#  observations2 <- apartments %>% filter(construction.year == 2008)
#  # Observations' names are taken from rownames
#  rownames(observations2) <- paste0(
#    observations2$district,
#    " ",
#    observations2$surface,
#    "m2 "
#  )

## ---- eval = FALSE------------------------------------------------------------
#  arena %>%
#    push_observations(observations2) %>%
#    upload_arena()

## ---- eval = FALSE------------------------------------------------------------
#  create_arena() %>%
#    push_observations(arena_push_observations2) %>%
#    push_model(expl_glm) %>%
#    push_model(expl_gbm100) %>%
#    push_model(expl_gbm500) %>%
#    upload_arena(append_data = TRUE)

