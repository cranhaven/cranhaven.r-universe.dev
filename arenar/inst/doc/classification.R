## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  eval = FALSE
)

## ----setup, message=FALSE, warning=FALSE--------------------------------------
#  library(arenar)
#  library(dplyr)
#  library(DALEX)
#  library(MASS)
#  library(gbm)
#  
#  #data set
#  HR <- DALEX::HR
#  
#  # Get random 10 samples to explain it
#  observations <- HR[sample(1:nrow(HR), size=10), ]
#  # Name observations
#  rownames(observations) <- paste0(
#    toupper(substr(observations$gender, 0, 1)),
#    substr(observations$gender, 2, 100),
#    " ",
#    round(observations$age),
#    "yr",
#    " Grade: ",
#    observations$evaluation
#  )

## -----------------------------------------------------------------------------
#  model_gbm <- gbm(status ~ ., data=HR, n.trees=100, interaction.depth = 3)
#  model_lda <- lda(status ~ ., data=HR)

## -----------------------------------------------------------------------------
#  # Create explainers
#  explainer_gbm <- DALEX::explain(model_gbm, data=HR, y=HR$status)
#  # For LDA we need to set model_info manualy
#  explainer_lda <- DALEX::explain(
#    model_lda,
#    data=HR,
#    y=HR$status,
#    model_info=list(package="MASS", ver="", type="multiclass"),
#    predict_function = function(m, x) predict(m, x)$posterior
#  )
#  
#  # Create new arena and add prepared observations and explainers
#  arena <-
#    create_arena() %>%
#    push_observations(observations) %>%
#    push_model(explainer_gbm) %>%
#    push_model(explainer_lda)
#  
#  # Now you can see that each explainer was splited into three
#  print(arena)
#  # ===== Static Arena Summary =====
#  # Models: gbm [fired], gbm [ok], gbm [promoted], lda [fired], lda [ok], lda [promoted]
#  # Observations: Male 31yr Grade: 2, Female 24yr Grade: 4, Female 21yr Grade: 3, Female 25yr Grade: 3, Male 43yr Grade: 3, Female 32yr Grade: 5, Female 54yr Grade: 2, Female 32yr Grade: 2, Male 21yr Grade: 2, Male 41yr Grade: 2
#  # Variables: gender, age, hours, evaluation, salary
#  # Plots count: 510
#  # NULL
#  
#  # Upload arena
#  if (interactive()) upload_arena(arena)

## -----------------------------------------------------------------------------
#  # Create new arena and add prepared observations
#  arena <- create_arena() %>% push_observations(observations)
#  
#  # Levels of target variable
#  levels(HR$status)
#  # [1] "fired"    "ok"       "promoted"
#  
#  # For each target level create explainers
#  for (status in levels(HR$status)) {
#    # Explainer for gbm
#    explainer_gbm <- explain(
#      model_gbm,
#      # Target variable as 0,1 for each level
#      y = as.numeric(HR$status == status),
#      data = HR[, -6], # Remove target variable
#      label = paste0("GBM [", status, "]"),
#      # In predict function we need to extract class probability
#      predict_function = function(m, x) predict(m, x, n.trees=100, type="response")[,status,]
#    )
#    # Explainer for lda
#    explainer_lda <- explain(
#      model_lda,
#      # Target variable as 0,1 for each level
#      y = as.numeric(HR$status == status),
#      data = HR[, -6], # Remove target variable
#      label = paste0("LDA [", status, "]"),
#      # In predict function we need to extract class probability
#      predict_function = function(m, x) predict(m, x)$posterior[,status]
#    )
#    # Add explainers
#    arena <- push_model(arena, explainer_gbm)
#    arena <- push_model(arena, explainer_lda)
#  
#    # Upload arena
#    if (interactive()) upload_arena(arena)
#  }

