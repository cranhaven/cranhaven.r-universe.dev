## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup, eval=FALSE--------------------------------------------------------
#  library(rbioacc)

## ----plotPP, eval=FALSE-------------------------------------------------------
#  data("Male_Gammarus_Single")
#  modelData_MGS <- modelData(Male_Gammarus_Single, time_accumulation = 4)
#  fit_MGS <- fitTK(modelData_MGS, iter = 10, chains = 2)
#  
#  # Compute a dataframe with priors and posteriors
#  df_MGS <- df_PriorPost(fit_MGS)
#  
#  # Compute a plot with priors and posteriors
#  plt_MGS <- plot_PriorPost(fit_MGS)

