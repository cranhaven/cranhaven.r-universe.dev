## ----include = FALSE----------------------------------------------------------
devtools::load_all('.')

## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = '#>'
)
library(fairness)

## ----eval = FALSE-------------------------------------------------------------
#  install.packages('fairness')
#  library(fairness)

## ----eval = FALSE-------------------------------------------------------------
#  library(devtools)
#  devtools::install_github('kozodoi/fairness')
#  library(fairness)

## ----eval = TRUE--------------------------------------------------------------
data('compas')

## ----eval = TRUE--------------------------------------------------------------
compas$Two_yr_Recidivism_01 <- ifelse(compas$Two_yr_Recidivism == 'yes', 1, 0) 

## ----eval = FALSE-------------------------------------------------------------
#  dem_parity(data    = compas,
#             outcome = 'Two_yr_Recidivism_01',
#             group   = 'ethnicity',
#             probs   = 'probability',
#             cutoff  = 0.5,
#             base    = 'Caucasian')

## ----eval = FALSE-------------------------------------------------------------
#  prop_parity(data    = compas,
#              outcome = 'Two_yr_Recidivism_01',
#              group   = 'ethnicity',
#              probs   = 'probability',
#              cutoff  = 0.5,
#              base    = 'Caucasian')

## ----eval = FALSE-------------------------------------------------------------
#  equal_odds(data    = compas,
#             outcome = 'Two_yr_Recidivism_01',
#             group   = 'ethnicity',
#             probs   = 'probability',
#             cutoff  = 0.5,
#             base    = 'African_American')

## ----eval = FALSE-------------------------------------------------------------
#  pred_rate_parity(data    = compas,
#                   outcome = 'Two_yr_Recidivism_01',
#                   group   = 'ethnicity',
#                   probs   = 'probability',
#                   cutoff  = 0.5,
#                   base    = 'African_American')

## ----eval = FALSE-------------------------------------------------------------
#  acc_parity(data    = compas,
#             outcome = 'Two_yr_Recidivism_01',
#             group   = 'ethnicity',
#             probs   = 'probability',
#             preds   = NULL,
#             cutoff  = 0.5,
#             base    = 'African_American')

## ----eval = FALSE-------------------------------------------------------------
#  fnr_parity(data    = compas,
#             outcome = 'Two_yr_Recidivism_01',
#             group   = 'ethnicity',
#             probs   = 'probability',
#             cutoff  = 0.5,
#             base    = 'African_American')

## ----eval = FALSE-------------------------------------------------------------
#  fpr_parity(data    = compas,
#             outcome = 'Two_yr_Recidivism_01',
#             group   = 'ethnicity',
#             probs   = 'probability',
#             cutoff  = 0.5,
#             base    = 'African_American')

## ----eval = FALSE-------------------------------------------------------------
#  npv_parity(data    = compas,
#             outcome = 'Two_yr_Recidivism_01',
#             group   = 'ethnicity',
#             probs   = 'probability',
#             cutoff  = 0.5,
#             base    = 'African_American')

## ----eval = FALSE-------------------------------------------------------------
#  spec_parity(data    = compas,
#              outcome = 'Two_yr_Recidivism_01',
#              group   = 'ethnicity',
#              probs   = 'probability',
#              cutoff  = 0.5,
#              base    = 'African_American')

## ----eval = FALSE-------------------------------------------------------------
#  roc_parity(data    = compas,
#             outcome = 'Two_yr_Recidivism_01',
#             group   = 'ethnicity',
#             probs   = 'probability',
#             base    = 'African_American')

## ----eval = FALSE-------------------------------------------------------------
#  mcc_parity(data    = compas,
#             outcome = 'Two_yr_Recidivism_01',
#             group   = 'ethnicity',
#             probs   = 'probability',
#             cutoff  = 0.5,
#             base    = 'African_American')

## ----echo = FALSE-------------------------------------------------------------
output <- pred_rate_parity(data    = compas,
                           outcome = 'Two_yr_Recidivism_01',
                           group   = 'ethnicity',
                           probs   = 'probability',
                           cutoff  = 0.5,
                           base    = 'Caucasian')

## -----------------------------------------------------------------------------
output$Metric

## ----echo = FALSE-------------------------------------------------------------
output <- pred_rate_parity(data    = compas, 
                           outcome = 'Two_yr_Recidivism_01', 
                           group   = 'ethnicity',
                           probs   = 'probability', 
                           cutoff  = 0.5, 
                           base    = 'Hispanic')

## -----------------------------------------------------------------------------
output$Metric

## ---- fig.width=5, fig.height=3-----------------------------------------------
output$Metric_plot

## ---- fig.width=5, fig.height=3-----------------------------------------------
output$Probability_plot

## ----echo = FALSE-------------------------------------------------------------
output <- pred_rate_parity(data    = compas, 
                           outcome = 'Two_yr_Recidivism_01', 
                           group   = 'Female',
                           probs   = 'probability', 
                           cutoff  = 0.4, 
                           base    = 'Male')

## ---- fig.width=5, fig.height=3-----------------------------------------------
output$Probability_plot

## ----echo = FALSE, message=FALSE----------------------------------------------
output <- roc_parity(data     = compas, 
                     outcome  = 'Two_yr_Recidivism_01', 
                     group    = 'Female',
                     probs    = 'probability', 
                     base     = 'Male')

## ---- fig.width=5, fig.height=3-----------------------------------------------
output$ROCAUC_plot

