## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(incidental)
library(ggplot2)

## ----input_data---------------------------------------------------------------
head(spanish_flu)
head(spanish_flu_delay_dist)
delay_dist <- spanish_flu_delay_dist$proportion
print(abs(sum(delay_dist) - 1))

## ----fit_incidence, eval = FALSE----------------------------------------------
#  indiana_model = fit_incidence(
#    reported = spanish_flu$Indiana,
#    delay_dist = delay_dist
#  )
#  
#  kansas_model = fit_incidence(
#    reported = spanish_flu$Kansas,
#    delay_dist = delay_dist
#  )
#  
#  philly_model = fit_incidence(
#    reported = spanish_flu$Philadelphia,
#    delay_dist = delay_dist
#  )

## ----fit_incidence_load, echo = FALSE-----------------------------------------
kansas_model = readRDS("./data/kansas_model.rds")
model_df_list = readRDS("./data/model_df_list.rds")
kansas_model_hyperparameters = readRDS("./data/kansas_model_hyperparameters.rds")
kansas_model_aic = readRDS("./data/kansas_model_aic.rds")
kansas_model_percent_thresh = readRDS("./data/kansas_model_percent_thresh.rds")

## ----plot_outputs, fig.width=5, fig.height=3----------------------------------
plot(kansas_model, times = spanish_flu$Date)

## ----linear_extrapolation, fig.width=6, fig.height=6, eval = FALSE------------
#  linear_tail_values = c(7, 14, 21)
#  extrapolation_prior_precision_values = c(0, 10, 100)
#  
#  model_df_list = list()
#  counter = 1
#  for (linear_tail_value in linear_tail_values) {
#    for (extrapolation_prior_precision_value in extrapolation_prior_precision_values) {
#      kansas_model_temp = fit_incidence(
#        reported = spanish_flu$Kansas,
#        delay_dist = delay_dist,
#        linear_tail = linear_tail_value,
#        extrapolation_prior_precision = extrapolation_prior_precision_value)
#      df_temp = incidence_to_df(
#        kansas_model_temp,
#        times = spanish_flu$Date)
#      df_temp$tail = linear_tail_value
#      df_temp$extrapolation = extrapolation_prior_precision_value
#      model_df_list[[counter]] = df_temp
#      counter = counter + 1
#    }
#  }

## ----linear_extrapolation_plot, fig.width=6, fig.height=6---------------------
data_subset = do.call("rbind", model_df_list)
ggplot(data_subset, aes(x = Time, y = Reported)) + geom_point(color = "coral2", shape = 3) +
  geom_line(aes(x = Time, y = Ihat), color = "black") + 
  geom_ribbon(aes(ymin = LowCI, ymax = HighCI), color = NA, fill = "cornflowerblue", alpha = 0.2) + 
  ggtitle(sprintf("Kansas: Reported morality and fitted incidence\nlinear_tail (rows) and extrapolation_prior_precision (columns)")) + ylab("Deaths") +
  facet_grid(tail ~ extrapolation)

## ----hyperparameter_grids, fig.width=5, fig.height=3, eval = FALSE------------
#  
#  kansas_model_hyperparameters = fit_incidence(
#    reported = spanish_flu$Kansas,
#    delay_dist = delay_dist,
#    dof_grid = seq(22, 26, 2),
#    lam_grid = 10^(seq(1,-4, -1))
#  )

## ----hyperparameter_grids_plot, fig.width=5, fig.height=3---------------------
print(sprintf("Selected degrees of freedom from original model: %s; and updated model: %s",
              as.character(kansas_model$best_dof),
              as.character(kansas_model_hyperparameters$best_dof)))
print(sprintf("Selected lambda from original model: %s; and updated model: %s",
              as.character(kansas_model$best_lambda),
              as.character(kansas_model_hyperparameters$best_lambda)))

plot(kansas_model_hyperparameters, times = spanish_flu$Date)

## ----hyperparameter_methods, fig.width=5, fig.height=3, eval = FALSE----------
#  kansas_model_aic = fit_incidence(
#    reported = spanish_flu$Kansas,
#    delay_dist = delay_dist,
#    dof_method = "aic",
#    lam_method = "aic"
#  )

## ----hyperparameter_methods_plot, fig.width=5, fig.height=3-------------------
print(sprintf("Selected degrees of freedom from original model: %s; and updated model: %s",
              as.character(kansas_model$best_dof), 
              as.character(kansas_model_aic$best_dof)))
print(sprintf("Selected lambda from original model: %s; and updated model: %s",
              as.character(kansas_model$best_lambda),
              as.character(kansas_model_aic$best_lambda)))

plot(kansas_model_aic, times = spanish_flu$Date)

## ----percent_thresh, fig.width=5, fig.height=3, eval = FALSE------------------
#  
#  kansas_model_percent_thresh = fit_incidence(
#    reported = spanish_flu$Kansas,
#    delay_dist = delay_dist,
#    percent_thresh = 0.2
#  )

## ----percent_thresh_plot, fig.width=5, fig.height=3---------------------------
print(sprintf("Selected degrees of freedom from original model: %s; and updated model: %s",
              as.character(kansas_model$best_dof),
              as.character(kansas_model_percent_thresh$best_dof)))
print(sprintf("Selected lambda from original model: %s; and updated model: %s",
              as.character(kansas_model$best_lambda),
              as.character(kansas_model_percent_thresh$best_lambda)))

plot(kansas_model_percent_thresh, times = spanish_flu$Date)

