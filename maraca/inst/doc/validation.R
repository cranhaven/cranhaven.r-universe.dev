## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(echo = TRUE, collapse = TRUE)
library(dplyr)
library(ggplot2)
library(maraca)

## ----maraca1, eval = TRUE-----------------------------------------------------
library(maraca)

data(hce_scenario_a)

maraca_dat <- maraca(
  data = hce_scenario_a,
  step_outcomes = c("Outcome I", "Outcome II", "Outcome III", "Outcome IV"),
  last_outcome = "Continuous outcome",
  fixed_followup_days = 3 * 365,
  column_names = c(outcome = "GROUP", arm = "TRTP", value = "AVAL0"),
  arm_levels = c(active = "Active", control = "Control"),
  compute_win_odds = TRUE
)

## ----fig.width=9, fig.height=6------------------------------------------------
# Save plot as its own object
maraca_plot <- plot(maraca_dat)
# The plot has its own class called "maracaPlot"
class(maraca_plot)
# Display plot
maraca_plot

## -----------------------------------------------------------------------------
validation_list <- validate_maraca_plot(maraca_plot) 
# Display which metrics are included
str(validation_list)

## -----------------------------------------------------------------------------
library(dplyr)
library(tidyr)

validation_list$proportions %>% 
  as.data.frame() %>%
  rename("proportion" = ".")

head(validation_list$tte_data)

validation_list$boxstat_data %>%
 unnest_wider(outliers, names_sep = "") %>%
 pivot_longer(., cols = -group, names_to = "stat_name", values_to = "values") %>%
 filter(!is.na(values)) %>% 
  as.data.frame()

head(validation_list$violin_data)

validation_list$wo_stats

