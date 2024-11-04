## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(echo = TRUE, collapse = TRUE)
library(dplyr)
library(maraca)

## -----------------------------------------------------------------------------
data("hce_scenario_a")
# Create data with binary version of continuous final endpoint
bin_data <- hce_scenario_a
# Index of all continuous outcome rows 
idx_cont <- bin_data$GROUP == "Continuous outcome"
# Rename outcome
bin_data[idx_cont,"GROUP"] <- "Binary outcome"
# Binary version (>= 0/< 0)
bin_data[idx_cont,"AVAL0"] <- bin_data[idx_cont,"AVAL0"] >= 0
bin_data[idx_cont,"AVAL"] <- bin_data[idx_cont,"AVAL0"] +
  bin_data[idx_cont,"GROUPN"]
head(bin_data)

## -----------------------------------------------------------------------------
column_names <- c(
    outcome = "GROUP",
    arm = "TRTP",
    value = "AVAL0"
)

step_outcomes <- c("Outcome I", "Outcome II",
                   "Outcome III", "Outcome IV")

last_outcome <- "Binary outcome"

arm_levels <- c(active = "Active",
                control = "Control")

mar <- maraca(
  bin_data, step_outcomes, last_outcome,
  arm_levels, column_names,
  fixed_followup_days = 3*365,
  compute_win_odds = TRUE,
  # Important change: Add information that last endpoint is 
  # not continuous (the default)
  last_type = "binary"
)

## ----fig.width = 7, fig.height = 6--------------------------------------------
plot(mar)

## -----------------------------------------------------------------------------
data("hce_scenario_a")
# Create data with binary version of continuous final endpoint
bin_data2 <- hce_scenario_a
# Index of all continuous outcome rows 
idx_bin <- bin_data2$GROUP %in% c("Outcome III", "Outcome IV")
# Binary version (>= 0/< 0), coded as 1
bin_data2[idx_bin,"AVAL0"] <- bin_data2[idx_bin,"AVAL0"] >= 500
bin_data2[idx_bin,"AVAL"] <- bin_data2[idx_bin,"AVAL0"] +
  bin_data2[idx_bin,"GROUPN"]
# Remove 0 rows (only include patients that had the outcome)
bin_data2 <- bin_data2[bin_data2$AVAL0 != 0,]
head(bin_data2)

## -----------------------------------------------------------------------------
column_names <- c(
    outcome = "GROUP",
    arm = "TRTP",
    value = "AVAL0"
)

step_outcomes <- c("Outcome I", "Outcome II",
                   "Outcome III", "Outcome IV")

last_outcome <- "Continuous outcome"

arm_levels <- c(active = "Active",
                control = "Control")

mar <- maraca(
  bin_data2, step_outcomes, last_outcome,
  arm_levels, column_names,
  fixed_followup_days = 3*365,
  compute_win_odds = TRUE,
  # Important change: Add information that last endpoint is 
  # not continuous (the default)
  step_types = c("tte","tte","binary","binary")
)

## ----fig.width = 7, fig.height = 6--------------------------------------------
plot(mar)

## ----fig.width = 7, fig.height = 6--------------------------------------------
plot(mar, continuous_grid_spacing_x = 20,
     theme = "color1")

## ----fig.width = 7, fig.height = 6--------------------------------------------
component_plot(mar,
     theme = "color2")

## ----fig.width = 7, fig.height = 6--------------------------------------------
cumulative_plot(mar,
     theme = "color1")

