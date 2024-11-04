## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(echo = TRUE, collapse = TRUE)
library(dplyr)
library(ggplot2)
library(maraca)

## ----maraca1, eval = TRUE-----------------------------------------------------
library(maraca)

data(hce_scenario_a, package = "maraca")

data <- hce_scenario_a
head(data)

## ----maraca2, eval = TRUE-----------------------------------------------------
column_names <- c(
    outcome = "GROUP",
    arm = "TRTP",
    value = "AVAL0"
)

## ----maraca3, eval = TRUE-----------------------------------------------------
unique(data[["GROUP"]])

## ----maraca4, eval = TRUE-----------------------------------------------------
step_outcomes <- c(
  "Outcome I", "Outcome II", "Outcome III", "Outcome IV"
)
last_outcome <- "Continuous outcome"

## ----maraca5, eval = TRUE-----------------------------------------------------
unique(data[["TRTP"]])

## ----maraca6, eval = TRUE-----------------------------------------------------
arm_levels = c(active = "Active", control = "Control")

## ----maraca7, eval = TRUE-----------------------------------------------------
mar <- maraca(
  data, step_outcomes, last_outcome, arm_levels, column_names, 
  fixed_followup_days = 3*365,
  compute_win_odds = TRUE
)

## ---- eval = TRUE-------------------------------------------------------------
mar$win_odds

## ----maraca8, eval = TRUE, fig.width = 7, fig.height = 6----------------------
plot(mar, continuous_grid_spacing_x = 20)

## ----maraca9, eval = TRUE, fig.width = 7, fig.height = 6----------------------
plot(mar, continuous_grid_spacing_x = 20, density_plot_type = "box")

## ----maraca10, eval = TRUE, fig.width = 7, fig.height = 6---------------------
plot(mar, continuous_grid_spacing_x = 20, density_plot_type = "scatter", vline_type = "mean")

## ----maraca11, eval = TRUE, fig.width = 7, fig.height = 6, message=FALSE, warning=FALSE----
p <- plot_maraca(mar, continuous_grid_spacing_x = 20, density_plot_type = "scatter", vline_type = "mean")
p + 
  scale_color_manual(values = c("#E69F00", "#999999")) + 
  theme(axis.text.x.bottom = element_text(vjust = 0.5, hjust = 0.5))

## ----fig.width = 7, fig.height = 6--------------------------------------------
Rates_A <- c(1.72, 1.74, 0.58, 1.5, 1)
Rates_P <- c(2.47, 2.24, 2.9, 4, 6)
hce_dat <- hce::simHCE(n = 2500, TTE_A = Rates_A, TTE_P = Rates_P,
              CM_A = -6, CM_P = 3, CSD_A = 15, CSD_P = 16, fixedfy = 3,
              seed = 31337)
plot(hce_dat, compute_win_odds = TRUE, lowerBetter = TRUE,
     trans = "reverse")

