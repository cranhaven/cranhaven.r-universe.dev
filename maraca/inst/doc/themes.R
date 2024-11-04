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

## ----fig.width = 8, fig.height = 6--------------------------------------------
plot(maraca_dat)

## ----fig.width = 8, fig.height = 6--------------------------------------------
plot(maraca_dat, theme = "color1")

## ----fig.width = 8, fig.height = 6--------------------------------------------
plot(maraca_dat, theme = "color2")

## ----fig.width = 8, fig.height = 6--------------------------------------------
plot(maraca_dat, theme = "maraca_old")

## ----fig.width = 8, fig.height = 6--------------------------------------------
plot(maraca_dat, theme = "none")

## ----fig.width = 8, fig.height = 6--------------------------------------------
library(ggplot2)

p <- plot(maraca_dat, theme = "none")

p <- p +
  theme(
    legend.position = "bottom",
    axis.text.x.bottom = element_text(
      angle = 90,
      vjust = 0.5,
      hjust = 1
    ),
    axis.title.x = element_blank(),
    axis.title.y = element_blank()
  ) +
  scale_color_viridis_d(end = 0.8) +
  scale_fill_viridis_d(end = 0.8)

p

## ----fig.width = 8, fig.height = 6--------------------------------------------
p <- plot(maraca_dat)

colorScheme <- c("Active" = "steelblue", "Control" = "seagreen3")

p <- p +
  scale_color_manual(values = colorScheme) +
  scale_fill_manual(values = colorScheme)

p

## ----fig.width = 8, fig.height = 6--------------------------------------------
keep_default <- GeomLabel$default_aes$size

# Changing default text size for the plot
update_geom_defaults("text", list(size = 3))

plot(maraca_dat)

# Make sure to change defaults back to default for new plots
update_geom_defaults("text", list(size = keep_default))

## ----fig.width = 8, fig.height = 6--------------------------------------------
plot(maraca_dat, continuous_grid_spacing_x = 20)

