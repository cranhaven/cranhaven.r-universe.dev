## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
# Load Libraries
library(mshap)
library(ggplot2)
library(dplyr)

## -----------------------------------------------------------------------------
set.seed(18)

dat <- data.frame(
  age = runif(1000, min = 0, max = 20),
  prop_domestic = runif(1000),
  model = sample(c(0, 1), 1000, replace = TRUE),
  maintain = rexp(1000, .01) + 200
)

shap <- data.frame(
  age = rexp(1000, 1/dat$age) * (-1)^(rbinom(1000, 1, dat$prop_domestic)),
  prop_domestic = -200 * rnorm(100, dat$prop_domestic, 0.02) + 100,
  model = ifelse(dat$model == 0, rnorm(1000, -50, 30), rnorm(1000, 50, 30)),
  maintain = (rnorm(1000, dat$maintain, 100) - 400) * 0.2
)

## ---- fig.width=5, fig.height=5,fig.align='center'----------------------------
summary_plot(
  variable_values = dat,
  shap_values = shap
)

## ---- fig.width=5, fig.height=5,fig.align='center'----------------------------
summary_plot(
  variable_values = dat,
  shap_values = shap,
  legend.position = "bottom"
)

## ---- fig.width=5, fig.height=5,fig.align='center'----------------------------
summary_plot(
  variable_values = dat,
  shap_values = shap,
  legend.position = "bottom",
  names = c("Age", "% Domestic", "Model", "Maintenence Hours")
)

## ---- fig.width=5, fig.height=5,fig.align='center'----------------------------
summary_plot(
  variable_values = dat,
  shap_values = shap,
  legend.position = "bottom",
  names = c("Age", "% Domestic", "Model", "Maintenence Hours"),
  colorscale = c("blue", "purple", "red"),
  font_family = "Arial",
  title = "A Custom Title"
)

## -----------------------------------------------------------------------------
expected_value <- 1000

## ---- fig.width=6, fig.height=5,fig.align='center'----------------------------
observation_plot(
  variable_values = dat[1,],
  shap_values = shap[1,],
  expected_value = expected_value
)

## ---- fig.width=6, fig.height=5,fig.align='center'----------------------------
observation_plot(
  variable_values = dat[1,],
  shap_values = shap[1,],
  expected_value = expected_value,
  names = c("Age", "% Domestic", "Model", "Maintenence Hours"),
  font_family = "Arial",
  title = "A Custom Title"
)

## ---- fig.width=6, fig.height=5,fig.align='center'----------------------------
observation_plot(
  variable_values = dat[1,] %>% mutate(model = ifelse(model == 0, "A", "B")),
  shap_values = shap[1,],
  expected_value = expected_value,
  names = c("Age", "% Domestic", "Model", "Maintenence Hours"),
  font_family = "Arial",
  title = "A Custom Title"
)

## ---- fig.width=6, fig.height=5,fig.align='center'----------------------------
observation_plot(
  variable_values = dat[1,] %>% mutate(model = ifelse(model == 0, "A", "B")),
  shap_values = shap[1,],
  expected_value = expected_value,
  names = c("Age", "% Domestic", "Model", "Maintenence Hours"),
  font_family = "Arial",
  title = "A Custom Title",
  fill_colors = c("red", "blue"),
  connect_color = "black",
  expected_color = "purple",
  predicted_color = "yellow"
)

## ---- fig.width=5, fig.height=5,fig.align='center'----------------------------
summary_plot(
  variable_values = dat,
  shap_values = shap,
  legend.position = "bottom",
  names = c("Age", "% Domestic", "Model", "Maintenence Hours")
) +
  theme(
    plot.background = element_rect(fill = "grey"),
    panel.background = element_rect(fill = "lightgrey")
  )

## ---- fig.width=6, fig.height=5,fig.align='center'----------------------------
observation_plot(
  variable_values = dat[1,] %>% mutate(model = ifelse(model == 0, "A", "B")),
  shap_values = shap[1,],
  expected_value = expected_value,
  names = c("Age", "% Domestic", "Model", "Maintenence Hours"),
  font_family = "Arial",
  title = "A Custom Title"
) +
  geom_label(
    aes(y = 950, x = 4, label = "This is a really big bar!"),
    color = "#FFFFFF",
    fill = NA
  )

