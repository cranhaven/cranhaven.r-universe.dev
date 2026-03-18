## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  message = FALSE,
  warning = FALSE
  # dev = "png", # uncomment if issues running locally on mac
  # dev.args = list(type = "cairo-png")
)

## ----setup--------------------------------------------------------------------
library(plotBart)
data(lalonde)
confounders <- c('age', 'educ', 'black', 'hisp', 'married', 'nodegr')

# plot balance across treatment and control groups
p <- plot_balance(.data = lalonde,
                  treatment = 'treat',
                  confounders = confounders)
p

## -----------------------------------------------------------------------------
p +
  labs(title = 'My comments on the results',
       subtitle = NULL,
       caption = 'December 2021',
       x = 'Mean diff b/t treatment and control')

## -----------------------------------------------------------------------------
p + 
  theme_classic()
# set the theme for all plots within this R session
theme_set(theme_bw())

## -----------------------------------------------------------------------------
p$data

