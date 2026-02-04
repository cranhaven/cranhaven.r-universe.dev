## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,  # collapse all the source and output blocks?
  comment = "#>"  # the prefix to be put before source code output
)

## ----libs, echo=TRUE, message=FALSE--------------------------------------
library(tvgeom)
library(dplyr)
library(tidyr)
library(purrr)
library(magrittr)
library(ggplot2)
library(ggthemes)
library(gridExtra)

## ---- results='asis', message=FALSE--------------------------------------
# A logistic curve, which we can use to create a monotonically increasing or
# decreasing probability of success.
logistic <- function(n, x0, L_min, L_max, k, ...) { 
  (L_max - L_min) / (1 + exp(-k * (seq_len(n) - x0))) + L_min
}

# Wrappers.
get_phi <- function(data) {
  data %>% pull(p_success) %>% c(1)
}
draw_from_tvgeom <- function(data, n_samples = 1000) {
  rtvgeom(n_samples, get_phi(data))
}

# The total number of trials.
n_days <- 100

# Create an array of intuition-building scenarios. The time-varying probability 
# of success (based upon which we will draw our samples) will depend entirely on
# the shape-controlling parameters of the curve for each scenario.
scenarios <- crossing(n = n_days, 
                      x0 = 60, 
                      L_min = 0, 
                      L_max = c(.1, .25, .7), 
                      k = c(-.2, 0, .5)
                      ) %>% 
  mutate(scenario = as.character(1:n()))

## ---- echo=FALSE---------------------------------------------------------
knitr::kable(scenarios, caption = 'Scenarios...')

## ---- message = FALSE, warning = FALSE-----------------------------------
# Calculate the probability of success for each scenario.
d_phi <- scenarios %>% 
  split(.$scenario) %>% 
  map(~ do.call(logistic, .)) %>% 
  bind_cols %>% 
  mutate(day = 1:n()) %>% 
  gather(scenario, p_success, -day) %>% 
  left_join(scenarios)

# On the basis of d_phi, make draws for new y's using rtvgeom.
d_y <- d_phi %>% select(scenario, p_success) %>% split(.$scenario) %>% 
  map(~ draw_from_tvgeom(.)) %>% 
  bind_cols %>% 
  gather(scenario, y) %>% 
  left_join(scenarios)

# Plotting.
plot_param <- function(d_phi, d_y, parameter, subset = NULL) {

  d1 <- d_phi %>% 
    mutate(focal_param = factor(get(parameter))) %>% 
    {`if`(!is.null(subset), filter_(., subset), .)}

  p1 <- ggplot(d1) +
    facet_grid(scenario ~ .) +
    geom_line(aes_string(x = 'day', y = 'p_success', color = 'focal_param'), 
              size = 1.01) +
    theme_hc(base_size = 13) +
    scale_color_hc(name = parameter) +
    labs(x = 'Day', y = expression(phi))
  
  d2 <- d_y %>% 
    mutate(focal_param = factor(get(parameter))) %>% 
    {`if`(!is.null(subset), filter_(., subset), .)}
  p2 <- ggplot(d2) +
    facet_grid(scenario ~ .) +
    geom_histogram(aes_string(x = 'y', y = '..density..', fill = 'focal_param'), 
                   color = 'black', alpha = .8) +
    theme_hc(base_size = 13) +
    scale_fill_hc(name = parameter) +
    labs(x = 'Day', y = 'Density') 
  
  grid.arrange(p1, p2, ncol = 2)
  
}


## ---- echo=FALSE, warning = FALSE, fig.width=7, fig.height=5, message=FALSE, fig.cap = "Rate of change."----
plot_param(d_phi, d_y, 'k', 'L_max == min(L_max)')

## ---- echo=FALSE, fig.width=7, fig.height=5, message=FALSE, fig.cap = "Max probability of success."----
plot_param(d_phi, d_y, 'L_max', 'k == max(k)')

