## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(tidycode)

## -----------------------------------------------------------------------------
cat(readLines(tidycode_example("example_plot.R")), sep = '\n')

## -----------------------------------------------------------------------------
cat(readLines(tidycode_example("example_analysis.R")), sep = '\n')

## -----------------------------------------------------------------------------
(d <- read_rfiles(
  tidycode_example("example_plot.R"),
  tidycode_example("example_analysis.R")
  ))

## -----------------------------------------------------------------------------
d[1, ]

## -----------------------------------------------------------------------------
d[1, "expr"][[1]]

## ---- message=FALSE, warning=FALSE--------------------------------------------
library(dplyr)

d_funcs <- d %>%
  unnest_calls(expr)

d_funcs

## -----------------------------------------------------------------------------
d_funcs[1, ]

## -----------------------------------------------------------------------------
d_funcs[1, "args"][[1]]

## -----------------------------------------------------------------------------
d_funcs %>%
  anti_join(get_stopfuncs())

## -----------------------------------------------------------------------------
d_funcs %>%
  anti_join(get_stopfuncs()) %>%
  inner_join(get_classifications("crowdsource", include_duplicates = FALSE)) %>%
  select(func, classification)

## -----------------------------------------------------------------------------
d_funcs %>%
  anti_join(get_stopfuncs()) %>%
  inner_join(get_classifications("crowdsource")) %>%
  select(func, classification, score)

