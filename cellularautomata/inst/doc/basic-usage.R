## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(cellularautomata)

## -----------------------------------------------------------------------------
ca(18) |> plot()
ca(30) |> plot()
ca(45) |> plot()
ca(195) |> plot()

## ----fig.height=1, fig.width=4------------------------------------------------
ca(30, ncols = 20, steps = 30) |> plot(animate = TRUE)

## ----fig.height=1, fig.width=4------------------------------------------------
wolfram_rule_def(18)
wolfram_rule_def(30)
wolfram_rule_def(45)
wolfram_rule_def(195)

## -----------------------------------------------------------------------------
wolfram_rule(30)

## ----eval=FALSE---------------------------------------------------------------
#  all_rules <- purrr::map(0:255, \(rule){
#      ca(rule,
#         ncols = 30,
#         steps = 30) |>
#        plot()
#    }) |>
#    patchwork::wrap_plots(nrow = 32)
#  
#  ggplot2::ggsave("all-cellular-automata.png",
#         plot = all_rules,
#         width = 12,
#         height = 48)

## -----------------------------------------------------------------------------
# sample rule 30 from different random starting points
purrr::map(1:25, \(i){
    ca(30, 
       initialstate = sample(c(0, 1), size = 10, replace = TRUE), 
       steps = 10) |> 
      plot(title = NULL)
  }) |> 
  patchwork::wrap_plots()

## -----------------------------------------------------------------------------
ca(193, steps = 50) |> plot(time_flow = "up", circle = TRUE)

## -----------------------------------------------------------------------------
ca(193, ncols = 25, steps = 100) |> plot(circle = TRUE, animate = TRUE)

