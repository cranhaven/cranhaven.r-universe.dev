## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 7,
  fig.height = 5
)
options(dplyr.summarise.inform = FALSE)

## ----setup, message = FALSE---------------------------------------------------
library(ffsimulator)
library(ggplot2)

## ----eval = FALSE-------------------------------------------------------------
#  foureight_conn <- mfl_connect(2021, 22627)
#  
#  foureight_sim <- ff_simulate(conn = foureight_conn, n_seasons = 10, n_weeks = 14)
#  foureight_sim

## ----echo = FALSE, message = FALSE--------------------------------------------
foureight_sim <- readRDS(system.file("cache/foureight_sim.rds", package = "ffsimulator"))
foureight_sim

## -----------------------------------------------------------------------------
plot(foureight_sim) # defaults to type = "wins"
plot(foureight_sim, type = "rank")
plot(foureight_sim, type = "points")

## -----------------------------------------------------------------------------
foureight_sim$summary_simulation

## -----------------------------------------------------------------------------
foureight_sim$summary_season

## -----------------------------------------------------------------------------
foureight_sim$summary_week

## -----------------------------------------------------------------------------
foureight_sim$roster_scores

## -----------------------------------------------------------------------------
foureight_sim$league_info

## ----eval = FALSE-------------------------------------------------------------
#  foureight_conn <- mfl_connect(2021, 22627)
#  
#  foureight_sim_week <- ff_simulate_week(conn = foureight_conn, n = 10)
#  
#  foureight_sim_week

## ----echo = FALSE-------------------------------------------------------------
foureight_sim_week <- .ffs_cache("foureight_sim_week.rds")
foureight_sim_week

## -----------------------------------------------------------------------------
plot(foureight_sim_week,type = "luck")

## -----------------------------------------------------------------------------
plot(foureight_sim_week, type = "points")

