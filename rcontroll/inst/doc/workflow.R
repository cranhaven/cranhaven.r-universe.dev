## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.height = 6,
  fig.width = 6
)

## ----message=TRUE, warning=TRUE, include=FALSE--------------------------------
if (Sys.info()[["sysname"]] == "Darwin") {
  knitr::opts_chunk$set(
    eval = FALSE
  )
}

## ----libs---------------------------------------------------------------------
suppressMessages(library(dplyr))
library(tidyr)
library(ggplot2)
suppressMessages(library(rcontroll))

## ----data---------------------------------------------------------------------
data("TROLLv3_species")
data("TROLLv3_climatedaytime12")
data("TROLLv3_daytimevar")
data("TROLLv3_output")
stack_parameters <- generate_parameters(
  cols = 100, rows = 100,
  iterperyear = 12, nbiter = 12 * 1
) %>%
  mutate(simulation = list(c("seed50000", "seed500"))) %>%
  unnest(simulation)
stack_parameters[62, 2] <- 500 # Cseedrain

## ----genPar-------------------------------------------------------------------
generate_parameters(cols = 250, rows = 250, nbiter = 12 * 1) %>%
  head() %>%
  knitr::kable()

## ----troll--------------------------------------------------------------------
sim <- troll(
  name = "test",
  global = generate_parameters(
    cols = 100, rows = 100,
    iterperyear = 12, nbiter = 12 * 1
  ),
  species = TROLLv3_species,
  climate = TROLLv3_climatedaytime12,
  daily = TROLLv3_daytimevar,
  verbose = TRUE
)

## -----------------------------------------------------------------------------
sim

## ----fullsimG1, fig.width=8---------------------------------------------------
rcontroll::autoplot(sim,
  what = "temporal",
  species = c(
    "Cecropia_obtusa", "Dicorynia_guianensis",
    "Eperua_grandiflora", "Vouacapoua_americana"
  )
) +
  theme(legend.position = "bottom")

## ----fullsimG2, eval=FALSE----------------------------------------------------
#  rcontroll::autoplot(sim, what = "spatial")

## ----fullsimG3----------------------------------------------------------------
rcontroll::autoplot(sim,
  what = "distribution",
  variables = c("dbh", "height", "LA", "LMA")
)

## ----fullstack----------------------------------------------------------------
sim_stack <- stack(
  name = "teststack",
  simulations = c("seed50000", "seed500"),
  global = stack_parameters,
  species = TROLLv3_species,
  climate = TROLLv3_climatedaytime12,
  daily = TROLLv3_daytimevar,
  verbose = FALSE,
  cores = 2,
  thin = c(1, 5, 10)
)

## -----------------------------------------------------------------------------
sim_stack

## ----fullstackG1--------------------------------------------------------------
rcontroll::autoplot(sim_stack,
  what = "temporal",
  variables = c("ba", "agb")
)

## ----fullstackG2, eval=FALSE--------------------------------------------------
#  rcontroll::autoplot(sim_stack, what = "spatial")

## -----------------------------------------------------------------------------
rcontroll::autoplot(TROLLv3_output, what = "temporal")

## ----eval=FALSE---------------------------------------------------------------
#  rcontroll::autoplot(TROLLv3_output, what = "spatial", variables = "age")

## -----------------------------------------------------------------------------
sim <- troll(
  name = "test",
  global = update_parameters(TROLLv3_output, nbiter = 12 * 1),
  species = TROLLv3_output@inputs$species,
  climate = TROLLv3_output@inputs$climate,
  daily = TROLLv3_output@inputs$daily,
  forest = get_forest(TROLLv3_output),
  verbose = FALSE
)

## -----------------------------------------------------------------------------
sim@ecosystem$iter <- sim@ecosystem$iter + max(TROLLv3_output@ecosystem$iter)
list(
  "TROLLv3_output" = TROLLv3_output@ecosystem,
  "sim" = sim@ecosystem
) %>%
  bind_rows(.id = "simulation") %>%
  ggplot(aes(iter / 12, agb, col = simulation)) +
  geom_point() +
  theme_bw() +
  xlab("Time (years)") +
  ylab(expression(Abovergound ~ biomass ~ (AGB ~ Kg ~ ha^{
    -1
  })))

## ----eval=F-------------------------------------------------------------------
#  gifs <- autogif(
#    name = "dynamic",
#    variables = "height_ct",
#    global = update_parameters(TROLLv3_output,
#      nbiter = 12 * 100,
#      extent_visual = 100
#    ),
#    species = TROLLv3_output@inputs$species,
#    climate = TROLLv3_output@inputs$climate,
#    daily = TROLLv3_output@inputs$daily,
#    forest = get_forest(TROLLv3_output),
#    verbose = FALSE
#  )
#  gifs$height_ct

