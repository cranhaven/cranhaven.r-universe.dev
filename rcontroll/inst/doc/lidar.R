## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.height = 6,
  fig.width = 6
)

## -----------------------------------------------------------------------------
suppressMessages(library(rcontroll))
data("TROLLv3_output")

## -----------------------------------------------------------------------------
sim <- troll(
  name = "test",
  global = update_parameters(TROLLv3_output, nbiter = 12 * 1),
  species = TROLLv3_output@inputs$species,
  climate = TROLLv3_output@inputs$climate,
  daily = TROLLv3_output@inputs$daily,
  forest = get_forest(TROLLv3_output),
  lidar = generate_lidar(mean_beam_pc = 10, iter_pointcloud_generation = 11),
  verbose = FALSE
)

## -----------------------------------------------------------------------------
sim

## -----------------------------------------------------------------------------
sim@las

## ----eval=FALSE---------------------------------------------------------------
#  lidR::plot(sim@las[[1]])

## -----------------------------------------------------------------------------
get_chm(sim)

## -----------------------------------------------------------------------------
rcontroll::autoplot(sim, what = "lidar")

## -----------------------------------------------------------------------------
sim_stack <- stack(
  name = "teststack",
  simulations = c("10pc", "20pc"),
  global = update_parameters(TROLLv3_output, nbiter = 12 * 1),
  species = TROLLv3_output@inputs$species,
  climate = TROLLv3_output@inputs$climate,
  daily = TROLLv3_output@inputs$daily,
  forest = get_forest(TROLLv3_output),
  lidar = list(
    "10pc" = generate_lidar(
      mean_beam_pc = 10,
      iter_pointcloud_generation = 11
    ),
    "20pc" = generate_lidar(
      mean_beam_pc = 10,
      iter_pointcloud_generation = 11
    )
  ) %>% dplyr::bind_rows(.id = "simulation"),
  verbose = FALSE,
  cores = 2,
  thin = c(1, 5, 10)
)

## -----------------------------------------------------------------------------
sim_stack

## -----------------------------------------------------------------------------
sim_stack@las

## ----eval=FALSE---------------------------------------------------------------
#  lidR::plot(sim_stack@las[[2]])

## -----------------------------------------------------------------------------
get_chm(sim_stack)

## -----------------------------------------------------------------------------
rcontroll::autoplot(sim_stack, what = "lidar")

