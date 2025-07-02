## ----knitr, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----load libraries-----------------------------------------------------------
library(ale)
library(dplyr)

## ----print var_cars-----------------------------------------------------------
print(var_cars)

## ----var_cars summary---------------------------------------------------------
summary(var_cars)

## ----gam_cars-----------------------------------------------------------------
gam_cars <- mgcv::gam(
  mpg ~ cyl + disp + hp + drat + wt + s(qsec) +
    vs + am + gear + carb + country,
  data = var_cars
)
summary(gam_cars)

## ----ale_cars, fig.width=7, fig.height=14-------------------------------------
ale_cars <- ALE(gam_cars)

# Print all plots
plot(ale_cars) |> 
  print(ncol = 2)

## ----ale_cars_2D, fig.width=7, fig.height=28----------------------------------
ale_cars_2D <- ALE(
  gam_cars,
  x_cols = list(d2 = TRUE)
)

# Print plots
plot(ale_cars_2D) |> 
  print(
    ncol = 2, 
    # By default, at most 20 plots are printed. Set max_print to increase this limit
    max_print = 100
  )

## ----cars_full, fig.width=7, fig.height=14------------------------------------
mb <- ModelBoot(
  gam_cars,
  boot_it = 10,  # 100 by default but reduced here for a faster demonstration
  seed = 2  # workaround to avoid random error on such a small dataset
)

plot(mb) |> 
  print(ncol = 2)

