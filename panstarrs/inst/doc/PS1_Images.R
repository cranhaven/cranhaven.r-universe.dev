## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(panstarrs)
library(magick)

## -----------------------------------------------------------------------------
coords_antennae <- ps1_mast_resolve('Antennae')

ps1_image_color(
  coords_antennae$ra, 
  coords_antennae$dec, 
  format = 'png', 
  size = 600
) |> 
  magick::image_read() |> 
  magick::image_resize("400x400")

