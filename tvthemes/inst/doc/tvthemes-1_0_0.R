## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(tvthemes)
library(ggplot2)

## ---- eval=FALSE--------------------------------------------------------------
#  ## Previously:
#  scales::show_col(tvthemes:::lannister_palette)
#  scales::show_col(tvthemes:::brooklyn99_dark_palette)
#  
#  ## Now:
#  scales::show_col(tvthemes:::westeros_palette$Lannister)
#  scales::show_col(tvthemes:::brooklyn99_palette$Dark)

## ----eval=FALSE---------------------------------------------------------------
#  ## Plot not shown
#  ggplot(mpg, aes(displ)) +
#    geom_histogram(aes(fill = class), col = "black", size = 0.1) +
#    scale_fill_westeros(palette = "Stannis", n = 7, reverse = TRUE)

## ----eval=FALSE---------------------------------------------------------------
#  ## Plot not shown
#  ggplot(mpg, aes(displ)) +
#    geom_histogram(aes(fill = class), col = "black", size = 0.1) +
#    scale_fill_kimPossible(n = 5, reverse = FALSE) +
#    theme_hildaDay(ticks = TRUE)

