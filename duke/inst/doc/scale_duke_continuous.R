## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 8,
  fig.asp = .618
)

## ----setup, warning = FALSE---------------------------------------------------
library(duke)
library(palmerpenguins)
library(ggplot2)

## ----scatterplot, warning = FALSE---------------------------------------------
plot <- ggplot2::ggplot(palmerpenguins::penguins, ggplot2::aes(bill_length_mm, flipper_length_mm)) +
  ggplot2::geom_point(ggplot2::aes(colour = bill_length_mm)) +
  ggplot2::labs(title = "Bill Length vs. Flipper Length", caption = "There are three different species of penguins.", x = "Bill Length (mm)", y = "Flipper Length (mm)")

plot +
  scale_duke_continuous()

