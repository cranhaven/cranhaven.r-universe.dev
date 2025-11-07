## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 8,
  fig.asp = 0.618
)

## ----penguins, warning = FALSE------------------------------------------------
library(palmerpenguins)
library(duke)
library(ggplot2)
library(ggmosaic)

## ----scatter plot, warning = FALSE--------------------------------------------
plot <- ggplot(penguins, aes(x = bill_length_mm, y = bill_depth_mm)) +
  geom_point(aes(color = species)) +
  labs(
    title = "Bill Length vs. Bill Depth", 
    caption = "(Colors used) \n Duke Royal Blue, Duke Navy Blue, Copper", 
    x = "Bill Length (mm)", 
    y = "Bill Depth (mm)"
  )

plot +
  scale_duke_color_discrete()

## ----jitter plot, warning = FALSE---------------------------------------------
plot <- ggplot(penguins, aes(x = island, y = bill_depth_mm)) +
  geom_jitter(aes(color = species)) +
  labs(
    title = "Bill Length vs. Bill Depth", 
    caption = "(Colors used) \n Duke Royal Blue, Duke Navy Blue, Copper", 
    x = "Bill Length (mm)", 
    y = "Bill Depth (mm)"
  )

plot +
  scale_duke_color_discrete()

