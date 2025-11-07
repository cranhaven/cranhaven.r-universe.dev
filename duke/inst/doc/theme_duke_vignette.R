## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 8,
  fig.asp = 0.618
)

## ----penguins, warning = FALSE------------------------------------------------
library(palmerpenguins)
library(ggplot2)
library(duke)

## ----scatter, warning = FALSE-------------------------------------------------
p <- ggplot(penguins, aes(bill_length_mm, flipper_length_mm)) +
  geom_point(aes(colour = species)) +
  labs(
    title = "Bill Length vs. Flipper Length", 
    caption = "There are three different species of penguins.", 
    x = "Bill Length (mm)", 
    y = "Flipper Length (mm)"
  )

p +
  theme_duke()

## ----bar, warning = FALSE-----------------------------------------------------
p <- ggplot(penguins, aes(species)) +
  geom_bar(aes(fill = species)) +
  labs(
    title = "Species Count", 
    caption = "There are three different species of penguins.", 
    x = "Species", 
    y = "Count"
  )

p +
  theme_duke()

## ----hist, warning = FALSE----------------------------------------------------
p <- ggplot(penguins, aes(body_mass_g)) +
  geom_histogram(aes(fill = species)) +
  labs(
    title = "Distribution of Penguin Body Mass", 
    caption = "There are three different species of penguins.", 
    x = "Body Mass (g)", 
    y = "Count"
  )

p +
  theme_duke()

## ----box----------------------------------------------------------------------
p <- ggplot(penguins, aes(sex, body_mass_g)) +
  geom_boxplot() +
  labs(
    title = "Comparison of Body Mass By Sex", 
    x = "Sex", 
    y = "Body Mass (g)"
  )

p +
  theme_duke()

## ----density, warning = FALSE-------------------------------------------------
p <- ggplot(penguins, aes(bill_depth_mm)) +
  geom_density() +
  labs(
    title = "Density of Penguin Bill Depth", 
    x = "Bill Depth (mm)", 
    y = "Densiy"
  )

p +
  theme_duke()

## ----jitter, warning = FALSE--------------------------------------------------
p <- ggplot(penguins, aes(year, body_mass_g)) +
  geom_jitter() +
  labs(
    title = "Comparison of Body Mass By Year", 
    x = "Year", 
    y = "Body Mass (g)"
  )

p +
  theme_duke()

