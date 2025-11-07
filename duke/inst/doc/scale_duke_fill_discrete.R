## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----penguins, warning = FALSE------------------------------------------------
library(palmerpenguins)
library(duke)
library(ggplot2)
library(ggmosaic)

## ----bar plot, warning = FALSE------------------------------------------------
plot <- ggplot(palmerpenguins::penguins, aes(x = species, fill = species)) +
  geom_bar() +
  labs(title = "Distribution of Species", caption = "(Colors used) \n Duke Royal Blue, Duke Navy Blue, Copper", x = "Species", y = "Count")

plot +
  scale_duke_fill_discrete()

## ----histogram plot, warning = FALSE------------------------------------------
plot2 <- ggplot(palmerpenguins::penguins, aes(x = bill_length_mm)) +
  geom_histogram(aes(fill = species)) +
  labs(title = "Distribution of Bill Length", caption = "(Colors used) \n Duke Royal Blue, Duke Navy Blue, Copper", x = "Bill Length (mm)", y = "Count")

plot2 +
  scale_duke_fill_discrete()

