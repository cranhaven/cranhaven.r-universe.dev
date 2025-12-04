## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
library(gghalves)
library(dplyr)

## -----------------------------------------------------------------------------
ggplot(iris, aes(x = Species, y = Sepal.Width)) + 
  geom_half_point()

## -----------------------------------------------------------------------------
ggplot(iris, aes(x = Species, y = Sepal.Width)) +
  geom_half_point(transformation_params = list(height = 0, width = 0.001, seed = 1))

## -----------------------------------------------------------------------------
ggplot(iris, aes(x = Species, y = Sepal.Width)) +
  geom_half_point(transformation = PositionIdentity)

## -----------------------------------------------------------------------------
ggplot(iris, aes(y = Sepal.Width)) +
  geom_half_boxplot() +
  geom_half_point_panel(aes(x = 0.5, color = Species), range_scale = .5)

## -----------------------------------------------------------------------------
ggplot(iris, aes(x = Species, y = Sepal.Width)) +
  geom_half_boxplot()

## -----------------------------------------------------------------------------
ggplot(iris, aes(x = Species, y = Sepal.Width)) +
  geom_half_boxplot(side = "r", center = TRUE, errorbar.draw = FALSE)

## -----------------------------------------------------------------------------
ggplot(iris, aes(x = Species, y = Sepal.Width)) +
  geom_half_violin()

## -----------------------------------------------------------------------------
ggplot() +
  geom_half_violin(
    data = ToothGrowth, 
    aes(x = as.factor(dose), y = len, split = supp, fill = supp),
    position = "identity"
  )

## -----------------------------------------------------------------------------
ggplot(iris, aes(x = Species, y = Sepal.Width)) +
  geom_half_violin() + 
  geom_dotplot(binaxis = "y", method="histodot", stackdir="up")

## -----------------------------------------------------------------------------
df <- data.frame(score = rgamma(150, 4, 1), 
                 gender = sample(c("M", "F"), 150, replace = TRUE), 
                genotype = factor(sample(1:3, 150, replace = TRUE)))

## -----------------------------------------------------------------------------
ggplot(df, aes(x = genotype, y = score, fill = gender)) +
  geom_half_violin() + 
  geom_dotplot(binaxis = "y", method="histodot", stackdir="up", position = PositionDodge)

## -----------------------------------------------------------------------------
ggplot(df, aes(x = genotype, y = score, fill = gender)) +
  geom_half_violin() + 
  geom_half_dotplot(method="histodot", stackdir="up")

## ---- eval=FALSE--------------------------------------------------------------
#  ggplot(iris, aes(x = Species, y = Sepal.Width)) +
#    geom_half_boxplot() +
#    geom_beeswarm(beeswarmArgs = list(side = 1))

## ---- eval=FALSE--------------------------------------------------------------
#  ggplot() +
#  
#    geom_half_boxplot(
#      data = iris %>% filter(Species=="setosa"),
#      aes(x = Species, y = Sepal.Length, fill = Species), outlier.color = NA) +
#  
#    ggbeeswarm::geom_beeswarm(
#      data = iris %>% filter(Species=="setosa"),
#      aes(x = Species, y = Sepal.Length, fill = Species, color = Species), beeswarmArgs=list(side=+1)
#    ) +
#  
#    geom_half_violin(
#      data = iris %>% filter(Species=="versicolor"),
#      aes(x = Species, y = Sepal.Length, fill = Species), side="r") +
#  
#    geom_half_dotplot(
#      data = iris %>% filter(Species=="versicolor"),
#      aes(x = Species, y = Sepal.Length, fill = Species), method="histodot", stackdir="down") +
#  
#    geom_half_boxplot(
#      data = iris %>% filter(Species=="virginica"),
#      aes(x = Species, y = Sepal.Length, fill = Species), side = "r", errorbar.draw = TRUE,
#      outlier.color = NA) +
#  
#    geom_half_point(
#      data = iris %>% filter(Species=="virginica"),
#      aes(x = Species, y = Sepal.Length, fill = Species, color = Species), side = "l") +
#  
#    scale_fill_manual(values = c("setosa" = "#cba1d2", "versicolor"="#7067CF","virginica"="#B7C0EE")) +
#    scale_color_manual(values = c("setosa" = "#cba1d2", "versicolor"="#7067CF","virginica"="#B7C0EE")) +
#    theme(legend.position = "none")

