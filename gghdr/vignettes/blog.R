## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)
library(tufte)
library(ggpubr)
library(gghdr)

## ----mpgBox1, fig.width = 5, fig.height = 4,echo=FALSE------------------------
p1 <- ggplot(data = mpg, 
       aes(x = hwy, fill = as.factor(cyl))) +
  facet_grid(as.factor(cyl)~.) + 
  geom_histogram(bins = 50) + theme(legend.position = "bottom") + ggtitle("a")

p2 <- ggplot(data = mpg, 
       # make sure to change x to y from geom_density to geom_hdr_boxplot
       aes(y = hwy, fill = as.factor(cyl))) + 
  geom_boxplot() + ggtitle("b")

p3 <- ggplot(data = mpg, 
       # make sure to change x to y from geom_density to geom_hdr_boxplot
       aes(y = hwy, fill = as.factor(cyl))) + 
  geom_hdr_boxplot()+ ggtitle("c")

p1
p2
p3

