## ----fig.width=6, fig.height=7, message=FALSE, warning=FALSE------------------
library(dplyr)
library(ggplot2)

census_11 <- parlitools::census_11

bes_2017 <- parlitools::bes_2017

elect_results <- left_join(census_11, bes_2017)

degree_plot <- ggplot(elect_results, aes(y=lab_17, x=degree)) +
  geom_point(alpha=0.75) +
  geom_smooth(size=1.75, colour = "#DC241F") +
  ylab("Share of Votes Cast for Labour") + 
  xlab("Percentage of Population with a University Degree")

degree_plot


