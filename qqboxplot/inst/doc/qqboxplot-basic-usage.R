## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>", 
  fig.width = 7, 
  fig.height = 4.8, 
  fig.align = "center"
)

## ----setup, message=FALSE-----------------------------------------------------
library(dplyr)
library(ggplot2)
library(qqboxplot)

## -----------------------------------------------------------------------------
simulated_data %>%
         ggplot(aes(factor(group, levels=c("normal, mean=2", "t distribution, df=32", "t distribution, df=16", "t distribution, df=8", "t distribution, df=4")), y=y)) +
         geom_qqboxplot(notch=TRUE, varwidth = TRUE, reference_dist="norm") +
         xlab("reference: normal distribution") +
         ylab(NULL) +
         guides(color=FALSE) +
         theme(axis.text.x = element_text(angle = 23, size = 15), axis.title.y = element_text(size=15),
               axis.title.x = element_text(size=15),
               panel.border = element_blank(), panel.background = element_rect(fill="white"),
               panel.grid = element_line(colour = "grey70"))


## ---- eval=FALSE--------------------------------------------------------------
#  tibble(y=c(rnorm(1000, mean=2), rt(1000, 16), rt(500, 4),
#                     rt(1000, 8), rt(1000, 32)),
#          group=c(rep("normal, mean=2", 1000),
#                  rep("t distribution, df=16", 1000),
#                  rep("t distribution, df=4", 500),
#                  rep("t distribution, df=8", 1000),
#                  rep("t distribution, df=32", 1000)))

## -----------------------------------------------------------------------------
simulated_data %>%
  ggplot(aes(factor(group, levels=c("normal, mean=2", "t distribution, df=32", "t distribution, df=16", "t distribution, df=8", "t distribution, df=4")), y=y)) +
  geom_qqboxplot(notch=TRUE, varwidth = TRUE, compdata=comparison_dataset) +
  xlab("reference: simulated normal dataset") +
  ylab(NULL) +
  theme(axis.text.x = element_text(angle = 23, size = 15), axis.title.y = element_text(size=15),
        axis.title.x = element_text(size=15),
        panel.border = element_blank(), panel.background = element_rect(fill="white"),
        panel.grid = element_line(colour = "grey70"))

## ---- eval=FALSE--------------------------------------------------------------
#  rnorm(1000, 5)

