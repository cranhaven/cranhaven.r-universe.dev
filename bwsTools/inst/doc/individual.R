## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup, message=FALSE-----------------------------------------------------
library(bwsTools)
library(dplyr)
library(tidyr)
library(ggplot2)

## ----run, eval=FALSE----------------------------------------------------------
#  set.seed(1839)
#  res1 <- diffscoring(vdata, "id", "block", "issue", "value")
#  res2 <- e_bayescoring(vdata, "id", "block", "issue", "value")
#  res3 <- eloscoring(vdata, "id", "block", "issue", "value")
#  res4 <- walkscoring(vdata, "id", "block", "issue", "value")
#  res5 <- prscoring(vdata, "id", "block", "issue", "value")

## ----calc_bayes, include=FALSE------------------------------------------------
res2 <- e_bayescoring(vdata, "id", "block", "issue", "value")

## ----kmeans-------------------------------------------------------------------
res2_wide <- res2 %>% 
  spread(issue, b_ebayes)

set.seed(1839)
clust <- kmeans(res2_wide[, -1], 3)

## ----show_clusters------------------------------------------------------------
res2_wide$cluster <- factor(clust$cluster)

res2_wide %>% 
  select(-id) %>% 
  group_by(cluster) %>% 
  summarise_all(mean) %>% 
  gather("issue", "mean", -cluster) %>% 
  ggplot(aes(x = cluster, y = mean)) +
  geom_bar(stat = "identity") +
  facet_wrap(~ issue)

