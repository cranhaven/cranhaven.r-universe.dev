## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(bwsTools)
library(dplyr)
library(tidyr)
library(ggplot2)

## ----make_data, message=FALSE-------------------------------------------------
dat <- vdata %>% 
  group_by(issue) %>% 
  summarise(
    totl = n(),
    best = sum(value == 1),
    wrst = sum(value == -1)
  )

## ----show_dat-----------------------------------------------------------------
dat

## ----ae_mnl-------------------------------------------------------------------
res1 <- ae_mnl(dat, "totl", "best", "wrst")

## ----show ae_mnl_res----------------------------------------------------------
res1

## -----------------------------------------------------------------------------
dat %>% 
  bind_cols(res1) %>% 
  arrange(b) %>% 
  mutate(issue = factor(issue, issue)) %>% 
  ggplot(aes(x = issue, y = b)) +
  geom_point() +
  geom_errorbar(aes(ymin = lb, ymax = ub), width = 0) +
  coord_flip()

## ----elo, eval=FALSE----------------------------------------------------------
#  set.seed(1839)
#  res2 <- elo(vdata, "id", "block", "issue", "value")

## ----load_up_res2, include=FALSE----------------------------------------------
res2 <- structure(list(item = c("abortion", "biasmedia", "corruption", 
"crime", "drugs", "economy", "education", "foreignaffairs", "guns", 
"healthcare", "natsecurity", "race", "taxes"), elo = c(983.604707872469, 
775.029578108074, 987.095165229659, 1005.70809857833, 908.116241829412, 
1166.78863775417, 1065.4369480057, 858.26118119395, 1022.80855081941, 
1194.37125023744, 1043.98840666976, 974.595472655393, 1013.59357525509
)), row.names = c(NA, -13L), class = c("tbl_df", "tbl", "data.frame"
))

## ----show_elo-----------------------------------------------------------------
res2

## ----correl-------------------------------------------------------------------
cor(res1$b, res2$elo)
plot(res1$b, res2$elo)

