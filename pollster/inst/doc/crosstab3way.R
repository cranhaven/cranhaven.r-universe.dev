## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(pollster)
library(dplyr)
library(knitr)
library(ggplot2)

## -----------------------------------------------------------------------------
illinois %>%
  # filter for recent years & limited ages
  filter(year > 2009,
         age > 39) %>%
  crosstab_3way(x = sex, y = educ6, z = maritalstatus, weight = weight,
                remove = c("widow/divorced/sep"),
                n = FALSE) %>%
  kable(digits = 0, caption = "Educational attainment by sex and marital status among Illinois residents ages 35+",
        format = "html")

## ---- fig.width=6, fig.height=4-----------------------------------------------
illinois %>%
  # filter for recent years & limited ages
  filter(year > 2009,
         age > 34) %>%
  crosstab_3way(x = sex, y = educ6, z = maritalstatus, weight = weight,
                remove = c("widow/divorced/sep"), 
                format = "long") %>%
  ggplot(aes(educ6, pct, fill = maritalstatus)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  facet_wrap(facets = vars(sex)) +
  labs("Educational attainment by sex and marital status",
       subtitle = "Illinois residents ages 40+") +
  theme(legend.position = "top")

## ---- fig.width=6, fig.height=4-----------------------------------------------
illinois %>%
  # filter for recent years & limited ages
  filter(year > 2009,
         age > 34) %>%
  moe_crosstab_3way(x = sex, y = educ6, z = maritalstatus, weight = weight,
                remove = c("widow/divorced/sep"), format = "long") %>%
  ggplot(aes(educ6, pct, fill = maritalstatus)) +
  geom_bar(stat = "identity", position = position_dodge(),
           alpha = 0.5) +
  geom_errorbar(aes(ymin = (pct - moe), ymax = (pct + moe),
                    color = maritalstatus),
                position = position_dodge()) +
  facet_wrap(facets = vars(sex)) +
  labs(title = "Educational attainment by sex and marital status",
       subtitle = "Illinois residents ages 35+",
       caption = "Current Population Survey, 2010-2018") +
  theme(legend.position = "top")

## -----------------------------------------------------------------------------
moe_wave_crosstab_3way(df = illinois, x = sex, y = educ6, z = year, weight = weight)

