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
crosstab(df = illinois, x = sex, y = educ6, weight = weight) %>%
  kable()

## -----------------------------------------------------------------------------
crosstab(df = illinois, x = sex, y = educ6, weight = weight, format = "long")

## -----------------------------------------------------------------------------
# cell percentages
crosstab(df = illinois, x = sex, y = educ6, weight = weight, pct_type = "cell")

# column percentages
crosstab(df = illinois, x = sex, y = educ6, weight = weight, pct_type = "column")

## ---- fig.width=5.6-----------------------------------------------------------
crosstab(df = illinois, x = sex, y = educ6, weight = weight, format = "long") %>%
  ggplot(aes(x = educ6, y = pct, fill = sex)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  labs(title = "Educational attainment of the Illinois adult population by gender")

## -----------------------------------------------------------------------------
moe_crosstab(illinois, educ6, voter, weight)

## -----------------------------------------------------------------------------
moe_crosstab(illinois, educ6, voter, weight, format = "wide")

## ---- fig.width=5-------------------------------------------------------------
illinois %>%
  filter(year == 2016) %>%
  moe_crosstab(educ6, voter, weight) %>%
  ggplot(aes(x = pct, y = educ6, xmin = (pct - moe), xmax = (pct + moe),
             color = voter)) +
  geom_pointrange(position = position_dodge(width = 0.2))

## -----------------------------------------------------------------------------
moe_wave_crosstab(df = illinois, x = year, y = rv, weight = weight)

