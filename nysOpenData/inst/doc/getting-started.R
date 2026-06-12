## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
knitr::opts_chunk$set(warning = FALSE, message = FALSE)
library(nysOpenData)
library(ggplot2)
library(dplyr)

## ----nys-list-datasets--------------------------------------------------------
nys_list_datasets() |> head()

## ----nys-lottery1-pull--------------------------------------------------------
nys_cash_for_life_uid <- nys_pull_dataset(
  dataset = "kwxv-fwze", limit = 2)

nys_cash_for_life_key <- nys_pull_dataset(
  dataset = "lottery_cash_4_life_winning_numbers_beginning_2014", limit = 2)

## ----filter-cash-ball4--------------------------------------------------------

lottery_04 <- nys_pull_dataset(dataset = "kwxv-fwze",limit = 2, filters = list(cash_ball = "04"))
lottery_04

# Checking to see the filtering worked
lottery_04 |>
  distinct(cash_ball)

## -----------------------------------------------------------------------------
lottery_01_02 <- nys_pull_dataset(dataset = "kwxv-fwze",limit = 2, filters = list(cash_ball = c("01","02")))
lottery_01_02

## ----compaint-type-graph, fig.alt="Horizontal bar chart showing the frequency of Cash Ball numbers in New York Cash 4 Life lottery winning tickets. Each bar represents how often a specific Cash Ball number appears in the sample.", fig.cap="Frequency of Cash Ball numbers among recent New York Cash 4 Life lottery winning tickets. Bars are ordered from least to most frequent to highlight the distribution of outcomes.", fig.height=5, fig.width=7----
# Visualizing the distribution, ordered by frequency

lottery <- nys_pull_dataset(dataset = "kwxv-fwze",limit = 50)

lottery |>
  count(cash_ball) |>
  ggplot(aes(
    x = n,
    y = reorder(cash_ball, n)
  )) +
  geom_col(fill = "steelblue") +
  theme_minimal() +
  labs(
    title = "Top 50 Cash Ball Numbers in Winning Lottery Tickets",
    x = "Number of Winners",
    y = "Cash Ball"
  )

