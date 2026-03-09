## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

can_plot <- requireNamespace("ggplot2", quietly = TRUE) && 
  requireNamespace("scales", quietly = TRUE)

has_prettyunits <- requireNamespace("prettyunits", quietly = TRUE)

## ----search-------------------------------------------------------------------
library(nisrarr)

x <- nisra_search()
head(x)

## ----search-keyword-----------------------------------------------------------
nisra_search(keyword = "employ")

nisra_search(variables = "Free School Meal Entitlement")

## ----read-data, eval=can_plot-------------------------------------------------
mye <- nisra_read_dataset("MYE01T04")
head(mye)

library(dplyr)
library(ggplot2)

mye <- mye |> 
  filter(
    `Broad age band (4 cat)` == "Age 65+",
    Sex %in% c("Females", "Males")
  ) |> 
  mutate(Year = as.numeric(Year))


ggplot(mye, aes(Year, value, colour = Sex)) +
  geom_line() +
  scale_y_continuous(labels = scales::label_comma()) +
  facet_wrap(
    vars(`Local Government District`), 
    scales = "free_y",
    labeller = label_wrap_gen(width = 18)
  ) +
  labs(
    title = "Population aged 65+ by sex and local government district, 2001 to 2022",
    x = NULL, 
    y = NULL, 
    colour = NULL
  ) +
  theme(legend.position = "top")

## ----meta---------------------------------------------------------------------
get_metadata(mye)

## ----meta-field, eval=has_prettyunits-----------------------------------------
updated <- get_metadata_field(mye, "updated")

updated |> 
  lubridate::ymd_hms() |> 
  prettyunits::time_ago()

