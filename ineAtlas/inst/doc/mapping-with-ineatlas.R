## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  warning = FALSE,
  message = FALSE,
  fig.width = 8,
  fig.height = 7
)

## ----packages-----------------------------------------------------------------
library(ineAtlas)
library(mapSpain)
library(dplyr)
library(ggplot2)
library(ggtext)

## ----get-data-----------------------------------------------------------------
# Get municipality level income data
mun_data <- get_atlas(
    category = "income",
    level = "municipality"
) %>%
    filter(year == 2022)

# Preview the data
head(mun_data)

## ----get-geometries-----------------------------------------------------------
# Get municipality geometries
mun_map <- esp_get_munic_siane() %>%
    # Join with our income data
    left_join(
        mun_data,
        by = c("LAU_CODE" = "mun_code")
    )

# Preview the joined data
glimpse(mun_map)

## ----create-map, fig.width=10, fig.height=8-----------------------------------
# Create the map
ggplot(mun_map) +
    geom_sf(
        aes(fill = cut(net_income_pc,
            breaks = c(-Inf, 8000, 10000, 12000, 14000, 16000, Inf),
            labels = c("<8k", "8-10k", "10-12k", "12-14k", "14-16k", ">16k")
        )),
        color = NA
    ) +
    labs(
        title = "Income per capita across Spanish municipalities, 2022",
        caption = "@pablogguz_ | The map shows net income per capita at the municipality level. Source: Spanish Statistical Office and author's calculations."
    ) +
    scale_fill_manual(
        name = "Net income per \ncapita, 2022 (€)",
        values = c("#67001F", "#B2182B", "#D6604D", "#4393C3", "#2166AC", "#053061"),
        na.value = "grey80"
    ) +
    theme_void() +
    theme(
        text = element_text(family = "Open Sans", size = 16),
        plot.title = element_text(size = 18, margin = margin(b = 20)),
        legend.position = c(0.2, 0.5),
        plot.caption = element_textbox_simple(
            size = 12,
            color = "grey40",
            margin = margin(t = 20),
            hjust = 0,
            halign = 0,
            lineheight = 1.2
        )
    )

## ----top-10-------------------------------------------------------------------
mun_data %>%
  arrange(desc(net_income_pc)) %>%
  select(mun_name, net_income_pc) %>%
  head(10) %>%
  mutate(
    net_income_pc = round(net_income_pc, 2)
  )

## ----bottom-10----------------------------------------------------------------
mun_data %>%
  arrange(net_income_pc) %>%
  select(mun_name, net_income_pc) %>%
  head(10) %>%
  mutate(
    net_income_pc = round(net_income_pc, 2)
  )

