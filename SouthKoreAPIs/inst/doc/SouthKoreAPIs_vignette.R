## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(SouthKoreAPIs)
library(dplyr)
library(ggplot2)

## ----southkorea-gdp,echo = TRUE,message = FALSE,warning = FALSE,results = 'markup'----


southkorea_gdp <- head(get_southkorea_gdp())

print(southkorea_gdp)


## ----southkorea-life-expectancy,echo = TRUE,message = FALSE,warning = FALSE,results = 'markup'----

southkorea_life_expectancy <- head(get_southkorea_life_expectancy())

print(southkorea_life_expectancy)


## ----southkorea-population,echo = TRUE,message = FALSE,warning = FALSE,results = 'markup'----

southkorea_population <- head(get_southkorea_population())

print(southkorea_population)


## ----southkorea-births-plot, message=FALSE, warning=FALSE, fig.width=7, fig.height=5----


# Filter data for a specific region, e.g., "Seoul"
births_seoul <- SouthKoreaBirths_tbl_df %>%
  filter(region == "Seoul") %>%
  group_by(time, age) %>%
  summarise(total_births = sum(births), .groups = "drop")

# Plot the evolution of births by age group in Seoul
ggplot(births_seoul, aes(x = time, y = total_births, color = age)) +
  geom_line(size = 1) +
  labs(
    title = "Evolution of Births in Seoul by Age Group",
    x = "Year",
    y = "Total Births",
    color = "Age Group"
  ) +
  theme_minimal()


