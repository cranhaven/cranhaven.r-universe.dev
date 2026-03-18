## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  fig.height=5, fig.width=8, 
  message=FALSE, warning=FALSE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------


## -----------------------------------------------------------------------------
library(sfo)
library(dplyr)
library(plotly)

d <- sfo_stats

head(d)

## -----------------------------------------------------------------------------
d$date <- as.Date(paste(substr(d$activity_period, 1,4), 
                        substr(d$activity_period, 5,6), 
                        "01", sep ="/"))

## -----------------------------------------------------------------------------
str(d)

## -----------------------------------------------------------------------------
d %>% 
  group_by(date) %>%
  summarise(landing_count = sum(landing_count)) %>%
  plot_ly(x = ~ date, y = ~ landing_count,
          type = "scatter", mode = "lines") %>% 
  layout(title = "Montly Landing in SFO Airport",
         yaxis = list(title = "Number of Landing"),
         xaxis = list(title = "Source: San Francisco data portal (DataSF)"))


## -----------------------------------------------------------------------------
d %>% 
  group_by(date, geo_region) %>%
  summarise(landing_count = sum(landing_count)) %>%
  as.data.frame() %>%
plot_ly(x = ~ date, 
        y = ~ landing_count,
        type = 'scatter', 
        mode = 'none', 
        stackgroup = 'one', 
        groupnorm = 'percent', fillcolor = ~ geo_region) %>%
  layout(title = "Dist. of Landing at SFO by Region",
         yaxis = list(title = "Percentage",
                      ticksuffix = "%"))

## -----------------------------------------------------------------------------
d %>% 
      filter(activity_period >= 201901 & activity_period < 202001,
             aircraft_manufacturer != "") %>%
      group_by(aircraft_manufacturer) %>%
      summarise(total_landing = sum(landing_count),
                `.groups` = "drop") %>%
      arrange(-total_landing) %>%
      plot_ly(labels = ~ aircraft_manufacturer,
              values = ~ total_landing) %>%
      add_pie(hole = 0.6) %>%
      layout(title = "Landing Distribution by Aircraft Manufacturer During 2019")

## -----------------------------------------------------------------------------
d %>% 
      filter(activity_period >= 201901 & activity_period < 202001,
             aircraft_manufacturer != "") %>%
      group_by(aircraft_manufacturer, aircraft_body_type) %>%
      summarise(total_landing = sum(landing_count),
                `.groups` = "drop") %>%
      arrange(-total_landing)

## -----------------------------------------------------------------------------
d %>%
  filter(activity_period >= 201901 & activity_period < 202001,
             aircraft_manufacturer != "") %>%
  group_by(geo_region, landing_aircraft_type, 
           aircraft_manufacturer, aircraft_model, 
           aircraft_body_type) %>%
  summarise(total_landing = sum(landing_count),
            groups = "drop") %>%
  sankey_ly(cat_cols = c("geo_region", 
                         "landing_aircraft_type", 
                         "aircraft_manufacturer",
                         "aircraft_model",
                         "aircraft_body_type"),
            num_col = "total_landing",
            title = "SFO Landing Summary by Geo Region and Aircraft Type During 2019")  

