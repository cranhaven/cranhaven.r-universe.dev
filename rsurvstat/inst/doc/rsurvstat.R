## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.height = 4,
  fig.width = 7
)

library(rsurvstat)
library(sf)


## -----------------------------------------------------------------------------
entero = get_timeseries(
  diseases$Enterovirus, 
  "Count",
  age_group = age_groups$zero_fifteen)

ggplot2::ggplot(
    entero, 
    ggplot2::aes(x=date, y=count, colour = age_name)
  ) + 
  ggplot2::geom_line()
  

## -----------------------------------------------------------------------------
entero2 = entero %>% 
  fit_population() %>% 
  dplyr::mutate(weekly_incidence_per_100K = count/population*100000)

ggplot2::ggplot(
    entero2,
    ggplot2::aes(x=date, y=weekly_incidence_per_100K, colour=age_name)
  ) + 
  ggplot2::geom_line()


## -----------------------------------------------------------------------------

covid_by_nuts = get_timeseries(
  disease = diseases$`COVID-19`, 
  measure="Incidence", 
  years = 2020:2022,
  geography = "nuts"
)

ggplot2::ggplot(
    covid_by_nuts,
    ggplot2::aes(x=date, y=incidence, colour=geo_name)
  ) + 
  ggplot2::geom_line() + 
  ggplot2::guides(colour = ggplot2::guide_none())


## -----------------------------------------------------------------------------

# Pick a set of dates around the peak:
peak_date = covid_by_nuts$date[covid_by_nuts$incidence == max(covid_by_nuts$incidence)]
peak_date = peak_date+c(-14,-7, 0)
peak = covid_by_nuts %>% dplyr::filter(date %in% peak_date)

ggplot2::ggplot(
    NutsKey71Map %>% dplyr::inner_join(peak, by=c("Id" = "geo_code")),
    ggplot2::aes(fill = incidence)
  )+
  ggplot2::geom_sf()+
  ggplot2::facet_wrap(~date,nrow = 1)+
  ggplot2::scale_fill_viridis_c()


## -----------------------------------------------------------------------------

pneumo_by_serotype = get_snapshot(
  disease = diseases$`Pneumococcus (IfSG`, 
  disease_subtype = TRUE, 
  season = 2024,
  season_start = 27
)

pneumo_by_serotype = pneumo_by_serotype %>% 
  # remove non typed and unknowns:
  dplyr::filter(startsWith(disease_subtype_name,"Sero")) %>%
  # removed serotypes with no detected cases:
  dplyr::filter(!is.na(count)) 

ggplot2::ggplot(
  pneumo_by_serotype,
  ggplot2::aes(x=disease_subtype_name, y=count)
)+
  ggplot2::geom_bar(stat="identity")+
  ggplot2::theme(axis.text.x = ggplot2::element_text(size = 8, angle = 90,hjust=1,vjust=0.5))



