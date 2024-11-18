## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  eval = FALSE
)

## ----setup, message=FALSE, warning=FALSE--------------------------------------
#  library(pct)
#  library(sf)
#  library(dplyr)
#  library(tmap)
#  tm_shape(pct_regions) +
#    tm_polygons() +
#    tm_text("region_name", size = 0.6)

## -----------------------------------------------------------------------------
#  region_of_interest = "west-yorkshire"
#  zones_region = get_pct_zones(region = region_of_interest)
#  # zones_region = get_pct_zones(region = region_of_interest, geography = "lsoa") # for smaller zones
#  names(zones_region)
#  plot(zones_region["bicycle"])

## ---- eval=FALSE, echo=FALSE--------------------------------------------------
#  
#  # tm_shape(zones_region) +
#  #   tm_fill("bicycle", palette = "RdYlBu") +
#  #   tm_shape(pct_regions) +
#  #   tm_borders() +
#  #   tm_text("region_name")
#  
#  # reproducible example of fail
#  
#  remotes::install_github("mtennekes/tmap")
#  library(tmap)
#  u = "https://github.com/npct/pct-outputs-regional-notR/raw/master/commute/lsoa/isle-of-wight/z.geojson"
#  z = sf::st_read(u)
#  plot(z["bicycle"])
#  qtm(z)
#  tm_shape(z) +
#    tm_fill("bicycle", palette = "RdYlBu")
#  tmap_mode("view")
#  qtm(z)
#  
#  # another region
#  tmap_mode("plot")
#  u = "https://github.com/npct/pct-outputs-regional-notR/raw/master/commute/lsoa/west-yorkshire/z.geojson"
#  z = sf::st_read(u)
#  plot(z["bicycle"])
#  qtm(z)
#  tm_shape(z) +
#    tm_fill("bicycle", palette = "RdYlBu")
#  tmap_mode("view")
#  qtm(z)
#  mapview::mapview(z)
#  
#  devtools::session_info()

## -----------------------------------------------------------------------------
#  unique(zones_region$lad_name)

## -----------------------------------------------------------------------------
#  zones = zones_region %>%
#    filter(lad_name == "Leeds")
#  tm_shape(zones) +
#    tm_fill("bicycle", palette = "RdYlBu")

## -----------------------------------------------------------------------------
#  scenarios_of_interest = c("govnearmkt_slc", "dutch_slc", "ebike_slc")
#  tm_shape(zones) +
#    tm_fill(scenarios_of_interest, palette = "RdYlBu", n = 9, title = "N. cycling") +
#    tm_facets(nrow = 1, free.scales = FALSE) +
#    tm_layout(panel.labels = scenarios_of_interest)

## -----------------------------------------------------------------------------
#  zones_mode_share = zones %>%
#    select(scenarios_of_interest) %>%
#    mutate_at(scenarios_of_interest, .funs = list(~ ./zones$all * 100))
#  tm_shape(zones_mode_share) +
#    tm_fill(scenarios_of_interest, palette = "RdYlBu", title = "% cycling") +
#    tm_facets(nrow = 1, free.scales = FALSE) +
#    tm_layout(panel.labels = scenarios_of_interest)

## -----------------------------------------------------------------------------
#  zones_region %>%
#    st_drop_geometry() %>%
#    group_by(lad_name) %>%
#    select(`2011 census` = bicycle, c(scenarios_of_interest, "all")) %>%
#    summarise_all(.funs = ~ round(sum(.)/sum(all)* 100)) %>%
#    select(-all, `Local Authority / % Cycling in scenario:` = lad_name) %>%
#    knitr::kable()

## ----national-dl--------------------------------------------------------------
#  zones_national = read_sf("https://github.com/npct/pct-outputs-national/raw/master/commute/msoa/z_all.geojson")

## -----------------------------------------------------------------------------
#  national_commute_totals = zones_national %>%
#    st_drop_geometry() %>%
#    select(all, census_2011 = bicycle, govtarget_slc, dutch_slc) %>%
#    summarise_all(.funs = ~sum(.))
#  national_commute_percentages = national_commute_totals / national_commute_totals$all * 100

## ---- echo=FALSE--------------------------------------------------------------
#  knitr::kable(bind_rows(national_commute_totals, national_commute_percentages), digits = 1,
#               caption = "Total counts and percentages of cycle commuters under different scenarios")

## -----------------------------------------------------------------------------
#  r = read.csv(stringsAsFactors = FALSE, text = "area
#  Greater London
#  Greater Manchester
#  Birmingham
#  Leeds
#  Glasgow
#  Liverpool
#  Newcastle
#  Bristol
#  Cardiff
#  Belfast
#  Southampton
#  Sheffield
#  ")
#  matching_las = pct_regions_lookup$lad16nm[pct_regions_lookup$lad16nm %in% r$area]
#  matching_regions = c("london", "greater-manchester")
#  pct_lookup = pct_regions_lookup %>%
#    rename(lad_name = lad16nm)
#  zones_national = inner_join(zones_national, pct_lookup)
#  zones_national = zones_national %>%
#    mutate(area = case_when(
#      region_name == "london" ~ "Greater London",
#      region_name == "greater-manchester" ~ "Greater Manchester",
#      lad_name %in% matching_las ~ lad_name,
#      TRUE ~ "Other"
#      ))
#  table(zones_national$area)
#  zones_aggregated = zones_national %>%
#    sf::st_drop_geometry() %>%
#    group_by(area) %>%
#    summarise(
#      Commuters = sum(all, na.rm = TRUE),
#      Bicycle_census = sum(bicycle),
#      Bicycle_govtarget = sum(govtarget_slc),
#      Bicycle_godutch = sum(dutch_slc)
#      )
#  
#  # plot(zones_aggregated["Commuters"], border = NA)
#  zones_aggregated %>%
#    inner_join(r, .) %>%
#    knitr::kable(digits = 0)

## -----------------------------------------------------------------------------
#  zones_aggregated_percents = zones_aggregated %>%
#    mutate_at(vars(-Commuters, -area), funs(./Commuters * 100))
#  names(zones_aggregated_percents)[3:5] = paste0(names(zones_aggregated_percents)[3:5], "_percent")
#  zones_aggregated_percents %>%
#    inner_join(r, .) %>%
#    knitr::kable(digits = 1)

