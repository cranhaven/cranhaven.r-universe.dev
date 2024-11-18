## -----------------------------------------------------------------------------
knitr::opts_chunk$set(eval = FALSE)

## ---- message=FALSE, warning=FALSE--------------------------------------------
#  # Load packages
#  library(pct)
#  library(tmap)

## ---- message=FALSE, warning=FALSE--------------------------------------------
#  #Get road network for preselected regin
#  rnet = pct::get_pct_rnet(region = "oxfordshire")
#  #Calculate road length
#  rnet$segment_length = as.numeric(sf::st_length(rnet))
#  #Calculate daily km's cycled
#  rnet$m_cycled_per_working_day = rnet$segment_length * rnet$bicycle

## ---- message=FALSE, warning=FALSE--------------------------------------------
#  # Get road network for preselected regin
#  zones = pct::get_pct_zones(region = "oxfordshire")
#  summary(sf::st_is_valid(zones))
#  zones = sf::st_make_valid(zones)
#  summary(sf::st_is_valid(zones))
#  cycled_m_per_zone = aggregate(rnet["m_cycled_per_working_day"], zones, FUN = sum)

## ---- message=FALSE, warning=FALSE--------------------------------------------
#  zones$mkm_cycled_for_commuting_per_year_estimated = cycled_m_per_zone$m_cycled_per_working_day *
#    2 * 200 / # estimate of trips days per year, morning and afternoon
#    1e9

## ---- message=FALSE, warning=FALSE, fig.align="center", fig.width = 7, fig.height = 6----
#    tm_shape(zones) +
#      tm_fill(
#        col = "mkm_cycled_for_commuting_per_year_estimated",
#        style = "quantile",
#        palette = "plasma",
#        title = "Yearly distance cycled by commuters per day\n(2011 Census Data)",
#        legend.size.is.portrait = TRUE
#      ) +
#      tm_layout(
#        title = "OXFORDSHIRE",
#        title.position = c("left", "top"),
#        bg.color = "honeydew3",
#        outer.bg.color = "honeydew",
#        legend.stack = "horizontal",
#        legend.outside = TRUE,
#        legend.outside.position = "left",
#        frame.lwd = 5
#      )

## ---- message=FALSE, warning=FALSE--------------------------------------------
#  pct_zones_rnet_current = function(region_name) {
#    # Get road network for preselected regin
#    rnet = pct::get_pct_rnet(region = region_name)
#    # Calculate road length
#    rnet$segment_length = as.numeric(sf::st_length(rnet))
#    # Calculate daily km cycled
#    rnet$m_cycled_per_working_day = rnet$segment_length * rnet$bicycle
#    # Convert to centroids to avoid double counting flows that cross zones
#    rnet_centroids = sf::st_centroid(rnet)
#    # Get LSOA spatial data
#    zones = sf::st_make_valid(pct::get_pct_zones(region = region_name))
#    # Calculate cyced miles per zone
#    cycled_m_per_zone = aggregate(rnet_centroids["m_cycled_per_working_day"], zones, FUN = sum)
#    # Calculate miles cycled per year from commuting
#    zones$mkm_cycled_for_commuting_per_year_estimated = cycled_m_per_zone$m_cycled_per_working_day *
#      2 * 200 / # estimate of trips days per year, morning and afternoon
#      1e9
#    # Plot results
#    tmap_mode("plot")
#    tm_shape(zones) +
#      tm_fill(
#        col = "mkm_cycled_for_commuting_per_year_estimated",
#        style = "quantile",
#        palette = "plasma",
#        title = "Million km's cycled by commuters per year\n(2011 Census Data)",
#        legend.size.is.portrait = TRUE
#      ) +
#      tm_layout(
#        title = toupper(region_name),
#        title.position = c("left", "top"),
#        bg.color = "honeydew3",
#        outer.bg.color = "honeydew",
#        legend.stack = "horizontal",
#        legend.outside = TRUE,
#        legend.outside.position = "bottom",
#        frame.lwd = 5
#      )
#  
#  }

## ---- message=FALSE, warning=FALSE, fig.align="center", fig.width = 7, fig.height = 6----
#  oxfordshire_results = pct_zones_rnet_current(region_name = "oxfordshire")
#  cambrideshire_results = pct_zones_rnet_current(region_name = "cambridgeshire")
#  tmap_arrange(oxfordshire_results, cambrideshire_results, ncol = 2)

## ---- message=FALSE, warning=FALSE, fig.align="center", fig.width = 7, fig.height = 6----
#  london_results = pct_zones_rnet_current(region_name = "london")
#  gm_results = pct_zones_rnet_current(region_name = "greater-manchester")
#  #tmap_mode("view")
#  tmap_arrange(london_results, gm_results, ncol = 2)

## ---- message=FALSE, warning=FALSE--------------------------------------------
#  pct_zones_rnet_ebikes <- function(region_name) {
#    # Get road network for pre-selected region
#    rnet = pct::get_pct_rnet(region = region_name)
#    # Calculate road length
#    rnet$segment_length = as.numeric(sf::st_length(rnet))
#    # Calculate daily miles cycled
#    rnet$m_cycled_per_working_day = rnet$segment_length * rnet$ebike_slc
#    # Convert to centroids to avoid double counting flows that cross zones
#    rnet_centroids = sf::st_centroid(rnet)
#    # Get LSOA spatial data
#    zones = sf::st_make_valid(pct::get_pct_zones(region = region_name))
#    # Calculate cycled miles per zone
#    cycled_m_per_zone = aggregate(rnet_centroids["m_cycled_per_working_day"], zones, FUN = sum)
#    # Calculate km cycled per year from commuting
#    zones$mkm_cycled_for_commuting_per_year_estimated = cycled_m_per_zone$m_cycled_per_working_day *
#      2 * 200 / # estimate of trips days per year, morning and afternoon
#      1e9
#    #Plot results
#    tmap_mode("plot")
#    tm_shape(zones) +
#      tm_fill(
#        col = "mkm_cycled_for_commuting_per_year_estimated",
#        style = "quantile",
#        palette = "plasma",
#        title = "Million km's cycled by commuters per year\n(E-Bike model)",
#        legend.size.is.portrait = TRUE
#      ) +
#      tm_layout(
#        title = toupper(region_name),
#        title.position = c("left", "top"),
#        bg.color = "honeydew3",
#        outer.bg.color = "honeydew",
#        legend.stack = "horizontal",
#        legend.outside = TRUE,
#        legend.outside.position = "bottom",
#        frame.lwd = 5
#      )
#  }

## ---- message=FALSE, warning=FALSE, fig.align="center", fig.width = 7, fig.height = 6----
#  london_results_ebikes = pct_zones_rnet_ebikes(region_name = "london")
#  gm_results_ebikes = pct_zones_rnet_ebikes(region_name = "greater-manchester")
#  #tmap_mode("view")
#  tmap_arrange(london_results_ebikes, gm_results_ebikes, ncol = 2)

