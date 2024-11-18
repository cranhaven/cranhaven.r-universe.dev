## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## -----------------------------------------------------------------------------
knitr::opts_chunk$set(eval = FALSE)

## ----libraries, message=FALSE-------------------------------------------------
#  # devtools::install_github("ATFutures/geoplumber")
#  # require("geojsonsf")
#  library(pct)

## -----------------------------------------------------------------------------
#  head(santiago_od)

## -----------------------------------------------------------------------------
#  sf:::print.sf(santiago_zones)
#  plot(santiago_zones)

## ---- warning=FALSE-----------------------------------------------------------
#  desire_lines = stplanr::od2line(flow = santiago_od, zones = santiago_zones)

## ---- out.width="100%"--------------------------------------------------------
#  plot(santiago_zones$geometry)
#  plot(santiago_lines["pcycle"], lwd = santiago_lines$n / 3, add = TRUE)
#  # gj = geojsonsf::sf_geojson(santiago_lines)
#  # path = file.path(tempdir(), "dl.geojson")
#  # write(gj, path)
#  # html_map = geoplumber::gp_map(path, browse_map = FALSE)
#  # htmltools::includeHTML(html_map)

## -----------------------------------------------------------------------------
#  desire_lines$hilliness = 0

## -----------------------------------------------------------------------------
#  desire_lines$distance = as.numeric(sf::st_length(desire_lines))

## -----------------------------------------------------------------------------
#  desire_lines$godutch_pcycle = uptake_pct_godutch(distance = desire_lines$distance, gradient = 0)

## -----------------------------------------------------------------------------
#  cor(x = desire_lines$pcycle, y = desire_lines$godutch_pcycle)
#  plot(x = desire_lines$pcycle, y = desire_lines$godutch_pcycle)
#  plot(x = desire_lines$distance, y = desire_lines$godutch_pcycle, ylim = c(0, 1))

## ---- out.width="50%", fig.show='hold'----------------------------------------
#  library(leaflet)
#  leaflet(width = "100%") %>%
#    addTiles() %>%
#    addPolylines(data = desire_lines, weight = desire_lines$pcycle * 5)
#  leaflet(width = "100%") %>%
#    addTiles() %>%
#    addPolylines(data = desire_lines, weight = desire_lines$godutch_pcycle * 5)

## ---- eval=FALSE--------------------------------------------------------------
#  santiago_routes_cs = stplanr::line2route(desire_lines)
#  # > 10 % out of 200 distances calculated
#  # > 20 % out of 200 distances calculated
#  # > 30 % out of 200 distances calculated
#  # > 40 % out of 200 distances calculated
#  # > 50 % out of 200 distances calculated
#  # > 60 % out of 200 distances calculated
#  # > 70 % out of 200 distances calculated
#  # > 80 % out of 200 distances calculated
#  # > 90 % out of 200 distances calculated
#  # > 100 % out of 200 distances calculated
#  # > Warning message:
#  # > In value[[3L]](cond) : Fail for line number 32

## -----------------------------------------------------------------------------
#  leaflet() %>%
#    addTiles() %>%
#    addPolylines(data = santiago_routes_cs[32, ])

## -----------------------------------------------------------------------------
#  routes = sf::st_sf(
#    cbind(sf::st_drop_geometry(santiago_routes_cs),
#    sf::st_drop_geometry(desire_lines)),
#    geometry = santiago_routes_cs$geometry
#  )

## -----------------------------------------------------------------------------
#  routes$godutch_slc = round(routes$godutch_pcycle * routes$all)
#  rnet = stplanr::overline2(routes, "godutch_slc")
#  plot(rnet, lwd = rnet$godutch_slc / mean(rnet$godutch_slc))
#  # library(tmap)
#  # tmap_mode("view")
#  #   tm_shape(rnet) +
#  #   tm_lines(lwd = "godutch_slc", scale = 9)

## ---- eval=FALSE--------------------------------------------------------------
#  route_segments_1_5 = route(l = desire_lines[1:5, ], route_fun = cyclestreets::journey)
#  mapview::mapview(route_segments_1_5)

