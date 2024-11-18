## ---- eval=FALSE, echo=FALSE--------------------------------------------------
#  # Aim: generate references.bib - run only if references change
#  refs = RefManageR::ReadZotero(group = "418217", .params = list(collection = "8Y9DU4DR", limit = 100))
#  RefManageR::WriteBib(refs, "vignettes/references.bib")
#  citr::tidy_bib_file(
#    rmd_file = "vignettes/pct.Rmd",
#    messy_bibliography = "vignettes/references.bib",
#    file = "vignettes/refs.bib")
#  file.remove("vignettes/references.bib")

## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## -----------------------------------------------------------------------------
knitr::opts_chunk$set(eval = FALSE)

## ---- eval=FALSE--------------------------------------------------------------
#  remotes::install_github("ITSLeeds/pct")

## -----------------------------------------------------------------------------
#  library(pct)

## ---- message=FALSE-----------------------------------------------------------
#  library(sf)
#  library(dplyr)
#  library(stplanr)
#  library(leaflet)
#  library(ggplot2)
#  library(pbapply)

## -----------------------------------------------------------------------------
#  wight_centroids = get_pct_centroids(region = "isle-of-wight", geography = "msoa")
#  wight_zones = get_pct_zones(region = "isle-of-wight", geography = "msoa")

## ----centroids, fig.show='hold'-----------------------------------------------
#  plot(wight_centroids[, "bicycle"])
#  plot(wight_zones[, "bicycle"])

## ----get_pct_lines------------------------------------------------------------
#  wight_lines_pct = get_pct_lines(region = "isle-of-wight", geography = "msoa")

## -----------------------------------------------------------------------------
#  wight_lines_30 = wight_lines_pct %>%
#    top_n(30, bicycle)

## ---- pct-lines-min-----------------------------------------------------------
#  lwd = wight_lines_30$all / mean(wight_lines_30$all) * 5
#  plot(wight_lines_30[c("bicycle", "car_driver", "foot")], lwd = lwd)

## ----leaflines, out.width="100%"----------------------------------------------
#  pal = colorNumeric(palette = "RdYlBu", domain = wight_lines_30$bicycle)
#  leaflet(data = wight_lines_30) %>%
#    addTiles() %>%
#    addPolylines(weight = lwd,
#                 color = ~ pal(bicycle)) %>%
#    addLegend(pal = pal, values = ~bicycle)

## ----isle-pct-bike, echo=FALSE, out.width="100%"------------------------------
#  # i = magick::image_read("vignettes/isle-pct-bike.png")
#  knitr::include_graphics("https://user-images.githubusercontent.com/1825120/54882128-c4f02980-4e4e-11e9-8eb8-49c43507165a.png")

## ---- eval=FALSE--------------------------------------------------------------
#  wight_od_all = get_od(region = "wight")

## ---- echo=FALSE, eval=FALSE--------------------------------------------------
#  saveRDS(wight_od_all, "wight_od_all.Rds")
#  piggyback::pb_upload("wight_od_all.Rds")
#  piggyback::pb_download_url("wight_od_all.Rds")

## ----echo=FALSE---------------------------------------------------------------
#  u = "https://github.com/ITSLeeds/pct/releases/download/0.5.0/wight_od_all.Rds"
#  wight_od_all = readRDS(url(u))

## ---- message=FALSE-----------------------------------------------------------
#  summary(wight_od_all$geo_code1 %in% wight_centroids$geo_code)
#  summary(wight_od_all$geo_code2 %in% wight_centroids$geo_code)

## -----------------------------------------------------------------------------
#  wight_od = wight_od_all %>%
#    filter(geo_code2 %in% wight_centroids$geo_code)

## ----pct-lines----------------------------------------------------------------
#  wight_lines = od2line(wight_od, wight_centroids)
#  nrow(wight_lines)
#  sum(wight_lines$all)

## ---- eval=FALSE, echo=FALSE--------------------------------------------------
#  # aim: test result of get_desire_lines
#  library(pct)
#  wight_od_all = get_od(region = "wight")
#  wight_od = wight_od_all[
#    wight_od_all$geo_code2 %in% wight_centroids$geo_code,]
#  wight_lines_census = stplanr::od2line(wight_od, wight_centroids)
#  wight_lines_census2 = get_desire_lines(region = "wight")
#  nrow(wight_lines_census)
#  nrow(wight_lines_census2)

## -----------------------------------------------------------------------------
#  wight_lines_census = wight_lines %>%
#    filter(geo_code1 != geo_code2)
#  nrow(wight_lines_census)
#  sum(wight_lines_census$all)

## -----------------------------------------------------------------------------
#  wight_lines_census1 = od_oneway(
#    wight_lines_census,
#    attrib = c("all", "bicycle")
#    )
#  nrow(wight_lines_census1) / nrow(wight_lines_census)
#  sum(wight_lines_census1$all) / sum(wight_lines_census$all)

## ----pct-routes-fast, eval=FALSE----------------------------------------------
#  wight_routes_fast = route(
#    l = wight_lines_census1,
#    route_fun = cyclestreets::journey,
#    plan = "fastest")

## ---- echo=FALSE, eval=FALSE--------------------------------------------------
#  saveRDS(wight_routes_fast, "wight_routes_fast.Rds")
#  piggyback::pb_upload("wight_routes_fast.Rds")
#  piggyback::pb_download_url("wight_routes_fast.Rds")

## ---- eval=FALSE--------------------------------------------------------------
#  u = "https://github.com/ITSLeeds/pct/releases/download/0.5.0/wight_routes_fast.Rds"
#  wight_routes_fast = readRDS(url(u))

## -----------------------------------------------------------------------------
#  wight_lines_census_30 = wight_lines_census1 %>%
#    top_n(30, bicycle)

## ---- eval=FALSE--------------------------------------------------------------
#  wight_routes_30_cs = wight_routes_fast %>%
#    group_by(geo_code1, geo_code2) %>%
#    summarise(
#      all = mean(all),
#      bicycle = mean(bicycle),
#      av_incline = weighted.mean(gradient_smooth, w = distances),
#      length = sum(distances),
#      time = sum(time)
#      ) %>%
#    ungroup() %>%
#    top_n(30, bicycle)

## ---- eval=FALSE, echo=FALSE--------------------------------------------------
#  usethis::use_data(wight_routes_30_cs, overwrite = TRUE)

## -----------------------------------------------------------------------------
#  d = as.numeric(st_length(wight_lines_census_30)) / 1000
#  plot(d, wight_routes_30_cs$length / 1000, xlim = c(0, 10))
#  abline(a = c(0, 1))

## -----------------------------------------------------------------------------
#  plot(wight_lines_30$rf_dist_km, wight_routes_30_cs$length)

## ----pct-goducth--------------------------------------------------------------
#  pcycle_govtarget = uptake_pct_govtarget(
#    distance = wight_routes_30_cs$length,
#    gradient = wight_routes_30_cs$av_incline * 100
#  )

## -----------------------------------------------------------------------------
#  wight_routes_30_cs$govtarget = wight_lines_census_30$bicycle +
#    pcycle_govtarget * wight_lines_census_30$all
#  wight_routes_30_cs$govtarget_pct = wight_lines_30$govtarget_slc
#  
#  ggplot(wight_routes_30_cs) +
#    geom_point(aes(length, govtarget), colour = "red") +
#    geom_point(aes(length, govtarget_pct), colour = "blue")
#  cor(wight_routes_30_cs$govtarget, wight_routes_30_cs$govtarget_pct)

## -----------------------------------------------------------------------------
#  wight_routes_30_ls = sf::st_cast(wight_routes_30_cs, "LINESTRING")
#  rnet = overline(wight_routes_30_ls, "govtarget")
#  plot(rnet)

## ---- eval=FALSE--------------------------------------------------------------
#  wight_routes_fast_gt = wight_routes_fast %>%
#    group_by(geo_code1, geo_code2) %>%
#    mutate(
#      govtarget = uptake_pct_govtarget(sum(distances), mean(gradient_smooth)) *
#        (sum(all) + sum(bicycle))
#    )
#  wight_routes_fast_gt = sf::st_cast(wight_routes_fast_gt, "LINESTRING")
#  wight_rnet = overline(wight_routes_fast_gt, "govtarget")

## ---- eval=FALSE, echo=FALSE--------------------------------------------------
#  usethis::use_data(wight_rnet, overwrite = TRUE)

## ---- out.width="100%"--------------------------------------------------------
#  pal = colorNumeric(palette = "RdYlBu", domain = wight_rnet$govtarget)
#  leaflet(data = wight_rnet) %>%
#    addTiles() %>%
#    addPolylines(color = ~ pal(govtarget)) %>%
#    addLegend(pal = pal, values = ~govtarget)

