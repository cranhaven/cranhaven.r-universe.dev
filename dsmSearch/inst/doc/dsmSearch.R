## ----eval = FALSE-------------------------------------------------------------
#  data <- dsmSearch::get_dsm_30(bbox = c(-83.783557,42.241833,-83.696525,42.310420), key = "Your KEY")

## ----eval = FALSE-------------------------------------------------------------
#  # search for lidar data information using bbox
#  search_result <- dsmSearch::lidar_search(bbox = c(-83.742282,
#                                                    42.273389,
#                                                    -83.733442,
#                                                    42.278724),
#                                           preview = FALSE)
#  search_result

## ----eval = FALSE-------------------------------------------------------------
#  # try coordinates -83.741289,42.270146 (in south Michigan, USA)
#  # radius is 1000ft
#  las <- dsmSearch::get_lidar(x = -83.741289,
#                              y = 42.270146,
#                              r = 1000,
#                              epsg = 2253)
#  # download with bbox
#  las <- dsmSearch::get_lidar(bbox = c(-83.742282,42.273389,-83.733442,42.278724),
#                              epsg = 2253)

## ----eval = FALSE-------------------------------------------------------------
#  # Create DTM
#  dtm_ <- lidR::rasterize_terrain(las, res = 5, lidR::tin())
#  terra::plot(dtm_)

## ----eval = FALSE-------------------------------------------------------------
#  # Create DSM
#  dsm_ <- lidR::rasterize_canopy(las, res = 5, lidR::dsmtin())
#  raster::plot(dsm_)

