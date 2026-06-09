## ----eval=FALSE---------------------------------------------------------------
# gs <- greenSD::get_gsdc(bbox = c(-83.272828,42.343950,-83.218926,42.379719), year = 2022, mask = TRUE)

## ----eval=FALSE---------------------------------------------------------------
# gs <- greenSD::get_gsdc(place = 'Detroit', year = 2022)

## ----eval=FALSE---------------------------------------------------------------
# gs <- greenSD::get_gsdc(location = c(-83.10215 42.38342), year = 2022)

## ----eval=FALSE---------------------------------------------------------------
# # check UID
# greenSD::check_available_cities()
# gs <- greenSD::get_gsdc(UID = 1825, year = 2022, time = c("03-01", "09-01"))

## ----eval=FALSE---------------------------------------------------------------
# ndvi <- greenSD::get_esa_wc(place = 'Detroit', datatype = "ndvi")

## ----eval=FALSE---------------------------------------------------------------
# lc <- greenSD::get_esa_wc(place = 'Detroit', datatype = "landcover")

## ----eval=FALSE---------------------------------------------------------------
# ndvi <- greenSD::get_s2a_ndvi(bbox = c(-83.087174,42.333373,-83.042542,42.358748),
#                               datetime = c("2022-08-01", "2022-09-01"),
#                               cloud_cover = 5,
#                               output_bands = NULL)

## ----eval=FALSE---------------------------------------------------------------
# # from Esri.WorldImagery map tiles
# green <- greenSD::get_tile_green(bbox = c(-83.087174,42.333373,-83.042542,42.358748),
#                                  provider = "esri",
#                                  zoom = 16)
# 
# # from Sentinel-2 cloudless mosaic tiles
# greenspace2 <- greenSD::get_tile_green(bbox = c(-83.087174,42.333373,-83.042542,42.358748),
#                                       zoom = 17,
#                                       provider = "eox",
#                                       year = 2022)

## ----eval=FALSE---------------------------------------------------------------
# boundary <- greenSD::check_urban_boundary(uid = 1825, plot = FALSE)
# samples <- sf::st_sample(boundary, size = 50)
# gs_samples <- greenSD::sample_values(samples, year = 2022)

## ----eval=FALSE---------------------------------------------------------------
# # Load example data (or use `gs` from previous step)
# sample_data <- terra::rast(system.file("extdata", "detroit_gs.tif", package = "greenSD"))
# 
# # Generate GIF
# gif <- greenSD::to_gif(
#   r = sample_data,
#   fps = 5,
#   width = 600,
#   height = 600,
#   axes = FALSE,
#   title_prefix = paste("greenspace - Day", 1:terra::nlyr(sample_data) * 10)
# )
# 
# # Display in RStudio Viewer or save
# print(gif)
# 
# # To save the GIF manually:
# magick::image_write(gif, "greenspace_animation.gif")

