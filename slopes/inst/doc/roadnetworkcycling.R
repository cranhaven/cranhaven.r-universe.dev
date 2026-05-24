## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width=7, fig.height=5, fig.align = "center",
  eval = FALSE
)

## ----setup1, warning=FALSE, message = FALSE-----------------------------------
# library(dplyr)
# library(sf)
# # remotes::install_github("ITSLeeds/osmextract")
# library(osmextract)

## ----get_iow, warning=FALSE, message = FALSE----------------------------------
# # get the network
# iow_osm = oe_get("Isle of Wight", provider = "geofabrik", stringsAsFactors = FALSE,
#                  quiet = FALSE, force_download = TRUE, force_vectortranslate = TRUE) # 7 MB
# 
# # filter the major roads
# iow_network = iow_osm %>%
#   dplyr::filter(highway %in% c('primary', "primary_link", 'secondary',"secondary_link",
#                                'tertiary', "tertiary_link", "trunk", "trunk_link",
#                                "residential", "cycleway", "living_street", "unclassified",
#                                "motorway", "motorway_link", "pedestrian", "steps", "track")) #remove: "service"

## ----setup2, warning=FALSE, message = FALSE-----------------------------------
# # remotes::install_github("ropensci/stplanr")
# library(stplanr)

## ----filter-------------------------------------------------------------------
# # filter unconnected roads
# iow_network$group = rnet_group(iow_network)
# iow_network_clean = iow_network %>% filter(group == 1) # the network with more connected segments

## ----breaking, warning=FALSE, message = FALSE---------------------------------
# iow_network_segments = rnet_breakup_vertices(iow_network_clean)

## ----import_dem, message=FALSE------------------------------------------------
# # Import and plot DEM
# u = "https://github.com/U-Shift/Declives-RedeViaria/releases/download/0.2/IsleOfWightNASA_clip.tif"
# f = basename(u)
# download.file(url = u, destfile = f, mode = "wb")
# dem = raster::raster(f)
# # res(dem) #27m of resolution
# network = iow_network_segments
# 
# library(raster)
# plot(dem)
# plot(sf::st_geometry(network), add = TRUE) #check if they overlay

## ----slopes_values------------------------------------------------------------
# # Get the slope value for each segment (abs), using slopes package
# library(slopes)
# library(geodist)
# network$slope = slope_raster(network, dem)
# network$slope = network$slope*100 #percentage
# summary(network$slope) #check the values

## ----classify-----------------------------------------------------------------
# # Classify slopes
# network$slope_class = network$slope %>%
#   cut(
#     breaks = c(0, 3, 5, 8, 10, 20, Inf),
#     labels = c("0-3: flat", "3-5: mild", "5-8: medium", "8-10: hard",
#                "10-20: extreme", ">20: impossible"),
#     right = F
#   )
# round(prop.table(table(network$slope_class))*100,1)

## ----map, message = FALSE, eval=FALSE-----------------------------------------
# # more useful information
# network$length = st_length(network)
# 
# # make an interactive map
# library(tmap)
# palredgreen = c("#267300", "#70A800", "#FFAA00", "#E60000", "#A80000", "#730000") #color palette
# # tmap_mode("view")
# tmap_options(basemaps = leaflet::providers$CartoDB.Positron) #basemap
# 
# slopemap =
#   tm_shape(network) +
#   tm_lines(
#     col = "slope_class",
#     palette = palredgreen,
#     lwd = 2, #line width
#     title.col = "Slope [%]",
#     popup.vars = c("Highway" = "highway",
#                    "Length" = "length",
#                   "Slope: " = "slope",
#                   "Class: " = "slope_class"),
#     popup.format = list(digits = 1),
#     # id = "slope"
#     id = "name" #if it gets too memory consuming, delete this line
#   )
# 
# slopemap

## ----export, echo=TRUE, eval=FALSE--------------------------------------------
# #export to html
# tmap_save(slopemap, "html/SlopesIoW.html")
# 
# # export information as geopackage
# st_write(network, "shapefiles/SlopesIoW.gpkg", append=F)

## ----tidyup, include=FALSE----------------------------------------------------
# rm(iow_osm,iow_network_clean,iow_network_segments, iow_network, slopemap)
# file.remove(f) # remove the file, tidy up

