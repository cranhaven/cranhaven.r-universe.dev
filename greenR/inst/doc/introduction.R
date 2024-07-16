## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----eval=FALSE---------------------------------------------------------------
#  # install.packages("devtools")
#  devtools::install_github("sachit27/greenR", dependencies = TRUE)

## ----echo = FALSE, message = FALSE--------------------------------------------
library(greenR)
data <- get_osm_data("City of London, United Kingdom")

## ----echo = FALSE, message = FALSE--------------------------------------------
green_areas_data <- data$green_areas
visualize_green_spaces(green_areas_data)
green_space_clustering(green_areas_data, num_clusters = 3)

## ----echo = FALSE, message = FALSE--------------------------------------------
mapbox_token <- "your_mapbox_access_token_here"
accessibility_mapbox(green_areas_data, mapbox_token)

## ----echo = FALSE, message = FALSE--------------------------------------------
accessibility_greenspace(green_areas_data, 47.56427527336772, 7.595820936462059)
map <- nearest_greenspace(data$highways, data$green_areas, 51.4761, -0.2008)

## ----eval = FALSE-------------------------------------------------------------
#  print(map)  # Displays the interactive map

## ----echo = FALSE, message = FALSE--------------------------------------------
data <- get_osm_data("City of London, United Kingdom")
green_areas_data <- data$green_areas
tree_data <- data$trees
hex_map <- hexGreenSpace(green_areas_data, tree_data, hex_size = 300, color_palette = "viridis")

## ----eval = FALSE-------------------------------------------------------------
#  print(hex_map$map)  # Display the map
#  print(hex_map$violin)  # Display the violin plot

## ----echo = FALSE, message = FALSE--------------------------------------------
green_index <- calculate_green_index(data, 4326, 100)

## ----eval = FALSE-------------------------------------------------------------
#  # Create a static plot
#  map <- plot_green_index(green_index)
#  
#  # Customize static plot
#  map <- plot_green_index(green_index, colors = c("#FF0000", "#00FF00"), line_width = 1, line_type = "dashed")
#  
#  # Create an interactive plot using Leaflet
#  map <- plot_green_index(green_index, interactive = TRUE, base_map = "CartoDB.DarkMatter")
#  
#  # Use a light-themed base map
#  map <- plot_green_index(green_index, interactive = TRUE, base_map = "CartoDB.Positron")
#  

## ----eval = FALSE-------------------------------------------------------------
#  print(map) #to view the map in the console. You can use htmlwidgets to save the html

## ----echo = FALSE, message = FALSE--------------------------------------------
mapbox_token <- "your_mapbox_access_token_here"

create_linestring_3D(green_index, "green_index", mapbox_token)

## ----echo = FALSE, message = FALSE--------------------------------------------
mapbox_token <- "your_mapbox_access_token_here"

# Create the 3D hexagon map
create_hexmap_3D(
  data = green_index,
  value_col = "green_index",
  mapbox_token = mapbox_token,
  output_file = "map.html",
  color_palette = "interpolateViridis"
)

## ----echo = FALSE, message = FALSE--------------------------------------------
percentage <- calculate_percentage(green_index)

## ----eval = FALSE-------------------------------------------------------------
#  download_file <- save_json(green_index, "File_Path") #file path has to be specified. For example "/Users/.../map.geojson"
#  map <- save_as_leaflet(green_index, "File_Path")

## ----eval = FALSE-------------------------------------------------------------
#  result <- calculate_and_visualize_GVI("image.png") #specify the image path
#  OpenImageR::imageShow(result$segmented_image) #To visualize the segmented image
#  green_pixels_raster <- as.raster(result$green_pixels_image) #To visualize green pixels
#  plot(green_pixels_raster)
#  
#  # Save the segmented image
#  OpenImageR::writeImage(result$segmented_image, "segmented_image.png")
#  
#  # Save the green pixels image
#  OpenImageR::writeImage(result$green_pixels_image, "green_pixels_image.png")

## ----eval = FALSE-------------------------------------------------------------
#  d1 <- get_osm_data("New Delhi, India")
#  dsf <- d1$green_areas$osm_polygons
#  d2 <- get_osm_data("Basel, Switzerland")
#  bsf <- d2$green_areas$osm_polygons
#  d3 <- get_osm_data("Medellin, Colombia")
#  msf <- d3$green_areas$osm_polygons
#  
#  cities_data <- list(dsf, bsf, msf)
#  gssi_values <- gssi(cities_data, "ESRI:54009")

