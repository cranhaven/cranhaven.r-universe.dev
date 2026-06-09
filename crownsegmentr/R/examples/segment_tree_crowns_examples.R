# Preparation ------------------------------------------------------------------

# Load a point cloud of some trees included in the lidR package
point_cloud <- lidR::readLAS(system.file(
  "extdata/MixedConifer.laz",
  package = "lidR"
))

# Set up a plotting function for segmented point clouds
plot_segmented_point_cloud <- function(
    point_cloud,
    crown_colors = NULL,
    size = 3) {
  # Generate random crown colors
  if (is.null(crown_colors)) {
    crown_colors <- lidR::pastel.colors(
      n = length(unique(point_cloud@data[["crown_id"]]))
    )
  }

  # Plot the segmented crown bodies
  lidR::plot(
    point_cloud,
    color = "crown_id",
    pal = crown_colors,
    nbreaks = length(crown_colors),
    size = size,
    axis = TRUE
  )
}


# Simple case: Segment Normalized Point Clouds ---------------------------------

segmented_point_cloud <- crownsegmentr::segment_tree_crowns(
  point_cloud,
  crown_diameter_to_tree_height = 0.25,
  crown_length_to_tree_height = 0.5
)

plot_segmented_point_cloud(segmented_point_cloud)


# Complete workflow with pre- and postprocessing -------------------------------

diameter_raster <- crownsegmentr::li_diameter_raster(
  point_cloud,
  crown_diameter_constant = 2
)
# If you have the package EBImage, you can also use
# watershed_diameter_raster() similarly

terra::plot(diameter_raster)

segmented_point_cloud <- crownsegmentr::segment_tree_crowns(
  point_cloud,
  crown_diameter_to_tree_height = diameter_raster,
  crown_length_to_tree_height = 0.4,
  crown_diameter_constant = 2,
  crown_length_constant = 3
)

plot_segmented_point_cloud(segmented_point_cloud)

processed_point_cloud <- crownsegmentr::remove_small_trees(
  segmented_point_cloud,
  min_radius = 1,
  min_height = 5
)

plot_segmented_point_cloud(processed_point_cloud)

# Additional examples ----------------------------------------------------------
# Call Preprocessing (diameter_raster) Function from Within the Main Function---

segmented_point_cloud <- crownsegmentr::segment_tree_crowns(
  point_cloud,
  crown_diameter_to_tree_height = "li2012",
  crown_length_to_tree_height = 0.4,
  crown_diameter_constant = 2,
  crown_length_constant = 3
)

plot_segmented_point_cloud(segmented_point_cloud)

# result is equivalent to the intermediate step above. Only for using all the
# options of the diameter_raster functions it is necessary to call them manually.

# Exclude Points Below Certain Height from Segmentation ----------------------------

segmented_point_cloud <- crownsegmentr::segment_tree_crowns(
  point_cloud,
  crown_diameter_to_tree_height = 0.25,
  crown_length_to_tree_height = 0.5,
  segment_crowns_only_above = 15 # exclude points below 15 m
)

plot_segmented_point_cloud(segmented_point_cloud)


# Segment Terraneous (i.e. Non-Normalized) Point Clouds -------------------

terraneous_point_cloud <- lidR::readLAS(system.file(
  "extdata/Topography.laz",
  package = "lidR"
))

# Either pass arguments for the lidR::rasterize_terrain() function
segmented_point_cloud <- crownsegmentr::segment_tree_crowns(
  terraneous_point_cloud,
  crown_diameter_to_tree_height = 0.5,
  crown_length_to_tree_height = 1,
  segment_crowns_only_above = 2,
  ground_height = list(algorithm = lidR::tin())
)

# Or pass a ground height raster
ground_height_grid <- lidR::rasterize_terrain(
  terraneous_point_cloud,
  algorithm = lidR::tin()
)

segmented_point_cloud <- crownsegmentr::segment_tree_crowns(
  terraneous_point_cloud,
  crown_diameter_to_tree_height = 0.5,
  crown_length_to_tree_height = 1,
  segment_crowns_only_above = 2,
  ground_height = ground_height_grid
)

plot_segmented_point_cloud(segmented_point_cloud)


# Account for Different Crown Shapes with Parameter Rasters ---------------

# Create a raster of crown_diameter_to_tree_height ratios with 0.25 in the
# western half and 0.75 in the eastern half
crown_diameter_to_tree_height_grid <- terra::rast(
  matrix(c(0.25, 0.75), ncol = 2),
  crs = lidR::st_crs(point_cloud)$wkt,
  extent = terra::ext(
    lidR::st_bbox(point_cloud)$xmin,
    lidR::st_bbox(point_cloud)$xmax,
    lidR::st_bbox(point_cloud)$ymin,
    lidR::st_bbox(point_cloud)$ymax
  )
)

segmented_point_cloud <- crownsegmentr::segment_tree_crowns(
  point_cloud,
  crown_diameter_to_tree_height = crown_diameter_to_tree_height_grid,
  crown_length_to_tree_height = 0.5
)

# Observe how adjacent crowns are undersegmented in the right half of the plot
# where the crown_diameter_to_tree_height parameter is set too high
plot_segmented_point_cloud(segmented_point_cloud)


# Additionally Return Centroids (terminal and/or prior) ------------------------------

# You can also return the centroids which were calculated during the
# segmentation in order to get a more in-depth impression of what happened
# internally.
segmentation_results <- crownsegmentr::segment_tree_crowns(
  point_cloud,
  crown_diameter_to_tree_height = 0.25,
  crown_length_to_tree_height = 0.5,
  also_return_terminal_centroids = TRUE,
  also_return_all_centroids = TRUE
)

# Generate crown colors "manually" so that we can use the same colors for
# points, terminal centroids, and prior centroids.
crown_colors <- lidR::random.colors(
  n = length(unique(
    segmentation_results$segmented_point_cloud@data[["crown_id"]]
  )) - 1
)

# Plot the segemented point cloud
plot_segmented_point_cloud(
  segmentation_results$segmented_point_cloud, crown_colors
)
# Plot the terminal_centroids
plot_segmented_point_cloud(
  segmentation_results$terminal_centroids, crown_colors
)
# Plot the centroids
plot_segmented_point_cloud(
  segmentation_results$centroids,
  crown_colors,
  size = 1
)


# Parameter Tweaking ------------------------------------------------------

# Observe how processing gets faster while segmentation accuracy slightly
# decreases when using a larger centroid convergence distance and a larger
# DBSCAN neighborhood.
system.time(
  segmented_point_cloud <- crownsegmentr::segment_tree_crowns(
    point_cloud,
    crown_diameter_to_tree_height = 0.25,
    crown_length_to_tree_height = 0.5,
    centroid_convergence_distance = 0.02,
    dbscan_neighborhood_radius = 0.5
  )
)

plot_segmented_point_cloud(segmented_point_cloud)

system.time(
  segmented_point_cloud <- crownsegmentr::segment_tree_crowns(
    point_cloud,
    crown_diameter_to_tree_height = 0.25,
    crown_length_to_tree_height = 0.5,
    centroid_convergence_distance = 0.01, # default value
    dbscan_neighborhood_radius = 0.3 # default value
  )
)

plot_segmented_point_cloud(segmented_point_cloud)


