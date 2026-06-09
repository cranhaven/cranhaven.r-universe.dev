# Simple case -------------------------------------------------------------

# Load a point cloud of some trees included in the lidR package
point_cloud <- lidR::readLAS(system.file(
  "extdata/MixedConifer.laz",
  package = "lidR"
))

# Calculate the diameter raster
diameter_raster_1 <- watershed_diameter_raster(point_cloud)

# Plot
terra::plot(diameter_raster_1)


# Use the raster as input for segment_tree_crowns
segmented_point_cloud <- segment_tree_crowns(
  point_cloud,
  crown_diameter_to_tree_height = diameter_raster_1,
  crown_length_to_tree_height = 0.5)


# Vary some arguments ---------------------------------------------------------
# Adding a crown diameter constant will reduce the raster values
diameter_raster_2 <- watershed_diameter_raster(
  point_cloud,
  crown_diameter_constant = 3
)

# Plot
terra::plot(diameter_raster_2)


# limits will keep the raster values confined
diameter_raster_3 <- watershed_diameter_raster(
  point_cloud,
  crown_diameter_constant = 3,
  limits = c(0.2,0.3)
)

# Plot
terra::plot(diameter_raster_3)


# Increasing the smoothing radius will create a more homogeneous raster
diameter_raster_4 <- watershed_diameter_raster(
  point_cloud,
  crown_diameter_constant = 3,
  limits = c(0.2,0.3),
  smoothing_radius = 20
)

# Plot
terra::plot(diameter_raster_4)


# Setting the smoothing radius to 0 will show the diameter to height ratio of
# each segmented crown
diameter_raster_5 <- watershed_diameter_raster(
  point_cloud,
  smoothing_radius = 0
)

# Plot
terra::plot(diameter_raster_5)

