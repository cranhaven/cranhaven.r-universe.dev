# Preparation ---------------------------------------------------

# Load a point cloud of some trees included in the lidR package
point_cloud <- lidR::readLAS(system.file(
  "extdata/MixedConifer.laz",
  package = "lidR"
))

# Set up a plotting function for segmented point clouds
plot_segmented_point_cloud <- function(
    point_cloud) {

  # Generate random crown colors
  crown_colors <- lidR::pastel.colors(
    n = length(unique(point_cloud@data[["crown_id"]])))

  # Plot the segmented crown bodies
  lidR::plot(
    point_cloud,
    color = "crown_id",
    pal = crown_colors,
    nbreaks = length(crown_colors),
    size = 3,
    axis = TRUE
  )
}

# Usage workflow ---------------------------------------------------------------

# Segment tree crowns
segmented_point_cloud <- segment_tree_crowns(
  point_cloud,
  crown_diameter_to_tree_height = 0.2,
  crown_length_to_tree_height = 0.5)

# Plot the segmented point cloud
plot_segmented_point_cloud(segmented_point_cloud)

# Remove small trees
processed_point_cloud_1 <- remove_small_trees(segmented_point_cloud)

# Plot the result
plot_segmented_point_cloud(processed_point_cloud_1)


# Vary some arguments ----------------------------------------------------------

# increase crown radius threshold
processed_point_cloud_2 <- remove_small_trees(
  segmented_point_cloud,
  min_radius = 2)

# Plot the result
plot_segmented_point_cloud(processed_point_cloud_2)


# increase the height threshold
processed_point_cloud_3 <- remove_small_trees(
  segmented_point_cloud,
  min_height = 20)

# Plot the result
plot_segmented_point_cloud(processed_point_cloud_3)

