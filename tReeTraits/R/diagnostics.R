#' Make 3-panel plot of tree point cloud to check for errors
#'
#' Plots 2 profiles X, Y, and and overhead Z view of a point
#' cloud to allow users to identify stray points, or errors in
#' segmentations.
#' @param las `LAS` object from `lidR` package representing
#' the CROWN of a tree. Crowns can be segmented using [segment_crown()].
#' @param res numeric - resolution of voxelization to speed up plotting
#' @param plot boolean - indicates whether to print the output plot, in
#' both cases a ggplot object is returned in the output.
#' @return A `ggplot` object containing the arranged diagnostic panels.
#' @examples
#' # example code
#' library(lidR)
#' file = system.file("extdata", "tree_0744.laz", package="tReeTraits")
#' las = readLAS(file, filter = '-thin_with_voxel 0.1')
#' las = clean_las(las)
#' plot_tree(las)
#' @importFrom lidR decimate_points random_per_voxel
#' @importFrom ggpubr ggarrange
#' @importFrom ggplot2 theme_bw coord_fixed theme element_blank
#' @importFrom ggplot2 scale_color_viridis_c labs element_text element_line
#' @importFrom ggplot2 ggplot aes
#' @export
plot_tree = function(las, res = 0.05, plot=TRUE) {
  las_thin = lidR::decimate_points(las, lidR::random_per_voxel(res=res))
  ggsettings = function(x) {
    x = x + ggplot2::geom_point(size=0.1)  +
      ggplot2::coord_fixed() +
      ggplot2::theme_bw() +
      ggplot2::theme(panel.grid = ggplot2::element_blank(),
                     legend.position = 'none') +
      ggplot2::scale_color_viridis_c(direction = 1, option='G') +
      ggplot2::labs(y=NULL, x=NULL) +
      ggplot2::theme(axis.text = ggplot2::element_text(colour = '#FFFFFF'),
                     axis.ticks = ggplot2::element_line(color='#FFFFFF'))
    return(x)
  }
  v1 = ggsettings(ggplot2::ggplot(las_thin@data, ggplot2::aes(x=.data$X,y=.data$Z, color=.data$Y)))
  v2 = ggsettings(ggplot2::ggplot(las_thin@data, ggplot2::aes(x=.data$Y,y=.data$Z, color=.data$X)))
  #v3 = ggsettings(ggplot2::ggplot(dplyr::filter(las@data,Z<3), ggplot2::aes(x=X,y=Z, color=Y)))
  v4 = ggsettings(ggplot2::ggplot(las_thin@data, ggplot2::aes(x=.data$X,y=.data$Y, color=-.data$Z)))
  profiles = ggpubr::ggarrange(v1, v2, v4, nrow=1, widths = c(0.3,0.3,0.4), labels=LETTERS)
  if(plot) plot(profiles)
  return(profiles)
}

#' Plot QSM in base R
#'
#' Simple function to create a diagnostic plot to view QSMs colored
#' by branching order.
#' @param qsm  a QSM loaded using `[load_qsm()]`.
#' @param scale a factor by which to multiply the `radius_cyl` column to
#' give line segments the appearance of volume
#' @param rotation boolean - indicates whether the plot should display the
#' tree from 2 angles TRUE, or just one FALSE.
#' @importFrom graphics par axis arrows
#' @return `NULL`, invisibly. Produces a base R plot as a side effect.
#' @examples
#' qsm_file = system.file("extdata", "tree_0744_qsm.txt", package='tReeTraits')
#' qsm = load_qsm(qsm_file)
#' plot_qsm2d(qsm)
#' @export
plot_qsm2d = function(qsm, scale = 150, rotation=TRUE) {
  oldpar <- par(no.readonly = TRUE)
  on.exit(par(oldpar))
  if(rotation) {par(mfrow = c(1,2))}
  graphics::par(mar=c(2,2,1,1)+0.5)
  ylim = range(qsm[, c('startZ', 'endZ')])
  plot(NA, NA, xlim = c(-1, 1), ylim = ylim ,asp=1, xlab='', ylab='', axes=FALSE)
  graphics::axis(1);graphics::axis(2);
  with(qsm, arrows(startX, startZ, endX, endZ, length=0, code=2, col=branching_order+1,lwd=radius_cyl*scale))
  if(rotation) {
    plot(NA, NA, xlim = c(-1, 1), ylim = ylim ,asp=1, xlab='', ylab='', axes=FALSE)
    graphics::axis(1);
    with(qsm, graphics::arrows(startY, startZ, endY, endZ, length=0, code=2, col=branching_order+1,lwd=radius_cyl*scale))
  }
  invisible(NULL)
}


#' Diagnostic Plot of Basic Tree Measurements
#'
#' Generates a diagnostic 2D plot showing basic tree measurements such as tree height, crown base height (CBH),
#' crown width, and diameter at breast height (DBH) over a subsampled LAS point cloud.
#'
#' @param las A `LAS` object (from the `lidR` package) containing the tree point cloud.
#' @param height Numeric. Total tree height.
#' @param cbh Numeric. Crown base height.
#' @param crown_width Numeric. Crown width.
#' @param dbh Numeric. Diameter at breast height.
#' @param res Numeric. Resolution for point cloud thinning; default is 0.1 m.
#'
#' @return A `ggplot` object displaying thinned tree points and markers for key measurements.
#' @importFrom lidR decimate_points random_per_voxel
#' @importFrom ggplot2 ggplot geom_point coord_fixed geom_line aes arrow unit
#' @importFrom ggplot2 element_blank theme
#' @importFrom grDevices grey
#' @details
#' The function first thins the point cloud using `lidR::decimate_points()` to improve plotting speed.
#' Then it overlays key measurement lines and markers for height, crown width, DBH, and crown base height.
#'
#' @examples
#' library(lidR)
#' path = system.file('extdata', 'tree_0744.laz', package='tReeTraits')
#' las = clean_las(readLAS(path))
#' ht = get_height(las)
#' dbh = get_dbh(las)
#' wid = get_width(las)[1]
#' cbh = get_crown_base(las)
#' basics_diagnostic_plot(las, height = ht, cbh = cbh,
#'                      crown_width = wid, dbh = dbh)
#' @export
basics_diagnostic_plot = function(las, height, cbh, crown_width, dbh, res=0.1) {
  las_thin = lidR::decimate_points(las, lidR::random_per_voxel(res=res))
  marker_df = data.frame(
    x = c(crown_width[1]/-2, crown_width[1]/2, 0, 0, min(las$X)+2, min(las$X)+2, min(las$X)+3, min(las$X)+3, dbh/-2, dbh/2),
    y = c(height-(height-cbh)/2, height - (height-cbh)/2, min(las$Z), cbh, min(las$Z), height, min(las$Z), cbh, 1.5, 1.5),
    name = sort(rep(letters[1:5],2)),
    color = c(rep('red',4), rep('black', 4), rep('blue',2)))

  crown_met = ggplot2::ggplot() +
    ggplot2::geom_point(data = las_thin@data, ggplot2::aes(x=.data$X, y=.data$Z), size=0.05, color=grDevices::grey(0.4))  +
    ggplot2::coord_fixed() +
    ggplot2::theme_bw() + ggplot2::geom_line(data = marker_df, ggplot2::aes(x=.data$x,y=.data$y,group=.data$name, color=.data$color),
                                             linewidth=1, arrow = ggplot2::arrow(length=ggplot2::unit(.1,"cm"), angle=90, ends="both")) +
    ggplot2::theme(legend.position='none') + ggplot2::labs(x=NULL, y=NULL) +
    ggplot2::theme(#plot.background = ggplot2::element_rect(fill = "black", colour = "black"),
      #panel.background = ggplot2::element_rect(fill = "black", colour = "black"),
      panel.grid = ggplot2::element_blank(),
      legend.position = 'none')
  return(crown_met)
}

#' Diagnostic Plot of Crown Convex Hulls
#'
#' Generates a 2D diagnostic plot to visualize the convex hulls and voxelized hulls
#' of tree crowns in a LAS point cloud. Useful for checking results of crown segmentation.
#'
#' @param las A `LAS` object from the `lidR` package. Must contain a column named `Crown`.
#' @param res Numeric. Resolution for voxelization in `voxel_hull_2D()`. Default is 0.1.
#'
#' @return A `ggplot` object displaying convex hulls (dashed) and voxel hulls (filled).
#' @importFrom lidR filter_poi
#' @importFrom ggplot2 ggplot geom_sf theme_bw theme element_blank
#' @details
#' The function first filters points marked as crown (`Crown == 1`) and then
#' computes both the 2D convex hull and a voxelized 2D hull. The resulting plot
#' overlays the voxel hull in color and the convex hull as a dashed outline.
#'
#' @examples
#' library(lidR)
#' file = system.file('extdata', file='tree_0744.laz', package='tReeTraits')
#' las <- readLAS(file)
#' las <- segment_crown(las)  # adds `Crown` column
#' hull_diagnostic_plot(las)
#' @export
hull_diagnostic_plot <- function(las, res = 0.1) {
  if (!'Crown' %in% colnames(las@data)) {
    stop('LAS object does not contain column called `Crown`. Use `segment_crown()` first.')
  }

  crown <- lidR::filter_poi(las, .data$Crown == 1)
  convex_hull <- convex_hull_2D(crown)
  voxel_hull <- voxel_hull_2D(crown, resolution = res)

  ggplot2::ggplot() +
    ggplot2::geom_sf(data = voxel_hull, fill = 'chartreuse4') +
    ggplot2::geom_sf(data = convex_hull, fill = NA, linewidth = 1, linetype = 'dashed', color = grey(0.2)) +
    ggplot2::theme_bw() +
    ggplot2::theme(panel.grid = ggplot2::element_blank())
}


#' Diagnostic Plot of Tree Taper
#'
#' Generates a diagnostic plot showing the fitted taper of a tree using the Kozak model.
#' This is a simple wrapper around `fit_taper_Kozak()` to extract its plot output.
#'
#' @param qsm A QSM object (e.g., data frame returned by `run_treeqsm()`) containing cylinder information.
#' @param dbh Numeric. Diameter at breast height of the tree, used as input to the taper function.
#'
#' @return A `ggplot` object showing the fitted taper along the tree stem.
#' @details
#' The function calls `fit_taper_Kozak()` with `plot = FALSE` and returns the plot component.
#' This allows quick visualization of the taper without modifying the underlying QSM.
#'
#' @examples
#' path = system.file('extdata', 'tree_0744_qsm.txt', package='tReeTraits')
#' qsm = load_qsm(path)
#' taper_diagnostic_plot(qsm, dbh = 0.25)
#' @export
taper_diagnostic_plot <- function(qsm, dbh) {
  fit_taper_Kozak(qsm, dbh, plot = FALSE)$plot
}

#' Diagnostic Plot of Branch Diameter Distribution
#'
#' Generates a diagnostic plot showing the distribution of branch diameters in a QSM.
#' This is a wrapper around `branch_size_distribution()` which computes branch metrics.
#'
#' @param qsm A QSM object (e.g., data frame returned by `run_treeqsm()`) containing cylinder information.
#'
#' @return A `ggplot` object displaying branch diameter (x-axis) versus total branch volume (y-axis).
#'
#' @details
#' The function calls `branch_size_distribution()` with `plot = FALSE` to compute branch volumes
#' at midpoints of diameter bins, then generates a bar plot showing total volume per diameter bin.
#' @importFrom ggplot2 ggplot aes geom_col theme_bw theme element_blank labs
#' @examples
#' qsm_file = system.file('extdata',"tree_0744_qsm.txt", package='tReeTraits')
#' qsm = load_qsm(qsm_file)
#' branch_distribution_plot(qsm)
#' @export
branch_distribution_plot <- function(qsm) {
  branches <- branch_size_distribution(qsm, plot = FALSE)
  myPlot <- ggplot2::ggplot(branches, ggplot2::aes(x = .data$midpoint, y = .data$volume_mL)) +
    ggplot2::geom_col() +
    ggplot2::theme_bw() +
    ggplot2::theme(panel.grid = ggplot2::element_blank()) +
    ggplot2::labs(x = 'Branch diameter (cm)', y = 'Total volume (mL)')
  return(myPlot)
}



#' Generate a diagnostic plot to assess basic metrics and QSM output
#'
#' @param las `LAS` object from `lidR` package representing
#' the CROWN of a tree. Crowns can be segmented using [segment_crown()]
#' @param qsm  a QSM loaded using `[load_qsm()]`.
#' @param height numeric - tree height, or generated from `get_height()`
#' @param dbh numeric - in cm, tree diameter at breast height, or generated
#' from `get_dbh()`
#' @param crown_width numeric - crown width height, or generated from `get_width()`
#' @param cbh numeric - crown base heigth, or generated from `get_crown_base()`.
#' @param res numeric - resolution of voxelization to speed up plotting
#' @importFrom ggpubr ggarrange
#' @importFrom ggplotify as.ggplot
#' @return A multi-panel `ggplot` object (class `ggarrange`) summarizing
#'   tree structural metrics and QSM diagnostics, suitable for printing
#'   or saving with `ggplot2`.
#' @examples
#' library(lidR)
#' las_file = system.file("extdata", "tree_0744.laz", package="tReeTraits")
#' las = lidR::readLAS(las_file, filter = '-thin_with_voxel 0.1')
#' las = clean_las(las, bole_height=3)
#' height = get_height(las)
#' crown_width = get_width(las)
#' dbh = get_dbh(las, select_n=30)
#' cbh = get_crown_base(las, threshold=0.25, sustain=2)
#' las = segment_crown(las, cbh)
#' qsm_file = system.file("extdata", "tree_0744_qsm.txt", package='tReeTraits')
#' qsm = load_qsm(qsm_file)
#' full_diagnostic_plot(las=las, qsm=qsm, height=height, cbh=cbh, crown_width=crown_width, dbh=dbh)
#' @export
full_diagnostic_plot = function(las, qsm, height, cbh, crown_width, dbh, res=0.1) {
  r = plot_tree(las, plot=FALSE)
  s = basics_diagnostic_plot(las, height, cbh, crown_width, dbh, res)
  t = hull_diagnostic_plot(las, res=res)
  u = ggplotify::as.ggplot(~plot_qsm2d(qsm, rotation=FALSE, scale=50))
  v = taper_diagnostic_plot(qsm, dbh)
  w = branch_distribution_plot(qsm)
  x = ggpubr::ggarrange(r, s, t, widths = c(0.5, 0.2, 0.3), ncol=3, labels=c(NA, 'D', 'E'))
  y = ggpubr::ggarrange(u, v, w, ncol=3, widths= c(0.2, 0.4,0.4), labels=c('F', 'G', 'H'))
  z = ggpubr::ggarrange(x, y, nrow=2)
  return(z)
}


#' Plot QSM Cylinders in 3D
#'
#' Renders a 3D visualization of tree cylinders (from a Quantitative Structure Model, QSM)
#' using actual geometric radii. Each cylinder is drawn between its start and end coordinates
#' with the radius provided in the QSM data.
#'
#' @param qsm A data frame or tibble containing QSM cylinder data with columns:
#'   \code{startX}, \code{startY}, \code{startZ}, \code{endX}, \code{endY}, \code{endZ}, and \code{radius_cyl}.
#' @param bg Background color for the 3D plot. Defaults to \code{"white"}.
#' @param color Cylinder color. Defaults to \code{"black"}.
#' @param alpha Transparency level for cylinders, between 0 (fully transparent) and 1 (fully opaque).
#'   Defaults to \code{0.7}.
#'
#' @details
#' This function uses \pkg{rgl} to draw 3D cylinders representing each segment
#' of a tree model. It is intended for visualizing QSM output such as that
#' produced by \pkg{PyTLidar} or other tree reconstruction algorithms.
#'
#' For large models, rendering may be slow because each cylinder is drawn as
#' a separate mesh. Consider downsampling or filtering before plotting.
#'
#' @return Opens an interactive 3D rgl window with rendered cylinders.
#'   Returns \code{NULL} invisibly.
#' @importFrom rgl open3d bg3d aspect3d cylinder3d shade3d axes3d title3d
#' @return A `ggplot` object combining multiple diagnostic panels.
#' @examples
#' # Load QSM output (example path)
#' qsm_file = system.file('extdata',"tree_0744_qsm.txt", package='tReeTraits')
#' qsm = load_qsm(qsm_file)
#' # Plot with real radii
#'\donttest{
#' plot_qsm3d(qsm, color = "forestgreen", alpha = 0.6)
#' }
#' @importFrom rgl open3d bg3d aspect3d cylinder3d shade3d title3d
#' @importFrom rlang .data
#' @export
plot_qsm3d <- function(qsm, bg='white', color='black', alpha=0.7) {
  # Open new 3D window
  rgl::open3d()
  rgl::bg3d(bg)
  rgl::aspect3d(1, 1, 1)

  # Use lapply for efficiency (draws cylinders directly)
  invisible(apply(qsm, 1, function(row) {
    rgl::shade3d(
      rgl::cylinder3d(
        rbind(
          c(row["startX"], row["startY"], row["startZ"]),
          c(row["endX"], row["endY"], row["endZ"])
        ),
        radius = as.numeric(row["radius_cyl"]),
        sides = 8
      ),
      color = color, alpha = alpha)
  }))
  rgl::axes3d()
  rgl::title3d(xlab = "X", ylab = "Y", zlab = "Z")

}


#' Extract patch diameter parameters from a QSM filename
#'
#' Internal helper function to parse patch diameters (D, DI, DA) from
#' a PyTLidar-generated QSM filename. Falls back to default values if not present.
#'
#' @param filepath Character. Full path to the QSM file.
#' @param default_patch1 Numeric. Default value for D (patch_diam1).
#' @param default_patch2min Numeric. Default value for DI (patch_diam2min).
#' @param default_patch2max Numeric. Default value for DA (patch_diam2max).
#' @importFrom tools file_path_sans_ext
#' @importFrom stringr str_match
#' @return A named list with elements `patch_diam1`,
#' `patch_diam2min`, and `patch_diam2max`.
#' @keywords internal
extract_patch_params <- function(filepath,
                                 default_patch1,
                                 default_patch2min,
                                 default_patch2max) {

  filename <- tools::file_path_sans_ext(basename(filepath))

  # Extract D (patch1)
  patch1 <- as.numeric(stringr::str_match(filename, "D([0-9.]+)")[,2])
  if (is.na(patch1)) patch1 <- default_patch1

  # Extract DI (patch2min)
  patch2min <- as.numeric(stringr::str_match(filename, "DI([0-9.]+)")[,2])
  if (is.na(patch2min)) patch2min <- default_patch2min

  # Extract DA (patch2max)
  patch2max <- as.numeric(stringr::str_match(filename, "DA([0-9.]+)")[,2])
  if (is.na(patch2max)) patch2max <- default_patch2max

  return(list(
    patch_diam1  = patch1,
    patch_diam2min = patch2min,
    patch_diam2max = patch2max
  ))
}
