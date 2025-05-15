#' @title Categorical Spatial Interpolation
#' @description Create a raster of probability of categorical values 
#'   interpolated across a 2-dimensional space given a set of points where 
#'   each is assigned to one of several classes.
#' 
#' @param data matrix or data.frame containing points and grouping designation.
#' @param x.col,y.col,group.col numbers or characters identifying which columns 
#'   in \code{data} are the x and y values and grouping designation.
#' @param num.grid number of grid cells for k-nearest neighbor interpolation.
#' @param knn number of nearest neighbors to consider for interpolation.
#' @param hull.buffer percent increase of convex hull to use as spatial area to
#'   interpolate over.
#' @param num.cores number of cores to distribute interpolations over.
#' @param num.batches number of batches to divide grid cell interpolations into.
#' 
#' @return A list containing a raster and points of buffered convex hull.
#' 
#' @author Eric Archer \email{eric.archer@@noaa.gov}
#' 
#' @references Adapted from code originally presented in a blog post on 
#'   Categorical Spatial Interpolation by Timo Grossenbacher
#'   \url{https://timogrossenbacher.ch/2018/03/categorical-spatial-interpolation-with-r/}
#' 
#' @examples 
#' \dontrun{
#' iris.mds <- stats::cmdscale(dist(iris[, 1:4]), k = 2)
#' mds.df <- setNames(
#'   cbind(iris.mds, data.frame(iris$Species)),
#'   c("dim1", "dim2", "Species")
#' )
#' 
#' result <- catSpatInterp(
#'   mds.df, x.col = "dim1", y.col = "dim2", group.col = "Species", 
#'   num.grid = 300, knn = 20, hull.buffer = 0.05,
#'   num.cores = 5, num.batches = NULL
#' )
#' 
#' library(ggplot2)
#' ggplot(mapping = aes(dim1, dim2)) +
#'   geom_raster(
#'     aes(fill = Species, alpha = prob), 
#'     data = result$raster
#'   ) +
#'   geom_polygon(data = result$hull.poly, fill = NA, col = "black") +
#'   geom_hline(yintercept = 0, col = "white") +
#'   geom_vline(xintercept = 0, col = "white") +
#'   geom_point(
#'     aes(fill = Species), 
#'     data = mds.df, 
#'     col = "black", 
#'     shape = 21, 
#'     size = 4
#'   ) + 
#'   theme(
#'     axis.ticks = element_blank(),
#'     axis.text = element_blank(),
#'     axis.title = element_blank(),
#'     legend.position = "top",
#'     panel.grid = element_blank(),
#'     panel.background = element_blank()
#'   )
#' }
#' 
#' @export
#' 
catSpatInterp <- function(data, x.col = "x", y.col = "y", group.col = "group",
                          num.grid = 100, knn = 10, hull.buffer = 0.1,
                          num.cores = 1, num.batches = NULL) {
  
  if(is.numeric(x.col)) x.col <- colnames(data)[x.col]
  if(is.numeric(y.col)) y.col <- colnames(data)[y.col]
  if(is.numeric(group.col)) group.col <- colnames(data)[group.col]
  if(!all(c(x.col, y.col, group.col) %in% colnames(data))) {
    stop("'x.col', 'y.col', and 'group.col' must be column names in 'data'")
  }
  
  # create data frame of points
  df <- as.data.frame(data[, c(x.col, y.col, group.col)])
  df <- df[stats::complete.cases(df), ]
  df$group <- as.character(df[[group.col]])
  if(group.col != "group") df[[group.col]] <- NULL
  
  # find convex hull around points and create buffer around that
  pt.hull <- df[grDevices::chull(df[, c(x.col, y.col)]), c(x.col, y.col)]
  pt.hull <- rbind(pt.hull, pt.hull[1, ])
  hull.poly <- sf::st_buffer(
    sf::st_polygon(list(as.matrix(pt.hull))),
    dist = max(
      abs(diff(range(df[[x.col]]))), 
      abs(diff(range(df[[y.col]])))
    ) * hull.buffer
  )
  # create grid that covers buffered hull
  hull.grid <- sf::st_make_grid(hull.poly, n = num.grid)
  
  # transform points to data.frame containing sf coordinates
  pts <- sf::st_as_sf(df, coords = c(x.col, y.col))
  train.df <- stats::setNames(
    cbind(
      pts$group, 
      as.data.frame(sf::st_coordinates(pts))[, 1:2]
    ),
    c("group", x.col, y.col)
  )
  
  # function to compute k-nearest neighbors at given grid points
  # return probability of most likely group
  .computeGrid <- function(grid, train.df, knn) {
    result <- stats::setNames(
      as.data.frame(sf::st_coordinates(grid))[, 1:2],
      c(x.col, y.col)
    )
    group.kknn <- kknn::kknn(
      group ~ ., train = train.df, test = result, 
      kernel = "gaussian", k = knn
    )
    result$group <- stats::fitted(group.kknn)
    result$prob = apply(group.kknn$prob, 1, max)
    result
  }
  
  # distribute k-nearest neighbor probability calculation among batches and cores
  # return sf coordinates of raster
  cl <- setupClusters(num.cores)  
  raster <- tryCatch({
    if(is.null(cl)) { # Don't parallelize if num.cores == 1
      .computeGrid(hull.grid, train.df, knn)
    } else { 
      parallel::clusterEvalQ(cl, require(swfscMisc))
      parallel::clusterExport(cl, c("hull.grid", "train.df", "knn"), environment())
      n <- length(hull.grid)
      if(is.null(num.batches)) num.batches <- ceiling(sqrt(n) / num.cores)
      start.i <- seq(1, n, ceiling(n / num.batches))
      raster.list <- parallel::parApply(
        cl, cbind(start = start.i, end = c(start.i[-1] - 1, n)), 1, 
        function(i, full.grid, train.df, knn) {
          .computeGrid(full.grid[i["start"]:i["end"]], train.df, knn)
        }, 
        full.grid = hull.grid, train.df = train.df, knn = knn
      )
      do.call(rbind, raster.list)
    }
  }, finally = if(!is.null(cl)) parallel::stopCluster(cl) else NULL)
  raster[[group.col]] <- as.character(raster$group)
  if(group.col != "group") raster$group <- NULL
  raster <- sf::st_as_sf(raster, coords = c(x.col, y.col), remove = F)
  
  # return raster clipped by hull polygon and points of hull polygon
  list(
    raster = raster[hull.poly, ], 
    hull.poly = stats::setNames(
      as.data.frame(as.matrix(hull.poly)), 
      c(x.col, y.col)
    )
  )
}