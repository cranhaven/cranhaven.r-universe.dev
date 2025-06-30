krige_surf <- function(fnc_df, grid = NULL, resample = 100, padding = 1.2, hull = NULL, alpha = 1, new_data = NULL){
  
  if (!inherits(fnc_df, "fnc_df")) {
    stop("'fnc_df' must be a fnc_df object, the output of a call to fnc.dataframe().", call. = FALSE)
  }
  func.names <- attr(fnc_df, "func.names")

  if (is.null(grid)) {
    grid2D <- resample_grid(fnc_df[1:2], resample = resample, padding = padding,
                            hull = hull, alpha = alpha, plot = FALSE)
    
    grid2D <- sp::SpatialPoints(grid2D)
    sp::gridded(grid2D) <- TRUE
  }
  else {
    grid <- check_coords(grid)
    grid2D <- as.data.frame(grid)[1:2]
    grid2D <- sp::SpatialPoints(grid2D)
    sp::gridded(grid2D) <- TRUE
    hull <- attr(grid, "hull")
    alpha <- attr(grid, "alpha")
  }
  
  #Creates a krige of the full grid in grid2D based on the original data
  #in fnc_df for each functional characteristic
  krige_grid <- lapply(func.names, function(i) {
    d <- as.data.frame(fnc_df[c("x", "y", i)])
    sp::coordinates(d) <- 1:2
    suppressWarnings(automap::autoKrige(d, new_data = grid2D))
  })
  names(krige_grid) <- func.names
  
  #Extracts interpolated values from grid krige for each functional characteristic
  #and appends them to grid, scaling to be between 0 and 1
  kriged_fn_df_grid <- as.data.frame(grid2D)
  for (i in func.names) {
    kriged_fn_df_grid[[i]] <- scale.z(krige_grid[[i]]$krige_output@data$var1.pred)
  }
  
  surfaces <- list(fnc_df = fnc_df,
                   autoKrige = krige_grid,
                   dataframes = list(grid = kriged_fn_df_grid))

  class(surfaces) <- "kriged_surfaces"
  attr(surfaces, "info") <- list(
    grid.size = if (!is.null(grid)) c(x = length(unique(grid2D$x)), y = length(unique(grid2D$y)))
                else c(x = resample, y = resample),
    hull = hull,
    alpha = if (!is.null(hull)) alpha else NULL
  )
  
  if (!is.null(new_data)){
    surfaces <- krige_new_data(surfaces, new_data)
  }
  
  return(surfaces)
}

#Add new_data krige to existing kriged_surfaces object in case new_data was 
#omitted in original call.
krige_new_data <- function(x, new_data) {
  if (missing(x) || !inherits(x, "kriged_surfaces")) {
    stop("'x' must be a kriged_surfaces object, the output of a call to krige_surf().", call. = FALSE)
  }
  
  if (missing(new_data)) {
    stop("'new_data' must be supplied.", call. = FALSE)
  }
  
  if (!is.null(x$dataframes$new_data)) {
    warning("'new_data' component already present in 'x'; overwriting.", call. = FALSE)
  }

  new_data <- check_coords(new_data)
  new_data_coords <- sp::SpatialPoints(new_data[1:2])
  
  func.names <- attr(x$fnc_df, "func.names")

  #Creates a krige of the individual creatures in new_data based on the original data
  #in fnc_df
  krige_new_data <- lapply(func.names, function(i) {
    d <- as.data.frame(x$fnc_df[c("x", "y", i)])
    sp::coordinates(d) <- 1:2
    suppressWarnings(automap::autoKrige(d, new_data = new_data_coords))
  })
  names(krige_new_data) <- func.names
  
  #Extracts interpolated values from new_data krige for each functional characteristic
  #and appends them to new_data, scaling to be between 0 and 1
  for (i in func.names) {
    new_data[[i]] <- scale.z(krige_new_data[[i]]$krige_output@data$var1.pred)
  }
  
  x$dataframes$new_data <- new_data
  
  return(x)
}

#Create a hull around input data. Allows for subsampling the total morphospace
resample_grid <- function(coords2D, resample = 100, padding = 1.2, hull = NULL, alpha = 1, plot = FALSE){
  coords2D <- check_coords(coords2D)
  
  if (!is.numeric(padding) || length(padding) != 1 || padding < 1) {
    stop("'padding' must be a single numeric value greater than or equal to 1.", call. = FALSE)
  }
  
  #Create full coordinates grid
  x_range <- range(coords2D[[1]])
  x_min <- x_range[1] - (padding - 1) * diff(x_range)
  x_max <- x_range[2] + (padding - 1) * diff(x_range)
  gridX <- seq(from = x_min, to = x_max,
               length = resample)
  
  y_range <- range(coords2D[[2]])
  y_min <- y_range[1] - (padding - 1) * diff(y_range)
  y_max <- y_range[2] + (padding - 1) * diff(y_range)
  gridY <- seq(from = y_min, to = y_max,
               length = resample)
  
  grid2D <- expand.grid(x = gridX, y = gridY, KEEP.OUT.ATTRS = FALSE)
  
  if(!is.null(hull)){
    
    if(hull=="concaveman"){
      datahull <- concaveman::concaveman(as.matrix(coords2D), concavity = alpha)
      # Restrict full coordinates grid to points in hull
      in_hull <- as.logical(sp::point.in.polygon(grid2D[,1], grid2D[,2], datahull[,1], datahull[,2]))
    }
    
    if(hull == "alphahull"){
      
      datahull <- suppressWarnings(alphahull::ahull(coords2D[[1]], coords2D[[2]], alpha = alpha))
      # Restrict full coordinates grid to points in hull
      in_hull <- alphahull::inahull(datahull, p = as.matrix(grid2D))
    }
    #Compute hull from original data

    if (plot) {
      in_hull_f <- factor(in_hull, levels = c(TRUE, FALSE),
                          labels = c("In hull", "Not in hull"))
      p <- ggplot(grid2D) +
        geom_raster(aes(x = .data$x, y = .data$y, fill = factor(in_hull_f))) +
        coord_fixed(expand = FALSE) +
        scale_fill_discrete(type = c("In hull" = "gray80", "Not in hull" = "gray20"),
                            name = NULL) +
        theme(axis.text = element_blank(),
              axis.ticks = element_blank(),
              axis.title = element_blank(),
              panel.background = element_blank(),
              panel.border = element_rect(fill = NA, color = "black"),
              panel.grid = element_blank(),
              legend.position = "bottom")
      print(p)
    }
    
    grid2D <- grid2D[in_hull,]
    attr(grid2D, "hull") <- hull
    attr(grid2D, "alpha") <- alpha
  }
  else {
    attr(grid2D, "hull") <- NULL
  }
  
  if (!is.null(hull) && plot) {
    return(invisible(grid2D))
  }
  
  return(grid2D)
}

#Check for coordinates dataset
check_coords <- function(coords, varname) {
  if (missing(varname)) {
    varname <- deparse(substitute(coords))
  }
  if (length(coords) == 0 || length(dim(coords)) != 2 || NCOL(coords) < 2) {
    stop(sprintf("'%s' must be a data frame or matrix with at least two columns.", varname), call. = FALSE)
  }
  attrs <- attributes(coords)
  if (!is.data.frame(coords)) {
    coords <- as.data.frame(coords)
    for (i in setdiff(names(attrs), c("class", "names", "col.names", "row.names", "dimnames", "dim"))) {
      attr(coords, i) <- attrs[[i]]
    }
  }
  if (!all(vapply(coords[1:2], is.numeric, logical(1L)))) {
    stop(sprintf("The first two columns of '%s' must contain numeric values containing coordinates.", varname), call. = FALSE)
  }
  if (anyNA(coords)) {
    stop(sprintf("Missing values are not allowed in '%s'.", varname), call. = FALSE)
  }
  
  names(coords)[1:2] <- c("x", "y")
  coords
}

#print kriged_surfaces object
print.kriged_surfaces <- function(x, ...) {
  
  cat("A kriged_surfaces object\n")
  
  cat("- functional characteristics:\n")
  cat("\t", paste(names(x$autoKrige), collapse = ", "), "\n", sep = "")
  
  cat("- surface size:\n")
  cat("\t", paste(attr(x, "info")$grid.size, collapse = " by "), "\n", sep = "")
  if (!is.null(attr(x, "info")$hull)) {
    cat("\t\u03B1-hull applied (\u03B1 = ", round(attr(x, "info")$alpha, 2), ")\n", sep = "")
  }
  
  cat("- original data:\n")
  cat("\t", nrow(x$fnc_df), " rows\n", sep = "")
  
  if (!is.null(x$dataframes$new_data)) {
    cat("- new data:\n")
    cat("\t", nrow(x$dataframes$new_data), " rows\n", sep = "")
  }

  invisible(x)
}