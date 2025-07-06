  #' Contour Lines to Points
  #'
  #' Get point coordinates and depth values along predetermined contours at a specified density.
  #'
  #' @param object polygon or multipolygon shapefile (.shp) with depths included as an attribute column. Can be an sf or spatVector object.
  #' @param depths character string describing column name of depth attribute
  #' @param geometry character string describing column name of geometries. Default = "geometry"
  #' @param density numeric value describing distance between points in meters, default = 10m
  #' @return dataframe of coordinates and associated depths
  #' @author Tristan Blechinger, Department of Zoology & Physiology, University of Wyoming
  #' @export
  #' @rawNamespace import(terra, except = c(union,intersect, animate))
  #' @import sf
  #' @examples
    #' # load test data
    #' data <- sf::read_sf(system.file("extdata", "example_contour.shp", package = 'rLakeHabitat'))
    #' #run function
    #' contourPoints(data, depths = "Z", geometry = "geometry", density = 50)

contourPoints <- function(object, depths = NULL, geometry = "geometry", density = 10){

  #input data checks
  sf_object <- sf::st_as_sf(object)
  sf_object <- as.data.frame(sf_object)

  if(!any(class(sf_object) %in% c("sf", "data.frame"))){
    stop("input object must be a polygon or multipolygon shapefile")
  }
  if(!inherits(depths, "character"))
    stop("depths must be a character giving the latitude column name")
  if(!inherits(geometry, "character"))
    stop("geometry must be a character giving the depth column name")
  if(depths %in% names(sf_object) == FALSE)
    stop("The value of 'depths' does not appear to be a valid column name")
  if(geometry %in% names(sf_object) == FALSE)
    stop("The value of 'geometry' does not appear to be a valid column name")
  if(!inherits(sf_object[, depths], "numeric"))
    stop("data in depths column is not formatted as numeric")
  if(!inherits(sf_object[, geometry], "sfc"))
    stop("data in geometry column is not formatted as sfc")
  if(!is.numeric(density))
    stop("density must be a numeric value")

  depth.coords <- data.frame()

  #identify column numbers
  j <- as.numeric(base::which(names(sf_object) == depths))
  m <- as.numeric(base::which(names(sf_object) == geometry))

  for (i in 1:nrow(sf_object)) {

    contour <- terra::vect(sf_object[i,m]) #change column number to name from function call
    depth <- as.numeric(sf_object[i,j])[1]

    line_segmented <- terra::densify(contour, interval = density)

    #convert segmented line to points
    points <- terra::as.points(line_segmented)

    #convert points to coordinates with x,y,z values
    coords <- terra::geom(points)
    coords <- as.data.frame(coords)
    coords <- coords[, c("x", "y", "hole")]

    colnames(coords) <- c("x", "y", "z")

    coords$z <- depth

    depth.coords <- base::rbind(depth.coords, coords)
}
return(depth.coords)
}
