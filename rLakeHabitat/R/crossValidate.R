  #' Cross Validate Interpolated Bathymetry
  #'
  #' Obtain residual mean square error (RMSE) from K-fold cross validation of bathymetry interpolation.
  #'
  #' @param outline shapefile outline of a waterbody
  #' @param df dataframe of coordinates and depths for a given waterbody
  #' @param x character giving name of longitude column
  #' @param y character giving name of latitude column
  #' @param z character giving name of depth column
  #' @param zeros logical describing if bounding zeros are needed (FALSE) or provided (TRUE), default = FALSE
  #' @param separation number describing distance between points, units from CRS
  #' @param k numeric value describing the number of folds to test, default = 5
  #' @param res number describing desired cell resolution in meters, default = 5
  #' @param crsUnits character describing CRS units of input outline, either "dd" (decimal degrees) or "m" (meters), default = "dd"
  #' @details
    #' If both 'crsUnit' and 'res' = NULL, the output raster will be in the same CRS and units as the input 'outline' and the resolution will be increased by 'fact' (default = 10). If both 'crsUnit' and 'res' are defined, fact = NULL and the output raster will be projected to the most appropriate UTM zone at the specified resolution.
  #' @param method character describing method of interpolation, options include Inverse Distance Weighted ("IDW") or Ordinary Kriging ("OK"). Default = "IDW"
  #' @param fact numeric value describing the factor by which raster resolution should be increased, default = NULL. If 'crsUnits' and 'res' are defined, fact = NULL
  #' @param nmax numeric value describing number of neighbors used in interpolation, default = 20
  #' @param idp numeric value describing inverse distance power value for IDW interpolation
  #' @param model character describing type of model used in Ordinary Kriging, options include 'Sph', 'Exp', 'Gau', 'Sta', default = 'Sph'
  #' @param psill numeric value describing the partial sill value for OK interpolation, default = NULL
  #' @param range numeric describing distance beyond which there is no spatial correlation in Ordinary Kriging models, default = NULL
  #' @param nugget numeric describing variance at zero distance in Ordinary Kriging models, default = 0
  #' @param kappa numeric value describing model smoothness, default = NULL
  #' @details
    #' For the model argument there are four different methods included here that are supported by gstat::vgm ("Sph", "Exp", "Gau", "Mat").
    #' "Sph" = The default gstat::vgm method. Spherical model characterized by a curve that rises steeply to defined range then flattens, indicates no spatial correlation between points beyond that range.
    #' "Exp" = Exponential model characterized by spatial correlation decaying with distance.
    #' "Gau" = Gaussian model similar to spatial model but with stronger decay at shorter distances.
    #' "Mat" = Matern model
    #' Three parameters (psill, range, kappa) are incorporated from a fitted variogram (default = NULL). If specified in function input, chosen values will overwrite variogram values.
  #'
  #' @return mean RMSE value across k number of folds
  #' @author Tristan Blechinger, Department of Zoology & Physiology, University of Wyoming
  #' @export
  #' @import dplyr
  #' @rawNamespace import(terra, except = c(union,intersect, animate))
  #' @examples
    #' #load example outline
    #' outline <- terra::vect(system.file("extdata", "example_outline.shp", package = 'rLakeHabitat'))
    #' #load example xyz data
    #' data <- read.csv(system.file("extdata", "example_depths.csv", package = 'rLakeHabitat'))
    #' #run function
    #' crossValidate(outline, data, "x", "y", "z", zeros = FALSE, separation = 10, k = 5, crsUnit = "dd",
    #' res = 50, method = "IDW", nmax = 4, idp = 1.5)

crossValidate <- function(outline, df, x, y, z, zeros = FALSE, separation = NULL, k = 5, crsUnits = "dd", res = 5, method = "IDW", fact = NULL, nmax = 20, idp = 2, model = "Sph", psill = NULL, range = NULL, nugget = 0, kappa = NULL){

  #transform outline shapefile into vector
  if(!inherits(outline, "SpatVector")){
    outline <- terra::vect(outline)
  }
  else{
    outline <- outline
  }

  #checks
  if(!inherits(df, "data.frame"))
    stop("df must be a dataframe")
  if(!inherits(x, "character"))
    stop("x must be a character giving the longitude column name")
  if(!inherits(y, "character"))
    stop("y must be a character giving the latitude column name")
  if(!inherits(z, "character"))
    stop("z must be a character giving the depth column name")
  if(x %in% names(df) == FALSE)
    stop("The value of x does not appear to be a valid column name")
  if(y %in% names(df) == FALSE)
    stop("The value of y does not appear to be a valid column name")
  if(z %in% names(df) == FALSE)
    stop("The value of z does not appear to be a valid column name")
  if(!inherits(df[, x], "numeric"))
    stop("data in x column is not formatted as numeric")
  if(!inherits(df[, y], "numeric"))
    stop("data in y column is not formatted as numeric")
  if(!inherits(df[, z], "numeric"))
    stop("data in z column is not formatted as numeric")
  if(!inherits(outline, "SpatVector"))
    stop("outline is not a SpatVector or cannot be transformed")
  if(is.na(k) || is.null(k))
    stop("k must be defined as a numeric value")
  if(!is.numeric(k))
    stop("k must be defined as a numeric value")

  #rest of data checks from interpBathy function

  max_depth <- max(df[[z]])

  #create 5 folds, add to df
  df <- df %>%
    dplyr::mutate(group = dplyr::case_when(between(z, 0, max_depth*.2) ~ 1,
                          dplyr::between(z, max_depth*.2, max_depth*.4) ~ 2,
                          dplyr::between(z, max_depth*.4, max_depth*.6) ~ 3,
                          dplyr::between(z,  max_depth*.6, max_depth*.8) ~ 4,
                          dplyr::between(z, max_depth*.8, max_depth) ~ 5)) %>%
    dplyr::group_by(df$group) %>% ##
    dplyr::mutate(fold = sample(1:k, n(), replace = T))

  k_values <- list()

  #conduct cross val
  for (i in 1:k) {
    testDat <- df %>% dplyr::filter(df$fold == i)
    trainDat <- df %>% dplyr::filter(df$fold != i) %>% ##
      dplyr::mutate(x = as.numeric(x)) %>%
      as.data.frame(df) ##

    dem <- interpBathy(outline, trainDat, x = x, y = y, z = z, zeros = zeros, separation = separation, crsUnits = crsUnits,
                       res = res, fact = fact, method = method, nmax = nmax, idp = idp, model = model, psill = psill,
                       range = range, nugget = nugget, kappa = kappa)

    dem <- dem[[1]]

    preds <- terra::extract(dem, testDat[,1:2], ID=F)
    testDat <- base::cbind(testDat, preds)

    testDat <- testDat %>%
      dplyr::rename(zpred = ncol(testDat))

    testDat <- testDat %>%
      dplyr::mutate(diff = (testDat$z - testDat$zpred)^2) %>% ##
      as.data.frame() ##

    total <- base::sum(testDat$diff, na.rm=T)

    k_values[[i]] <- base::sqrt(total/nrow(testDat))

    names(testDat)[ncol(testDat)] <- "zPred"
  }

  rmse_value <- mean(unlist(k_values))

  return(c("RMSE", rmse_value))
}
