  #' Interpolate bathymetry
  #'
  #' Generate a bathymetric digital elevation model (DEM) for a given waterbody using either Inverse Distance Weighting or Ordinary Kriging interpolation. For high densities of point data, we recommend rarifying prior to interpolation to improve accuracy and reduce computation time (see rarify function).
  #'
  #' @param outline shapefile outline of a waterbody
  #' @param df dataframe of coordinates and depths for a given waterbody
  #' @param x character giving name of longitude column
  #' @param y character giving name of latitude column
  #' @param z character giving name of depth column
  #' @param zeros logical describing if bounding zeros are needed (FALSE) or provided (TRUE), default = FALSE
  #' @param separation number describing distance between points, units from CRS
  #' @param res number describing desired cell resolution in meters, default = 10
  #' @param crsUnits character describing CRS units of input outline, either "dd" (decimal degrees) or "m" (meters), default = "dd"
  #' @details
    #' If 'res' and 'crsUnits' are specified (recommended), the output raster is returned in the original projection at the specified resolution.
    #' If 'res' and 'crsUnits' are not specified, 'fact' must be defined as a numeric value by which the resolution of the output DEM will be increased (1 = no change). Output raster will be returned in the original projection of the input.
  #' @param method character describing method of interpolation, options include Inverse Distance Weighted ("IDW") or Ordinary Kriging ("OK"). Default = "IDW"
  #' @param fact numeric value describing the factor by which raster resolution should be increased, default = NULL If 'crsUnits' and 'res' are defined, fact = NULL
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
  #' @return DEM of waterbody bathymetry
  #' @details
    #' DEMs generated using OK method will have two layers; the first are the interpolated values and the second are the variances associated with each measurement
    #'
  #' @author Tristan Blechinger & Sean Bertalot, Department of Zoology & Physiology, University of Wyoming
  #' @export
  #' @import dplyr
  #' @rawNamespace import(terra, except = c(union,intersect, animate))
  #' @import gstat
  #' @examples
    #' \donttest{
    #' #load example outline
    #' outline <- terra::vect(system.file("extdata", "example_outline.shp", package = 'rLakeHabitat'))
    #' #load example xyz data
    #' data <- read.csv(system.file("extdata", "example_depths.csv", package = 'rLakeHabitat'))
    #' #run function
    #' interpBathy(outline, data, "x", "y", "z", zeros = FALSE, separation = 10,
    #' crsUnit = "dd", res = 5, method = "IDW", nmax = 4, idp = 2)}

interpBathy <- function(outline, df, x, y, z, zeros = FALSE, separation = NULL, crsUnits = "dd", res = 10, method = "IDW", fact = NULL, nmax = 20, idp = 2, model = "Sph", psill = NULL, range = NULL, nugget = 0, kappa = NULL){

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
  if(!is.logical(zeros))
    stop("zeros must be either 'T', 'F', TRUE, or FALSE")
  if(zeros == F){
    if(is.null(separation) || is.na(separation))
      stop("separation value must be specified")
  }
  if(zeros == T){
    if(!is.null(separation))
      stop("separation must be null if zeros = T")
  }
  if(!is.null(crsUnits)){
    if(!is.character(crsUnits)){
      stop("crsUnits must be a character (e.g., 'dd', 'm')")
    }
    if(!crsUnits %in% c("m", "dd")){
      stop("crsUnits must be either 'm' or 'dd'")
    }
  }
  if(!is.null(res)){
    if(!is.numeric(res)){
      stop("res must be numeric")
    }
  }
  if(is.null(fact) || is.na(fact)){
  if(!is.null(crsUnits) & is.null(res)){
    stop("both crsUnits and res must be specified")
  }
  if(is.null(crsUnits) & !is.null(res)){
    stop("both crsUnits and res must be specified")
  }
  if(!is.null(crsUnits) & !is.null(res)){
    fact = NULL
  }
  }
  if(!is.null(fact)){
    if(!is.numeric(fact))
      stop("fact must be numeric")
      crsUnits <- NULL
      res <- NULL
    }
  if(!method %in% c("IDW", "OK"))
    stop("method misspecified. Please choose either 'IDW' or 'OK'")
  if(method == "IDW"){
    if(!is.numeric(nmax))
      stop("nmax must be numeric")
    if(!is.numeric(idp))
      stop("idp must be numeric")
    if(is.null(nmax) || is.na(nmax))
      stop("nmax must be specified")
    if(is.null(idp) || is.na(idp))
      stop("idp must be specified")
    if(nmax > nrow(df))
      stop("nmax cannot exceed number of observations in df")
    model <- NULL
    psill <- NULL
    range <- NULL
    nugget <- NULL
    kappa <- NULL
  }
  if(method == "OK"){
    if(!is.character(model) || !model %in% c("Sph", "Exp", "Gau", "Mat"))
      stop("model must be character string of either 'Sph', 'Exp', 'Gau', or 'Mat'")
    if(!is.numeric(nmax))
      stop("nmax must be numeric")
    if(nmax > nrow(df))
      stop("nmax cannot exceed number of observations in df")
    if(!is.numeric(nugget))
      stop("nugget must be numeric")
    if(!is.null(range)){
      if(!is.numeric(range))
      stop("range must be numeric")
    }
    else{
      range <- NA
    }
    if(!is.null(psill)){
      if(!is.numeric(psill))
      stop("psill must be numeric")
    }
    else{
      psill <- NA
    }
    if(!is.null(kappa)){
      if(!is.numeric(kappa))
      stop("kappa must be numeric")
    }
    else{
      kappa <- NA
    }
    idp <- NULL
  }
  if(!is.null(crsUnits) & !is.null(res)){
    test_crs <- terra::crs(outline)
    if(is.na(test_crs)){
      stop("CRS of 'outline' is unable to be defined.")
    }
  }


  #### Reproject outline

  # If input unit is meters, forego projecting to UTM crs
  get_res <- function(outline, res, crsUnits) {

    # If input unit is meters, forego projecting to UTM crs
    if (crsUnits == "m") {
      meters <- outline # Set input shapefile to "meters" (in this case it should already be in meter units)
      ext <- terra::ext(meters) # gets extent
      ext_length <- base::abs(ext$xmin - ext$xmax)
      length <- # Finds length of extent in m
        ext_height <- base::abs(ext$ymax - ext$ymin) # Finds height of extent in m

      set_ext_x <- ext_length / res # Divides by res
      set_ext_y <- ext_height / res

      xy <- c(set_ext_x, set_ext_y) # List of length and height
      return(xy)
    }
    else {
      # Read in list of all UTM zones as crs strings
      crs_list <- c(crs("EPSG:32601"), crs("EPSG:32602"), crs("EPSG:32603"), crs("EPSG:32604"), crs("EPSG:32605"),
                    crs("EPSG:32606"), crs("EPSG:32607"), crs("EPSG:32608"), crs("EPSG:32609"), crs("EPSG:32610"),
                    crs("EPSG:32611"), crs("EPSG:32612"), crs("EPSG:32613"), crs("EPSG:32614"), crs("EPSG:32615"),
                    crs("EPSG:32616"), crs("EPSG:32617"), crs("EPSG:32618"), crs("EPSG:32619"), crs("EPSG:32620"),
                    crs("EPSG:32621"), crs("EPSG:32622"), crs("EPSG:32623"), crs("EPSG:32624"), crs("EPSG:32625"),
                    crs("EPSG:32626"), crs("EPSG:32627"), crs("EPSG:32628"), crs("EPSG:32629"), crs("EPSG:32630"),
                    crs("EPSG:32631"), crs("EPSG:32632"), crs("EPSG:32633"), crs("EPSG:32634"), crs("EPSG:32635"),
                    crs("EPSG:32636"), crs("EPSG:32637"), crs("EPSG:32638"), crs("EPSG:32639"), crs("EPSG:32640"),
                    crs("EPSG:32641"), crs("EPSG:32642"), crs("EPSG:32643"), crs("EPSG:32644"), crs("EPSG:32645"),
                    crs("EPSG:32646"), crs("EPSG:32647"), crs("EPSG:32648"), crs("EPSG:32649"), crs("EPSG:32650"),
                    crs("EPSG:32651"), crs("EPSG:32652"), crs("EPSG:32653"), crs("EPSG:32654"), crs("EPSG:32655"),
                    crs("EPSG:32656"), crs("EPSG:32657"), crs("EPSG:32658"), crs("EPSG:32659"), crs("EPSG:32660"),
                    crs("EPSG:32701"), crs("EPSG:32702"), crs("EPSG:32703"), crs("EPSG:32704"), crs("EPSG:32705"),
                    crs("EPSG:32706"), crs("EPSG:32707"), crs("EPSG:32708"), crs("EPSG:32709"), crs("EPSG:32710"),
                    crs("EPSG:32711"), crs("EPSG:32712"), crs("EPSG:32713"), crs("EPSG:32714"), crs("EPSG:32715"),
                    crs("EPSG:32716"), crs("EPSG:32717"), crs("EPSG:32718"), crs("EPSG:32719"), crs("EPSG:32720"),
                    crs("EPSG:32721"), crs("EPSG:32722"), crs("EPSG:32723"), crs("EPSG:32724"), crs("EPSG:32725"),
                    crs("EPSG:32726"), crs("EPSG:32727"), crs("EPSG:32728"), crs("EPSG:32729"), crs("EPSG:32730"),
                    crs("EPSG:32731"), crs("EPSG:32732"), crs("EPSG:32733"), crs("EPSG:32734"), crs("EPSG:32735"),
                    crs("EPSG:32736"), crs("EPSG:32737"), crs("EPSG:32738"), crs("EPSG:32739"), crs("EPSG:32740"),
                    crs("EPSG:32741"), crs("EPSG:32742"), crs("EPSG:32743"), crs("EPSG:32744"), crs("EPSG:32745"),
                    crs("EPSG:32746"), crs("EPSG:32747"), crs("EPSG:32748"), crs("EPSG:32749"), crs("EPSG:32750"),
                    crs("EPSG:32751"), crs("EPSG:32752"), crs("EPSG:32753"), crs("EPSG:32754"), crs("EPSG:32755"),
                    crs("EPSG:32756"), crs("EPSG:32757"), crs("EPSG:32758"), crs("EPSG:32759"), crs("EPSG:32760"))

      # Get the existing CRS of the shapefile
      shapefile_crs <- terra::crs(outline)

      # Function to determine the best UTM CRS
      get_best_utm <- function(outline) {
        # Get centroid of the input vector (assuming it's a SpatVector or SpatRaster)
        centroid <- terra::centroids(outline) # Get a sample point

        # Extract longitude and latitude
        lon <- terra::crds(centroid)[1]
        lat <- terra::crds(centroid)[2]

        # Compute UTM zone
        utm_zone <- base::floor((lon + 180) / 6) + 1

        # Determine Northern or Southern Hemisphere
        hemisphere <- base::ifelse(lat >= 0, 32600, 32700)  # 326xx for North, 327xx for South

        # Construct the EPSG code
        epsg_code <- hemisphere + utm_zone

        # Return the CRS in terra format
        return(terra::crs(paste0("EPSG:", epsg_code)))
      }

      best_crs <- get_best_utm(outline)

      meters <- terra::project(outline, best_crs) # Projects to meter CRS
      ext <- terra::ext(meters) # gets extent
      ext_length <- base::abs(ext$xmin - ext$xmax)
      length <- # Finds length of extent in m
        ext_height <- base::abs(ext$ymax - ext$ymin) # Finds height of extent in m

      set_ext_x <- ext_length / res # Divides by res
      set_ext_y <- ext_height / res

      xy <- c(set_ext_x, set_ext_y) # List of length and height
      return(xy)
    }
  }

  if(!is.null(crsUnits) & !is.null(res)){
  xy <- get_res(outline, res, crsUnits)

  empty_raster <- terra::rast(ext(outline), ncol = xy[1], nrow = xy[2])
  }

  #select and order df columns
  df <- df %>%
    dplyr::select(all_of(c(x, y, z))) %>%
    dplyr::rename(x = x, y = y, z = z)

  #add bounding zeros to dataframe if not included
  if(zeros == F){
    #segment line
    line_segmented <- terra::densify(outline, interval = separation)

    #convert segmented line to points
    points <- terra::as.points(line_segmented)

    #convert points to coordinates with x,y,z values
    coords <- terra::geom(points)
    zeros <- as.data.frame(coords)
    zeros <- zeros %>% dplyr::select(c("x", "y", "hole"))
    colnames(zeros) <- c("x", "y", "z")

    df <- base::rbind(df, zeros)
  }
  else{
    df <- df
  }

  #generate empty raster of waterbody
  #either by specified resolution or a factor

  if(!is.null(crsUnits) & !is.null(res)){
    disagg.ras <- empty_raster
  }


  #increase resolution by a factor
  if(is.null(crsUnits) & is.null(res)){
    #Creates an empty raster with the extent lake shapefile
    empty.ras <- terra::rast(outline)

    #Increases the resolution of the raster by a factor of 200
    if(fact == 1){
      disagg.ras <- empty.ras
    }
    if(fact != 1){
      disagg.ras <- terra::disagg(empty.ras, fact = fact)
    }
  }

  #creates a raster of the shape outline in the grid dissagg.ras
  ras <- terra::rasterize(outline, disagg.ras)

  #masks the raster for the shapefile (everything outside the reservoir = NA)
  grid <- terra::mask(ras, outline)

  #inverse distance weighted interpolation
  if(method == "IDW"){

    #Interpolation function for deriving contours
    gs <- gstat::gstat(formula=z~1,
                locations=~x+y, #Locations correspond to the locations of the sampling sites
                data=df,
                nmax = nmax,# number of neighbors used per point
                set = list(idp = idp))

    #remove NA cells from the grid (interpolate doesn't like NA's)
    grid <- terra::na.omit(grid)
    #Create DEM's for both lakes with the interpolate function and the gstat formula above
    DEM <- terra::interpolate(grid, gs)

    #mask the interpolation so that you reintroduce NA's where there's no water
    mask.na <- terra::init(grid, NA)
    #create a grid with only 0 where the land is
    mask.2 <- terra::init(grid, 0)

    #replaces NA values with the max height
    replace.na <- terra::cover(grid, mask.2, values = NA)
    #fills the lake with NA values
    replace.water <- terra::cover(replace.na, mask.na, values = 1)

    #merges the two rasters (one has the perimeter of the lake set to 0, one has the bathymetry)
    final_DEM <- terra::merge(replace.water, DEM)

    #isolate layer 1, bathymetry
    final_DEM <- final_DEM[[1]]

    #remove all external zeros from original lake shape - may not be necessary but makes the plot easier/cleaner
    final_DEM <- terra::mask(final_DEM, outline)

    return(final_DEM)
  }

  #thin plate spline interpolation
  if(method == "OK"){

    #run and save variogram output
    if(is.na(psill) || is.na(range) || is.na(kappa)){
      vgram <- gstat::variogram(z~1, locations = ~x+y, data = df)
      gramParam <- gstat::fit.variogram(vgram, model = vgm(model = model))
    }
    if(is.na(psill)){
      psill <- gramParam$psill
    }
    if(is.na(range)){
      range <- gramParam$range
    }
    if(is.na(kappa)){
      kappa <- gramParam$kappa
    }

    #define bathymetry model
    gs <- gstat::gstat(formula=z~1,
            locations=~x+y,
            data=df,
            model = vgm(model = model, range = range, nugget = nugget, nmax = nmax, psill = psill, kappa = kappa, fit.kappa = T))

    #remove NA cells from the grid (interpolate doesn't like NA's)
    grid <- terra::na.omit(grid)
    #Create DEM's for both lakes with the interpolate function and the gstat formula above
    DEM <- terra::interpolate(grid, gs)

    #mask the interpolation so that you reintroduce NA's where there's no water
    mask.na <- terra::init(grid, NA)
    #create a grid with only 0 where the land is
    mask.2 <- terra::init(grid, 0)

    #replaces NA values with the max height
    replace.na <- terra::cover(grid, mask.2, values = NA)
    #fills the lake with NA values
    replace.water <- terra::cover(replace.na, mask.na, values = 1)

    #get error raster
    error <- DEM[[2]]

    #merges the two rasters (one has the perimeter of the lake set to 0, one has the bathymetry)
    final_DEM <- terra::merge(replace.water, DEM)
    final_DEM_error <- terra::merge(replace.water, error)

    #isolate layer 1, bathymetry
    final_DEM <- final_DEM[[1]]
    final_DEM_error <- final_DEM_error[[1]]

    #remove all external zeros from original lake shape - may not be necessary but makes the plot easier/cleaner
    final_DEM <- terra::mask(final_DEM, outline)
    final_DEM_error <- terra::mask(final_DEM_error, outline)
    final_stack <- list(final_DEM, final_DEM_error)

    return(final_stack)
  }
}
