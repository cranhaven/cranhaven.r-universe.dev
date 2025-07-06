#' Rarify Depth Data
#'
#' Reduce density of mapped depth data to improve accuracy and computation time.
#'
#' @param outline shapefile outline of a waterbody
#' @param df dataframe of coordinates and depths for a given waterbody
#' @param x character giving name of longitude column
#' @param y character giving name of latitude column
#' @param z character giving name of depth column
#' @param res number describing by how much to increase point resolution, default = 10
#' @param crsUnits character describing CRS units of input outline, either "dd" (decimal degrees) or "m" (meters), default = "dd"
#' @return dataframe of rarified xyz coordinates
#' @author Sean Bertalot & Tristan Blechinger, Department of Zoology & Physiology, University of Wyoming
#' @export
#' @import dplyr
#' @rawNamespace import(terra, except = c(union,intersect, animate))
#' @examples
#' #load test data
#' outline <- terra::vect(system.file("extdata", "example_outline.shp", package = 'rLakeHabitat'))
#' depths <- read.csv(system.file("extdata", "example_depths.csv", package = 'rLakeHabitat'))
#' #run function
#' rarify(outline = outline, df = depths, x = "x", y = "y", z = "z", res = 100, crsUnits = "dd")

rarify <- function(outline, df, x, y, z, res = 10, crsUnits = "dd"){

  #input data checks

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
  if(is.na(res) || is.null(res))
    stop("res must be specified as a numeric value")
  if(!is.numeric(res))
    stop("res must be specified as a numeric value")
  if(!is.null(crsUnits)){
    if(!is.character(crsUnits)){
      stop("crsUnits must be a character (e.g., 'dd', 'm')")
    }
    if(!crsUnits %in% c("m", "dd")){
      stop("crsUnits must be either 'm' or 'dd'")
    }
  }
  if(!is.null(crsUnits) & is.null(res)){
    stop("both crsUnits and res must be specified")
  }
  if(is.null(crsUnits) & !is.null(res)){
    stop("both crsUnits and res must be specified")
  }

  ## Additional Helper Function get_res
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

  if(!is.null(crsUnits) & !is.null(res)){
    disagg.ras <- empty_raster
  }

  #increase resolution by a factor
  if(is.null(crsUnits) & is.null(res)){
    #Creates an empty raster with the extent lake shapefile
    empty.ras <- terra::rast(outline)
    disagg.ras <- terra::disagg(empty.ras, fact = res)
  }

  # take in xyz dataframe and make it spatial dataframe using vect (get CRS from shapefile of lake/reservoir)
  points_unrarified <- terra::vect(df, geom = c(x, y), crs = crs(outline))

  #creates a raster of the shape outline in the grid dissagg.ras
  ras <- terra::rasterize(outline, disagg.ras)

  #masks the raster for the shapefile (everything outside the reservoir = NA)
  grid <- terra::mask(ras, outline)

  #rasterize points using the empty grid we generated using the get_res coords
  rasterized_points <- terra::rasterize(points_unrarified, grid, field = z, fun = "mean")

  #turn back into xyz dataframe
  points_df <- as.data.frame(rasterized_points, xy = TRUE) %>%
    dplyr::rename(z = mean)

  return(points_df)
}
