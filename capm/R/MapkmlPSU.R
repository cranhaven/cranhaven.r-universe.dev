#' Creates *.kml files of a subset of polygons from a polygon shapefile
#' @description Subset the polygons specified in \code{psu} and creates *.kml files of theses polygons.
#' @param shape string with the path of a polygon shapefile or an object of \code{\link{class}} \link[sf]{sf} (see examples).
#' @param psu polygons to subset.
#' @param id column of the *.dbf file with the values to be matched against.
#' @param path \code{\link{class}} character with the path in which maps will be saved. If the path already exists, it will be overwrited.
#' @return *.kml files of the subsetted polygons.
#' @details If there are *.kml files in the working directory, the new created files will overwrite it in case of name matching.
#' 
#' \code{shape} must receive a shapefile with appropriate coordinate reference system, otherwise, \code{MapkmlPSU} report an error.
#' @references Baquero, O. S., Marconcin, S., Rocha, A., & Garcia, R. D. C. M. (2018). Companion animal demography and population management in Pinhais, Brazil. Preventive Veterinary Medicine.
#' 
#' \url{http://oswaldosantos.github.io/capm}
#' @export
#' @examples
#' data("psu_ssu")
#' 
#' # Take a sample of 5 PSU.
#' (selected_psu <- SamplePPS(psu.ssu = psu_ssu, psu = 5))
#'  
#' ## Define shape from shapefile.
#' shp_path <- system.file("extdata/35SEE250GC_SIR.shp", package="capm")
#' # The code above used a shapefile avaliable in the
#' # capm package.
#' # You might want to write a code like:
#' # shp.path  <- 'path_to_the_folder_with_the_shapefile'
#'  
#' # Create *kml files of 10 polygons.
#' \dontrun{
#' MapkmlPSU(shape = shp_path,
#'          psu = selected_psu[, "selected_psu"],
#'          id = "CD_GEOCODI")
#' 
#' ## Define the shape argument as an object x of class sf.
#' x <- read_sf(shp_path)
#' MapkmlPSU(shape = x, psu = selected_psu[, "selected_psu"], id = "CD_GEOCODI")
#' }
#' 
MapkmlPSU <- function(shape = NULL, psu = NULL, id = NULL, path = "./psu_maps/") {
  if (any(class(shape) == 'sf')) {
    tmp <- shape
  } else {
    tmp <- st_read(shape, quiet = TRUE)
  }
  tmp <- st_transform(tmp, 4326)
  tmp2 = NULL
  if (dir.exists(path)) {
    unlink(path, recursive = TRUE)
  }
  dir.create(path)
  path <- normalizePath(path)
  for(i in 1:length(psu)) {
    tmp1 <- tmp[which(as.character(tmp[[id]]) == psu[i]), ]
    if (file.exists(paste(path, eval(psu[i]), '.kml', sep =''))) {
      file.remove(paste(path, eval(psu[i]), '.kml', sep =''))
    }
    st_write(tmp1, dsn = paste(path, "/", eval(psu[i]), '.kml', sep =''), 
             layer = 'selected_psu', driver = 'KML', delete_layer = TRUE,
             quiet = TRUE)
    tmp2[i] <- which(as.character(tmp[[id]]) == psu[i])
  }
  tmp2 <- tmp[tmp2, ]
  if (file.exists(paste0(path, '/all_psu.kml'))) {
    file.remove(paste0(path, '/all_psu.kml'))
  }
  st_write(tmp2, dsn = paste0(path, '/all_psu.kml'), layer = 'all_selected_psu',
           driver = 'KML', delete_layer = TRUE, quiet = TRUE)
  return(cat('\n', 'The maps are in the directory:', '\n\n', path))
}