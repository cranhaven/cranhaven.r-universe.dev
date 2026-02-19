#' Evaluate the cleaning of occurrences records
#'
#' This function compare the area occupied by
#' a species before and after pass through the cleaning procedure according to
#' the chosen level of filter.
#' The comparison can be made by measuring area in the geographical and in the
#' environmental space
#'
#' @param occ.cl data frame with occurrence records information already
#'   classified by \code{\link{classify_occ}} function.
#' @param geo.space a SpatialPolygons* or sf object defining the geographical
#'  space
#' @param env.space a SpatialPolygons* or sf object defining the environmental
#'  space. Use the \code{\link{define_env_space}} for create this object.
#'  By default \code{env.space = NULL}, hence do not evaluate the cleaning
#'  in the environmental space.
#' @param level.filter a character vector including the levels in
#' 'naturaList_levels' column which filter the occurrence data set.
#' @param r a raster with 2 layers representing the environmental variables. If
#'   \code{env.space = NULL}, it could be a single layer raster, from which
#'   the cell size and extent are extracted to produce the composition matrix.
#' @param species column name of \code{occ.cl} with the species names.
#' @param scientific.name deprecated, use \code{species} instead.
#' @param decimal.longitude column name of \code{occ.cl} longitude in decimal
#'  degrees.
#' @param longitude deprecated, use \code{decimal.longitude} instead
#' @param decimal.latitude column name of \code{occ.cl} latitude in decimal
#'  degrees.
#' @param latitude deprecated, use \code{decimal.latitude} instead
#'
#' @return a list in which:
#'
#' \code{area} data frame remaining area after cleaning proportional to the area
#'   before cleaning. The values vary from 0 to 1. Column named \code{r.geo.area}
#'   is the remaining area for all species in the geographic space and the
#'   \code{r.env.area} in the environmental space.
#'
#' \code{comp} data frame with composition of species in sites (cells from raster
#'   layers) before cleaning (\code{comp$comp$BC}) and after cleaning
#'   (\code{comp$comp$AC}). The number of rows is equal the number of cells in
#'   \code{r}, and number of columns is equal to the number of species in the
#'   \code{occ.cl}.
#'
#' \code{rich} data frame with a single column with the richness of each site
#'
#' \code{site.coords} data frame with site's coordinates. It facilitates to built
#'   raster layers from results using \code{\link[raster]{rasterFromXYZ}}
#'
#' @importFrom methods is
#' @importFrom stats na.omit
#' @importFrom rlang .data
#'
#' @seealso \code{\link{define_env_space}}
#' @export
#'
#' @examples
#' \dontrun{
#'
#' library(sp)
#' library(raster)
#'
#'
#' data("speciaLists") # list of specialists
#' data("cyathea.br") # occurrence dataset
#'
#'
#' # classify
#' occ.cl <- classify_occ(cyathea.br, speciaLists)
#'
#' # delimit the geographic space
#' # land area
#' data("BR")
#'
#'
#' # Transform occurrence data in SpatialPointsDataFrame
#' spdf.occ.cl <- sp::SpatialPoints(occ.cl[, c("decimalLongitude", "decimalLatitude")])
#'
#'
#' # load climate data
#' data("r.temp.prec") # mean temperature and annual precipitation
#' df.temp.prec <- raster::as.data.frame(r.temp.prec)
#'
#' ### Define the environmental space for analysis
#' # this function will create a boundary of available environmental space,
#' # analogous to the continent boundary in the geographical space
#' env.space <- define_env_space(df.temp.prec, buffer.size = 0.05)
#'
#' # filter by year to be consistent with the environmental data
#' occ.class.1970 <-  occ.cl %>%
#'   dplyr::filter(year >= 1970)
#'
#' ### run the evaluation
#' cl.eval <- clean_eval(occ.class.1970,
#'                       env.space = env.space,
#'                       geo.space = BR,
#'                       r = r.temp.prec)
#'
#' #area results
#' head(cl.eval$area)
#'
#'
#' ### richness maps
#' ## it makes sense if there are more than one species
#' rich.before.clean <- raster::rasterFromXYZ(cbind(cl.eval$site.coords,
#'                                                  cl.eval$rich$rich.BC))
#' rich.after.clean <- raster::rasterFromXYZ(cbind(cl.eval$site.coords,
#'                                                 cl.eval$rich$rich.AC))
#'
#' raster::plot(rich.before.clean)
#' raster::plot(rich.after.clean)
#'
#' ### species area map
#' comp.bc <- as.data.frame(cl.eval$comp$comp.BC)
#' comp.ac <- as.data.frame(cl.eval$comp$comp.AC)
#'
#' c.villosa.bc <- raster::rasterFromXYZ(cbind(cl.eval$site.coords,
#'                                             comp.bc$`Cyathea villosa`))
#' c.villosa.ac <- raster::rasterFromXYZ(cbind(cl.eval$site.coords,
#'                                             comp.ac$`Cyathea villosa`))
#'
#' raster::plot(c.villosa.bc)
#' raster::plot(c.villosa.ac)
#' }
#'

clean_eval <- function(
  occ.cl,
  geo.space,
  env.space = NULL,
  level.filter = c("1_det_by_spec"),
  r,
  species = "species",
  decimal.longitude = "decimalLongitude",
  decimal.latitude = "decimalLatitude",
  scientific.name,
  longitude,
  latitude
  ){

  if (!missing(scientific.name)) {
    warning("argument 'scientific.name' is deprecated; please use 'species' instead.",
            call. = FALSE)
    species <- scientific.name
  }

  if (!missing(latitude)) {
    warning("argument 'latitude' is deprecated; please use 'decimal.latitude' instead.",
            call. = FALSE)
    decimal.latitude <- latitude
  }

  if (!missing(longitude)) {
    warning("argument 'longitude' is deprecated; please use 'decimal.longitude' instead.",
            call. = FALSE)
    decimal.longitude <- longitude
  }


# Tests for arguments rules -----------------------------------------------


  # test for a classified occurrence data set
  natList_column <- "naturaList_levels" %in% colnames(occ.cl)
  if(!natList_column){
    stop("'occ.cl' must be classified by 'classify_occ' function")
  }


  # test for polygons as sf object
  # geo.space
  if(is(geo.space, "SpatialPolygons")) {
    geo.space <- sf::st_as_sf(geo.space)
  }
  if(!is(geo.space, "sf"))
    errorCondition("geo.space must be of class: sf or SpatialPolygons*")

  geo.space <- sf::st_geometry(geo.space)
  # env.space


  if(!is.null(env.space)){
    if(is(env.space, "SpatialPolygons")) {
      env.space <- sf::st_as_sf(env.space)
    }

    if(!is(env.space, "sf"))
      errorCondition("env.space must be of class: sf or SpatialPolygons*")

    if(raster::nlayers(r) != 2) errorCondition("raster objetct must have two layers")

  }



# Inicial data ------------------------------------------------------------


  occ.full <- occ.cl %>%
    dplyr::rename("species" = species ,
                  "decimalLongitude" = decimal.longitude,
                  "decimalLatitude" = decimal.latitude) %>%
    dplyr::select(.data$decimalLongitude, .data$decimalLatitude, .data$species) %>%
    dplyr::arrange(species)

  occ.cleaned <- occ.cl %>%
    dplyr::filter(.data$naturaList_levels %in% level.filter) %>%
    dplyr::select(.data$decimalLongitude, .data$decimalLatitude, .data$species) %>%
    dplyr::arrange(species)

  occ.list <- list(occ.full = occ.full,
                   occ.cleaned = occ.cleaned)

  names.sp.full <- as.character(unique(occ.full$species))

  v <- ifelse(is.na(raster::values(r[[1]])),
              NA, 0)

  sitexsp <- matrix(rep(v, length(names.sp.full)),
                    nrow = raster::ncell(r),
                    ncol = length(names.sp.full))

  colnames(sitexsp) <- names.sp.full

# Metrics computaion ------------------------------------------------------


  msg <- c("Calculating metrics before cleaning",
           "Calculating metrics after cleaning")

  res.list <- vector("list", 2)

  for(i in seq_along(occ.list)){
    occ <- occ.list[[i]]
    message(msg[i])

    ## vector for area output

    geo.area <- rep(0, length(names.sp.full))
    names(geo.area) <- names.sp.full
    env.area <- rep(0, length(names.sp.full))
    names(env.area) <- names.sp.full

    names.current <- as.character(unique(occ$species))
    names.sp <- names.sp.full %in% names.current

    # Geographical space
    message("..Step 1 - Geographical space")
    geo.polygon <- lapply(unique(occ$species), function(i){
      x <- dplyr::filter(occ, .data$species == i)
      pt <- sf::st_multipoint(as.matrix(x[,1:2]))

      if(nrow(x) <=3){
        sp.pol <- sf::st_buffer(pt, 0.5)
      }

      if(nrow(x) > 3){
        sp.pol <- sf::st_convex_hull(pt)
        sp.pol <- sf::st_buffer(sp.pol, 0.5)
      }
      geo <- sf::st_geometry(sp.pol)
      sf::st_crs(geo) <- 4326
      suppressMessages(sf::st_intersection(geo, geo.space))
    })

    res.geo.area <- sapply(geo.polygon, function(x) sum(sf::st_area(x)))
    geo.area[names.sp] <- res.geo.area

    # rasters for composition and richness
    geo.raster <- lapply(geo.polygon, function(x){
      x <- sf::st_cast(x, "MULTIPOLYGON")


      fasterize::fasterize(sf::st_sf(a = 1, x),
                r[[1]],
                background = 0)
    })

    stk <- raster::stack(geo.raster)
    msk <- fasterize::fasterize(sf::st_sf(a = 1, geo.space),
                     r[[1]])
    stk <- raster::mask(stk, msk)
    sitexsp[,names.sp] <-  raster::values(stk)

    # Enviromental space


    if(!is.null(env.space)){
      message("..Step 2 - Enviromental space")

      env.std <- vegan::decostand(raster::as.data.frame(r),
                                  "range",  na.rm = T)


      env.polygon <- lapply(unique(occ$species), function(i){
        x <- dplyr::filter(occ, .data$species == i)

        sp.cell <- unique(raster::cellFromXY(r[[1]], x[, 1:2]))
        env.row <- row.names(env.std) %in% sp.cell
        env.xy <- env.std[sp.cell,]

        if(any(is.na(env.xy))){
          warningCondition("There are occurrence points in raster cells without values (NA)")
        }

        pt <- sf::st_multipoint(na.omit(as.matrix(env.xy)))

        if(nrow(x) <=3){
          sp.pol <- sf::st_buffer(pt, 0.025)
        }

        if(nrow(x) > 3){
          sp.pol <- sf::st_convex_hull(pt)
          sp.pol <- sf::st_buffer(sp.pol, 0.025)
        }

        suppressMessages(sf::st_intersection(sf::st_geometry(sp.pol), env.space))
      })

      res.env.area <- sapply(env.polygon, function(x) sum(sf::st_area(x)))
      env.area[names.sp] <- res.env.area


      # results list for the loop
      res.list[[i]] <- list(geo.area = geo.area,
                            env.area = env.area,
                            sitexsp = sitexsp)
    }

    # results list for the loop
    if(is.null(env.space)) {
      res.list[[i]] <- list(geo.area = geo.area,
                            sitexsp = sitexsp)
    }



  }

  message("Preparing outputs")
  site.coords <- raster::coordinates(r)

  remmain.geo.area <- round(res.list[[2]]$geo.area/res.list[[1]]$geo.area, 2)

  if(!is.null(env.space)) {
    remmain.env.area <- round(res.list[[2]]$env.area/res.list[[1]]$env.area, 2)

    area <- data.frame(r.geo.area = remmain.geo.area,
                       r.env.area = remmain.env.area)
  }
  if(is.null(env.space)) {
    area <- data.frame(r.geo.area = remmain.geo.area)
  }



  #composition before cleaning (BC) and after cleaning(AC)
  comp <- list(comp.BC = res.list[[1]]$sitexsp,
               comp.AC = res.list[[2]]$sitexsp)

  rich <- data.frame(rich.BC = rowSums(res.list[[1]]$sitexsp),
                     rich.AC = rowSums(res.list[[2]]$sitexsp))

  results <- list(area = area,
                  comp = comp,
                  rich = rich,
                  site.coords = site.coords)

  message("DONE!")
  return(results)


}

