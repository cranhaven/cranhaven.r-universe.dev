#' #' @title reconstruction
#' #'
#' #' @author Johannes De Groeve
#' #' @description reconstruct paleo or present day landscape using a bathymetric model, reference island dataset and a seacurve
#' #'
#' #' @param x data object - bat, ref, cur, cor
#' #' @param topo bathymetric model object or path to dataset
#' #' @param region reference data object
#' #' @param names naming column
#' #' @param buffer apply buffer around region
#' #' @param curve sea curve object
#' #' @param correction tectonic uplift object or path to dataset
#' #' @param units units of topo curve and correction
#' #' @param res_fact resolution factor
#' #' @param biome sea level stand below sea level for which to calculate the sea shelf extent (e.g. -40 to 0; isohypse=40)
#' #' @param noise maximum number of pixels considered as noise
#' #' @param noiserm boolean, whether noise should be removed
#' #' @param fillHoles fill holes in polygons
#' #' @param filename name of exported zipfile
#' #' @param verbose verbose
#' #' @param tempdir root directory where to create temporary datasets
#' #'
#' #' @return a list including the reference dataset and the paleo reconstruction
#' #'
#' #'
#' #' @keywords internal
#' #' @export
#' #'
#' #' @examples
#' #'
#' #'
#' #' \dontrun{
#' #' # ISLAND MODUS
#' #'
#' #' # load data sample
#' #' # sample data
#' #' ryuchyu_bat <- terra::rast(system.file("extdata", "ryuchyu_bat.tif", package = "piac"))
#' #' ryuchyu_ref <- terra::vect(system.file("extdata", "ryuchyu_ref.shp", package = "piac"))
#' #'
#' #' paleo <- reconstruction(x=ryuchyu_bat,y=ryuchyu_ref,curve=lambeck)
#' #'
#' #' # LOAD DATA
#' #' data <- load_data(x='./data_raw/gebco_2021_sub_ice_topo/GEBCO_2021_sub_ice_topo.nc')
#' #'
#' #' # EXPLORE PRESENT
#' #' present <- reconstruction(x=data$bathymetry,y=data$reference)
#' #'
#' #' # EXPLORE PALEO
#' #' paleo <- reconstruction(x=data$bathymetry,y=data$reference,curve=lambeck)
#' #' paleo <- reconstruction(x=data$bathymetry,y=data$reference,curve=cutler)
#' #'
#' #' # SCIALLA MODUS
#' #'
#' #'
#' #' }
#' #'
#' #'
#' reconstruction <- function(x=NULL,
#'                            topo=NULL, # TODO **piac v2** SCIALLA: Test if both path and file work
#'                            region=NULL, # TODO **piac v2** SCIALLA: region is currently just the path to a vector dataset to name the GIF that is made
#'                            names=NULL,
#'                            buffer=NULL,
#'                            units=list(topo = "m", curve = c(names = "yr", value = "m"), correction = "mm/yr"),
#'                            curve=NULL, # TODO **piac v2** SCIALLA: test if temporal sea level Curves work.
#'                            correction=NULL, # TODO **piac v2** SCIALLA: add uplift grid option / RECONSTRUCT: test if it works
#'                            res_fact=0, # BOTH resolution to be used
#'                            filename=NULL, # BOTH name of the root directory or ZIP
#'                            verbose=F, # BOTH print messages
#'                            tempdir=NULL, # BOTH where to store the temporary output
#'                            noise=5, # TODO **piac v2** Test RECONSTRUCTION wrapper function (super-function that does reconstruct or scialla)
#'                            noiserm=TRUE, # TODO **piac v2** SCIALLA: add noise / noiserm
#'                            fillHoles=T,
#'                            biome=NULL # TODO **piac v2** SCIALLA: integrate also isohypse zones above sea level
#'                            #y=NULL, # NOTE **piac v2** RECONSTRUCT: used to name islands, SCIALLA: no functionality currently (originally for GIF naming)
#'                            #e=NULL, # BOTH reconstruct: extent to be reconstructed (xmin,xmax,ymin,ymax). If NULL global reconstruction.
#'                            #metadata=NULL # TODO **piac v1** add metadata json and csv to zip; reconstruct: metadata available but got an error with the first example in the help of the function
#' ){
#'   # install the data sets at first use
#'   # data_download_wrapper()
#' 
#'   if(is.null(biome)){ # USE RECONSTRUCT (island naming modus)
#' 
#'     message('biome is set to 0, all pixels of bathymetry/topography that are above the curve-value (e.g. RSL, UFL) are defined as part of the biome')
#'     # if (!is.null(biome)){
#'     #   message('biome not integrated for islandnaming mode')
#'     # }
#'     if (!is.null(verbose)){
#'       message('verbose not integrated for modus')
#'     }
#'     if (!is.null(tempdir)){
#'       message('tempdir not yet integrated for modus')
#'     }
#' 
#'     r <- reconstruct(x=x,
#'                      topo=topo,
#'                      region=region,
#'                      names=names,
#'                      buffer=buffer,
#'                      curve=curve,
#'                      correction=correction,
#'                      res_level=res_fact,
#'                      units=units,
#'                      noise=noise,
#'                      noiserm=noiserm,
#'                      fillHoles=fillHoles,
#'                      filename=filename,
#'                      #metadata=metadata,
#'                      verbose=verbose,
#'                      tempdir=tempdir)
#'     return(r)
#'   } else {
#'     if (!is.null(noise) | !is.null(noise)){
#'       message('noiserm and noise not yet implemented when multiple biomes are defined')
#'     }
#' 
#'     #if (!is.null(metadata)){
#'       message('metadata creation not yet implemented when multiple biomes are defined')
#'     #}
#'     if (!is.null(correction)){
#'       message('correction not yet implemented when multiple biomes are defined')
#'     }
#' 
#'     # if(!is.null(region)){
#'     # try(if(class(region)[1] != 'character') stop("region is defined by a polygon in biome mode"))
#'     # }
#' 
#'     if(is.null(region)){ # global reconstruction, v will be default (plates)
#'       e <- NULL
#'     } else { # set extent
#'       region <- get_region(region=region,buffer=buffer,name=names)
#'       e <- as.vector(ext(region))
#'     }
#' 
#'     # use a global reconstruction
#'     r <- scialla(r=topo,
#'                  v=region, # only used currently for naming the GIF files
#'                  curve=curve,
#'                  e=e, # e NULL means global reconstruction
#'                  res_fact=res_fact,
#'                  name=basename(filename),
#'                  path=paste0(dirname(filename),'/'),
#'                  mosaic=T,
#'                  ZIP=T,
#'                  GIF=T,
#'                  isohypse=biome,
#'                  tempdir=tempdir,
#'                  verbose=verbose)
#'     return(r)
#'   }
#' 
#' }
#' 
#' 
#' # TODO **piac v2** PALEOBATHYMETRY AND TOPOGRAPHIC METRICS
#' # NOTE we would also need to have a function to compute paleobathymetry and derived
#' # NOTE environmental variables per island or extent. This has been done in a simplified
#' # NOTE manner for Greece. We masked out all cells below sea level from the bathymetric model
#' # NOTE (so no uplift correction). The mask itself is defined including uplift. From the
#' # NOTE paleobathymetry many summary stats can be calculated on topography such as ruggedness etc.
#' # NOTE A script has already been developed but needs to be integrated as a function in current
#' # NOTE version of PIAC.
#' 
#' # TODO **piac v1** DISTANCE
#' # NOTE a function to reconstruct island distance matrix per island-pairs / interconnected
#' # NOTE groups / extent or defined polygon area. Can be used to: (1) calculate a distance curve
#' # NOTE between islands (2) calculate dendrograms. Output should include (TO BE IMPROVED):
#' # NOTE (1) ISLAND MODE: island pair, distance, kyr_bp, original ID in raster, optional
#' # NOTE vector with of interconnected islands (2) EXTENT MODE: list of matrices with island
#' # NOTE distances per kyr, list with extent coordinates, distance, kyr_bp a script has already
#' # NOTE been created to calculate the matrix and used for Sunda, but still needs implementation
#' # NOTE as function.
#' 
#' # TODO **piac v2** DENDROGRAM
#' # NOTE a function that can reconstruct a dendrogram from
#' # NOTE the distance matrix - ask Buntaro filter per extent per island list, per reference island,
#' # NOTE size filter (exclude mini islands)
#' # NOTE **piac v1** a paleo area size reconstruction function for an island or extent filter. We need a
#' # NOTE function to reconstruct island size per island / extent (area is provided, but we can
#' # NOTE simplify output for plotting). Output should include: (1) ISLAND MODE: name of island,
#' # NOTE island size, kyr_bp, original ID in raster, optional interconnected islands vector could
#' # NOTE be useful EXTENT MODE: extent coordinates, surface size, kyr_bp. Working version exists
#' # NOTE for previous version of PIAC and needs some modification
#' 
#' # TODO **piac v2** DATABASE IMPLEMENTATION
#' # NOTE for the time being I think we should refrain from the postgres database implementation.
#' # NOTE Instead, for the time being I think it is better to use local datasets. We can add download functions so
#' # NOTE that users can download the required input datasets in a predefined folder structure. I think the database
#' # NOTE would be great for the future, especially because I would like that we can improve the island reference
#' # NOTE dataset in an interactive manner (people adding missing islands by digitizing them, adding island names
#' # NOTE to the database). but I think we can think about that when we actually have an audience. For the time being
#' # NOTE we could make it work like this that we store the islands on a separate github page and make also a upload
#' # NOTE folder in which users can upload missing islands.
#' 
#' 
#' 
#' 
#' 
#' 
