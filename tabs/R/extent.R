#' @importFrom leaflet leafletOutput renderLeaflet addTiles setView layersControlOptions addLayersControl
#' @importFrom leaflet.extras addDrawToolbar selectedPathOptions editToolbarOptions
#' @importFrom mapedit drawFeatures
#' @importFrom utils capture.output
#' @importFrom dplyr "%>%"
#'
NULL
#'
#' @title get_extent
#'
#' @description set an extent of interest from the map by drawing a polygon main function
#'
#' @param overwrite boolean whether to create a new extent
#'
#' @return extent object
#'  
#' @noRd
#' @keywords internal
#' 
#'
get_extent <- function(overwrite=FALSE)
  {
  
  if(isFALSE(options()$tabs.current_extent) | is.null(options()$tabs.current_extent) | overwrite==TRUE){ # is.null(options()$tabs.current_extent)
    # Leaflet
    map <- default_leaflet() # default leaflet 
    #class <- match.arg(class)
  
    #if(!exists('e')){
    # re-run if overwrite is true 
    # as sf
    p <- suppressMessages(suppressWarnings({mapedit::drawFeatures(map=map,editor="leaflet.extras")})) # draw 
    if(is.null(p)) stop('no feature created')
    e <- extent_outputs(p) 
    
    # ex <- terra::ext(p)
    # v <- as.vector(ex)
    # l <- map %>% 
    #   leaflet::fitBounds(lng1=terra::xmin(ex), 
    #                      lat1=terra::ymin(ex), 
    #                      lng2=terra::xmax(ex), 
    #                      lat2=terra::ymax(ex), 
    #                      options = list())
    # 
    # # reset based on new extent 
    # toreset <- options()[c('tabs.current_extent_poly','tabs.current_extent_leaf','tabs.current_extent_vect','tabs.current_extent')] 
    # toreset[c(1:length(toreset))] <- list(p,l,v,ex) 
    # options(toreset)
    # 
    # attr(v,'pol') <- p
    # attr(v,'ext') <- ex
    # attr(v,'leaf') <- l
    # e <<- v
    #} 
    
  } else {
    message('set overwrite to true to update the area of interest')
    v <- options()$tabs.current_extent_vect
    p <- options()$tabs.current_extent_poly
    l <- options()$tabs.current_extent_leaf
    ex <- options()$tabs.current_extent
    attr(v,'pol') <- p
    attr(v,'ext') <- ex
    attr(v,'leaf') <- l
    e <- v
  }
  #return(eval(parse(text=attr(e,'leaf'))))
  return(e)
  }


#' @title default_leaflet
#'
#' @description set an extent of interest from the map by drawing a polygon main function
#'
#' @return leaflet map 
#'
#' @noRd
#' @keywords internal
#'
#'
default_leaflet <- function(){
  map <- leaflet::leaflet() %>%
    leaflet::addTiles(urlTemplate='http://server.arcgisonline.com/ArcGIS/rest/services/World_Imagery/MapServer/tile/{z}/{y}/{x}',group='esri.sat') %>%
    # OSM in local language
    leaflet::addTiles(group = 'OSM') %>%
    # Google satellite
    leaflet::addTiles(urlTemplate='https://server.arcgisonline.com/ArcGIS/rest/services/World_Topo_Map/MapServer/tile/{z}/{y}/{x}',group = 'esri.topo') %>%
    # OSM in english
    leaflet::addTiles(urlTemplate='https://maptiles.p.rapidapi.com/en/map/v1/{z}/{x}/{y}.png?rapidapi-key=YOUR-X-RAPIDAPI-KEY ', group='OSM (Eng.)') %>%
    leaflet::setView(lng = 24, lat = 37, zoom = 6) %>%
    # Layers control
    leaflet::addLayersControl(
      baseGroups = c('esri.sat', 'OSM', 'esri.topo'),
      options = leaflet::layersControlOptions(collapsed = FALSE)
    ) 
  return(map)
}

#' @title extent_outputs
#'
#' @description set an extent of interest from the map by drawing a polygon main function
#'
#' @param p sf or sp object 
#' 
#' @return extent object
#' 
#' 
#' @noRd 
#' @keywords internal
#' 
#'
extent_outputs <- function(p){
  if(class(p)[1]=='SpatVector') p <- st_as_sf(p)
  ex <- terra::ext(p)
  v <- as.vector(ex)
  l <- default_leaflet() %>% 
    leaflet::fitBounds(lng1=terra::xmin(ex), 
                       lat1=terra::ymin(ex), 
                       lng2=terra::xmax(ex), 
                       lat2=terra::ymax(ex), 
                       options = list()) %>% 
    addPolygons(data=p)
  l <- paste(capture.output(dput(l)), collapse = "\n")
  # l <- paste0(default_leaflet(), paste0(' %>% 
  #               leaflet::fitBounds(lng1=terra::xmin(',ex,'), 
  #                                  lat1=terra::ymin(',ex,'), 
  #                                  lng2=terra::xmax(',ex,'), 
  #                                  lat2=terra::ymax(',ex,'), 
  #                                  options = list()) %>% 
  #               addPolygons(data=p)'))
  
  # reset based on new extent 
  toreset <- options()[c('tabs.current_extent_poly','tabs.current_extent_leaf','tabs.current_extent_vect','tabs.current_extent')] 
  toreset[c(1:length(toreset))] <- list(p,l,v,ex) 
  options(toreset)  
  attr(v,'pol') <- p
  attr(v,'ext') <- ex
  attr(v,'leaf') <- l
  e <- v
  #e <<- e
  
  # my_env <- new.env()
  # assign('e', e, envir=my_env) 
  # foo1 <- function(){
  #   e <<- e
  # }
  # foo1()

  
  return(e)
}

.Last <- function(){
  options('tabs.current_extent_poly' = NULL) 
  options('tabs.current_extent_leaf' = NULL) 
  options('tabs.current_extent_vect' = NULL) 
  options('tabs.current_extent' = NULL) 
  }


