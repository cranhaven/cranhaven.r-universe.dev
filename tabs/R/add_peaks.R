#' @importFrom terra as.polygons fillHoles disagg expanse extract vect crs merge as.data.frame geom centroids
#' @importFrom dplyr mutate filter arrange group_by n row_number
#' @importFrom rlang .data
#' 
NULL 
#' 
#' @title aslr_to_aslv
#' 
#' @author Johannes De Groeve
#' @description function used to convert a raster into vector with unique polygons
#'
#' @param aslr raster object 
#'
#' @return vect object
#'
#' 
#' @noRd
#' @keywords internal
#'
#' 
aslr_to_aslv <- function(aslr, fillholes=TRUE){
  aslv0 <- suppressMessages(terra::as.polygons(aslr))
  if(fillholes){
  v <- suppressMessages(terra::fillHoles(aslv0))
  } else {
  v <- aslv0
  }
  v <- disagg(v)
  v$unique_id <- 1:nrow(v)
  v$area <- terra::expanse(v)
  v$z <- NULL
  aslv <- v
  return(aslv)
}
#'
#' @title add_peaks
#' @author Johannes De Groeve
#' @description function to add the highest point within a polygon to the vector dataset 
#'
#' @param bat bathymetric model
#' @param vec aslv or reference dataset 
#' @param reference boolean, TRUE/FALSE, reference dataset used as vec, or not. 
#'
#' @return vect object
#'
#' @noRd
#' @keywords internal
#'
#' 
add_peaks <- function(bat, vec, reference=TRUE,verbose=FALSE){ #,centroid=FALSE
  t <- suppressMessages(terra::extract(bat,vec,xy=TRUE))
  
  if(as.numeric(verbose) > 1){message('z values extracted ')}
  aslp <- t %>% 
    group_by(.data$ID) %>%
    mutate(n=n()) %>% #,altitude_mean=mean(t), altitude_min=min(t), altitude_sd=sd(t),altitude_median=median(t)) %>%
    filter(.data$topo == max(.data$topo)) %>%
    filter(row_number()==1) %>%
    arrange(.data$ID,.data$x,.data$y,.data$n) #%>%
    #vect(geom=c('x','y'), crs=crs(vec))
  colnames(aslp)[colnames(aslp) == 'topo'] <- 'z' # elevation?

  if(reference){
    aslp_temp <- suppressMessages(terra::merge(x=terra::as.data.frame(aslp),y=data.frame(ID=1:nrow(vec)),all.y=TRUE, by.x='ID',by.y='ID'))
    aslp_temp$uniquename <- vec$uniquename
    if('area_km2' %in% colnames(vec)){
      aslp_temp$area_km2 <- vec$area_km2
      } else {
      aslp_temp$area_km2 <- terra::expanse(vec,unit='km')
    }
    
    aslp$uniquename <- aslp_temp[is.na(aslp_temp$x)==FALSE,]$uniquename
    aslp$refarea <- aslp_temp[is.na(aslp_temp$x)==FALSE,]$area_km2 * 1000000
    if(as.numeric(verbose) > 1){message('peaks of known islands')}
    vec$refx <- aslp_temp$x
    vec$refy <- aslp_temp$y
    vec$refz <- aslp_temp$z
    vec$refn <- aslp_temp$n
    if(as.numeric(verbose) > 1){message('peaks of known islands added to polygon layer')}
    
    # add centroids for islands which miss a coordinate for the highest point
    if(nrow(vec[which(is.na(vec$refx)),]) > 0){
      if(as.numeric(verbose) > 1){message(paste0(nrow(vec[which(is.na(vec$refx)),]), ' islands without coordinates'))}
      ct <- suppressMessages(terra::geom(terra::centroids(vec[which(is.na(vec$refx)),]), wkt = FALSE))
      vec[which(is.na(vec$refx)),]$refx <- ct[,'x']
      vec[which(is.na(vec$refy)),]$refy <- ct[,'y']
    }
    
  } else {
    # add information to paleov
    vec$x <- aslp$x
    vec$y <- aslp$y
    vec$z <- aslp$z  
    vec$n <- aslp$n
  }
  
  if(as.numeric(verbose) > 1){message('added variables to vec')}
  return(vec)
}
