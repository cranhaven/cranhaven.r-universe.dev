#' Create and equal area grid for multiple maps
#' @return A list of sf objects
#' @param places a list of sf object
#' @param titles a character vector of title for the maps
#' @param buffer a numeric vector of buffer values
#' @param map_theme a ggplot theme
#' @import sf
#' @import ggplot2
#' @export st_equal_grid
#' @keywords sf
#' @keywords spatial
#' @keywords map

st_equal_grid <- function(places,titles,buffer,
                     map_theme){
  ## titles is optional
  if(missing(titles)) {
    titles = rep("",length(places))
  } else {
    titles = titles
  }
  ## buffer is optional
  if(missing(buffer)){
    buffer = rep(0,length(places))
  } else {
    buffer = buffer
  }
  # calculates width and height of all maps 
  ranger <- function(x){ 
    box = places[[x]] %>% sf::st_geometry() %>% sf::st_bbox()
    range_x = as.numeric(box[3]-box[1])
    range_y = as.numeric(box[4]-box[2])
    return(c(range_x,range_y))
  }
  
  # calculates centroids 
  centroids <- (sapply(1:length(places), FUN = function(x) places[[x]] %>% sf::st_centroid() %>% 
                        sf::st_geometry()))  
  centroids
  ranges <- sapply(1:length(places), FUN = function(x) ranger(x))
  # calculates maximum widths and heights
  
  x_max <- max(ranges[1,])
  y_max <- max(ranges[2,])
  
  x_padding <- 0.55*x_max
  y_padding <- 0.55*y_max
  
  if(missing(map_theme)){
    ggplot2::theme_set(theme_void() + theme(plot.title = element_text(hjust = 1)))
  } else {
    ggplot2::theme_set(eval(map_theme))
  } 
  
  graph <- function(x){
    bbox <- places[[x]] %>% st_bbox() %>% as.numeric()
    ggplot2::ggplot(places[[x]] %>% sf::st_buffer(dist = buffer[x])) +
      geom_sf() + 
      coord_sf(xlim = c(centroids[[x]][1]-x_padding , 
                        centroids[[x]][1]+x_padding), 
               ylim = c(centroids[[x]][2]-y_padding , 
                        centroids[[x]][2]+y_padding), 
               expand = TRUE) + 
    
      
      # scale_x_continuous(breaks = seq(bbox[1],rounder), round(bbox[3],rounder), x_graticule_spacing) +
      # scale_y_continuous(breaks = seq(bbox[2],rounder), round(bbox[4],rounder), y_graticule_spacing) +
      labs(title = titles[x])
  }
  plot_list <- lapply(X = 1:length(places), FUN = graph)
}
