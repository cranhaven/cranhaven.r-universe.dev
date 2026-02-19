#' @title Sporades sample dataset including topo, labs, curve and correction
#' 
#' @author Johannes De Groeve
#' 
#' @return List of input sample datasets (topo, labs, curve, correction)
#' 
#' @description Sample dataset for the Sporades Archipelago in Greece
#'
#' @export 
#'
sporades <- function(){
  DIR <- qs2::qs_read(system.file("extdata", "sporades.qs2", package = "tabs"))

  topo <- terra::unwrap(DIR$topo)
  attr(topo,'source') <- 'gebco sporades (sample)'
  correction <- terra::unwrap(DIR$correction)
  attr(correction,'source') <- 'correction sporades (sample)'
  curve <- terra::unwrap(DIR$curve)
  attr(curve, 'source') <- 'st_curve (sample)'
  labs <- terra::vect(DIR$labs)
  attr(labs,'source') <- 'global shoreline vector sporades (sample)'
  
  sampledata <- list(topo=topo,
                     labs=labs,
                     correction=correction,
                     curve=curve)
  
  return(sampledata)
}

