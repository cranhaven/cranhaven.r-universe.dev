#' @importFrom terra wrap svc
#' @importFrom qs2 qs_read
#' 
#' @title Import a reconstruction as class tabs object
#' 
#' @author Johannes De Groeve
#' @description import TABS object 
#'
#' @param filename character. Path where outputs of the reconstruct-function were exported. Data exported in the following formats can be imported: directory tree, .qs2 and .rds.
#'
#' @return object of class tabs including a list of input (topo, labs, curve, correction) and output (recvect, recrast, recarea) datasets
#'
#' @export
#'
#' @inherit reconstruct examples
#' 
import<- function(filename){
  
if(is.character(filename)){
  if(grepl('.rds$|.qs2$',filename)){
    if(grepl('rds$',filename)) {file <- readRDS(filename)}
    if(grepl('qs2$',filename)) {file <- qs2::qs_read(filename)} 
    file$recvect <- terra::svc(lapply(file$recvect, function(x) terra::vect(x)))
    
    file$recrast <- rast(file$recrast)
    names(file$recvect) <- names(file$recrast)
    file$recarea <- file$recarea
    
    file$labs <- terra::vect(file$labs)
    file$topo <- terra::rast(file$topo)
    # correction
    if(inherits(file$correction,'PackedSpatRaster')){
    file$correction <- terra::rast(file$correction)
    } else {
    file$correction <- file$correction
    }
    # curve 
    if(inherits(file$curve,'PackedSpatRaster')){
    file$curve <- terra::rast(file$curve)
    } else {
    file$curve <- file$curve
    }
    file <- create_tabs_class(file)
    
  } else {
  
    #### OUTPUT ####
    ############# TO BE FIXED BASED ON THE NEW EXPORT STRUCTURE
    if(grepl('.gpkg$',filename)){ vect_name<-'' } else { vect_name<-'/vect.gpkg' }
    
    filename <- base::path.expand(filename)
    content <- gpkg::gpkg_contents(paste0(filename,vect_name))
    tables <- content$table_name
    
    
    # IMPORT RECVECT 
    layers_v_output <- tables[grepl('^recvect[A-Z].*', tables)]
    v_output <- lapply(layers_v_output, function(x) terra::vect(paste0(filename,vect_name),layer=x))#'/output/reconstruction.gpkg'
    periods <- gsub('recvect','',layers_v_output)
    v_output <- terra::svc(v_output) 
    names(v_output) <- periods
    
    # IMPORT RECRAST
    if(grepl('.gpkg$',filename)){ 
      r <- rast(paste0(filename,vect_name))
      layers_r_output <- names(r)[grepl('^recrast.*_1|^recrast.*[0-9][0-9][0-9][0-9][0-9][0-9]$',names(r))]
      r_output <- terra::rast(paste0(filename,vect_name),lyrs=layers_r_output) #'/output/reconstruction.gpkg'
      names(r_output) <- gsub('_[0-9]$','',names(r_output))
    } else {
      layers_r_output <- list.files(pattern='.tif$',paste0(filename,'/recrast'),full.names=TRUE)
      r_output <- terra::rast(layers_r_output)
    }
    # layers_r <- terra::describe(paste0(filename,vect_name),options='subdataset',sds=TRUE)$var #'/output/reconstruction.gpkg'
    # layers_r_output <- layers_r[grep('^rec.*',layers_r)]
    # layers_v_output <- gsub('recrast','recvect',layers_r_output)
    

   # names(r_output) <- rep('iso',length(v_output))
    
    cls <- sf::st_read(paste0(filename,vect_name),layer='iso',quiet=TRUE)
    if(nrow(cls)==1){terra::NAflag(r_output) <- 0}
    for (i in 1:length(names(r_output))){levels(r_output[[i]]) <- cls} # IMPORT ISO LEVELS!!! 
    names(r_output) <- periods
    
    
    #NAflag(r_output) <- NaN
    
    a_output <- sf::st_read(paste0(filename,vect_name),
                        layer='recarea',
                        quiet=TRUE)
    
    # a_output <- read.csv(paste0(filename,'/area_curve.csv'),sep=';',row.names = 'X') #'/output/area_curve.csv'
    
    #### INPUT ####
    
    #### LABS ####
    labs <- terra::vect(paste0(filename,vect_name),'labs') #'/input/input.gpkg'
    #### TOPO ####
    if(grepl('.gpkg$',filename)){ 
    r_input <- terra::rast(paste0(filename,vect_name)) #'/input/input.gpkg'
    topo <- r_input$topo
    names(topo) <- 'topo'
    }else{
    topo <- terra::rast(paste0(filename,'/topo.tif')) #'/input/input.gpkg'
    }
    
    #### FUNCTION: READ CURVE AND CORRECTION ####
    read_curcor_gpkg <- function(curcor){
      if(sort(unique(grepl(curcor,names(r))),decreasing=TRUE)[1]){ # check if there raster curve layers
        curcor_v <- r_input[[grepl(curcor,names(r))]]
        names(curcor_v) <- periods
      } else { 
        layers_v <- suppressWarnings(sf::st_layers(paste0(filename,vect_name))$name)
        
        if(sort(unique(grepl(curcor,layers_v)),decreasing=TRUE)[1]){
          curcor_df <- st_read(paste0(filename,vect_name),
                              layer=curcor,
                              quiet=TRUE)
          curcor_v <- curcor_df$value
          names(curcor_v) <- curcor_df$year_before_after_present
          attr(curcor_v,'source') <- unique(curcor_df$name)
        }
      }
      return(curcor_v)
      
    }
    
    read_curcor <- function(curcor){
      layers_v <- suppressWarnings(sf::st_layers(paste0(filename,vect_name))$name)
      if(sort(unique(grepl(curcor,layers_v)),decreasing=TRUE)[1]){
        curcor_df <- st_read(paste0(filename,vect_name),
                             layer=curcor,
                             quiet=TRUE)
        curcor_v <- curcor_df$value
        names(curcor_v) <- curcor_df$year_before_after_present
        attr(curcor_v,'source') <- unique(curcor_df$name)
      } else {
        # raster 
        layers_r_output <- list.files(paste0(filename,'/',curcor),full.names=TRUE)
        curcor_v <- terra::rast(layers_r_output)
        names(curcor_v) <- gsub('.tif','',gsub(curcor,'',basename(layers_r_output)))
      }
      return(curcor_v)
    }
    
    #### CURVE ####
    if(grepl('.gpkg$',filename)){ 
    curve <- read_curcor_gpkg(curcor='curve')
    } else {
    curve <- read_curcor(curcor='curve')
    }
    
    #### CORRECTION ####
    if(grepl('.gpkg$',filename)){ 
    correction <- read_curcor_gpkg(curcor='correction')
    } else {
    correction <- read_curcor(curcor='correction')
    }
    
    #### METADATA ####
    metadata<- sf::st_read(paste0(filename,vect_name),layer='metadata',quiet=TRUE)$metadata
    
    #### STRUCTURE #### 
    file <- create_tabs_class(list(recvect=v_output,
                 recrast=r_output,
                 recarea=a_output,
                 labs=labs,
                 topo=topo,
                 curve=curve, 
                 correction=correction, 
                 metadata=metadata
                 ))
  }
  }
return(file)
}
  


