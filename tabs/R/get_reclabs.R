#' @importFrom terra crs values as.data.frame
#' @importFrom dplyr "%>%" mutate arrange group_by summarise ungroup
#' @importFrom sf st_as_sf st_join sf_use_s2 st_intersects st_union
#' @importFrom jsonlite toJSON
#'
#'
NULL
#'
#' @title get_labs_step_a 
#' 
#' @author Johannes De Groeve
#' 
#' @description label polygons using labs dataset and noise parameter
#' 
#' @return SpatVector
#'
#' @param labs labeling dataset
#' @param aslv polygon dataset 
#' @param noise number of cells considered as noise
#' @param reference whether island naming is based on a reference dataset
#' @param verbose boolean. FALSE: No messages are printed. TRUE: Standard verbose mode, providing progress bar. 2: Very verbose mode, displaying detailed information. 
#' 
#' @noRd
#' 
#' @keywords internal
#' 
#'
#'
#'
get_labs_step_a <- function(labs, aslv, noise=5,reference=TRUE,verbose=FALSE){
  #message('assign names to features')

  #### CREATE SF OBJECTS ####
  
  # LABS 
  # create sf points from spatvect polygons 
  islandsp_sf <- sf::st_as_sf(terra::as.data.frame(labs, geom='WKT'), coords=c('refx','refy'), crs=terra::crs(aslv))
  islandsp_sf$unique_id <- NULL
  islandsp_sf$id <- gsub("\\D+",' ',islandsp_sf$uniquename) #NULL
  # create sf polygons from spatvect polygons
  labs_sf <- sf::st_as_sf(terra::as.data.frame(labs, geom='WKT'), wkt="geometry", crs=terra::crs(aslv))
  labs_sf$id <- gsub("\\D+",' ',labs_sf$uniquename) #NULL
  
  # RECONSTRUCTION 
  # create sf points from spatvect polygons 
  aslp_sf <- sf::st_as_sf(terra::as.data.frame(aslv, geom='WKT'), coords=c('x','y'), crs=terra::crs(aslv))
  # create sf polygons from spatvect polygons
  aslv_sf <- sf::st_as_sf(terra::as.data.frame(aslv, geom='WKT'), wkt="geometry", crs=terra::crs(aslv))
  sf::sf_use_s2(FALSE)
  
  # island naming (present day, intersect with highest points, order by island size)
  
  #### INTERSECT RECPOLS & REFPTS ####
  
  # intersection between reconstruction polygons and labeling points ordered by z 
  join_ps <- suppressMessages(sf::st_join(aslv_sf,islandsp_sf,join=st_intersects, left=TRUE))
  join_ps <- join_ps[order(-join_ps$unique_id, -join_ps$refz),] #n
  row.names(join_ps) <- NULL
  
  ### LIST unique names (uniquename) of labeling points that intersect with a reconstruction polygon 
  r <- unlist(lapply(split(join_ps, join_ps$unique_id), function(x) paste0(x$uniquename,collapse='_')))
  # ADD label names STRINGS intersecting with the reconstruction polygon 
  aslv$islandstr <- gsub( "\\d+",'',r) # remove all numbers
  if(as.numeric(verbose) > 1){message('all islands intersecting the reconstructed polygon')}
  aslv$islandnstr <- gsub('^ ','',gsub("\\D+",' ',r)) # remove all characters 
  if(as.numeric(verbose) > 1){message('all island numbers intersecting the reconstructed polygon')}
  aslv$islanduniqueidstr <- r # unique id string (combination of character name and numeric name)
  if(as.numeric(verbose) > 1){message('all islands and numbers intersecting the reconstructed polygon')}
  aslv$refnames <- sapply(split(as.data.frame(join_ps)[,c('name','id')],join_ps$unique_id), toJSON)

  ### SINGLE NAME assigned based on the highest labeling point intersecting with a reconstruction polygon
  aslv$islandn <- unlist(lapply(split(join_ps, join_ps$unique_id), function(x) gsub('\\D+','',x$uniquename[1])))
  #pv$islandn <- unlist(lapply(split(join_ps, join_ps$unique_id), function(x) gsub('UNKNOWN[0-9].*','UNKNOWN',x$uniquename[1])))
  if(as.numeric(verbose) > 1){message('highest island intersecting the reconstructed polygon')}
  #aslv$island <- unlist(lapply(split(join_ps, join_ps$unique_id), function(x) str_extract(x$uniquename[1], "\\D+")))
  aslv$island <- unlist(lapply(split(join_ps, join_ps$unique_id), function(x) gsub("\\d+", "", x$uniquename[1])))
  if(as.numeric(verbose) > 1){message('highest island number intersecting the reconstructed polygon')}
  aslv$islanduniqueid <- unlist(lapply(split(join_ps, join_ps$unique_id), function(x) x$uniquename[1]))
  if(as.numeric(verbose) > 1){message('highest island and number intersecting the reconstructed polygon')}
  
  #pv$area_km2 <- unlist(lapply(split(join_ps, join_ps$unique_id), function(x) as.numeric(str_extract(x$area_km2[1], "\\d+"))))
  #message('largest island reference area intersecting the reconstructed polygon')
  
  #### INTERSECT RECPTS & REFPOLS ####
  # island naming based on reconstruction points intersected with reference polygons 
  aslv$uniquename <- suppressMessages(sf::st_join(aslp_sf,labs_sf,join=st_intersects, left=TRUE)$uniquename)
  #pv$area_km2 <- st_join(p_paleo[[i]],islands_sf,join=st_intersects, left=TRUE)$uniquename
  if(as.numeric(verbose) > 1){message('island name based on paleo point intersecting with islandpolygon')}
  #aslv$recid <- str_extract(aslv$uniquename, "\\d+")
  #aslv$recname <- str_extract(aslv$uniquename, "\\D+")
  aslv$recid <- gsub('\\D+','',aslv$uniquename)
  aslv$recname <- gsub('\\d+','',aslv$uniquename)
  aslv$paleouniqueid <- aslv$uniquename
  
  # In case there are polygons which remain unassigned, assign by the highest labeling polygon intersecting with the reconstruction polygon
  aslv[(is.na(aslv$recname) & is.na(aslv$island) == FALSE),'recid'] <- aslv[(is.na(aslv$recname) & is.na(aslv$island) == FALSE),]$islandn
  aslv[(is.na(aslv$recname) & is.na(aslv$island) == FALSE),'paleouniqueid'] <- aslv[(is.na(aslv$recname) & is.na(aslv$island) == FALSE),]$islanduniqueid
  aslv[(is.na(aslv$recname) & is.na(aslv$island) == FALSE),'recname'] <- aslv[(is.na(aslv$recname) & is.na(aslv$island) == FALSE),]$island
  
  #aslv$recname_original <- aslv$recname
  #aslv$recname <- aslv$paleouniqueid
  
  #pv[(is.na(pv$recname) & is.na(pv$island) == FALSE),'area_km2'] <- pv[(is.na(pv$recname) & is.na(pv$island) == FALSE),]$area_km2
  if(as.numeric(verbose) > 1){message('use the name derived by first approach if the latter does not work')}
  if(reference){
    #aslv$period <- 0
    #aslv$curve <- 0 
    aslv[which(is.na(aslv$uniquename) & !aslv$intersects & is.na(aslv$island)),'recname'] <- paste0('S-',aslv$period[1],'-',as.vector(unlist(terra::values(aslv[which(is.na(aslv$uniquename) & !aslv$intersects & is.na(aslv$island)),'unique_id']))))
    aslv[which(aslv$n <= noise & !aslv$intersects),'recname'] <- paste0('N-',aslv$period[1],'-',as.vector(unlist(terra::values(aslv[which(aslv$n <= noise & !aslv$intersects),'unique_id']))))
    aslv[which(aslv$n <= noise & !aslv$intersects),'paleouniquename'] <- paste0('N-',aslv$period[1],'-',as.vector(unlist(terra::values(aslv[which(aslv$n <= noise & !aslv$intersects),'unique_id']))))
    
  }
  
  return(aslv)
}


#' @title get_labs_step_b
#'
#' @author Johannes De Groeve
#' 
#' @description naming of remaining reconstructed polygons based on when they appear for the first time
#' 
#' @param paleov named paleo vector 
#' @param verbose boolean. FALSE: No messages are printed. TRUE: Standard verbose mode, providing progress bar. 2: Very verbose mode, displaying detailed information. 
#' 
#' @return SpatVector
#' 
#' 
#' @noRd
#' @keywords internal
#' 
#'
#'
get_labs_step_b <- function(paleov,verbose=FALSE){
  # NAMING OF THE PALEOISLANDS THAT REMAIN UNNAMED 
  paleov_sf <- lapply(paleov, function(x) sf::st_as_sf(terra::as.data.frame(x, geom='WKT'), wkt="geometry", crs=crs(x))[,1:ncol(x)])
  paleov_sf <- do.call('rbind.data.frame', paleov_sf)
  paleov_sf <- paleov_sf %>%
    group_by(.data$x,.data$y,.data$iso) %>% #,.data$iso
    arrange(.data$x,.data$y,.data$period,.data$iso) %>% # .data$iso
    mutate(paleouniqueid = {if(is.na(.data$paleouniqueid[1])) paste0('S-',.data$period[1],'-',.data$unique_id[1]) else .data$paleouniqueid[1]}, # original recname  = { else .data$islanduniqueid[1]}
           recname = {if(is.na(.data$recname[1])) paste0('S-',.data$period[1],'-',.data$unique_id[1]) else .data$recname[1]},
           recid = {if(is.na(.data$recid[1])) paste0(strrep("9", 12 - nchar(paste0(gsub('[A-Z][A-Z]','',.data$period[1]),.data$unique_id[1]))), paste0(gsub('[A-Z][A-Z]','',.data$period[1]),.data$unique_id[1]))  else .data$recid[1]} #paste0(9,gsub('[A-Z][A-Z]','',.data$period[1]),sprintf("%07d", .data$unique_id[1]))
           
           #number <- 123
           #formatted_number <- paste0(strrep("9", 7 - nchar(number)), number)
           
           #, 
           #paleouniqueid2 = {if(is.na(.data$island[1])) paste0('S-',.data$period[1],'-',.data$unique_id[1]) else .data$paleouniqueid[1]},
           #recname2 = {if(is.na(.data$island[1])) paste0('S-',.data$period[1],'-',.data$unique_id[1]) else .data$recname[1]}
           )  %>% #,.data$iso[1],'-'
    arrange(.data$unique_id)
  
  #### Aggregate polygons which have the same ID ###
  paleov_sf <- suppressMessages(paleov_sf %>%
    dplyr::group_by(.data$period,.data$iso, .data$curve,.data$correction,.data$recname,.data$recid,.data$paleouniqueid) %>%
    dplyr::arrange(-.data$z) %>% # .data$iso
    dplyr::summarise(
      unique_id = .data$unique_id[1], #first(.data$unique_id, order_by=-.data$z), # or any other column you want to take the first value from based on order
      x =  .data$x[1], #first(.data$x, order_by=-.data$z) # or any other column you want to take the first value from based on order
      y =  .data$y[1], #first(.data$y, order_by=-.data$z) # or any other column you want to take the first value from based on order
      z = .data$z[1], #first(.data$z, order_by=-.data$z), # or any other column you want to take the first value from based on order
      area = sum(.data$area),        # sum of areas
      n = sum(.data$n),    # sum of cells 
      geometry = sf::st_union(.data$geometry), # aggregate geometries
      refnames = .data$refnames[1], #first(.data$refnames, na_rm=FALSE, order_by=-.data$z) # or any other column you want to take the first value from based on order
    ) %>% #,.data$iso[1],'-'
    dplyr::arrange(.data$unique_id) %>%
    dplyr::ungroup())
  #aggregated_sf <- split(aggregated_sf, f=aggregated_sf$period)
  #aggregated_sf <- lapply(aggregated_sf, function(x) x[,c('iso','unique_id','area','x','y','n','z','period','curve','recname','paleouniqueid','recid','refnames','geometry')]) #'recname_original',
  
  
  paleov_sf <- split(paleov_sf, f=paleov_sf$period)
  paleov_sf <- lapply(paleov_sf, function(x) x[,c('iso','unique_id','area','x','y','n','z','period','correction','curve','recname','paleouniqueid','recid','refnames','geometry')]) #'recname_original','islandstr','islandnstr',
  
  #### PALEOISLAND-STRING #### 
  if(as.numeric(verbose) == 1) {pb <- pbar(max=length(paleov_sf))}

  paleop_sf <- lapply(paleov_sf, function(x) st_as_sf(terra::as.data.frame(x),coords=c('x','y'),crs=terra::crs(paleov[[1]])))
  for(i in 1:length(paleov_sf)){
    s <- paleov_sf[[i]]
    rr <- list()
    for(j in 1:i){
      p <- paleop_sf[[j]]
      #message('d')
      join_ps <- suppressMessages(st_join(s,p,join=st_intersects, left=TRUE))
      #message('d')
      join_ps <- unique(join_ps[order(-join_ps$unique_id.y, -join_ps$z.y),c('unique_id.x','period.y','period.x','recname.x','recname.y','recid.x','recid.y','paleouniqueid.x','paleouniqueid.y','z.y','area.y')]) #n
      rr[[j]]<-join_ps
      if(as.numeric(verbose) > 1){message('paleov: ',i,' paleop: ',j)}
    }
    test <- do.call('rbind.data.frame',rr)
    vv <- unique(test[order(test$unique_id.x,-test$z.y,-test$area.y),c('unique_id.x','paleouniqueid.y','recid.y','recname.y')])
    vv <- vv[is.na(vv$paleouniqueid.y)==FALSE,]
    vv$geometry <- NULL
    r <- unlist(lapply(split(vv, vv$unique_id.x), function(x) paste0(x$paleouniqueid.y,collapse='_')))
    paleov_sf[[i]]$paleoislandstr <- r
    colnames(vv) <- c('unique_id','paleouniqueid','id','name')
    paleov_sf[[i]]$recnames <- sapply(split(as.data.frame(vv)[,c('name','id')],vv$unique_id), jsonlite::toJSON)
      
      #aslv$refnames <- sapply(split(as.data.frame(join_ps)[,c('name','id')],join_ps$unique_id), toJSON)
    if(as.numeric(verbose) == 1) {update_pbar(pb, i)}
  }
  if(as.numeric(verbose) == 1) {close(pb)}
  paleov <- lapply(paleov_sf,function(x) vect(x))
  
  return(paleov)
}


#' @title get_reclabs
#' 
#' @author Johannes De Groeve
#' 
#' @description reconstruct the labels based on labeling dataset. In a second step polygons are labelled per time step based on their most recent appearance
#' 
#'
#' @param labs labeling dataset
#' @param recvect polygon dataset 
#' @param noise number of cells considered as noise
#' @param verbose boolean. FALSE: No messages are printed. TRUE: Standard verbose mode, providing progress bar. 2: Very verbose mode, displaying detailed information. 
#' 
#' @return SpatVector
#' 
#' @keywords internal
#'
#'
#'
get_reclabs <- function(labs,recvect,noise,verbose){
  
  
  # LABEL 
  if(as.numeric(verbose) > 1) message('label reconstructed polygons')
  if(as.numeric(verbose) == 1) message('2.2. label polygons (step 1)')
  rec_l <- length(recvect)
  if(as.numeric(verbose) == 1) { pb <- pbar(max=rec_l)}
  reclabs <- list()
  #if(!labs$uniquename[1]=='extent000001'){
    
  for(i in 1:rec_l){
    #aslv <- rec[[i]]
    if(rec_l > 1){ reference<-FALSE } else { reference<-TRUE }
    reclabs[[i]] <- get_labs_step_a(labs,recvect[[i]],noise=noise,reference=reference,verbose=verbose)
    if(as.numeric(verbose) == 1) { update_pbar(pb, i)}
  }
  # close the progress bar 
  if(as.numeric(verbose) == 1) { close(pb)}
  if(as.numeric(verbose) == 1) message('2.3. label polygons (step 2)')
  
  reclabs <- get_labs_step_b(reclabs,verbose=verbose)
  #} #else {
  #reclabs <- recvect
  #}
  # Final selection of columns 
  reclabs <- lapply(reclabs, function(x) {
    x <- x[,c('period','curve','correction','iso','unique_id','area','n','x','y','z','recid','recname','recnames','refnames')]
    #x <- terra::makeValid(x)
    return(x)
  })

  # paleov in svc 
  reclabs_names <- names(reclabs)
  reclabs <- svc(reclabs)
  names(reclabs) <- reclabs_names 
  
  return(reclabs)
  }

