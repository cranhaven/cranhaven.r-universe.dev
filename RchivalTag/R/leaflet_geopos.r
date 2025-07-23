leaflet_geopos <- function(data, ID_label, add_label=NULL, except_label=NULL, collapsedLayers=TRUE,
                           radius=1000, pal, layer_title=ID_label, colorby="date", cb.title, cbpos="bottomright",
                           showScaleBar=TRUE, showSlideBar=FALSE){
  if(missing(cb.title)) cb.title <- stringr::str_to_title(colorby)
  mapID <- paste0("map",round(runif(1,0,10000000),0))
  ID_labels <- c("DeployID","Serial","datetime","speed","prob_lim")
  if(missing(ID_label)) {
    warning("No ID_label defined, setting to DeployID!")
    ID_label <- "DeployID"
  }
  if(!(ID_label %in% names(data))) stop(paste(ID_label,"not in geolocation data. Please revise!"))
  data[[ID_label]] <- as.character(data[[ID_label]]) # required to facilitate label-selection in legend
  # if(missing(except_label)) except_label <- c()
  # except_label <- c(except_label,ID_label)
  
  cmap <- NULL
  data(cmap, package='oceanmap', envir = environment())
  if(missing(pal)){
    pal <- cmap$jet
  }else{
    if(length(pal) == 1){
      pal <- cmap[[pal]]
    }
  }
  
  m <- leaflet::leaflet(data,elementId = mapID) %>%
    leaflet::addTiles(group = "OSM (default)") %>%
    leaflet::addProviderTiles(providers$Esri.OceanBasemap, group = "Esri.OceanBasemap") %>%
    leaflet::addProviderTiles(providers$Esri.WorldImagery, group = "Esri.WorldImagery")
  
  if(colorby != "date"){
    data$ID <- data[[ID_label]]
    data$IDnm <- as.numeric(data$ID)
    cpal <- colorFactor(palette = pal, domain = data$ID, na.color = "white")
    data <- data[order(data$IDnm,data$date,data$datetime),]
    overlayGroups <- ids <- unique(data[[ID_label]])
    data <- .make_labels(data,ID_label="IDnm",add_label=add_label, except_label=except_label)
    labs <- as.list(data$X)
    
    if(is(data,"data.frame")){
      for(id in ids){
        add <- data[which(data[[ID_label]] == id),]
        add <- add[order(add$IDnm),]
        m <- m %>% leaflet::addCircles(lng = add$Lon, lat =add$Lat, color = ~cpal(add$IDnm),
                              label = lapply(labs, shiny::HTML),radius =radius, group = id)
      }
    }
    m <- m %>% 
      leaflet::addLegend(cbpos, pal = cpal, values = as.character(ids), 
                         title = cb.title, opacity = 1) 
    
    ltitle <- "
        function() {
            $('#mapID .leaflet-control-layers-overlays').prepend('<label style=\"text-align:center\">ID_label</label>');
        }
    "
    ltitle <- gsub("ID_label",layer_title,ltitle)
    ltitle <- gsub("mapID",mapID,ltitle)

    m <- m %>%
      htmlwidgets::onRender(ltitle)

  }else{
    data$datenm <- as.numeric(as.Date(data$datetime))
    cpal = leaflet::colorNumeric(palette = pal, domain = data$datenm) 
    data <- .make_labels(data,ID_label=ID_label,add_label=add_label, except_label=except_label)
    labs <- as.list(data$X)
    
    if(showSlideBar) {
      labs <- data$X
      
      if(is(data,"data.frame")){
        data <- SpatialPointsDataFrame(data=data,data[,c("Lon","Lat")])
        data <- sf::st_as_sf(data)
        data <- st_cast(data, "POINT")
        data$time <- data$datetime
      }else{
        data <- sf::st_as_sf(data)
        data$time <- data$datetime
      }
      
      m <- m %>% leaflet.extras2::addTimeslider(data = data, color = ~cpal(data$datenm),
                               # opacity = opac, fillOpacity = opac,
                               # radius = sample(5:15, nrow(data), TRUE),
                               popupOptions = popupOptions(maxWidth = 1000, closeOnClick= F, closeButton = FALSE),
                               popup = labs,
                               options = timesliderOptions(
                                 alwaysShowDate = TRUE,
                                 sameDate = TRUE,
                                 range = TRUE))
      overlayGroups <- character(0)
    }else{
      
      overlayGroups <- ids <- unique(data[[ID_label]])
      
      if(is(data,"SpatialPolygonsDataFrame")){
        for(id in ids){
          add <- data[which(data[[ID_label]] == id),]
          # add <- add[order(add$datetime),]
          add@plotOrder <- order(-as.numeric(add$Ptt))
          m <- m %>% addPolygons(data = add, color = ~cpal(datenm),label = lapply(labs, shiny::HTML), group = id) 
        }
      }
      
      if(is(data,"data.frame")){
        for(id in ids){
          add <- data[which(data[[ID_label]] == id),]
          add <- add[order(add$datetime),]
          m <- m %>% leaflet::addCircles(lng = add$Lon, lat =add$Lat, color = ~cpal(add$datenm),
                                label = lapply(labs, shiny::HTML),radius =radius, group = id)
        }
      }
      
      if(is(data,"SpatialPointsDataFrame")){
        for(id in ids){
          add <- data[which(data[[ID_label]] == id),]
          add <- add[order(add$datetime),]
          m <- m %>% leaflet::addCircles(lng = add$Lon, lat =add$Lat, color = ~cpal(add$datenm),
                                label = lapply(labs, shiny::HTML),radius =radius, group = id)
        }
      }
      
      ltitle <- "
        function() {
            $('#mapID .leaflet-control-layers-overlays').prepend('<label style=\"text-align:center\">ID_label</label>');
        }
    "
      ltitle <- gsub("ID_label",layer_title,ltitle)
      ltitle <- gsub("mapID",mapID,ltitle)
      
      m <- m %>%
        htmlwidgets::onRender(ltitle)
    }
    
    m <- m %>% 
      leaflet::addLegend(cbpos, pal = cpal, values = data$datenm, 
                         title = cb.title, opacity = 1, 
                         labFormat = .myLabelFormat(dates=TRUE)) 
  }
  if(showScaleBar) m <- m %>% leaflet::addScaleBar('bottomright') 
  m <- m %>%
    leaflet::addLayersControl(
      baseGroups = c("OSM (default)", "Esri.OceanBasemap", "Esri.WorldImagery"),
      overlayGroups = overlayGroups,
      options = layersControlOptions(collapsed = collapsedLayers)
    ) 
  
  return(m)
}




.myLabelFormat = function(..., dates=FALSE){
  if(dates){
    function(type = "numeric", cuts){
      as.Date(cuts, origin="1970-01-01")
    }
  }else{
    labelFormat(...)
  }
}


.make_labels <- function(data,ID_label,add_label=NULL,except_label=NULL){
  ids <- unique(c(ID_label,"DeployID","Serial","datetime","speed","prob_lim"))
  if(!is.null(add_label)) ids <- c(ids,add_label)
  if(!is.null(except_label)) ids <- ids[which(!(ids %in% except_label))]
  if("speed" %in% names(data)){
    data$speed <- gsub("kilometers","km",data$speed)
    data$speed <- gsub("day","d",data$speed)
    data$speed <- gsub("hour","h",data$speed)
    data$speed <- gsub("meters","m",data$speed)
    data$speed <- gsub("seconds","s",data$speed)
  }
  data$X <- c()
  ids <- ids[which(ids %in% names(data))]
  for(i in ids){
    data$X <- paste0(data$X,'<strong>',i,': </strong>',data[[i]],'<br>')
  }
  return(data)
}

update_leaflet_elementId <- function(map){
  new_map_ID <- paste0("map",round(runif(1,0,10000000),0))
  old_map_ID <- map$elementId
  map$elementId <- new_map_ID
  map$jsHooks$render[[1]]$code <- gsub(old_map_ID,new_map_ID,map$jsHooks$render[[1]]$code)
  return(map)
}