#' @importFrom sf st_as_sf st_sf 
#' @importFrom dplyr "%>%"
#' @importFrom leaflet leaflet highlightOptions addControl addMarkers addPolygons addTiles fitBounds addCircles labelOptions renderLeaflet leafletOutput clearGroup addLayersControl layersControlOptions hideGroup setView addProviderTiles
#' @importFrom leaflet.extras addDrawToolbar editToolbarOptions selectedPathOptions
#' @importFrom leaftime addTimeline timelineOptions styleOptions
#' @importFrom terra xmin xmax ymin ymax 
#' @importFrom htmlwidgets saveWidget
#' @importFrom geojsonio geojson_json
#' @importFrom htmltools tags HTML 
#' 
#' 
#' 
NULL
#' 
#' 
#' @title exploring
#' 
#' @description explore identification of islands and naming 
#' 
#' @param labs labeling dataset
#' @param aslv reconstructed dataset 
#' @param path NULL; path where to store the output html
#'
#' @return html file
#' 
#'
#' @noRd
#' @keywords internal
#'
#'
exploring <- function(labs, aslv, simplify=TRUE, path=NULL){
  
  isl_style <- reclass_names(labs$name)
  asl_style <- reclass_names(aslv$recname)
  
  col_labs <-data.frame(class=c('^UNKNOWN$','^UNNAMED$','^I$','^N$'),
                             col=c('darkorange4','darkgoldenrod','steelblue','steelblue'),
                             col_locs=c('red','red','red','red'),
                             col_landsat=c('darkgrey','lightgrey',NA,NA),
                             stringsAsFactors = FALSE)
  asl_style$col <- '#80cdc1' # lightgreen
  asl_style$colbor <- '#01665e' # darkgreen
  asl_style$col_locs <- 'black' # black
  isl_style$col <- '#dfc27d' # lightbrown
  isl_style$colbor <- '#8c510a' # darkbrown
  for(i in 1:nrow(col_labs)){
    asl_style[grepl(col_labs[i,'class'], asl_style$class),'col'] <- col_labs[i,'col']
    asl_style[grepl(col_labs[i,'class'], asl_style$class),'colbor'] <- col_labs[i,'col']
    asl_style[grepl(col_labs[i,'class'], asl_style$class),'col_locs'] <- col_labs[i,'col_locs']
    isl_style[grepl(col_labs[i,'class'], isl_style$class),'col'] <- col_labs[i,'col_landsat']
    isl_style[grepl(col_labs[i,'class'], isl_style$class),'colbor'] <- col_labs[i,'col_landsat']
  }
  asl_polys <- suppressWarnings(sf::st_as_sf(aslv))
  isl_polys <- suppressWarnings(sf::st_as_sf(labs))
  asl_peaks <- suppressWarnings(sf::st_as_sf(as.data.frame(aslv),coords=c('x','y'),crs=crs(aslv)))
  isl_peaks <- suppressWarnings(sf::st_as_sf(as.data.frame(labs),coords=c('refx','refy'),crs=crs(labs)))

  if(simplify){
  asl_polys  <- suppressWarnings(sf::st_simplify(asl_polys, preserveTopology = TRUE, dTolerance = 0.001))
  isl_polys  <- suppressWarnings(sf::st_simplify(isl_polys, preserveTopology = TRUE, dTolerance = 0.001))
  }
  
  tag.map.title <- htmltools::tags$style(htmltools::HTML(".leaflet-control.map-title {
                                            position: absolute;
                                            left: 100%;
                                            text-align: left;
                                            padding-left: 10px;
                                            width: 70px;
                                            white-space: nowrap;
                                            bottom:100;
                                            color:rgb(47,79,79);
                                            padding-right: 10px;
                                            font-weight: bold;
                                            font-size: 9.5px;
                                          }
                                        "))
  
  title <- htmltools::tags$div(tag.map.title, htmltools::HTML('&copy; De Groeve, J., Rijsdijk, K.F., Rentier, E.S., Flantua, S.G.A., Norder, S.J. (2025)'))
  
  peakname <- 'locations' 
  leaf <- leaflet::leaflet() %>%
    leaflet::addTiles(urlTemplate="http://server.arcgisonline.com/ArcGIS/rest/services/World_Imagery/MapServer/tile/{z}/{y}/{x}",group="esri.sat") %>%
    # OSM in local language
    leaflet::addTiles(group = "OSM") %>%
    leaflet::addControl(title,  className='map-title') %>% 
    leaflet::addProviderTiles(provider = leaflet::providers$CartoDB.Voyager,group = "carto") %>%
    leaflet::addTiles(urlTemplate='https://server.arcgisonline.com/ArcGIS/rest/services/World_Topo_Map/MapServer/tile/{z}/{y}/{x}',group = "esri.topo") %>%
    leaflet::addTiles(group = 'transparent', options=leaflet::tileOptions(opacity=0)) %>%
    leaflet::addPolygons(data = asl_polys,
                         fillColor =asl_style$col,
                         color=asl_style$colbor,
                         opacity = 0.8,
                         fillOpacity = 0,weight=1,
                         highlightOptions = leaflet::highlightOptions(color = 'white',
                                                                      weight = 1,
                                                                      bringToFront = TRUE),
                         group='reconstruction') %>%
    leaflet::addCircles(data=asl_peaks,
                        popup = asl_style$name,
                        color= asl_style$col_locs,
                        label =  ~as.character(recname),
                        labelOptions = leaflet::labelOptions(noHide = TRUE, textOnly = TRUE), 
                        group=peakname) %>%
    # Layers control
    leaflet::addLayersControl(
      baseGroups = c("carto","esri.sat", "OSM", "esri.topo",'transparent'),
      overlayGroups = c("reconstruction","labs",peakname), #
      options = leaflet::layersControlOptions(collapsed = FALSE)) %>% 
    leaflet::hideGroup(peakname)
  
    if (inherits(isl_polys$geometry, "sfc_POLYGON") | inherits(isl_polys$geometry, "sfc_MULTIPOLYGON") | grepl('POLYGON',sf::st_geometry_type(isl_polys)[1])) {
      leaf <- leaf %>% leaflet::addPolygons(data = sf::st_transform(isl_polys,sf::st_crs(asl_polys)),
                           fillColor =isl_style$col,
                           color=isl_style$colbor,
                           opacity = 0.8,
                           fillOpacity = 0,
                           weight=1,
                           highlightOptions = leaflet::highlightOptions(color = 'white',
                                                                        weight = 1,
                                                                        bringToFront = TRUE),
                           group='labs')
    } else {
      leaf <- leaf %>% leaflet::addCircles(data = sf::st_transform(isl_polys,sf::st_crs(asl_polys)),
                          fillColor =isl_style$col,
                          color=isl_style$colbor,
                          opacity = 0.8,
                          fillOpacity = 0,
                          weight=1,
                          highlightOptions = leaflet::highlightOptions(color = 'white',
                                                                       weight = 1,
                                                                       bringToFront = TRUE),
                          group='labs')      
    }
  
  #message('test')
  #message(leaf)
  #message('test')
  
  return(leaf)
}



#' timelapse
#' @title timelapse 
#' 
#' @description generate a timelapse for a reconstructed island dataset
#' 
#' @param paleostack islandstack generated by make_island_stack
#' @param simplify boolean (TRUE/FALSE) if simplify is TRUE than a smoothed polygon is generated for visualisation purposes
#' @param overlay How many periods overlap in the timelapse
#' @param popup Boolean not working at the moment 
#'
#' @return html file
#'
#' @noRd
#' @keywords internal
#'
#' 
#'
timelapse <- function(paleostack,simplify=TRUE,overlay=2, popup=FALSE){
  # paleostack returned by the function make_tabs
  # simplify = TRUE if polygons plotted on the timelapse should be simplified for visualisation purposes
  # numCores to use for generating the timelapse
  # parallel = Should computations be done in parallel
  # overlay = How many paleo layers overlap in the timelapse
  style <- lapply(paleostack,function(x) reclass_names(x$recname))
  
  colprep <-function(df){
    col_labs <-data.frame(class=c('^UNKNOWN$','^UNNAMED$','^I$','^N$'),
                               col=c('darkorange4','darkgoldenrod','steelblue','steelblue'),
                               col_locs=c('red','red','red','red'),
                               stringsAsFactors = FALSE)
    
    df$col <- '#80cdc1' # lightgreen
    df$colbor <- '#01665e' # darkgreen
    df$col_locs <- 'black' # black
    for(i in 2:nrow(col_labs)){
      df[grepl(col_labs[i,'class'], df$class),'col'] <- col_labs[i,'col']
      df[grepl(col_labs[i,'class'], df$class),'colbor'] <- col_labs[i,'col']
      df[grepl(col_labs[i,'class'], df$class),'col_locs'] <- col_labs[i,'col_locs']
    }
    return(df)
  }
  
  style <-lapply(style, function(x) colprep(x))
  
  # spatial objects
  locs<- lapply(paleostack, function(x) sf::st_as_sf(x,coords=c('x','y'),crs=crs(x)))
  polys<- lapply(paleostack, function(x) sf::st_as_sf(x))
  
  # locs_df <- lapply(locs, function(x) x[,c('paleox','paleoy','paleoname_final')])
  # (today <- Sys.Date())
  # for(i in 1:length(locs_df)){
  #   locs_df[[i]]$start<- format(today - (length(locs_df)-(i)),"%d-%b-%Y")}
  # power_point <- do.call(rbind.data.frame,locs_df)
  # power_point$start <- do.call("as.Date",list(x=power_point$start,form='%d-%b-%Y'))
  
  #extract_period <- as.numeric(sub("^0+", "", names(polys), perl=TRUE))
  extract_period <- as.numeric(gsub('[A-Z]','',names(paleostack)))
  extract_period[grepl('BP',names(paleostack))] <- -extract_period[grepl('BP',names(paleostack))]
  extract_period_scaled <- ((extract_period - min(extract_period)) / (max(extract_period) - min(extract_period)) * (length(extract_period) -1) +1) * (overlay+1)
  #extract_period_timed <- ymd('0000-01-01') + years(extract_period)

  polysdf<- lapply(polys, function(x) sf::st_sf(x))
  #(today <- Sys.Date())
  #today <- as.Date('2000-01-01',format='%Y-%m-%d')
  for(i in 1:length(polysdf)){
    polysdf[[i]] <- sf::st_sf(polysdf[[i]])
    polysdf[[i]]$start <- extract_period_scaled[i]
    #polysdf[[i]]$start <- extract_period_timed[i]
    #polysdf[[i]]$end <- extract_period_timed[i+overlay]
    
    # if(i == 1){
    # polysdf[[i]]$end <- NA 
    # } else {
    # }
    #polysdf[[i]]$start <- lubridate::years(extract_period[i])
  
    # polysdf[[i]]$start<- format(today - (length(polysdf)-(i) ),"%d-%b-%Y")
    
    ### #polysdf[[i]]$start<- format(lubridate::round_date(as.Date('2000-01-01',format='%Y-%m-%d') -  (( (extract_period[i]/100 * 365)/4*3) + ((extract_period[i] * 366)/4 ) - (i*7.33)), unit='year'),"%d%b%Y")
  }
  power_poly <- do.call(rbind.data.frame,polysdf)
  #power_poly$start <- do.call("as.Date",list(x=power_poly$start,form='%d-%b-%Y'))
  
  # set start same as end
  #  adjust however you would like
  #power_point$end <- power_point$start + overlay
  #power_poly$end <- power_poly$start + overlay 
  power_poly$end <- power_poly$start + (overlay) #(overlay)
  #power_poly$end_time <- power_poly$start + extract_period_timed[overlay]
  
  #power_poly$end <- power_poly$start + lubridate::years(extract_period[overlay])
  # use geojsonio to convert our data.frame
  #  to GeoJSON which timeline expects
  #power_geo_point <- geojson_json(power_point,lat='paleoy',lon='paleox',pretty = T)
  power_geo_poly <- geojsonio::geojson_json(power_poly,lat='y',lon='x',pretty = TRUE)
  
  #power_geo_poly <- sf::st_as_text(st_geometry(power_poly),projjson=TRUE,pretty=TRUE)
  # function to remove the timestamp information
  #formatOutputFun <- function() {htmlwidgets::JS("function(date) {return new Date(date).toDateString()}")}
  #formatOutputFun <- function() {htmlwidgets::JS("function(date) {return new Date(date).getFullYear().toString()}")}
  formatOutputFun <- function() {htmlwidgets::JS("function(date) {return 'Timeline'}")}
  
  
  #formatOutputFun <- function() {htmlwidgets::JS("function(num) {var start = new num; return start.toString()}")}
  
  # add labs university
  tag.map.title <- htmltools::tags$style(htmltools::HTML(".leaflet-control.map-title {
                                            position: absolute;
                                            left: 100%;
                                            text-align: left;
                                            padding-left: 10px;
                                            width: 70px;
                                            white-space: nowrap;
                                            bottom:100;
                                            color:rgb(47,79,79);
                                            padding-right: 10px;
                                            font-weight: bold;
                                            font-size: 9.5px;
                                          }
                                        "))
  title <- htmltools::tags$div(tag.map.title, htmltools::HTML('&copy; De Groeve, J., Rijsdijk, K.F., Rentier, E.S., Flantua, S.G.A., Norder, S.J. (2025)'))
  # ibeduva <- 'https://palaeolim.files.wordpress.com/2018/02/ibed-2018-uva-logo.jpg?w=580'
  
  
  
  # addTimeline(
  #   timelineOpts = timelineOptions(
  #     pointToLayer = htmlwidgets::JS(
  #       "
  #       function(data, latlng) {
  #         return L.circleMarker(latlng, {
  #           radius: 10,
  #           color: 'black',
  #           fillColor: 'pink',
  #           fillOpacity: 1
  #         }).bindTooltip(
  #           // data.properties will have the columns from sf in R
  #           'id: ' + data.properties.id + '<br/>start: ' + data.properties.start,
  #           {permanent: true}
  #         ).openTooltip()
  #       }
  #       "
  #     )
  #   )
  # )
  
  
  slideropts <- paste0(" leaftime::sliderOptions(formatOutput = formatOutputFun(),enableKeyboardControls=TRUE,steps=",length(extract_period)*10,") ")
  
  # logo <-"<img src='https://palaeolim.files.wordpress.com/2018/02/ibed-2018-uva-logo.jpg?w=580' style='width:83px;height:83px;'>"
  ibeduva <-"<img src='https://palaeolim.files.wordpress.com/2018/02/ibed-2018-uva-logo.jpg?w=580' style='width:83px;height:83px;'>"
  draw <- " leaflet::addScaleBar(position='bottomright') %>% leaflet::addMeasure(position = 'topleft',primaryLengthUnit='meters',secondaryLengthUnit='kilometers',primaryAreaUnit='sqmeters',secondaryAreaUnit='hectares') %>% leaflet.extras::addDrawToolbar(polylineOptions = FALSE,targetGroup='draw',editOptions = leaflet.extras::editToolbarOptions(selectedPathOptions = leaflet.extras::selectedPathOptions())) %>% "
  #time_leaf <- " leaftime::addTimeline(data = power_geo_poly, timelineOpts = leaftime::timelineOptions(styleOptions = leaftime::styleOptions(radius = 2,color =  '#01665e' ,stroke = TRUE,fill = '#80cdc1',fillColor = '#80cdc1',fillOpacity = NULL)),sliderOpts =formatOutputFun() ) %>% "
  time_leaf <- paste0(" leaftime::addTimeline(data = power_geo_poly, timelineOpts = leaftime::timelineOptions(styleOptions = leaftime::styleOptions(radius = 2,color =  '#01665e' ,stroke = TRUE, fill = '#80cdc1',fillColor = '#80cdc1',fillOpacity = NULL)),sliderOpts =  ",slideropts," ) ")
  #time_leaf <- paste0(" leaftime::addTimeline(data = power_geo_poly, timelineOpts = leaftime::timelineOptions(styleOptions = leaftime::styleOptions(radius = 2,color =  '#01665e' ,stroke = TRUE, fill = '#80cdc1',fillColor = '#80cdc1',fillOpacity = NULL))) ")
  
  
  # LEAFLET LOOP
  if(popup){
    #dd <- dygraphs_leaflet(paleostack = paleostack)
    #c<- paste0("leaf<-leaflet::leaflet() %>% ", draw, " leaflet::addMarkers(data=points_ref, popup=leafpop::popupGraph(dd, type = 'html'),group='island size') %>% leaflet::addControl(html=ibeduva, position ='topright') %>% leaflet::addControl(title,  className='map-title') %>% leaflet::addTiles(urlTemplate='http://server.arcgisonline.com/ArcGIS/rest/services/World_Imagery/MapServer/tile/{z}/{y}/{x}',group='esri.sat') %>% leaflet::addTiles(group = 'OSM') %>% leaflet::addProviderTiles(provider = leaflet::providers$CartoDB.Voyager,group = 'carto') %>% leaflet::addTiles(urlTemplate='https://server.arcgisonline.com/ArcGIS/rest/services/World_Topo_Map/MapServer/tile/{z}/{y}/{x}',group ='esri.topo') %>% leaflet::addTiles(group = 'OSM') %>% leaflet::fitBounds(raster::extent(paleostack$paleostack[[1]])@xmin, raster::extent(paleostack$paleostack[[1]])@ymin, raster::extent(paleostack$paleostack[[1]])@xmax, raster::extent(paleostack$paleostack[[1]])@ymax) %>% ",time_leaf,paste0("leaflet::addPolygons(data=polys[[",1:length(polys),"]],color ='#01665e',stroke = TRUE,fill = '#80cdc1',fillColor = '#80cdc1',fillOpacity = NULL, label =  ~as.character(paleoname_final),labelOptions = leaflet::labelOptions(noHide = TRUE, textOnly = TRUE), group='",extract_period," BP') %>%",collapse=' '),"leaflet::addLayersControl(baseGroups = c('carto','esri.sat', 'OSM', 'esri.topo'),overlayGroups = c('draw','island size',",paste0("'",extract_period," BP'",collapse=','),"),options = leaflet::layersControlOptions(collapsed = FALSE)) %>% leaflet::hideGroup(group=c(",paste0("'",extract_period," BP'",collapse=','),"))",collapse=' ')
  } else {
    c<- paste0("leaf<-leaflet::leaflet() %>% ", draw, " 
               leaflet::addControl(html=ibeduva, position ='topright') %>% 
               leaflet::addControl(title,  className='map-title') %>% 
               leaflet::addTiles(urlTemplate='http://server.arcgisonline.com/ArcGIS/rest/services/World_Imagery/MapServer/tile/{z}/{y}/{x}',group='esri.sat') %>% 
               leaflet::addTiles(group = 'OSM') %>% leaflet::addProviderTiles(provider = leaflet::providers$CartoDB.Voyager,group = 'carto') %>% 
               leaflet::addTiles(urlTemplate='https://server.arcgisonline.com/ArcGIS/rest/services/World_Topo_Map/MapServer/tile/{z}/{y}/{x}',group ='esri.topo') %>% 
               leaflet::addTiles(group = 'transparent', options=leaflet::tileOptions(opacity=0)) %>%
               leaflet::fitBounds(lng1=xmin(paleostack[[1]]), lat1=ymin(paleostack[[1]]),lng2=xmax(paleostack[[1]]), lat2=ymax(paleostack[[1]])) %>% ",
               paste0(" leaflet::addPolygons(data=polys[[",1:length(polys),"]], color ='#01665e', stroke = TRUE,weight=0.4, fill = '#80cdc1', fillColor = '#80cdc1', fillOpacity = NULL, label =  ~as.character(recname), labelOptions = leaflet::labelOptions(noHide = FALSE, textOnly = TRUE), group='",extract_period,"') %>% ",collapse=' ')," 
               leaflet::addLayersControl(baseGroups = c('carto','esri.sat', 'OSM', 'esri.topo','transparent'),overlayGroups = c(",paste0("'",extract_period,"'",collapse=','),",'draw'),options = leaflet::layersControlOptions(collapsed = TRUE)) %>% leaflet::hideGroup(group=c(",paste0("'",extract_period,"'",collapse=','),")) %>% ", time_leaf, collapse=' ')
  }
  leaf <- eval(parse(text = c))
  

  return(leaf)
}



#' explore
#' @title Leaflet map of present and paleo configurations for biogeographic systems
#' 
#' @description generate a timelapse or exploration visualisation for a reconstructed biomes 
#' 
#' @param x tabs. Object of class tabs, after running the reconstruct-function.
#' @param timelapse integer, specifies the speed of the html-animation, the higher the number the slower the animation 
#' @param filename name of the file to save
#'
#' @return html file
#'
#' @export
#'
#' 
#' @inherit reconstruct examples
#' 
explore <- function(x,
                    timelapse=NULL, #TODO add option to edit (select reference polygons that are missing and update the topo - set pixel(s) to 1 m above sealevel)
                    filename=NULL){
  
    if(is.character(x)){
      if(grepl('.rds$|.qs2$',x)){
        x <- import(x)
        #x$recvect <- lapply(x$recvect, function(x) terra::vect(x))
        #x$labs <- terra::vect(x$labs)
      }
    }
    # EXPLORE 
    if(!is.null(timelapse)){
      if(isTRUE(timelapse)){timelapse=1}
      # timelapse 
      paleostack <- x$recvect
      leaf <- timelapse(paleostack, overlay=timelapse, filename)

    } else {
      recon <- x$recvect[[1]]
      labs <- x$labs
      leaf <- exploring(labs=labs,aslv=recon,simplify=FALSE)
    }
    
    # EXPORT 
    if(!is.null(filename)){
      saveWidget(widget=leaf,filename)
    }
    
  return(leaf)
  }


#' @title reclass_names
#' 
#' @description reclass the unnamed / unknown names 
#' 
#' @param islandnames island names 
#'
#' @return style class
#' 
#'
#' @noRd
#' @keywords internal
#'
#'
reclass_names <- function(islandnames){
  style <- data.frame(name=islandnames,class='NAMED')
  style[grepl('^I-',style$name),'class'] <- 'I'
  style[grepl('^N-',style$name),'class'] <- 'N'
  style[grepl('^UNNAMED',style$name),'class'] <- 'UNNAMED'
  style[grepl('^UNKNOWN',style$name),'class'] <- 'UNKNOWN'
  return(style)
}






