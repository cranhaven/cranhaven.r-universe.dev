app_server <- function(input, output, session) {
  
  showNotification(HTML("<b>Loading... please wait.</b>"), id = "waiting.msg", duration = NULL, type = "message") # see 'removeNotification' below
  
  # UI: credit ----
  output$credit <- renderUI({
    txt <- paste("Powered by <a href=github.com/sebastien-plutniak/spatialCatalogueViewer target=_blank>spatialCatalogueViewer</a> v",
                             utils::packageVersion("spatialCatalogueViewer"), sep="")
    div(HTML(txt))
  })
  
  # UI: show areas ----
  
  output$show.areas <- renderUI({
    if(getShinyOption("map.show.areas") %in% c(TRUE, FALSE)){
      checkboxInput("show.areas.box", "Show data coverage areas.", value = getShinyOption("map.show.areas"))
    }
  })
  
  # UI: tab contents ----
  output$tab.contents <- renderUI({
    home.tab <-    tabPanel("Home",  # home ----
                            tags$head(
                              tags$style(HTML(getShinyOption("css"))),
                              tags$script(HTML(getShinyOption("js")))
                            ),
                         fluidRow(
                           column(1),
                           column(3, align="center",
                                  HTML(getShinyOption("text.left")) 
                           ), #end column
                           column(7, align="left",
                                  HTML(getShinyOption("text.top")),
                                  br(),
                                  leaflet::leafletOutput("map", width="100%", height = getShinyOption("map.height")), ## map  ----
                                  fluidRow(
                                    column(2, 
                                           HTML("&thinsp;"),
                                           actionLink("reset.selections", "Reset selections"), 
                                           style="padding:10px;"),
                                    column(4, uiOutput("show.areas")),
                                  ),
                                  fluidRow(
                                    column(8, 
                                           HTML("<b>Map to Table</b>: Draw a rectangle to filter the table with the datasets related to the selected area.<br>
                                                <b>Table to Map</b>: Click on a row to zoom the map on the corresponding item."),
                                            br(),
                                            HTML(getShinyOption("text.bottom"))
                                    ))
                           ) # end column
                         ), #end fluidrow
                         fluidRow(align = "left",
                                  column(1),
                                  column(10,  align = "center",
                                         br(),
                                         DT::dataTableOutput("table", width = "100%"), ##  table output ----
                                         br(),
                                         uiOutput("download.table.button"),
                                         br(),br(),
                                         uiOutput("credit"),
                                         br()
                                  )  # end column
                         )# end fluidrow
                ) # end tabpanel
    
    optional.tabs <- getShinyOption("tabs.contents")
    
    tab.list <- lapply(seq_len(length(optional.tabs)), function(x)
      tabPanel(names(optional.tabs)[x],
               fluidRow(column(1),
                        column(11,
                               HTML(optional.tabs[[x]])
                               )
                        )
      ))
    
    tab.list <- rev(append(rev(tab.list), list(home.tab)))
    
    do.call(tabsetPanel, tab.list)
  })
  
  # data preparation----
  data <- getShinyOption("data")
  data$id <- seq_len(nrow(data))
  
  ## convert longitudes in the map is centered ----
  
   if(getShinyOption("map.set.lon") >= 100){
    idx <- which(data$bbox.lon1 < 0 & data$bbox.lon2 < 0)
    data[idx, ]$bbox.lon1 <- 180 + (180 + data[idx, ]$bbox.lon1)
    idx <- which(data$bbox.lon2 < 0)
    data[idx, ]$bbox.lon2 <- 180 + (180 + data[idx, ]$bbox.lon2)
    data[which(data$lon < 0), ]$lon <- 180 + (180 + data[which(data$lon < 0), ]$lon)
   }
  
  ## popup ----
  if(! any(colnames(data) == "popup")){
    data$popup <- data$resource.name
  }
  
  ## colors ----
  data$color <- getShinyOption("map.area.fill.color")
  
  if(length(getShinyOption("map.legend.labels")) > 0){
    data$color <- eval(parse(text = paste0(
      "as.character(factor(data$'",  getShinyOption("map.legend.variable"), 
      "', levels = c(",  paste0("'", getShinyOption("map.legend.labels"), "'", collapse = ", "),
      "), labels = c(",  paste0("'", getShinyOption("map.legend.colors"), "'", collapse = ", "), ")))"
    )))
  }
  data$fillColor <- getShinyOption("map.area.fill.color")
  data$fillOpacity <- getShinyOption("map.area.fill.opacity")
  
  ## area ----
  area.km2 <- function(lon1, lat1, lon2, lat2){
    coords <- as.numeric(c(lon1, lon2, lat1, lat2)) * pi / 180 # convert to radian
    res <- 6378 ^ 2 * (sin(coords[3]) - sin(coords[4])) * (coords[1] - coords[2]) # earth radius: 6378137 m
    abs(res) # / 1000000
  }
  
  if(any(colnames(data) == "bbox.lon1")){
    data$area <- apply(data, 1, function(x) 
      area.km2(
        lon1 = x[which(names(data) == "bbox.lon1")],
        lat1 = x[which(names(data) == "bbox.lat1")],
        lon2 = x[which(names(data) == "bbox.lon2")],
        lat2 = x[which(names(data) == "bbox.lat2")]
      )
    )
    
    ## areas table ----  
    surfaces <- data[ ! is.na(data$bbox.lon1), c("id", "resource.name", "bbox.lon1", "bbox.lat1", "bbox.lon2", "bbox.lat2", "area", "popup", "fillOpacity", "fillColor", "color")]
    surfaces <- surfaces[order(surfaces$area, decreasing = TRUE),  ]
  }
  
  ## rectangle selection control ---- 
  rectangle.selection <- reactiveValues(val = NULL)
  
  observeEvent(input$map_draw_new_feature, {
    rectangle.selection$val <- input$map_draw_new_feature
  })
  
  # Table output ----
    tab <- eventReactive(rectangle.selection$val, {
    data.sub <- data[, - which(names(data) %in% c("lat", "lon", "bbox.lon1", "bbox.lat1", "bbox.lon2", "bbox.lat2", "color", "fillColor", "fillOpacity", "area", "popup") )]
    
    if( ! is.null(rectangle.selection$val)){  ## rectangle selection----
      lon <- c(input$map_draw_new_feature$geometry$coordinates[[1]][[1]][[1]],
               input$map_draw_new_feature$geometry$coordinates[[1]][[2]][[1]],
               input$map_draw_new_feature$geometry$coordinates[[1]][[3]][[1]],
               input$map_draw_new_feature$geometry$coordinates[[1]][[4]][[1]])
      
      lat <- c(input$map_draw_new_feature$geometry$coordinates[[1]][[1]][[2]],
               input$map_draw_new_feature$geometry$coordinates[[1]][[2]][[2]],
               input$map_draw_new_feature$geometry$coordinates[[1]][[3]][[2]],
               input$map_draw_new_feature$geometry$coordinates[[1]][[4]][[2]])
      
      min.lon <- min(lon)
      max.lon <- max(lon)
      min.lat <- min(lat) + 90 # convert to positive values only
      max.lat <- max(lat) + 90
      
      # selected points:
      idx.points <- (data$lon >= min.lon & data$lon <=  max.lon)    &    (data$lat + 90 >= min.lat & data$lat + 90 <=  max.lat)
      idx.points <- which(idx.points)
      
      # select surfaces:
      ## area with no horizontal overlap with the selected area: 
      horiz.overlap <- (data$bbox.lon1 < min.lon & data$bbox.lon2 < min.lon) | 
        (data$bbox.lon1 > max.lon & data$bbox.lon2 > max.lon)
      ## area with no vertical overlap with the selected area: 
      vert.overlap  <- (data$bbox.lat1 + 90 > max.lat & data$bbox.lat2 + 90 > max.lat) | 
        (data$bbox.lat1 + 90 < min.lat & data$bbox.lat2 + 90 < min.lat)
      
      idx.surf <- which( ! horiz.overlap & ! vert.overlap)
      # subset
      data.sub <-  data.sub[c(idx.surf, idx.points), ]
    }
    removeNotification("waiting.msg")
    data.sub
  }, ignoreNULL = FALSE, ignoreInit = FALSE)
  

    
  output$table <- DT::renderDataTable({
    DT::datatable(tab(), rownames = FALSE,  escape = FALSE, selection = 'single',
                  filter =  getShinyOption("table.filter"),
                  options = list(
                    pageLength = getShinyOption("table.pageLength"),
                    orderClasses = TRUE,
                    columnDefs = list(list(visible = FALSE, targets = c("id", getShinyOption("table.hide.columns")) ))))
  })
  
  # Table download   ----
  output$download.table <- downloadHandler(
    filename = "spatialCatalogueViewer-data.csv",
    content = function(file){
      utils::write.csv(getShinyOption("data"), file, row.names = FALSE)
    }
  )
  
  output$download.table.button <- renderUI({
    if(getShinyOption("data.download.button")){
      downloadButton("download.table", "Download the data (CSV)")
    }
  })
  
  # show area  control ----
  show.areas <- reactive({
    value <- FALSE
    if(getShinyOption("map.show.areas") == "always"){ 
      value <- TRUE
    } else if(getShinyOption("map.show.areas") %in% c(TRUE, FALSE)){
      if( ! is.null(input$show.areas.box)){
        value <- input$show.areas.box
      }
    }
    value
  })
  
  # Base map -----
  map.base <-
    leaflet::leaflet() |>
    leaflet::setView(lng = getShinyOption("map.set.lon"),
                     lat = getShinyOption("map.set.lat"),
                     zoom = getShinyOption("map.min.zoom"))  |>
    leaflet::addProviderTiles(getShinyOption("map.provider"),
                              options = leaflet::providerTileOptions(minZoom = getShinyOption("map.min.zoom"))) |>
    leaflet.extras::addDrawToolbar(targetGroup = 'draw', 
                                   polylineOptions = FALSE, polygonOptions = FALSE, circleOptions = FALSE,
                                   markerOptions = FALSE, circleMarkerOptions = FALSE, 
                                   singleFeature = TRUE)  
  
  if(length(getShinyOption("map.legend.labels") > 0)){
    labels.1 <- getShinyOption("map.legend.labels")
    labels.2 <- table(eval(parse(text = paste0("data$'", getShinyOption("map.legend.variable"), "'"))))
    
    if(sum(! labels.1 %in% names(labels.2)) > 0) {
      warning(paste0("Some 'map.legend.labels' values are absent in the '", getShinyOption("map.legend.variable"), "' variable."))
    }
    
    labels.2 <- labels.2[labels.1]
    legend.labels <- sapply(seq_len(length(labels.1)), function(x)   paste0(labels.1[x], " (", labels.2[x], ")" ))
    
    map.base <- map.base |>
      leaflet::addLegend("bottomright",    ## legend ----
                         title = getShinyOption("map.legend.variable"),
                         colors = getShinyOption("map.legend.colors"),
                         labels = legend.labels,
                         opacity = 0.8) 
  }
  
  # Convenient mapping functions ----
  # function to add rectangle to represent data coverage:
  add.areas.if.required <- function(map.data, areas.data, condition){
    output <- map.data
    if(condition){
      output <- output |> leaflet::addRectangles(data = areas.data,
                                           lng1 = ~bbox.lon1,
                                           lat1 = ~bbox.lat1,
                                           lng2 = ~bbox.lon2,
                                           lat2 = ~bbox.lat2,
                                           popup = ~popup,
                                           color = "black",
                                           opacity = 1,
                                           fillColor = ~fillColor,
                                           fillOpacity = ~fillOpacity,
                                           weight = .5,
                                           label = ~resource.name,
                                           options = leaflet::pathOptions(clickable = TRUE, interactive = TRUE),
                                           popupOptions = leaflet::popupOptions(closeOnClick = TRUE))
    }
    output
  }
 
  # function to add circle markers:
  add.circles.markers <- function(map.data, circle.data){
    map.data |>
      leaflet::addCircleMarkers(data = circle.data, lng= ~lon, lat = ~lat,
                              popup = ~popup,
                              layerId = ~id,
                              label = ~resource.name,
                              color = ~color,
                              radius = 6,
                              fillOpacity = ~fillOpacity,
                              opacity = 0.99,
                              options = leaflet::pathOptions(clickable = TRUE, interactive = TRUE),
                              popupOptions = leaflet::popupOptions(closeOnClick = TRUE),
                              clusterOptions = leaflet::markerClusterOptions(spiderfyDistanceMultiplier = 1.5)
                              )
  }
  
  observeEvent(input$reset.selections, { # Reset map ----
    output$map <- leaflet::renderLeaflet({ 
      
      map <- map.base 
      rectangle.selection$val <- NULL # reset the selection to reset the table.
      
      map <- add.areas.if.required("map.data" = map, "areas.data" = surfaces, "condition" = show.areas())
      
      add.circles.markers(map, data[ ! is.na(data$lat), ])
    })
  }, ignoreInit = FALSE, ignoreNULL = FALSE)
  
  

  # Map update  ----
  ## row selection ----
  observeEvent(input$table_rows_selected, {
    
    tab <- tab()
    row <- input$table_rows_selected
    
    row <- tab[row, ]$id
    row <- which(data$id == row)
    
    map <- leaflet::leafletProxy("map") 
    
    if( ! is.null(data[row, ]$lon) ){
      if( ! is.na(data[row, ]$lon)){
        data$fillOpacity <- getShinyOption("map.area.fill.opacity") # reset value
        data[row, ]$fillOpacity <- 1 # selected point is plain filled
        target.lon <- data[row, ]$lon
        target.lat <- data[row, ]$lat
        zoom.lvl <- getShinyOption("map.min.zoom") + 4
      } else if( ! is.null(data[row, ]$bbox.lon1))  {
        if( ! is.na(data[row, ]$bbox.lon1)){
          surfaces$fillOpacity <- getShinyOption("map.area.fill.opacity") # reset value
          surfaces$fillColor <- getShinyOption("map.area.fill.color") # reset value
          target.lon <- stats::median(c(data[row, ]$bbox.lon2, data[row, ]$bbox.lon1))
          target.lat <- stats::median(c(data[row, ]$bbox.lat2, data[row, ]$bbox.lat1))
          zoom.lvl <- getShinyOption("map.min.zoom") + 1
          # put the selected surface on top and increase its opacity:
          surfaces <- surfaces[order(surfaces$area, decreasing = TRUE), ]
          idx <- which(surfaces$resource.name == data[row, ]$resource.name)
          
          surfaces[idx, ]$fillOpacity <- .8
          surfaces[idx, ]$fillColor <- surfaces[idx, ]$color 
          surfaces <- rbind(surfaces[-idx, ], surfaces[idx, ]) 
          surfaces$id <- seq_len(nrow(surfaces))
        }
      }
      map <- map                  |> 
        leaflet::clearMarkers()            |>
        leaflet::clearMarkerClusters()     |>
        leaflet::clearShapes()             |>
        leaflet::setView(lng = target.lon, lat = target.lat, zoom = zoom.lvl)  
    }
    
    map <- add.areas.if.required("map.data" = map, "areas.data" = surfaces, "condition" = show.areas())
    
    add.circles.markers(map, data[ ! is.na(data$lat), ])
  })
  
  ## show areas ----
  observeEvent(input$show.areas.box, {
    
    map <- leafletProxy("map")  |> 
      leaflet::clearShapes()             
    
    map <- add.areas.if.required("map.data" = map, "areas.data" = surfaces, "condition" = show.areas())
    
    add.circles.markers(map, data[ ! is.na(data$lat), ])
  }, ignoreInit = FALSE, ignoreNULL = FALSE)
} # end of server.R

