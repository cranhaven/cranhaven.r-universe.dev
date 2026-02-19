spatialCatalogueViewer <- function(data = NULL,
                                   text.title = NULL, text.left = NULL, text.top = NULL, text.bottom = NULL,
                                   map.provider = "Stadia.StamenTerrainBackground",
                                   map.set.lon = 0, map.set.lat = 0, 
                                   map.legend.variable = NULL, map.legend.labels = NULL, map.legend.colors = NULL, map.height = 500,
                                   map.area.fill.color = "grey", map.area.fill.opacity = 0.5, map.show.areas = "never", map.min.zoom = NULL,
                                   table.hide.columns = NULL, table.filter = "none", table.pageLength = NULL,
                                   data.download.button = TRUE,
                                   tabs.contents = NULL,
                                   theme = "cosmo", css = NULL, js = NULL
                      ){
  
  
  # tests parameters----
  if(is.null(data)) stop("No data provided.")
  if( ! is.data.frame(data)) stop("'data' must be a dataframe.")
  if(! (sum(c("lon", "lat") %in% colnames(data)) | sum(! c("bbox.lon1", "bbox.lat1", "bbox.lon2", "bboxlat2") %in% colnames(data)))) {
    stop("'data' must have either 'lon' and 'lat' columns or 'bbox.lon1', 'bbox.lat1', 'bbox.lon2', and 'bboxlat2' columns.")
  }
  if(length(map.legend.labels) != length(map.legend.colors)) stop("'map.legend.labels' and 'map.legend.colors' must have equal lengths.")
  
  # define shiny options ----
  shiny::shinyOptions("data" = data,
               "text.title" = text.title,
               "text.left" = text.left,
               "text.top" = text.top,
               "text.bottom" = text.bottom,
               "map.provider" = map.provider,
               "map.set.lon" = map.set.lon,
               "map.set.lat" = map.set.lat,
               "map.legend.variable" = map.legend.variable,
               "map.legend.labels" = map.legend.labels, 
               "map.legend.colors" = map.legend.colors, 
               "map.height" = map.height,
               "map.area.fill.color" = map.area.fill.color,
               "map.area.fill.opacity" = map.area.fill.opacity,
               "map.show.areas" = map.show.areas, 
               "map.min.zoom" = map.min.zoom,
               "table.hide.columns" = table.hide.columns, 
               "table.filter" = table.filter, 
               "table.pageLength" = table.pageLength,
               "data.download.button" = data.download.button,
               "tabs.contents" = tabs.contents,
               "theme" = theme,
               "css" = css,
               "js" = js)
  
  shinyApp(ui = app_ui, server = app_server)
}
