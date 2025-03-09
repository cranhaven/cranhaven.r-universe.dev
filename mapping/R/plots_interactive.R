plot_interactive_choro <- function(data, var = NULL, colID = NULL,
                                   options = mapping.options())

{

  tiles <- providers[options$interactive.tiles]
  data <- st_transform(data, "+init=epsg:4326")

  map <- leaflet(data) %>%
    addTiles() %>%
    addProviderTiles(as.character(tiles))


  if(isFALSE(options$interactive.popup.id))
  {
    colID <- NULL
  }



  if(is.null(options$interactive.popup.vars))
  {
    popupVar <- popupTable(x = data,
                           row.numbers = FALSE,
                           feature.id = FALSE,
                           zcol = c(colID,var))
  }else{
    popupVar <- popupTable(x = data,
                           row.numbers = FALSE,
                           feature.id = FALSE,
                           zcol = c(colID,options$interactive.popup.vars))
  }

  labels = NULL

  if(isTRUE(options$interactive.hovered.id))
  {
    labels <- lapply(data[, c(colID), drop = TRUE],
                     function(x) paste(c(colID), x, "<br/>",sep =  " "))%>%
      lapply(htmltools::HTML)
  }

  pal_cont_tmp <- 0
  pal_cat_tmp <- 0


if(is.null(var))
{

  map <- map %>% addPolygons(fillOpacity = 0.1,color = options$border.col, weight = options$border.lwd,
                  opacity = options$border.alpha,
                  highlightOptions = highlightOptions(weight = options$interactive.highlight.weight, color = options$interactive.highlight.color,
                                                      fillOpacity = options$interactive.highlight.alpha, bringToFront = options$interactive.highlight.front),
                  label = labels,
                  labelOptions = labelOptions(style = list("font-weight" = "normal", padding = "3px 8px")))

}else{
  for(i in 1:length(var))
  {

    vr <- (data[,var[i], drop = TRUE])

    if(all(is.na(vr)))
    {
      vr <- c(0,vr)
    }

    if(is.character(vr) | is.factor(vr))
    {
      pal_cat_tmp <- pal_cat_tmp + 1
      palette = ifelse(length(options$palette.cat) > 1, options$palette.cat[pal_cat_tmp],options$palette.cat)
      col <- colorFactor(palette, domain = vr)

    }else{

      vr <- as.numeric(vr)
      pal_cont_tmp <- pal_cont_tmp + 1
      palette = ifelse(length(options$palette.cont) >1, options$palette.cont[pal_cont_tmp],options$palette.cont)

      if(options$col.style == "pretty")
      {
        col <- colorBin(palette = palette,
                        domain = vr,
                        na.color = options$NA.color, bins = options$nclass,pretty = TRUE)

      }else if(options$col.style == "equal")
      {
        col <- colorBin(palette = palette,
                        domain = vr,
                        na.color = options$NA.color, bins = options$nclass,pretty = FALSE)
      }else
      {
        col <- colorNumeric(palette = palette,
                            domain = vr,
                            na.color = options$NA.color)

      }

    }

    if(is.na(options$legend.title))
    {
      legend.title <- var[i]
    }else{
      legend.title <- options$legend.title
    }

    map <- map %>% addPolygons(fillColor = ~col(vr),
                               fillOpacity = options$alpha,color = options$border.col, weight = options$border.lwd,
                               opacity = options$border.alpha, group = var[i], popup = popupVar,
                               popupOptions = popupOptions(closeButton = options$interactive.popup.closeButton, maxWidth = options$interactive.popup.width.max, minWidth = options$interactive.popup.width.min),
                               highlightOptions = highlightOptions(weight = options$interactive.highlight.weight, color = options$interactive.highlight.color,
                                                                   fillOpacity = options$interactive.highlight.alpha, bringToFront = options$interactive.highlight.front),
                               label = labels,
                               labelOptions = labelOptions(style = list("font-weight" = "normal", padding = "3px 8px"))) %>%

      addLegend(position = paste(options$legend.position[2],options$legend.position[1],sep = ""), pal = col, values = vr,
                title = legend.title,
                labFormat = labelFormat(prefix = "  "),
                opacity = 1, group = var[i])

  }


  map <- map %>% addLayersControl(overlayGroups  = var,
                                  options = layersControlOptions(collapsed = options$interactive.control.collapse),
                                  position =  paste(options$interactive.layer.control.position[2], options$interactive.layer.control.position[1], sep = ""))

  if(length(var) > 1)
  {
    map <- map %>% hideGroup(var[-1])
  }

}

  map

}


plot_interactive_choro_facetes_freeScale <- function(data, var, colID = NULL, facets,
                                           options = mapping.options())
{

  data <- st_transform(data, "+init=epsg:4326")

  fct <- factor(data[,facets, drop = TRUE])

  map <- vector(mode = "list",length = nlevels(fct))

  options$interactive.control.collapse = FALSE

  data <- st_transform(data, "+init=epsg:4326")


  for(i in 1:nlevels(fct))
  {

    options$legend.title <- levels(fct)[i]
    dt <- data[fct == levels(fct)[i],]
    map[[i]] <- plot_interactive_choro(data = dt, colID = colID,
                                       var = var,
                                       options = options)


  }

  sync(map, ncol = 2, sync = "none", sync.cursor = TRUE)
}

plot_interactive_choro_facetes <- function(data, var, colID = NULL, facets,
                                                     options = mapping.options())
{

  data <- st_transform(data, "+init=epsg:4326")

  fct <- factor(data[,facets, drop = TRUE])

  map <- vector(mode = "list",length = nlevels(fct))

  options$interactive.control.collapse = FALSE

  data <- st_transform(data, "+init=epsg:4326")

  for(i in 1:nlevels(fct))
  {

    options$legend.title <- levels(fct)[i]
    dt <- data[fct == levels(fct)[i],]
    map[[i]] <- plot_interactive_choro_noFreeScale(dataCom = data, data = dt, colID = colID,
                                                   var = var,
                                                   options = options)


  }

  sync(map, ncol = 2, sync = "none", sync.cursor = TRUE)
}

plot_interactive_choro_noFreeScale <- function(dataCom,data, var, colID = NULL,
                                   options = mapping.options())

{

  tiles <- providers[options$interactive.tiles]

  map <- leaflet(data) %>%
    addTiles() %>%
    addProviderTiles(as.character(tiles))

  if(isFALSE(options$interactive.popup.id))
  {
    colID <- NULL
  }


  if(is.null(options$interactive.popup.vars))
  {
    popupVar <- popupTable(x = data,
                           row.numbers = FALSE,
                           feature.id = FALSE,
                           zcol = c(colID,var))
  }else{
    popupVar <- popupTable(x = data,
                           row.numbers = FALSE,
                           feature.id = FALSE,
                           zcol = c(colID,options$interactive.popup.vars))
  }

  labels = NULL

  if(isTRUE(options$interactive.hovered.id))
  {
    labels <- lapply(data[, c(colID), drop = TRUE],
                     function(x) paste(c(colID), x, "<br/>",sep =  " "))%>%
      lapply(htmltools::HTML)
  }

  vrConst <- (unlist(dataCom[,var, drop = TRUE]))

  for(i in 1:length(var))
  {

    vr <- (data[,var[i], drop = TRUE])


    if(all(is.na(vr)))
    {
      vr <- c(0,vr)
    }

    if(is.character(vrConst) | is.factor(vrConst))
    {
      palette = ifelse(length(options$palette.cat) >1, options$palette.cat[i],options$palette.cat)
      col <- colorFactor(palette, domain = vrConst)

    }else{

      vrConst <- as.numeric(vrConst)
      vr <- as.numeric(vr)
      palette = ifelse(length(options$palette.cont) >1, options$palette.cont[i],options$palette.cont)

      if(options$col.style == "pretty")
      {
        col <- colorBin(palette = palette,
                        domain = vrConst,
                        na.color = options$NA.color, bins = options$nclass,pretty = TRUE)

      }else if(options$col.style == "equal")
      {
        col <- colorBin(palette = palette,
                        domain = vrConst,
                        na.color = options$NA.color, bins = options$nclass,pretty = FALSE)
      }else
      {
        col <- colorNumeric(palette = palette,
                            domain = vrConst,
                            na.color = options$NA.color)

      }

    }

    if(is.na(options$legend.title))
    {
      legend.title <- var[i]
    }else{
      legend.title <- options$legend.title
    }

    map <- map %>% addPolygons(fillColor = ~col(vr),
                               fillOpacity = options$alpha,color = options$border.col, weight = options$border.lwd,
                               opacity = options$border.alpha, group = var[i], popup = popupVar,label = labels,
                               popupOptions = popupOptions(closeButton = options$interactive.popup.closeButton, maxWidth = options$interactive.popup.width.max, minWidth = options$interactive.popup.width.min),
                               highlightOptions = highlightOptions(weight = options$interactive.highlight.weight, color = options$interactive.highlight.color,
                                                                   fillOpacity = options$interactive.highlight.alpha, bringToFront = options$interactive.highlight.front),
                               labelOptions = labelOptions(style = list("font-weight" = "normal", padding = "3px 8px"))) %>%

      addLegend(position = paste(options$legend.position[2],options$legend.position[1],sep = ""), pal = col, values = vrConst,
                title = legend.title,
                labFormat = labelFormat(prefix = "  "),
                opacity = 1, group = var[i])

  }


  map <- map %>% addLayersControl(overlayGroups  = var,
                                  options = layersControlOptions(collapsed = options$interactive.control.collapse),
                                  position =  paste(options$interactive.layer.control.position[2], options$interactive.layer.control.position[1], sep = ""))

  if(length(var) > 1)
  {
    map <- map %>% hideGroup(var[-1])
  }

  map

}


