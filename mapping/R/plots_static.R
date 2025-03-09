mapping_tmap <- function(data, var = NULL,
                         facets = NULL, add_text = NULL,
                         options = mapping.options())
{

  suppressMessages(tmap_mode("plot"))
  legend.outside.size <- options$legend.width

  layout <- tm_layout(main.title = options$title, main.title.size = options$title.size,
                      main.title.color = options$title.color, main.title.fontface = options$title.fontface,
                      main.title.position = options$title.position, compass.type = options$compass,
                      frame = options$map.frame, bg.color = options$background.color,
                      legend.format = list(digits = options$legend.digits,
                                           decimal.mark = options$legend.decimal.mark,
                                           big.mark = options$legend.big.mark,
                                           format = options$legend.format,
                                           text.separator = options$legend.text.separator,
                                           text.align = options$legend.text.align))



  border <- tm_borders(col = options$border.col,lwd = options$border.lwd,
                       lty = options$border.type, alpha = options$border.alpha)

  style <- tm_style(style = options$style)

  centroids <- NULL
  fill_text <- NULL

  if(!is.null(add_text))
  {
    centroids <- suppressWarnings(st_centroid(data))
    centroids <- tm_shape(centroids, name = add_text)

    fill_text <- tm_text(add_text, size = options$text.size,
                         col = options$text.col, fontface = options$text.fontface,
                         shadow = options$text.shadow, alpha = options$text.alpha)

  }



  if(!is.null(options$credits.source) & !is.null(options$credits.author))
  {
    srcAuth <- paste(options$credits.source, " \n", options$credits.author,sep = "")

  }else if (is.null(options$credits.source) & is.null(options$credits.author))
  {
    srcAuth <- ""

  }else if(is.null(options$credits.author) )
  {
    srcAuth <- options$credits.source

  }else if(is.null(options$credits.source)){

    srcAuth <- options$credits.author
  }

  srcAuth <- tm_credits(srcAuth,
                        size = options$credits.size,
                        col = options$credits.color,
                        fontface = options$credits.fontface,
                        position = options$credits.position)


  if(is.null(var))
  {
    tm <- tm_shape(shp = data) + style + border + layout + srcAuth + centroids + fill_text
    tm

  }else
  {
    nvars <- length(var)

    if(is.character(data[,var, drop = TRUE]) | is.factor(data[,var, drop = TRUE]))
    {
      palette = options$palette.cat
    }else{
      palette = options$palette.cont
    }

    if(nvars == 1 | (nvars != 1 & (length(palette) == 1 | is.null(palette))))
    {


      palette <- mapPalette(type = palette, nclass = options$nclass)

      var.text <- options$legend.title
      var.id <- var

      if(is.character(var.id))
      {
        var.id <- match(var.id,colnames(data))
      }

      if(is.null(var.text))
      {
        var.text = ""

      }else if(is.na(var.text))
      {
        var.text <- colnames(data)[var.id]
      }

      if(options$col.style == "order")
      {

        fill <- tm_fill(col = var, n = options$nclass, palette = palette, style = options$col.style,
                        interval.closure = options$interval.closure,title = var.text, legend.is.portrait = options$legend.portrait,
                        textNA = options$NA.text, colorNA = options$NA.color, labels = options$labels)

      }else{

        fill <- tm_fill(col = var, n = options$nclass, palette = palette, style = options$col.style,
                        breaks = options$breaks, interval.closure = options$interval.closure, legend.is.portrait = options$legend.portrait,
                        textNA = options$NA.text, colorNA = options$NA.color, labels = options$labels,
                        title = var.text)

      }




      if(!is.null(facets))
      {
        legend <- tm_legend(legend.show = options$legend.show,
                            legend.only = options$legend.only, legend.position = options$legend.position,
                            legend.outside = options$legend.outside.facetes, legend.width = options$legend.width, legend.text.size = options$legend.text.size, legend.title.color = options$legend.title.color,
                            legend.title.size = options$legend.title.size, legend.title.fontface = options$legend.title.fontface,legend.text.fontface = options$legend.text.fontface,
                            legend.text.color = options$legend.text.color, legend.frame = options$legend.frame, legend.outside.size = legend.outside.size)

        tm <- tm_shape(shp = data) + style + border + fill + layout + legend + tm_facets(by = facets, ncol = options$facetes.cols,
                                                                                         nrow = options$facetes.rows,
                                                                                         free.scales.fill = options$facets.free.scale) + centroids + fill_text + srcAuth + tm_facets(by = facets,
                                                                                                                                                                                     free.scales.fill = options$facets.free.scale,
                                                                                                                                                                                     ncol = options$facetes.cols,
                                                                                                                                                                                     nrow = options$facetes.rows)

      }else if(isTRUE(facets))
      {
        legend <- tm_legend(legend.show = options$legend.show,
                            legend.only = options$legend.only, legend.position = options$legend.position,legend.title.color = options$legend.title.color,
                            legend.outside = options$legend.outside.facetes,legend.width = options$legend.width, legend.text.size = options$legend.text.size,
                            legend.title.size = options$legend.title.size, legend.title.fontface = options$legend.title.fontface, legend.text.fontface = options$legend.text.fontface,
                            legend.text.color = options$legend.text.color, legend.frame = options$legend.frame, legend.outside.size = legend.outside.size)

        tm <- tm_shape(shp = data) + style + border + fill + layout + legend + tm_facets(by = unit, ncol = options$facetes.cols,
                                                                                         nrow = options$facetes.rows,
                                                                                         free.scales.fill = options$facets.free.scale) + centroids + fill_text + srcAuth + tm_facets(by = unit, ncol = options$facetes.cols,
                                                                                                                                                                                     nrow = options$facetes.rows,
                                                                                                                                                                                     free.scales.fill = options$facets.free.scale)

      }else{

        legend.outside.size = options$legend.outside.facetes.size

        legend <- tm_legend(legend.show = options$legend.show,
                            legend.only = options$legend.only, legend.position = options$legend.position, legend.title.color = options$legend.title.color,
                            legend.outside = options$legend.outside,legend.width = options$legend.width, legend.text.size = options$legend.text.size,
                            legend.title.size = options$legend.title.size, legend.title.fontface = options$legend.title.fontface, legend.text.fontface = options$legend.text.fontface,
                            legend.text.color = options$legend.text.color, legend.frame = options$legend.frame, legend.outside.size = legend.outside.size)

        tm <- tm_shape(shp = data) + style + border  + fill + layout + centroids + fill_text + srcAuth + legend
      }
      
      tm

    }else{

      legend <- tm_legend(legend.show = options$legend.show,
                          legend.only = options$legend.only, legend.position = options$legend.position, legend.title.color = options$legend.title.color,
                          legend.outside = options$legend.outside,legend.width = options$legend.width, legend.text.size = options$legend.text.size,
                          legend.title.size = options$legend.title.size, legend.title.fontface = options$legend.title.fontface, legend.text.fontface = options$legend.text.fontface,
                          legend.text.color = options$legend.text.color, legend.frame = options$legend.frame, legend.outside.size = legend.outside.size)


      var.text <- options$legend.title
      var.id <- var

      if(is.character(var.id))
      {
        var.id <- match(var.id,colnames(data))
      }

      if(is.null(var.text))
      {
        var.text = ""

      }else if(is.na(var.text))
      {
        var.text <- colnames(data)[var.id]
      }



      map_ls <- vector("list", nvars)


      npal_cont <- length(options$palette.cont)
      npal_cat <- length(options$palette.cat)
      pal_cont_tmp <- 0
      pal_cat_tmp <- 0

      for(i in 1:nvars)
      {


        if(is.character(data[,var[i], drop = TRUE]) | is.factor(data[,var[i], drop = TRUE]))
        {

          pal_cat_tmp <- pal_cat_tmp + 1
          palette <- mapPalette(type = options$palette.cat[pal_cat_tmp], nclass = options$nclass)


          if(is.null(palette))
          {
            palette <- mapPalette(type = options$palette.cat[1], nclass = options$nclass)

          }

        }else{

          pal_cont_tmp <- pal_cont_tmp + 1
          palette <- mapPalette(type = options$palette.cont[pal_cont_tmp], nclass = options$nclass)


          if(is.null(palette))
          {
            palette <- mapPalette(type = options$palette.cont[1], nclass = options$nclass)

          }
        }


        if(options$col.style == "order")
        {

          fill <- tm_fill(col = var[i], n = options$nclass, palette = palette, style = options$col.style,
                          interval.closure = options$interval.closure,title = var.text, legend.is.portrait = options$legend.portrait,
                          textNA = options$NA.text, colorNA = options$NA.color, labels = options$labels)

        }else{

          fill <- tm_fill(col = var[i], n = options$nclass, palette = palette, style = options$col.style,
                          breaks = options$breaks, interval.closure = options$interval.closure, legend.is.portrait = options$legend.portrait,
                          textNA = options$NA.text, colorNA = options$NA.color, labels = options$labels,
                          title = var.text)

        }

        map_ls[[i]] <-  tm_shape(shp = data) +  style + border + fill + layout + centroids + fill_text + srcAuth + legend



      }

      do.call("tmap_arrange", map_ls)

    }
  }

}




mapping_tmap_US <- function(data, var = NULL, add_text = NULL,
                            options = mapping.options())
{


  suppressMessages(tmap_mode("plot"))


  if(!is.null(options$credits.source) & !is.null(options$credits.author))
  {
    srcAuth <- paste(options$credits.source, " \n", options$credits.author,sep = "")

  }else if (is.null(options$credits.source) & is.null(options$credits.author))
  {
    srcAuth <- ""

  }else if(is.null(options$credits.author) )
  {
    srcAuth <- options$credits.source

  }else if(is.null(options$credits.source)){

    srcAuth <- options$credits.author
  }



  srcAuth <- tm_credits(srcAuth,
                        size = options$credits.size,
                        col = options$credits.color,
                        fontface = options$credits.fontface,
                        position = options$credits.position)

  border <- tm_borders(col = options$border.col,lwd = options$border.lwd,
                       lty = options$border.type, alpha = options$border.alpha)

  style <- tm_style(style = options$style)


  layout <- tm_layout(main.title = options$title, main.title.size = options$title.size,
                      main.title.color = options$title.color, main.title.fontface = options$title.fontface,
                      main.title.position = options$title.position, compass.type = options$compass,
                      frame = FALSE,  inner.margins = c(0.1, 0.1, 0.05, 0.05), bg.color = options$background.color,
                      legend.format = list(digits = options$legend.digits,
                                           decimal.mark = options$legend.decimal.mark,
                                           big.mark = options$legend.big.mark,
                                           format = options$legend.format,
                                           text.separator = options$legend.text.separator,
                                           text.align = options$legend.text.align))

  legend <- tm_legend(legend.show = options$legend.show,
                      legend.only = options$legend.only, legend.position = c("right","bottom"), legend.title.color = options$legend.title.color,
                      legend.outside = options$legend.outside,legend.width = options$legend.width, legend.text.size = options$legend.text.size,
                      legend.title.size = options$legend.title.size, legend.title.fontface = options$legend.title.fontface, legend.text.fontface = options$legend.text.fontface,
                      legend.text.color = options$legend.text.color, legend.frame = options$legend.frame)





  if(is.null(data$state))
  {
    if(is.null(data$state_number))
    {
      al <- data$state_number=="02"
      hw <- data$state_number=="15"
      alhw <- !(data$state_number %in% c("02", "15"))
    }else{
      al <- data$state_id =="AK"
      hw <- data$state_id =="HI"
      alhw <- !(data$state_id %in% c("AK", "HI"))
    }
  }else{

    al <- data$state=="Alaska"
    hw <- data$state=="Hawaii"
    alhw <- !(data$state %in% c("Alaska", "Hawaii"))
  }

  al <- which(al == TRUE)
  hw <- which(hw == TRUE)
  alhw <- which(alhw == TRUE)

  nn_al <- "Alaska"
  nn_hw <- "Hawaii"

  centroids_al <- NULL
  centroids_hw <- NULL
  centroids <- NULL


  fill_text_al <- NULL
  fill_text_hw <- NULL
  fill_text <- NULL



  if(!is.null(add_text))
  {
    centroids_al <- suppressWarnings(st_centroid(data[al,]))
    centroids_al <- tm_shape(centroids_al, name = add_text)

    fill_text_al <- tm_text(add_text, size = options$text.size,
                            col = options$text.col, fontface = options$text.fontface,
                            shadow = options$text.shadow, alpha = options$text.alpha)


    centroids_hw <- st_centroid(data[hw,])
    centroids_hw <- tm_shape(centroids_hw, name = add_text)

    fill_text_hw <- tm_text(add_text, size = options$text.size,
                            col = options$text.col, fontface = options$text.fontface,
                            shadow = options$text.shadow, alpha = options$text.alpha)

    centroids <- st_centroid(data[data[alhw,],])
    centroids <- tm_shape(centroids, name = add_text)

    fill_text <- tm_text(add_text, size = options$text.size,
                         col = options$text.col, fontface = options$text.fontface,
                         shadow = options$text.shadow, alpha = options$text.alpha)
    nn_al <- " "
    nn_hw <- " "
  }


  if(is.null(var))
  {

    alaska <- tm_shape(data[al,], projection = 3338) + style + border + tm_layout(nn_al, legend.show = FALSE, bg.color = NA, title.size = 0.8, frame = FALSE) + centroids_al + fill_text_al
    hawaii <- tm_shape(data[hw,], projection = 3759) + style + border + tm_layout(nn_hw,legend.show = FALSE, bg.color=NA, title.position = c("LEFT", "BOTTOM"), title.size = 0.8, frame=FALSE) + centroids_hw + fill_text_hw

    alk <- viewport(x = 0.15, y = 0.15, width = 0.3, height = 0.3)
    haw <- viewport(x = 0.4, y = 0.1, width = 0.2, height = 0.1)

    continent <- tm_shape(data[alhw,],  projection=2163) + style + border +  layout + legend + srcAuth + centroids + fill_text

  }else if(length(var) == 1){

    if(is.character(data[,var, drop = TRUE]) | is.factor(data[,var, drop = TRUE]))
    {
      palette = options$palette.cat
    }else{
      palette = options$palette.cont
    }

    var.text <- options$legend.title
    var.id <- var

    if(is.character(var.id))
    {
      var.id <- match(var.id,colnames(data))
    }

    if(is.null(var.text))
    {
      var.text = ""
    }else if(is.na(var.text))
    {
      var.text <- colnames(data)[var.id]
    }


    fill <- tm_fill(col = var, n = options$nclass, palette = palette, style = options$col.style,
                    breaks = options$breaks, interval.closure = options$interval.closure,
                    textNA = options$NA.text, colorNA = options$NA.color, labels = options$labels,
                    title = var.text, legend.is.portrait = options$legend.portrait)


    other <- tm_shape(data) + style + border + fill
    other <- print(other, show = FALSE)

    if(attributes(data)$aggregated)
    {

      dt <- get(paste("us_","2018","_", "state", "_","20m",".rda", sep = ""), pos = "package:mapping")

      alaska <- NULL

      if(length(attributes(data)$alaska) != 0)
      {
        fill_al <- tm_fill(col = other$gps$plot1$tmLayer2$fill[attributes(data)$alaska], legend.is.portrait = options$legend.portrait)
        alaska <- tm_shape(dt[dt$state=="Alaska",], projection = 3338) + style + border + fill_al + tm_layout(nn_al, legend.show = FALSE, bg.color = NA, title.size = 0.8, frame = FALSE)  + centroids_al + fill_text_al
      }

      hawaii <- NULL
      if(length(attributes(data)$hawaii) != 0)
      {
        fill_hw <- tm_fill(col = other$gps$plot1$tmLayer2$fill[attributes(data)$hawaii], legend.is.portrait = options$legend.portrait)
        hawaii <- tm_shape(dt[dt$state=="Hawaii",], projection = 3759) + style + border + fill_hw + tm_layout(nn_hw,legend.show = FALSE, bg.color=NA, title.position = c("LEFT", "BOTTOM"), title.size = 0.8, frame=FALSE)  + centroids_hw + fill_text_hw
      }


      alk <- viewport(x = 0.15, y = 0.15, width = 0.3, height = 0.3)
      haw <- viewport(x = 0.4, y = 0.1, width = 0.2, height = 0.1)

      fill <- tm_fill(col = var, palette = palette, style = ifelse(options$col.style == "log10", "cont", options$col.style),
                      breaks = other$gps$plot1$tmLayer2$fill.legend.values, interval.closure = options$interval.closure,
                      textNA = options$NA.text, colorNA = options$NA.color, labels = other$gps$plot1$tmLayer2$fill.legend.labels,
                      title = var.text, legend.is.portrait = options$legend.portrait)

      if(length(alhw) == 0)
      {
        alhw <- 1:nrow(data)
      }
      continent <- tm_shape(data[alhw,],  projection=2163) + style + border + fill +  layout + legend + srcAuth + centroids + fill_text

    }else{
      fill_al <- tm_fill(col = other$gps$plot1$tmLayer2$fill[al], legend.is.portrait = options$legend.portrait)
      alaska <- tm_shape(data[al,], projection = 3338) + style + border + fill_al + tm_layout(nn_al, legend.show = FALSE, bg.color = NA, title.size = 0.8, frame = FALSE)  + centroids_al + fill_text_al

      fill_hw <- tm_fill(col = other$gps$plot1$tmLayer2$fill[hw], legend.is.portrait = options$legend.portrait)
      hawaii <- tm_shape(data[hw,], projection = 3759) + style + border + fill_hw + tm_layout(nn_hw,legend.show = FALSE, bg.color=NA, title.position = c("LEFT", "BOTTOM"), title.size = 0.8, frame=FALSE)  + centroids_hw + fill_text_hw

      alk <- viewport(x = 0.15, y = 0.15, width = 0.3, height = 0.3)
      haw <- viewport(x = 0.4, y = 0.1, width = 0.2, height = 0.1)

      fill <- tm_fill(col = var, palette = palette, style = ifelse(options$col.style == "log10", "cont", options$col.style),
                      breaks = other$gps$plot1$tmLayer2$fill.legend.values, interval.closure = options$interval.closure,
                      textNA = options$NA.text, colorNA = options$NA.color, labels = other$gps$plot1$tmLayer2$fill.legend.labels,
                      title = var.text, legend.is.portrait = options$legend.portrait)

      continent <- tm_shape(data[alhw,],  projection=2163) + style + border + fill +  layout + legend + srcAuth + centroids + fill_text

    }

    print(continent)

    if(length(alaska$tm_shape$shp$state_id) != 0)
    {
      print(alaska, vp = alk)
    }
    if(length(hawaii$tm_shape$shp$state_id) != 0)
    {
      print(hawaii, vp = haw)
    }

  }else if(length(var) == 2)
  {

    var.text <- options$legend.title
    var.id <- var

    if(is.character(var.id))
    {
      var.id <- match(var.id,colnames(data))
    }

    if(is.null(var.text))
    {
      var.text = ""
    }else if(is.na(var.text))
    {
      var.text <- colnames(data)[var.id]
    }


    alk <- list(alk1 = viewport(x = 0.3, y = 0.6, width = 0.15, height = 0.15),
                alk2 = viewport(x = 0.3, y = 0.1, width = 0.15, height = 0.15))

    haw <- list(haw1 = viewport(x = 0.4, y = 0.6, width = 0.1, height = 0.05),
                haw2 = viewport(x = 0.4, y = 0.1, width = 0.1, height = 0.05))

    nvars <- length(var)
    map_ls <- vector("list", nvars)

    npal_cont <- length(options$palette.cont)
    npal_cat <- length(options$palette.cat)
    pal_cont_tmp <- 0
    pal_cat_tmp <- 0


    free_scale <- options$facets.free.scale
    ctn <- sapply(data[,var, drop = TRUE], function(x) is.character(x) | is.factor(x))

    if(any(ctn))
    {
      free_scale <- TRUE
    }

    if(free_scale)
    {
      for(i in 1:nvars)
      {
        if(ctn[i])
        {

          pal_cat_tmp <- pal_cat_tmp + 1
          palette <- mapPalette(type = options$palette.cat[pal_cat_tmp], nclass = options$nclass)


          if(is.null(palette))
          {
            palette <- mapPalette(type = options$palette.cat[1], nclass = options$nclass)

          }

        }else{

          pal_cont_tmp <- pal_cont_tmp + 1
          palette <- mapPalette(type = options$palette.cont[pal_cont_tmp], nclass = options$nclass)


          if(is.null(palette))
          {
            palette <- mapPalette(type = options$palette.cont[1], nclass = options$nclass)

          }
        }

        fill <- tm_fill(col = var[i], n = options$nclass, palette = palette, style = options$col.style,
                        breaks = options$breaks, interval.closure = options$interval.closure,
                        textNA = options$NA.text, colorNA = options$NA.color, labels = options$labels,
                        title = var.text, legend.is.portrait = options$legend.portrait)



        other <- tm_shape(data) + style + border + fill
        other <- print(other, show = FALSE)

        fill_al <- tm_fill(col = other$gps$plot1$tmLayer2$fill[al], legend.is.portrait = options$legend.portrait)
        alaska <- tm_shape(data[al,], projection = 3338) + style + border + fill_al + tm_layout(nn_al, legend.show = FALSE, bg.color = NA, title.size = 0.8, frame = FALSE)  + centroids_al + fill_text_al

        fill_hw <- tm_fill(col = other$gps$plot1$tmLayer2$fill[hw], legend.is.portrait = options$legend.portrait)
        hawaii <- tm_shape(data[hw,], projection = 3759) + style + border + fill_hw + tm_layout(nn_hw,legend.show = FALSE, bg.color=NA, title.position = c("LEFT", "BOTTOM"), title.size = 0.8, frame=FALSE)  + centroids_hw + fill_text_hw

        alk <- viewport(x = 0.15, y = 0.15, width = 0.3, height = 0.3)
        haw <- viewport(x = 0.4, y = 0.1, width = 0.2, height = 0.1)

        fill <- tm_fill(col = var[i], palette = palette, style = ifelse(options$col.style == "log10", "cont", options$col.style),
                        breaks = other$gps$plot1$tmLayer2$fill.legend.values, interval.closure = options$interval.closure,
                        textNA = options$NA.text, colorNA = options$NA.color, labels = other$gps$plot1$tmLayer2$fill.legend.labels,
                        title = var.text, legend.is.portrait = options$legend.portrait)

        continent <- tm_shape(data[alhw,],  projection=2163) + style + border + fill +  layout + legend + srcAuth + centroids + fill_text


        map_ls[[i]] <- list(continent, alaska,hawaii)
      }

    }else{
      palette1 <- mapPalette(type = options$palette.cont[1], nclass = options$nclass)
      palette2 <- mapPalette(type = options$palette.cont[2], nclass = options$nclass)


      if(is.null(palette1))
      {
        palette1 <- mapPalette(type = options$palette.cont[1], nclass = options$nclass)
      }

      if(is.null(palette2))
      {
        palette2 <- mapPalette(type = options$palette.cont[1], nclass = options$nclass)
      }

      ddt <- data[,var[2]]
      names(ddt)[1] <- var[1]
      ddt <- rbind(data[,var[1]], ddt)

      fill1 <- tm_fill(col = var[1], n = options$nclass, palette = palette1, style = options$col.style,
                      breaks = options$breaks, interval.closure = options$interval.closure,
                      textNA = options$NA.text, colorNA = options$NA.color, labels = options$labels,
                      title = var.text, legend.is.portrait = options$legend.portrait)

      fill2 <- tm_fill(col = var[1], n = options$nclass, palette = palette2, style = options$col.style,
                      breaks = options$breaks, interval.closure = options$interval.closure,
                      textNA = options$NA.text, colorNA = options$NA.color, labels = options$labels,
                      title = var.text, legend.is.portrait = options$legend.portrait)

      other1 <- tm_shape(ddt) + style + border + fill1
      other1 <- print(other1, show = FALSE)

      other2 <- tm_shape(ddt) + style + border + fill2
      other2 <- print(other2, show = FALSE)

      fl_1 <- other1$gps$plot1$tmLayer2$fill[1:nrow(data)]
      fl_2 <- other2$gps$plot1$tmLayer2$fill[(nrow(data)+1):nrow(ddt)]

      fill_al <- tm_fill(col = fl_1[al], legend.is.portrait = options$legend.portrait)
      alaska1 <- tm_shape(data[al,], projection = 3338) + style + border + fill_al + tm_layout(nn_al, legend.show = FALSE, bg.color = NA, title.size = 0.8, frame = FALSE)  + centroids_al + fill_text_al

      fill_hw <- tm_fill(col = fl_1[hw], legend.is.portrait = options$legend.portrait)
      hawaii1 <- tm_shape(data[hw,], projection = 3759) + style + border + fill_hw + tm_layout(nn_hw,legend.show = FALSE, bg.color=NA, title.position = c("LEFT", "BOTTOM"), title.size = 0.8, frame=FALSE)  + centroids_hw + fill_text_hw

      fill_al <- tm_fill(col = fl_2[al], legend.is.portrait = options$legend.portrait)
      alaska2 <- tm_shape(data[al,], projection = 3338) + style + border + fill_al + tm_layout(nn_al, legend.show = FALSE, bg.color = NA, title.size = 0.8, frame = FALSE)  + centroids_al + fill_text_al

      fill_hw <- tm_fill(col = fl_2[hw], legend.is.portrait = options$legend.portrait)
      hawaii2 <- tm_shape(data[hw,], projection = 3759) + style + border + fill_hw + tm_layout(nn_hw,legend.show = FALSE, bg.color=NA, title.position = c("LEFT", "BOTTOM"), title.size = 0.8, frame=FALSE)  + centroids_hw + fill_text_hw


      alk <- viewport(x = 0.15, y = 0.15, width = 0.3, height = 0.3)
      haw <- viewport(x = 0.4, y = 0.1, width = 0.2, height = 0.1)

      fill <- tm_fill(col = var[1], palette = palette1, style = ifelse(options$col.style == "log10", "cont", options$col.style),
                      breaks = other1$gps$plot1$tmLayer2$fill.legend.values, interval.closure = options$interval.closure,
                      textNA = options$NA.text, colorNA = options$NA.color, labels = other1$gps$plot1$tmLayer2$fill.legend.labels,
                      title = var.text, legend.is.portrait = options$legend.portrait)

      continent1 <- tm_shape(data[alhw,],  projection=2163) + style + border + fill +  layout + legend + srcAuth + centroids + fill_text
      continent1$tm_fill$breaks <- other1$gps$plot1$tmLayer2$fill.legend.values
      continent1$tm_fill$labels <- other1$gps$plot1$tmLayer2$fill.legend.labels

      fill <- tm_fill(col = var[2], palette = palette2, style = ifelse(options$col.style == "log10", "cont", options$col.style),
                      breaks = other1$gps$plot1$tmLayer2$fill.legend.values, interval.closure = options$interval.closure,
                      textNA = options$NA.text, colorNA = options$NA.color, labels = other1$gps$plot1$tmLayer2$fill.legend.labels,
                      title = var.text, legend.is.portrait = options$legend.portrait)

      continent2 <- tm_shape(data[alhw,],  projection=2163) + style + border + fill +  layout + legend + srcAuth + centroids + fill_text
      continent2$tm_fill$breaks <- other2$gps$plot1$tmLayer2$fill.legend.values
      continent2$tm_fill$labels <- other2$gps$plot1$tmLayer2$fill.legend.labels

      map_ls[[1]] <- list(continent1, alaska1,hawaii1)
      map_ls[[2]] <- list(continent2, alaska2,hawaii2)
    }


    alk2 <- viewport(x = 0.3, y = 0.1, width = 0.15, height = 0.15)
    haw2 <- viewport(x = 0.4, y = 0.1, width = 0.1, height = 0.05)


    alk1 <- viewport(x = 0.3, y = 0.6, width = 0.15, height = 0.15)
    haw1 <- viewport(x = 0.4, y = 0.6, width = 0.1, height = 0.05)

    mp1 <- map_ls[[1]][[1]]
    mp2 <- map_ls[[2]][[1]]
    print(tmap_arrange(mp1, mp2, ncol = 1))


    if(length(map_ls[[1]][[2]]$tm_shape$shp$state_id) != 0)
    {
      print(map_ls[[1]][[2]], vp = alk1)
    }
    if(length(map_ls[[1]][[3]]$tm_shape$shp$state_id) != 0)
    {
      print(map_ls[[1]][[3]], vp = haw1)
    }

    if(length(map_ls[[2]][[2]]$tm_shape$shp$state_id) != 0)
    {
      print(map_ls[[2]][[2]], vp = alk2)
    }
    if(length(map_ls[[2]][[3]]$tm_shape$shp$state_id) != 0)
    {
      print(map_ls[[2]][[3]], vp = haw2)
    }

    

  }



}

mapping_choro <- function(data, var = NULL,
                          options = mapping.options())
{
  # crs <- options$crs
  #
  # if(!is.null(crs))
  # {
  #   data <- st_transform(data, crs = crs)
  # }

  oldpar <- par(no.readonly = TRUE)
  on.exit(par(oldpar))

  if(isFALSE(options$legend.show))
  {
    lgpos <- "n"
  }else if(length(options$legend.position) == 2)
  {
    lgpos <- paste(options$legend.position[2],options$legend.position[1],sep = "")
  }else{
    lgpos <- options$legend.position
  }

  var.text <- options$legend.title
  var.id <- var

  if(is.character(var.id))
  {
    var.id <- match(var.id,colnames(data))
  }


  if(is.null(var.text))
  {
    legend.title = ""

  }else if(is.na(var.text))
  {
    legend.title <- colnames(data)[var.id]
  }

  if(is.null(options$title))
  {

    title = ""

  }else if(is.na(options$title))
  {
    title = ""
  }

  source <- options$credits.source
  aut <- options$credits.author

  if(is.null(source))
  {
    source <- ""

  }

  if(is.null(aut)){

    aut <- ""
  }


  if(is.null(var))
  {
    plot(st_geometry(data))

  }else{



    if(length(var) != 1)
    {

      if(length(options$title) != length(var))
      {
        warning(paste(length(var), " titles shoud be provided."))
      }


      ncl <- ifelse(length(var)>3, 4,length(var))
      nrw <- ceiling(length(var)/4)
      par(mfrow=c(nrw,ncl))

      if(length(options$palette.cont) == 1)
      {
        palette <- rep(options$palette.cont, length(var))
      }else{
        palette <- options$palette.cont
      }
      a <- lapply(1:length(var), function(xx) {
        choroLayer(data, var = var[xx], nclass = options$nclass, col = mapPalette(type = palette[xx], nclass = options$nclass),
                   breaks = options$breaks, legend.pos = lgpos,
                   lwd = options$border.lwd, border = options$border.col,
                   colNA = options$NA.color,
                   legend.title.txt = legend.title[xx],
                   legend.title.cex = options$legend.title.size,
                   legend.values.cex = options$legend.text.size, legend.values.rnd = options$legend.digits,
                   legend.frame = options$legend.frame)


        layoutLayer(title = title, coltitle = options$title.color, col = "white",
                    postitle = options$title.position,
                    frame = options$map.frame, scale = FALSE,
                    sources = source,
                    author = aut, horiz = TRUE, tabtitle = FALSE)


      })
      par(mfrow=c(1,1))

    }else if(length(var) == 1)
    {
      if(is.character(data[var]) | is.factor(data[var]))
      {
        palette = options$palette.cat
      }else{
        palette = options$palette.cont
      }
      palette <- mapPalette(type = palette, nclass = options$nclass)
      choroLayer(data, var = var, nclass = options$nclass, col = palette,
                 breaks = options$breaks, legend.pos = lgpos,
                 lwd = options$border.lwd, border = options$border.col,
                 colNA = options$NA.color,
                 legend.title.txt = legend.title,
                 legend.title.cex = options$legend.title.size,
                 legend.values.cex = options$legend.text.size, legend.values.rnd = options$legend.digits,
                 legend.frame = options$legend.frame)

      if(!is.null(options$labels))
      {
        labelLayer(
          x = data,
          txt = options$labels,
          col= options$labels.color,
          cex = options$labels.size,
          overlap = FALSE,
          show.lines = FALSE
        )
      }

      layoutLayer(title = title, coltitle = options$title.color, col = "white",
                  postitle = options$title.position,
                  frame = options$map.frame, scale = FALSE,
                  sources = source,
                  author = aut, horiz = TRUE, tabtitle = FALSE)

      par(mfrow=c(1,1))
    }
  }
}


mapping_typo <- function(data, var = NULL,
                         options = mapping.options())
{

  oldpar <- par(no.readonly = TRUE)
  on.exit(par(oldpar))

  if(isFALSE(options$legend.show))
  {
    lgpos <- "n"
  }else if(length(options$legend.position) == 2)
  {
    lgpos <- paste(options$legend.position[2],options$legend.position[1],sep = "")
  }else{
    lgpos <- options$legend.position
  }


  var.text <- options$legend.title
  var.id <- var

  if(is.character(var.id))
  {
    var.id <- match(var.id,colnames(data))
  }

  if(is.null(var.text))
  {
    legend.title = ""

  }else if(is.na(var.text))
  {
    legend.title <- colnames(data)[var.id]
  }

  if(is.null(options$title))
  {

    title = ""

  }else if(is.na(options$title))
  {
    title = ""
  }

  source <- options$credits.source
  aut <- options$credits.author

  if(is.null(source) )
  {
    source <- ""

  }

  if(is.null(aut)){

    aut <- ""
  }

  if(is.null(var))
  {
    plot(st_geometry(data))

  }else{


    if(length(var) != 1)
    {
      ncl <- ifelse(length(var)>3, 4,length(var))
      nrw <- ceiling(length(var)/4)
      par(mfrow=c(nrw,ncl))

      if(length(options$palette.cat) == 1)
      {
        palette <- rep(options$palette.cat, length(var))
      }else{
        palette <- options$palette.cat
      }


      lapply(1:length(var), function(xx)
      {
        modal <- nlevels(factor(data[,var[xx], drop = TRUE]))

        typoLayer(data, var = var[xx], col = mapPalette(type = palette[xx], nclass = modal),
                  ,legend.pos = lgpos,
                  lwd = options$border.lwd, border = options$border.col,
                  colNA = options$NA.color,
                  legend.title.txt = legend.title[xx],
                  legend.title.cex = options$legend.title.size,
                  legend.values.cex = options$legend.text.size,
                  legend.frame = options$legend.frame)

        layoutLayer(title = title, coltitle = options$title.color, col = "white",
                    postitle = options$title.position,
                    frame = options$map.frame, scale = FALSE,
                    sources = source,
                    author = aut, horiz = TRUE, tabtitle = FALSE)

      }
      )
      par(mfrow=c(1,1))

    }else if(length(var) == 1)
    {

      modal <- nlevels(factor(data[,var, drop = TRUE]))
      palette <- mapPalette(type = options$palette.cat, nclass = modal)
      typoLayer(data, var = var, col = palette,
                legend.pos = lgpos,
                lwd = options$border.lwd, border = options$border.col,
                colNA = options$NA.color,
                legend.title.txt = legend.title,
                legend.title.cex = options$legend.title.size,
                legend.values.cex = options$legend.text.size,
                legend.frame = options$legend.frame)

      layoutLayer(title = title, coltitle = options$title.color, col = "white",
                  postitle = options$title.position,
                  frame = options$map.frame, scale = FALSE,
                  sources = source,
                  author = aut, horiz = TRUE, tabtitle = FALSE)
      par(mfrow=c(1,1))
    }

  }
}


mapping_bar <- function(data, var = NULL,
                        options = mapping.options())
{


  oldpar <- par(no.readonly = TRUE)
  on.exit(par(oldpar))

  if(is.null(var))
  {
    plot(st_geometry(data))

  }else{

    if(length(var) != 1)
    {
      ncl <- ifelse(length(var)>3, 4,length(var))
      nrw <- ceiling(length(var)/4)
      par(mfrow=c(nrw,ncl))
      palette <- rep(palette, length(var))
      lapply(1:length(var), function(xx)   {    plot(st_geometry(obj = data)) ;propSymbolsLayer(x = data, var = var[xx],symbols = "bar" )})
      par(mfrow=c(1,1))

    }else if(length(var) == 1)
    {
      plot(st_geometry(obj = data))
      propSymbolsLayer(x = data, var = var,symbols = "bar" )
      par(mfrow=c(1,1))
    }

  }
}


