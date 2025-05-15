cv.values <- function(datos){
  modelos <- unique(datos$name)
  puntos <- vector(mode = "list", length = length(modelos))
  j <- 1
  for (modelo in modelos) {
    res <- datos[datos$name == modelo,]
    punto <- vector(mode = "list", length = nrow(res))
    for (i in 1:nrow(res)) {
      punto[i] <- append(list(c(res$rep[i], res$value[i])), punto)
    }
    puntos[[j]] <- list(data = punto,
                        type  = "line",  
                        color = res$color, 
                        name = modelo)
    j <- j + 1
  }
  return(puntos)
}

summary_indices_v <- function(data){
  return(c( max(data), min(data),quantile(data, prob=c(0.25)),quantile(data, prob=c(0.75))))
}

#Obtiene los nombres de columnas o regresa un string vacio
colnames.empty <- function(res){
  res <- colnames(res)
  if(is.null(res))
    return("")
  return(res)
}

validar.tamanno <- function(text){
  if(nchar(text) > 15){
    aux <- unlist(strsplit(text, " "))
    if(length(aux) == 2)
      aux <- paste0(substr(aux[1], 1, 1), ". ", aux[2])
    if(length(aux) == 3)
      aux <- paste0(substr(aux[1], 1, 1), ". ", aux[2]," ", aux[3])
    if(length(aux) == 4)
      aux <- paste0(substr(aux[1], 1, 1), ". ",substr(aux[2], 1, 1), ". ", aux[3]," ", aux[4])
    return(aux)
  }
  return(text)  
}


indices.comp <- function(MCs, n){

  col_      <- gg_color_hue(n)
  rep       <- vector(mode = "numeric",   length = n)
  value     <- vector(mode = "numeric",   length = n)
  ea        <- vector(mode = "numeric",   length = n)
  er        <- vector(mode = "numeric",   length = n)
  corr      <- vector(mode = "numeric",   length = n)
  color     <- vector(mode = "character", length = n)
  
  for (i in 1:n){
    rep[i]    <- i
    color[i]  <- col_[i]
    value[i]  <- MCs[[i]]$Raiz.Error.Cuadratico
    ea[i]     <- MCs[[i]]$Error.Absoluto
    er[i]     <- MCs[[i]]$Error.Relativo/100
    corr[i]   <- MCs[[i]]$Correlacion

  }
  
  grafico    <- data.frame(rep, value, color, ea, er, corr)
  
  resultados <- list(grafico = grafico, global = value)
  return(list(grafico = grafico, global = value))
}

indices.cv <- function( cant.vc, kernels, MCs.knn){

  col_      <- gg_color_hue(length(kernels))
  value     <- vector(mode = "numeric",   length = cant.vc * length(kernels))
  ea        <- vector(mode = "numeric",   length = cant.vc * length(kernels))
  er        <- vector(mode = "numeric",   length = cant.vc * length(kernels))
  corr      <- vector(mode = "numeric",   length = cant.vc * length(kernels))
  name      <- vector(mode = "character", length = cant.vc * length(kernels))
  color     <- vector(mode = "character", length = cant.vc * length(kernels))
  rep       <- vector(mode = "numeric",   length = cant.vc * length(kernels))
  indice    <- 1
  
  for (kernel in 1:length(kernels)){
    n <- kernel * cant.vc
    rep[indice:(n)]       <- 1:cant.vc
    name[indice:(n)]      <- kernels[kernel]
    color[indice:(n)]     <- col_[kernel]
    valores <- sapply(MCs.knn[[paste0("MCs.", kernels[kernel])]], function(x) {x})
    value[indice:(n)]     <- valores[1,]
    ea[indice:(n)]        <- valores[2,]
    er[indice:(n)]        <- valores[3,]/100
    corr[indice:(n)]      <- valores[4,]
    indice <- indice + cant.vc
  }
  
  grafico    <- data.frame(rep, name, value, color)
  
  resultados <- list(grafico = grafico, ea = ea, er = er, corr = corr)
  return(resultados)
}



resumen.lineas <- function(datos.grafico, labels = c("Global", "repeticion"), percent = FALSE, vals) {
  puntos <- cv.values(datos.grafico)
  opts <- list(
    xAxis = list(show = TRUE),
    yAxis = list(show = TRUE),
    series = puntos)
  
  comp_plot <- e_charts() |>  
    e_list(opts)  |>
    e_title(labels[1],
            left = "center",
            top = 5,
            textStyle = list(fontSize = 20))  |>
    e_datazoom(show = F,startValue=1) |>
    e_legend(show = T, type = "scroll", bottom = 1) |>
    e_show_loading()|> e_x_axis(nameLocation = 'middle', nameGap = 35)|>
    e_group_g(
      right    = "15%",
      top      = "80%",
      z        = 100,
      bounding = 'raw',
      children = 
        list(
          list(
            type  = 'rect',
            left  = 'center',
            top   = 'center',
            z     = 100,
            shape = list(
              width  = 210,
              height = 100
            ),
            style = list(
              fill   = 'rgba(0,0,0,0.3)'
            )
          ),
          list(
            type  = 'text',
            left  = 'center',
            top   = 'center',
            z     = 100,
            style = list(
              fill = '#fff',
              text = paste0(labels[4] ,': ',vals[2] ,'\n', 
                            labels[3] ,': ',vals[1] ,'\n',
                            labels[5] ,': ',vals[3] ,'\n',
                            labels[6] ,': ',vals[4]),
              font = 'bold 18px sans-serif'
            )
          )
        )
    )
  
  
  
  
  if(percent){
    comp_plot <- comp_plot|>  
      e_y_axis(formatter = e_axis_formatter("percent",
                                            digits = 0)) |>
      e_axis_labels(x = labels[2],
                    y = paste('%', labels[1]))|>
      e_tooltip(formatter = e_JS("function(params){
                                           return('<strong>' + params.value[0] + ' </strong>' +",
                                 " parseFloat(params.value[1] * 100).toFixed(5) + '%' )}"))
  }else{
    comp_plot <- comp_plot|>  
      e_axis_labels(x = labels[2],
                    y = labels[1])|>
      e_tooltip(formatter = e_JS("function(params){
                                           return('<strong>' + params.value[0] + ' </strong>' +",
                                 " parseFloat(params.value[1] ).toFixed(5))}"))
  }
  comp_plot
}

resumen.barras <- function(datos.grafico, labels = c("Global", "iteracion"), percent = FALSE, rotacion = FALSE, vals) {
  
  datos.grafico <- datos.grafico |>
    dplyr::group_by( name, color ) |>
    dplyr::summarise(value = mean(value), .groups = 'drop')
  datos.grafico$name <- unlist(lapply(datos.grafico$name, validar.tamanno))
  
  resumen <- datos.grafico |>
    e_charts( name) |>
    e_bar(value, name = var) |> 
    e_add_nested("itemStyle", color)  |>
    e_title(labels[1],
            left  = "center",
            top   = 5,
            textStyle = list(fontSize = 20)) |>
    e_datazoom(show = F) |>
    e_legend(show = T, type = "scroll", bottom = 1) |>
    e_show_loading()|> e_x_axis(nameLocation = 'middle', nameGap = 35)|>
    e_group_g(
      right    = "15%",
      top      = "80%",
      z        = 100,
      bounding = 'raw',
      children = 
        list(
          list(
            type  = 'rect',
            left  = 'center',
            top   = 'center',
            z     = 100,
            shape = list(
              width  = 210,
              height = 100
            ),
            style = list(
              fill   = 'rgba(0,0,0,0.3)'
            )
          ),
          list(
            type  = 'text',
            left  = 'center',
            top   = 'center',
            z     = 100,
            style = list(
              fill = '#fff',
              text = paste0(labels[4], ': ' , vals[2] ,'\n',
                            labels[3], ': ' , vals[1] ,'\n',
                            labels[5], ': ' , vals[3] ,'\n',
                            labels[6], ': ' , vals[4]),
              font = 'bold 18px sans-serif'
            )
          )
        )
    )
  
  
  
  
  if(rotacion)
    resumen <- resumen |> e_x_axis(axisLabel = list(interval= 0, rotate= 30))
  
  if(percent){
    resumen <- resumen|>  
      e_y_axis(formatter = e_axis_formatter("percent",
                                            digits = 0)) |>
      e_axis_labels(x = labels[2],
                    y = paste('%', labels[1])) |>
      e_tooltip(formatter = e_JS("function(params){
                                           return('<strong>' + params.value[0] + ' </strong>' +",
                                 " parseFloat(params.value[1] * 100).toFixed(5) + '%' )}"))|>
      e_labels(show     = TRUE,
               position = 'top' ,
               formatter =  e_JS("function(params){
                                           return(parseFloat(params.value[1] *100).toFixed(3) + '%' )}"))
  }else{
    resumen <- resumen|>  
      e_axis_labels(x = labels[2],
                    y = labels[1])|>
      e_tooltip(formatter = e_JS("function(params){
                                           return('<strong>' + params.value[0] + ' </strong>' +",
                                 " parseFloat(params.value[1]).toFixed(5) )}"))|>
      e_labels(show     = TRUE,
               position = 'top' ,
               formatter =  e_JS("function(params){
                                           return(parseFloat(params.value[1]).toFixed(5))}"))
  }
  resumen$x$opts$legend$data <- datos.grafico$name
  resumen
}

resumen.error <- function(datos.grafico, labels = c("Global", "iteracion", "Valor Maximo", "Valor Minimo"), percent = FALSE, vals) {
  
  datos.grafico <- datos.grafico |> 
    dplyr::group_by( name, color ) |>
    dplyr::summarise( min = min(value), 
                      max = max(value), 
                      value = mean(value), .groups = 'drop')
  
  resumen <- datos.grafico |>
    e_charts(name) |>
    e_scatter(value, name = var, symbol_size = 10) |> 
    e_error_bar(min, max, 
                tooltip = list(formatter = e_JS(paste0("function(params){",
                                                       "return('<b>", labels[3], ": </b>' + ",
                                                       "Number.parseFloat(params.value[2]).toFixed(5) + ",
                                                       "'<br/><b>", labels[4], ": </b>' + ",
                                                       "Number.parseFloat(params.value[1]).toFixed(5))}"))))|>
    e_add_nested("itemStyle", color) |>
    e_title(labels[1],
            left  = "center",
            top   = 5,
            textStyle = list(fontSize = 20))  |>
    e_datazoom(show = F) |>
    e_legend(show = T, type = "scroll", bottom = 1) |>
    e_show_loading()|> e_x_axis(nameLocation = 'middle', nameGap = 35)|>
    e_group_g(
      right    = "15%",
      top      = "80%",
      z        = 100,
      bounding = 'raw',
      children = 
        list(
          list(
            type  = 'rect',
            left  = 'center',
            top   = 'center',
            z     = 100,
            shape = list(
              width  = 210,
              height = 100
            ),
            style = list(
              fill   = 'rgba(0,0,0,0.3)'
            )
          ),
          list(
            type  = 'text',
            left  = 'center',
            top   = 'center',
            z     = 100,
            style = list(
              fill = '#fff',
              text = paste0(labels[4] ,': ',vals[2] ,'\n',
                            labels[3] ,': ',vals[1] ,'\n',
                            labels[5] ,': ',vals[3] ,'\n',
                            labels[6] ,': ',vals[4]),
              font = 'bold 18px sans-serif'
            )
          )
        )
    )
  
  
  
  
  
  if(percent){
    resumen <- resumen|>  
      e_y_axis(formatter = e_axis_formatter("percent",
                                            digits = 0)) |>
      e_axis_labels(x = labels[2],
                    y = paste('%', labels[1]))|>
      e_tooltip(formatter = e_JS("function(params){
                                           return('<strong>' + params.value[0] + ' </strong>' +",
                                 " parseFloat(params.value[1] ).toFixed(5) + '%' )}"))
  }else{
    resumen <- resumen|>  
      e_axis_labels(x = labels[2],
                    y = labels[1])|>
      e_tooltip(formatter = e_JS("function(params){
                                           return('<strong>' + params.value[0] + ' </strong>' +",
                                 " parseFloat(params.value[1]).toFixed(5)  )}"))
  }
  resumen$x$opts$legend$data <- datos.grafico$name
  resumen
}

comp.lineas <- function(datos.grafico, labels = c("Global", "repeticion"), percent = FALSE) {
  
  comp_plot <- datos.grafico |>
    e_charts(rep) |>
    e_line(value, name = var) |>
    e_title(labels[1],
            left = "center",
            top = 5,
            textStyle = list(fontSize = 20)) |>
    e_datazoom(show = F,startValue=1) |>
    e_legend(show = T, type = "scroll", bottom = 1) |>
    e_show_loading()|> e_x_axis(nameLocation = 'middle', nameGap = 35)
  
  
  if(percent){
    comp_plot <- comp_plot|>  
      e_y_axis(formatter = e_axis_formatter("percent",
                                            digits = 0)) |>
      e_axis_labels(x = labels[2],
                    y = paste('%', labels[1]))|>
      e_tooltip(formatter = e_JS("function(params){
                                           return('<strong>' + params.value[0] + ' </strong>' +",
                                 " parseFloat(params.value[1] ).toFixed(5) + '%' )}"))
  }else{
    comp_plot <- comp_plot|>  
      e_axis_labels(x = labels[2],
                    y = labels[1])|>
      e_tooltip(formatter = e_JS("function(params){
                                           return('<strong>' + params.value[0] + ' </strong>' +",
                                 " parseFloat(params.value[1]).toFixed(5)  )}"))
  }
  comp_plot
}