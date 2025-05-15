# Calcula los puntos para el gráfico de líneas
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

# Valida el tamaño del nombre de los modelos para mostrar uno más corto al momento de visualizar sus nombres en los gráficos de comparación
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

# Gráfico de líneas para la comparación de modelos
resumen.lineas <- function(datos.grafico, labels = c("Global", "repeticion")) {
  puntos <- cv.values(datos.grafico)
  opts <- list(
    xAxis = list(show = TRUE),
    yAxis = list(show = TRUE),
    series = puntos)
  
  comp_plot <- e_charts() |>  
    e_list(opts) |>  
    e_y_axis(formatter = e_axis_formatter("percent",
                                          digits = 0)) |>
    e_axis_labels(x = labels[2],
                  y = paste('%', labels[1])) |>
    e_title(labels[1],
            left = "center",
            top = 5,
            textStyle = list(fontSize = 20)) |>
    e_tooltip(formatter = e_JS("function(params){
                                           return('<strong>' + params.value[0] + ' </strong>' +",
                               " parseFloat(params.value[1] * 100).toFixed(2) + '%' )}")) |>
    e_datazoom(show = F,startValue=1) |>
    e_legend(show = T, type = "scroll", bottom = 1) |>
    e_show_loading()|> e_x_axis(nameLocation = 'middle', nameGap = 35)
  comp_plot$x$opts$yAxis$max <- 1
  comp_plot
}

# Gráfico de barras para la comparación de modelos
resumen.barras <- function(datos.grafico, labels = c("Global", "iteracion"), rotacion = FALSE) {
  datos.grafico <- datos.grafico |>
    dplyr::group_by(name, color) |>
    dplyr::summarise(value = mean(value), .groups = 'drop')
  
  datos.grafico$name <- unlist(lapply(datos.grafico$name, validar.tamanno))
    
  resumen <- datos.grafico |>
    e_charts( name) |>
    e_bar(value, name = var) |> 
    e_add_nested("itemStyle", color) |>
    e_labels(show     = TRUE,
             position = 'top' ,
             formatter =  e_JS("function(params){
                                           return(parseFloat(params.value[1] *100).toFixed(2) + '%' )}")) |>
    e_y_axis(formatter = e_axis_formatter("percent",
                                          digits = 0)) |>
    e_axis_labels(x = labels[2],
                  y = paste('%', labels[1])) |>
    e_title(labels[1],
            left  = "center",
            top   = 5,
            textStyle = list(fontSize = 20)) |>
    e_tooltip(formatter = e_JS("function(params){
                                           return('<strong>' + params.value[0] + ' </strong>' +",
                               " parseFloat(params.value[1] * 100).toFixed(2) + '%' )}")) |>
    e_datazoom(show = F) |>
    e_legend(show = T, type = "scroll", bottom = 1) |>
    e_show_loading()|> e_x_axis(nameLocation = 'middle', nameGap = 35)
  
  if(rotacion)
    resumen <- resumen |> e_x_axis(axisLabel = list(interval= 0, rotate= 30))
  resumen$x$opts$yAxis[[1]]$max <- 1
  resumen$x$opts$legend$data <- datos.grafico$name
  resumen
}

# Gráfico de variación del error para la comparación de modelos
resumen.error <- function(datos.grafico, labels = c("Global", "iteracion", "Valor Maximo", "Valor Minimo")) {
  
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
                                                       "Number.parseFloat(params.value[2]).toFixed(2) + ",
                                                       "'<br/><b>", labels[4], ": </b>' + ",
                                                       "Number.parseFloat(params.value[1]).toFixed(2))}"))))|>
    e_add_nested("itemStyle", color) |>
    e_y_axis(formatter = e_axis_formatter("percent",
                                          digits = 0)) |>
    e_axis_labels(x = labels[2],
                  y = paste('%', labels[1])) |>
    e_title(labels[1],
            left  = "center",
            top   = 5,
            textStyle = list(fontSize = 20)) |>
    e_tooltip(formatter = e_JS("function(params){
                                           return('<strong>' + params.value[0] + ' </strong>' +",
                               " parseFloat(params.value[1] * 100).toFixed(2) + '%' )}")) |>
    e_datazoom(show = F) |>
    e_legend(show = T, type = "scroll", bottom = 1) |>
    e_show_loading()|> e_x_axis(nameLocation = 'middle', nameGap = 35)
  resumen$x$opts$legend$data <- datos.grafico$name
  resumen$x$opts$yAxis[[1]]$max <- 1
  resumen
}

# Calcula la precisión por clase 
precision <- function(clase){
  function(mc){
    indices = general.indexes(mc = mc)
    indices$category.accuracy[clase]
  }
}

# Calcula la precisión global
precision.global <- function(x) sum(diag(x))/sum(x)

#####################   Indices CrossVal   ########################

# category: Categorías de la variable a predecir
# cant.vc: Cantidad de Validaciones
# kernels: modelos/kernels seleccionados
# MCs.cv: lista de listas de Matrices de Confusión
indices.cv <- function(category, cant.vc, kernels, MCs.cv){
  # Lista para almacenar los índices de cada categoría
  ind.categ  <- vector(mode = "list",   length = length(category))
  names(ind.categ) <- category
  
  for (cat in category) {
    # Actualiza el tamaño para que almacene los resultados de cada kernel en cada validación
    ind.categ[[cat]] <- vector(mode = "numeric", length = cant.vc * length(kernels))
  }
  # Colores de cada modelo para el gráfico 
  col_      <- gg_color_hue(length(kernels))
  value     <- vector(mode = "numeric",   length = cant.vc * length(kernels)) # Valor del índice (PG, EG, PP, PN...)
  name      <- vector(mode = "character", length = cant.vc * length(kernels)) # Nombre del modelo
  color     <- vector(mode = "character", length = cant.vc * length(kernels)) # Colores
  rep       <- vector(mode = "numeric",   length = cant.vc * length(kernels)) # Numéro de Validación
  indice    <- 1 # Posición inicial
  
  # Recorre cada modelo
  for (kernel in 1:length(kernels)){
    n <- kernel * cant.vc # Límite de la posición final, asigna espacio para colocar los valores de cada validación 
    rep[indice:(n)]       <- 1:cant.vc # Agrega las validaciones
    name[indice:(n)]      <- kernels[kernel] # Repite el nombre del nombre
    color[indice:(n)]     <- col_[kernel] # Agrega el color del modelo 
    value[indice:(n)]     <- sapply(MCs.cv[[paste0("MCs.", kernels[kernel])]], 
                                    precision.global) # Guarda la PG para cada validación, se obtiene de las matrices de confución almacenadas
    
    for (cat in category) {
      # Recorre las categorías de la variable a predecir para guardar su precisión.
      aux <- sapply(MCs.cv[[paste0("MCs.", kernels[kernel])]], precision(cat))
      ind.categ[[cat]][indice:(n)] <- aux
      names(ind.categ[[cat]])[indice:(n)] <- rep(kernels[kernel], length(aux))
    }
    # Mueve el índice la cantidad de validación que hay 
    indice <- indice + cant.vc
  }
  
  # Estructura el df para realizar el gráfico de comparación 
  grafico    <- data.frame(rep, name, value, color)
  
  # Devuelve los resultados en una lista para ser utilizados cuándo se requieran
  # El valor por defecto del gráfico va a ser la PG
  resultados <- list(grafico = grafico, global = value, categories = ind.categ)
  return(list(grafico = grafico, global = value, categories = ind.categ))
}

##OBTENR PARAMETROS DE FUNCION
#as.list(args(append))[-length(as.list(args(append)))]
#res <- names(as.list(args(append))[-length(as.list(args(append)))])


# Gráfico comparativo de variación del error
comp.lineas <- function(datos.grafico, labels = c("Global", "repeticion") ) {
  
  comp_plot <- datos.grafico |>
    e_charts(rep) |>
    e_line(value, name = var) |>  
    e_y_axis(formatter = e_axis_formatter("percent",
                                          digits = 0)) |>
    e_axis_labels(x = labels[2],
                  y = paste('%', labels[1])) |>
    e_title(labels[1],
            left = "center",
            top = 5,
            textStyle = list(fontSize = 20)) |>
    e_tooltip(formatter = e_JS("function(params){
                                           return('<strong>' + params.value[0] + ' </strong>' +",
                               " parseFloat(params.value[1] * 100).toFixed(2) + '%' )}")) |>
    e_datazoom(show = F,startValue=1) |>
    e_legend(show = T, type = "scroll", bottom = 1) |>
    e_show_loading()|> e_x_axis(nameLocation = 'middle', nameGap = 35)
  comp_plot$x$opts$yAxis[[1]]$max <- 1
  comp_plot
}


# Gráfico comparativo de variación del error
# category: Categorías de la variable a predecir
# n: cantidad de modelos generados, máximo 10
# MCs: lista de Matrices de Confusión
indices.comp <- function(category, MCs, n){
  # Lista que guarda las precisiones por categoría
  ind.categ  <- vector(mode = "list",   length = length(category))
  names(ind.categ) <- category
  
  for (cat in category) {
    # llena la lista con vectores que almacenaran los resultados
    ind.categ[[cat]] <- vector(mode = "numeric",   length = n)
  }
  
  
  col_      <- gg_color_hue(n) # Colores para cada kernel (Aún no se usa)
  rep       <- vector(mode = "numeric",   length = n) # Numéro de modelo generado
  value     <- vector(mode = "numeric",   length = n) # Valor del índice
  color     <- vector(mode = "character", length = n) # Vector para almacenar los colores de cada 
  
  for (i in 1:n){
    rep[i]       <- i
    color[i]     <- col_[i]
    value[i]     <- precision.global(MCs[[i]]) # Guarda PG
    
    for (cat in category) {
      ind.categ[[cat]][i] <- sapply(MCs[i], 
                                    precision(cat))# Guarda Precisión por categoría (aún no se usa)
    }
  }
  
  grafico    <- data.frame(rep, value, color)
  
  resultados <- list(grafico = grafico, global = value, categories = ind.categ)
  return(list(grafico = grafico, global = value, categories = ind.categ))
}






