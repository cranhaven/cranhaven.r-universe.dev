#Calcula proporciones

dist_cat_predecir <- function(data, variable, variable.pr){
  res <- data.frame()
  for (i in levels(data[[variable]])) {
    x <- which(data[[variable]] == i)
    for (j in levels(data[[variable.pr]])) {
      y <- which(data[[variable.pr]] == j)
      if(nrow(data[intersect(x,y),]) != 0){
        count <- nrow(data[intersect(x,y),])
        res   <- rbind(res, data.frame(as.factor(i), as.factor(j), count = count))
      }
    }
  }
  
  colnames(res) <- c(
    "variable" ,
    "var.predecir" ,
    "count" 
  )
  prop <- c()
  for (i in levels(data[[variable]])) {
    x <- which(res[["variable"]] == i)
    for (j in levels(data[[variable.pr]])) {
      y    <- which(res[["var.predecir"]] == j)
      cant <- res[intersect(x,y),"count"]
      tot  <- sum(res[x,"count"])
      prop <- c(prop,round(cant/tot,4))
    }
  }
  
  res <- cbind(res, prop)
  return(res)
}

#Gráfica el pairs
pairs_poder  <- function(datos,variable.predecir, col){
  vars.p <- datos[,variable.predecir]
  tam    <- length(unique(vars.p))
  if(tam == 2){
    col <- base::rev(col)
  }
  r      <- pairs.panels(var.numericas(datos),bg = col[datos[,variable.predecir]],
                         pch = 22, main = '', hist.col = col[1], ellipses = FALSE, oma=c(3,3,3,15))
  legend('topright', 
         fill   = unique(col[datos[,variable.predecir]]), 
         legend = c(levels(datos[,variable.predecir])))
  return(r)
}

#Gráfica la densidad de las variables númericas
e_numerico_dens <- function(datos.dens, variable, variable.predecir, label = "${X} ${Y}", colores =c("#F8766D", "#00BFC4", "#00BA38", "#C77CFF", "#00B0F6",
                                                                                                            "#EEEE00", "#CD661D", "#006400","#EE82EE", "#000080")){
  label = str_interp(label,list(X = variable,Y = variable.predecir))
  datos.plot <- data.frame(
    "variable" = datos.dens[, variable],
    "variable.predecir" = datos.dens[, variable.predecir]
  )
  
  categorias <- levels(datos.plot[,"variable.predecir"])
  dens       <- tapply(datos.plot[,"variable"],
                       datos.plot[,"variable.predecir"], 
                       function(x) {density(x, n = 20)})
  res        <- list()
  i = 1
  for (cat in categorias ) {
    dens.data <- lapply(1:20, function(i){c(dens[[cat]]$x[i], dens[[cat]]$y[i])})
    res[[i]]  <- list(type  = "line", 
                      data  = dens.data,  
                      color = colores[i],  
                      name  = cat, 
                      areaStyle = TRUE, 
                      smooth = TRUE)
    i <- i + 1
  }
  
  opts <- list(
    xAxis  = list(show = TRUE),
    yAxis  = list(show = TRUE),
    series = res)
  
  e_charts() |>  
    e_list(opts) |> 
    e_x_axis(scale = T)|> 
    e_title(label,
            left = 'left',
            top = 5,
            textStyle = list(fontSize = 18)) |> 
    e_legend(orient = 'vertical',
             right = '5', top = '15%') |> 
    e_tooltip() |>  e_datazoom(show = F) |>  e_show_loading()
}

#Hace la gráfica de distribuciones según la variable predictiva
e_categorico_dist <- function(datos, variable, var.predecir, label = "${X} ${Y}", labels = c("Porcentaje", "Cantidad"), color =c("#F8766D", "#00BFC4", "#00BA38", "#C77CFF", "#00B0F6",
                                                                                                                                          "#EEEE00", "#CD661D", "#006400","#EE82EE", "#000080") ){
  label = str_interp(label,list(X = variable,
                                Y = var.predecir))
  dataplot <- dist_cat_predecir(datos, variable, var.predecir)
  
  dataplot |> 
    group_by(var.predecir) |> 
    e_charts(variable, stack = "grp", color = color) |> 
    e_bar(prop, count, label = list(show = T)) |> 
    e_title(label,
            left = "left",
            top  = 5,
            textStyle = list(fontSize = 18)) |> 
    e_legend(orient = 'vertical',
             right  = '5', top = '15%') |> 
    e_flip_coords() |> 
    e_tooltip(formatter = e_JS(paste0("function(params){
                                 return('<strong>' + params.value[1] +
                                        '</strong><br/>",labels[1],": ' + parseFloat(params.value[0] * 100).toFixed(2)+
                                        '%<br /> ' + '",labels[2],": ' + params.name)}")))|> 
    e_x_axis(
      formatter = e_axis_formatter("percent", digits = 0)) |> 
    e_labels(position = 'inside' ,formatter =  e_JS("
                                        function(params){
                                        return(params.name + ' (' + parseFloat(params.value[0] *100).toFixed(2) + '%)' )}"))|>  
    e_datazoom(show = F) |>  
    e_show_loading()
}


###############################Generar Codigo##############################################

#Gráfica de distribución de la Variable a predecir
code.dist.varpred <- function(var) {
  paste0(
    "datos.plot <- data.frame (\n",
    "  label = levels(datos[['", var, "']]), value = summary(datos[['", var, "']],\n",
    "  maxsum = length(levels(datos[['", var, "']])))\n",
    ")\n\n",
    "datos.plot |>  e_charts(label) |>  e_bar(value, name = var) |> \n",
    "  e_tooltip() |>  e_datazoom(show = F) |>  e_show_loading()\n"
  )
}

#Gráfica el pairs
code.pairs_poder <- function(variable.predecir){
  return(paste0("vars.p <- datos[,'",variable.predecir,"']\n",
                "col <- rainbow((length(unique(vars.p)) + 1)*2)[seq(2,(length(unique(vars.p)) + 1)*2,2)]\n",
                "col <- col[2:length(col)]\n",
                "pairs.panels(var.numericas(datos),bg = col[datos[,'",variable.predecir,"']],
                pch= 22, main='', hist.col = gg_color_hue(1), ellipses = FALSE, oma=c(3,3,3,15))
                legend('topright', fill = unique(col[datos[,'",variable.predecir,"']]), legend = c(levels(datos[,'",variable.predecir,"'])))\n"))
}
