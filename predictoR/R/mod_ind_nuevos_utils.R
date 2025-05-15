# -------------------  Prediccion Nuevos

#' @import traineR 
#' 

carga.datos.np <- function(nombre.filas = T, ruta = NULL, separador = ";",
                           sep.decimal = ",", encabezado = T) {
  if(!is.null(ruta)) {
    ruta <- gsub("\\", "/", ruta, fixed = T)
  }
  if(nombre.filas) {
    res <- read.table(
      ruta, header = encabezado, sep = separador, dec = sep.decimal,
      stringsAsFactors = T, row.names = 1)
  } else {
    res <- read.table(
      ruta, header = encabezado, sep = separador, dec = sep.decimal,
      stringsAsFactors = T)
  }
  return(res)
}

accion.NAs <- function(datos, deleteNA = T) {
  if(deleteNA) {
    return(na.omit(datos))
  } else {
    moda <- function(x) x[which.max(summary(x))]
    
    for (var in colnames(datos)) {
      if(any(is.na(datos[, var]))) {
        if(class(datos[, var]) %in% c('numeric', 'integer')) {
          datos[, var][is.na(datos[, var])] <- mean(datos[, var], na.rm = T)
        } else {
          datos[, var][is.na(datos[, var])] <- moda(datos[, var])
        }
      }
    }
    return(datos)
  }
}
