code.carga <- function (nombre.filas = T, ruta = NULL, separador = ";", sep.decimal = ",", 
          encabezado = T, incluir.NA = F) 
{
  res <- paste0("##### doccarga #####\n", "datos <- fread('", 
                ruta, "', sep = '", separador, "', dec = '", sep.decimal, 
                "', header = ", encabezado, ", stringsAsFactors = T, data.table = F, check.names = T)\n")
  if (nombre.filas) {
    res <- paste0(res, "row.names(datos) <- datos[[1]]\n")
    res <- paste0(res, "datos[[1]] <- NULL\n")
  }
  res <- paste0(res, "\n", code.NA(incluir.NA))
  return(res)
}
code.NA <- function (deleteNA = TRUE) 
{
  res <- ifelse(deleteNA, "datos <- na.omit(datos)\n", paste0("Mode <- function(x) {\n  x[which.max(summary(x))]\n}\n", 
                                                              "for (var in colnames(datos)) {\n", "  if(any(is.na(datos[, var]))){\n", 
                                                              "    if(class(datos[, var]) %in% c('numeric', 'integer')) {\n", 
                                                              "      datos[, var][is.na(datos[, var])] <- mean(datos[, var], na.rm = TRUE)\n", 
                                                              "    } else {\n", "      datos[, var][is.na(datos[, var])] <- Mode(datos[, var])\n", 
                                                              "    }\n  }\n}\n"))
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