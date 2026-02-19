#' Datos abiertos de COVID-19
#'
#' Base de datos que contiene una extraccion pequenia de la base de datos
#' abiertos que se obtiene mediante `descarga_datos_abiertos`.
#'
#' @details El proposito de esta base es poder probar las funciones que se aplican sobre
#' `datos_covid`. La base contiene solo las entidades de `BAJA CALIFORNIA` y
#' `BAJA CALIFORNIA SUR` durante julio y agosto del 2021.
#'
#' @format Una lista con tres objetos
#' \describe{
#'   \item{dats}{Base de datos de la DGE actualizada el 8 septiembre 2022 filtrada a BC,
#'               BCS en julio y agosto del 2022}
#'   \item{dict}{Diccionario de datos}
#'   \item{disconnect}{Funcion que simula desconexion de `duckdb`}
#' }
#' @source \url{https://www.gob.mx/salud/documentos/datos-abiertos-152127}
"datosabiertos"
