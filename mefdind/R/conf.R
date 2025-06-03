#' Parámetros de configuración y mantenimiento
#' @name conf
#' @docType data
#' @description
#' Lista con párametros de configuración y mantenimiento. Incluye patrones
#' para identificar nombre de bases de datos y urls en htmls, además de el
#' url con estadísticas del MEFD.
#'
#' @keywords data
#' @export
conf <- list(

  ##### mefd_meta: identifica el titulo de la base de datos del indicador
  tit_p = '\"titulo t2\">(.*?) *<',

  ##### mefd_name: identifica nombre de la base de datos
  file_p = ".*&file= *(.*?) *.px&",
  tabpx = "Tabla.htm",

  ##### mefd_url: identifica url

  ## parte 1 y 2
  url_1 = "https://estadisticas.educacion.gob.es/",
  url_2 = "EducaJaxiPx/files/_px/es/csv_bdsc",

  ## parte 3
  # seccion con el nombre del indicador
  pre_p = ".*Tabla.htm[?]path= *(.*?) *.&file",
  # sufijo del url (.csv)
  suf_url = "_bdsc?nocab=1"
)


#' meta_mefd
#'
#' Metadatos de indicadores. Incluye el nombre del indicadore, el archivo .csv, el url.
#'
#' @name meta_mefd
#' @keywords datasets
#' @docType data
"meta_mefd"


#' par_url
#'
#' Parámetros con url de páginas del MEFD para crear meta_mefd
#'
#' @name par_url
#' @keywords datasets
#' @docType data
"par_url"
