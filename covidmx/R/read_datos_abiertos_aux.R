#' Auxiliares de lectura para la base de la Direccion General de Epidemiologia
#'
#' @description
#' La funcion principal es [read_datos_abiertos()] la cual decide si los lee de `zip`,
#' `duckdb` o `csv` Tambien puedes usar las auxiliares respectivas
#' * [read_datos_abiertos_zip()]     Si sólo descargaste los datos de la DGE en `.zip`
#' * [read_datos_abiertos_csv()]     Si descargaste los datos de la DGE en `.zip` y 
#'                                   los descomprimiste.
#' * [read_datos_abiertos_duckdb()]  Si ya creaste tu table en `duckdb`
#'
#' @note Para guardar tu base con `duckdb` cambia el `dbdir` a un archivo `.duckdb`. Como ejemplo
#' `dbdir = "ejemplo.duckdb"`.
#'
#' @return Lista de valores:
#' \itemize{
#'   \item dats        - Tabla conectada mediante `DBI::dbConnect` (si `duckdb`) o
#'                       tibble (si `tibble`)
#'   \item disconnect  - Funcion para cerrar la conexion a la base de datos.
#'   \item dict        - Lista de `tibble`s con el diccionario de datos para cada variable
#' }
#'
#' @examples
#' \donttest{
#' # Lee los datos de duckdb una vez descargados
#' # quita la opción de sites.covid para descargar los de la DGE. 
#' # Esto es solo un ejemplo.
#' file_ejemplo <- tempfile(fileext = ".duckdb")
#' 
#' #Estos links deben omitirse en una corrida normal. Se incluyen por ahora como ejemplo
#' #pero las opciones site.covid.dic y sites.covid deben eliminarse de abajo.
#' dlink        <- "https://github.com/RodrigoZepeda/covidmx/raw/main/datos_abiertos_covid19.zip"
#' diclink      <- "https://github.com/RodrigoZepeda/covidmx/raw/main/diccionario_datos_covid19.zip"
#' 
#' #En el ejemplo de R por normas de CRAN tenemos que hacerlo así pero en tu
#' #computadora puedes solo usar descargar datos sin el if else
#' if (RCurl::url.exists(dlink) & RCurl::url.exists(diclink)){
#' 
#'   #Necesitamos la base para verificar los reads
#'   datos_covid <- descarga_datos_abiertos(
#'     dbdir = file_ejemplo,
#'     sites.covid = dlink, 
#'     site.covid.dic = diclink,
#'     show_warnings = FALSE
#'   )
#'   datos_covid$disconnect()
#'   
#'   datos_covid <- read_datos_abiertos(file_ejemplo, show_warnings = FALSE, 
#'                     site.covid.dic = diclink)
#'   datos_covid$disconnect()
#'
#'    # Es lo mismo que:
#'   datos_covid <- read_datos_abiertos_duckdb(file_ejemplo, show_warnings = FALSE,
#'                                             site.covid.dic = diclink)
#'   datos_covid$disconnect()
#'
#'   # Descarga los datos y lee de un zip guardandolos a la vez en
#'   # base de nombre datos_desde_zip.duckdb
#'   direccion_zip <- descarga_db_datos_abiertos_tbl(sites.covid = dlink, show_warnings = FALSE,
#'                                                   site.covid.dic = diclink)
#'                                                  
#'   datos_covid <- read_datos_abiertos(direccion_zip,
#'     dbdir = file_ejemplo,
#'     site.covid.dic = diclink,
#'     show_warnings = FALSE
#'   )
#'   datos_covid$disconnect()
#'
#'   # Es lo mismo que:
#'   datos_covid <- read_datos_abiertos_zip(direccion_zip,
#'     site.covid.dic = diclink,
#'     dbdir = file_ejemplo,
#'     show_warnings = FALSE
#'   )
#'   datos_covid$disconnect()
#'
#'   # Descarga los datos y lee de un csv
#'   direccion_zip <- descarga_db_datos_abiertos_tbl(sites.covid = dlink, show_warnings = FALSE)
#'   direccion_csv <- unzip_db_datos_abiertos_tbl(direccion_zip)
#'   datos_covid   <- read_datos_abiertos(direccion_csv, show_warnings = FALSE, 
#'                                           site.covid.dic = diclink)
#'   datos_covid$disconnect()
#'
#'   # Es lo mismo que:
#'   direccion_csv <- unzip_db_datos_abiertos_tbl(direccion_zip)
#'   datos_covid   <- read_datos_abiertos_csv(direccion_csv, show_warnings = FALSE,
#'                           site.covid.dic = diclink)
#'   datos_covid$disconnect()
#' }
#' }
#' @encoding UTF-8
#' @export
#' @inheritParams descarga_datos_abiertos
#' @inheritParams read_datos_abiertos
read_datos_abiertos_zip <- function(datos_abiertos_zip_paths,
                                    diccionario_zip_path = NULL,
                                    diccionario_unzipped_path = NULL,
                                    diccionario = NULL,
                                    read_format = c("duckdb", "tibble"),
                                    tblname = "covidmx",
                                    drv = duckdb::duckdb(),
                                    dbdir = tempfile(fileext = ".duckdb"),
                                    colClasses = get_col_class(),
                                    download_process = c("pins", "download.file"),
                                    site.covid.dic = paste0(
                                      "http://datosabiertos.salud.",
                                      "gob.mx/gobmx/salud/datos_a",
                                      "biertos/diccionario_datos_",
                                      "covid19.zip"
                                    ),
                                    unzip_command = Sys.getenv("unzip_command"),
                                    unzip_args = Sys.getenv("unzip_args"),
                                    unzip_args_dict = list("exdir" = ".", "overwrite" = TRUE),
                                    check_unzip_install = TRUE,
                                    clear_zip = (download_process[1] != "pins"),
                                    clear_csv = TRUE,
                                    use_dict = TRUE,
                                    quiet = FALSE,
                                    cache_datos = NULL,
                                    use_cache_on_failure = TRUE,
                                    cache_diccionario = NULL,
                                    force_download = FALSE,
                                    show_warnings = TRUE,
                                    board_url_name = "datos_abiertos",
                                    board_url_name_dict = "diccionario_covid",
                                    download_file_args = list(
                                      method   = "curl",
                                      destfile = tempfile(),
                                      quiet    = quiet
                                    ),
                                    descarga_db_diccionario_ssa_args = list(),
                                    ...) {
  do.call(descarga_datos_abiertos, as.list(environment()))
}

#' @export
#' @rdname read_datos_abiertos_zip
# Wrapper para leer los datos abiertos de un csv
read_datos_abiertos_csv <- function(datos_abiertos_unzipped_path,
                                    diccionario_zip_path = NULL,
                                    diccionario_unzipped_path = NULL,
                                    diccionario = NULL,
                                    read_format = c("duckdb", "tibble"),
                                    tblname = "covidmx",
                                    drv = duckdb::duckdb(),
                                    dbdir = tempfile(fileext = ".duckdb"),
                                    colClasses = get_col_class(),
                                    download_process = c("pins", "download.file"),
                                    site.covid.dic = paste0(
                                      "http://datosabiertos.salud.",
                                      "gob.mx/gobmx/salud/datos_a",
                                      "biertos/diccionario_datos_",
                                      "covid19.zip"
                                    ),
                                    unzip_args_dict = list("exdir" = ".", "overwrite" = TRUE),
                                    clear_csv = TRUE,
                                    quiet = FALSE,
                                    use_cache_on_failure = TRUE,
                                    cache_diccionario = NULL,
                                    force_download = FALSE,
                                    show_warnings = TRUE,
                                    board_url_name_dict = "diccionario_covid",
                                    download_file_args = list(
                                      method   = "curl",
                                      destfile = tempfile(),
                                      quiet    = quiet
                                    ),
                                    descarga_db_diccionario_ssa_args = list(),
                                    ...) {
  do.call(descarga_datos_abiertos, as.list(environment()))
}

#' @export
#' @rdname read_datos_abiertos_zip
read_datos_abiertos_duckdb <- function(datos_abiertos_tbl,
                                       drv = duckdb::duckdb(),
                                       tblname = "covidmx",
                                       pragma_memory_limit = Sys.getenv("pragma_memory_limit"),
                                       diccionario_zip_path = NULL,
                                       diccionario_unzipped_path = NULL,
                                       diccionario = NULL,
                                       download_process = c("pins", "download.file"),
                                       site.covid.dic = paste0(
                                         "http://datosabiertos.salud.",
                                         "gob.mx/gobmx/salud/datos_a",
                                         "biertos/diccionario_datos_",
                                         "covid19.zip"
                                       ),
                                       unzip_args_dict = list("exdir" = ".", "overwrite" = TRUE),
                                       clear_zip = download_process[1] != "pins",
                                       clear_csv = TRUE,
                                       use_dict = TRUE,
                                       quiet = FALSE,
                                       use_cache_on_failure = TRUE,
                                       cache_diccionario = NULL,
                                       force_download = FALSE,
                                       show_warnings = TRUE,
                                       board_url_name_dict = "diccionario_covid",
                                       download_file_args = list(
                                         method   = "curl",
                                         destfile = tempfile(),
                                         quiet    = quiet
                                       ),
                                       descarga_db_diccionario_ssa_args = list(),
                                       ...) {
  datos_abiertos_tbl <- do.call(descarga_datos_abiertos, as.list(environment()))
}
