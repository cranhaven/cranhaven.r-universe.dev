#' Lee la base de datos de la direccion general de epidemiologia que ya descargaste
#'
#' @description
#' `read_datos_abiertos` Lee los datos abiertos almacenados en tu base de `duckdb` que
#' bajaste con `descarga_datos_abiertos`. Intenta de manera automática determinar
#' si los lee de `duckdb`, `csv` ó `zip`
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
#' @param datos_abiertos_path (**obligatorio**) Camino a los datos abiertos si son un `zip`,
#' un `csv` o un `.duckdb`
#' @param ... (**opcional**) parametros adicionales para [descarga_datos_abiertos()]
#' @inheritParams descarga_datos_abiertos
#' @seealso [descarga_datos_abiertos()]  [descarga_datos_red_irag()]
#' [descarga_datos_variantes_GISAID()] [casos()]
#' @examples
#' \donttest{
#' #Archivo temporal donde guardar las cosas es cualquier .duckdb
#' file_duck <- tempfile(fileext = ".duckdb")
#' 
#' #Estos links deben omitirse en una corrida normal. Se incluyen por ahora como ejemplo
#' #pero las opciones site.covid.dic y sites.covid deben eliminarse de abajo.
#' dlink   <- "https://github.com/RodrigoZepeda/covidmx/raw/main/datos_abiertos_covid19.zip"
#' diclink <- "https://github.com/RodrigoZepeda/covidmx/raw/main/diccionario_datos_covid19.zip"
#' 
#' if (RCurl::url.exists(dlink) & RCurl::url.exists(diclink)){
#'   # EJEMPLO 0: Descarga los datos abiertos en archivo file_duck
#'   descarga_datos_abiertos(dbdir = file_duck, sites.covid = dlink, show_warnings = FALSE, 
#'       site.covid.dic = diclink)$disconnect()
#' 
#'   # EJEMPLO 1: Lee los datos de duckdb una vez descargados
#'   datos_covid <- read_datos_abiertos(file_duck, show_warnings = FALSE, 
#'                                       site.covid.dic = diclink) # Lee duckdb
#'   datos_covid$disconnect()
#'
#' # EJEMPLO 2: Lee los datos desde un zip descargado
#' # Descarga archivos de la DGE y guarda el zip
#' direccion_zip <- descarga_db_datos_abiertos_tbl(sites.covid = dlink, show_warnings = FALSE)
#' # Lee zip
#' datos_covid <- read_datos_abiertos(direccion_zip, dbdir = file_duck, show_warnings = FALSE,
#'                     site.covid.dic = diclink)
#' datos_covid$disconnect()
#'
#' # EJEMPLO 3: Lee los datos desde un zip descargado
#' # Descarga archivos zip de la DGE
#' direccion_zip <- descarga_db_datos_abiertos_tbl(sites.covid = dlink, show_warnings = FALSE)
#' direccion_csv <- unzip_db_datos_abiertos_tbl(direccion_zip) # Descomprime el zip para tener csv
#' # Lee los csv
#' datos_covid <- read_datos_abiertos(direccion_csv, dbdir = file_duck, show_warnings = FALSE,
#'                                      site.covid.dic = diclink) 
#' datos_covid$disconnect()
#'
#' # EJEMPLO 4: Si ya tenias el diccionario lo puedes agregar
#' # Simula la idea de ya tener el diccionario
#' diccionario <- descarga_diccionario(show_warnings = FALSE, site.covid.dic = diclink) 
#' datos_covid <- read_datos_abiertos(file_duck, diccionario = diccionario, show_warnings = FALSE)
#' datos_covid$disconnect()
#'
#' # EJEMPLO 5: Si ya tenias el diccionario como archivo zip
#' # Descarga el diccionario para tenerlo como zip
#' diccionario_zip <- descarga_db_diccionario_ssa(show_warnings = FALSE, site.covid.dic = diclink) 
#' datos_covid <- read_datos_abiertos(file_duck, diccionario_zip_path = diccionario_zip, 
#'                                     show_warnings = FALSE)
#' datos_covid$disconnect()
#'
#' # EJEMPLO 6: Si ya tenias el diccionario como archivo xlsx
#' # Descarga el diccionario para tenerlo como zip
#' diccionario_zip <- descarga_db_diccionario_ssa(show_warnings = FALSE, site.covid.dic = diclink)
#' # Abre el csv del diccionario
#' diccionario_csv <- unzip_db_diccionario_ssa(diccionario_zip) 
#' datos_covid <- read_datos_abiertos(file_duck,
#'   diccionario_unzipped_path = diccionario_csv,
#'   show_warnings = FALSE
#' )
#' datos_covid$disconnect()
#' }
#' }
#' @encoding UTF-8
#' @export
read_datos_abiertos <- function(datos_abiertos_path = NULL,
                                dbdir = tempfile(fileext = ".duckdb"),
                                tblname = "covidmx",
                                pragma_memory_limit = Sys.getenv("pragma_memory_limit"),
                                drv = duckdb::duckdb(),
                                colClasses = get_col_class(),
                                read_format = c("duckdb", "tibble"),
                                ...) {

  # Fix if first entry is a duckdb set dbdir also
  if (!is.null(datos_abiertos_path) & all(tools::file_ext(datos_abiertos_path) == "duckdb")) {
    dbdir <- datos_abiertos_path
    datos_abiertos_path <- NULL
  }

  # Case duckdb
  if (is.null(datos_abiertos_path) & all(tools::file_ext(dbdir) == "duckdb")) {
    datos_covid <- read_datos_abiertos_duckdb(
      datos_abiertos_tbl = dbdir,
      pragma_memory_limit = pragma_memory_limit,
      drv      = drv,
      tblname  = tblname,
      ...
    )

    # Case csv
  } else if (!is.null(datos_abiertos_path) & all(tools::file_ext(datos_abiertos_path) == "csv")) {
    datos_covid <- read_datos_abiertos_csv(
      datos_abiertos_unzipped_path = datos_abiertos_path,
      drv      = drv,
      tblname  = tblname,
      dbdir    = dbdir,
      read_format = read_format,
      colClasses = colClasses,
      ...
    )

    # Case zip
  } else if (!is.null(datos_abiertos_path) & all(tools::file_ext(datos_abiertos_path) == "zip")) {
    datos_covid <- read_datos_abiertos_zip(
      datos_abiertos_zip_paths = datos_abiertos_path,
      drv         = drv,
      tblname     = tblname,
      dbdir       = dbdir,
      read_format = read_format,
      colClasses  = colClasses,
      ...
    )
  }

  return(datos_covid)
}
