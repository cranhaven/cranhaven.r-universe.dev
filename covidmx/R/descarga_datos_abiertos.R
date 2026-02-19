#' Descarga de datos abiertos
#'
#' @description Funcion para la descarga de datos abiertos de
#' la Direccion General de Epidemiologia (DGE)
#'
#'
#' @details
#' La funcion de descarga principal es [descarga_datos_abiertos()] llama las siguientes funciones
#' en orden:
#'
#' * [descarga_diccionario()] Se encarga de descargar y formatear el diccionario de datos
#' * [descarga_db()] Se encarga de descargar y formatear la base de datos
#' * [pega_db_datos_abiertos_tbl_y_diccionario()] Pega ambos en el formato lista de `covidmx`
#'
#' A su vez [descarga_diccionario()] ejecuta las siguientes para obtener el diccionario de datos:
#' * [descarga_db_diccionario_ssa()] Descarga el diccionario de la DGE
#' * [unzip_db_diccionario_ssa()] Libera el archivo `zip` descargado
#' * [parse_db_diccionario_ssa()] Genera una lista de tiblles con el diccionario por variable
#'
#' Por otro lado,[descarga_db()] ejecuta las siguientes para obtener los datos abiertos:
#' * [descarga_db_datos_abiertos_tbl()] Descarga las bases de datos de covid de la DGE
#' * [unzip_db_datos_abiertos_tbl()] Libera el archivo `zip` descargado
#' * [parse_db_datos_abiertos_tbl()] Genera una base de datos en `duckdb` (o `tibble`) c
#'                                   on la informacion
#'
#' Si en algun momento se interrumpio la descarga o hubo problemas de conexion o detuviste
#' el proceso de generacion de la base de datos abiertos puedes llamar a las funciones
#' de [read_datos_abiertos()].
#'
#' @section Memoria `RAM`:
#'
#' Si tienes RAM que te sobre puedes no crear una base de datos en `duckdb` sino leer directo
#' el archivo `csv`. Esto se logra con `read_format = tibble`. No lo recomiendo pues puedes
#' terminar con tu sesion de `R` si se te acaba la memoria.
#'
#' _Windows_ Para abrir el archivo `.zip` quiza requieras tambien descargar e instalar
#'  [`7Zip`](https://www.7-zip.org/) por default el sistema lo busca en
#'  `C:\\Program Files\\7-Zip\\7z.exe` pero si no esta ese directorio es necesario que
#'  en `unzip_command` especifiques el camino donde se instalo `7z.exe`.
#'
#' @section Uso de `pins`:
#'
#' Para almacenar los datos se utiliza un pequenio cambio sobre la libreria `pins`. Los datos
#' se descargan y se almacenan en cache junto con informacion sobre cuando fue la descarga. Si
#' no ha pasado un dia desde la ultima descarga no se descarga nada nuevo. Si los datos que
#' se tienen no han cambiado respecto a lo que esta en linea tampoco se vuelven a descargar aunque
#' haya pasado mas de un dia.
#'
#' **Si se te fue el Internet** No te preocupes, `pins` lee tu descarga mas reciente.
#'
#' Para ver donde estan descargados tus datos usa `pins::board_cache_path()`. Para borrarlos usa
#' `pins::cache_prune()`.
#'
#' @section Metodos de `unzip`:
#'
#' Por default el programa intenta abrir la base de datos con `utils::unzip()`. Sin embargo
#' historicamente la base de datos ha estado codificada de tal forma que `utils::unzip()` no
#' pueda abrirla. Para ello se utilizaban diferentes comandos en particular el default que
#' hemos visto funcionaba son los comandos de terminal `unzip` (en Linux/OSX) y `7zip` (en Windows).
#' En caso de ser requeridos el sistema te lo hara saber junto con las instrucciones de instalacion
#'
#' @note No te recomiendo borrar el cache con `clear_zip` o editarlo por cualquier otro medio si
#' estas usando `pins` pues puede romperse la dependencia. Si accidentalmente lo borraste
#' usa `pins::board_cache_path()` para ir al `path` y borrar manualmente toda la carpeta.
#'
#' @param dbdir (**opcional**) Direccion donde guardar la base de datos con terminacion `.duckdb`.
#' Corresponde al argumento de [duckdb::dbConnect__duckdb_driver()]
#'
#' @param sites.covid (**opcional**)  Sitios web con el vinculo a los archivos `.zip` de l
#' os datos abiertos. Puedes cambiarlo por uno de los historicos, por ejemplo. La estructura es
#' `c("nombre" = "url", "nombre2" = "url2")`. La ultima verificacion del sitio web default fue
#' el 6 de septiembre del 2022.
#'
#' @param site.covid.dic (**opcional**)  Sitio desde el cual descarga del diccionario de datos.
#'  La ultima verificacion del sitio fue el 6 de septiembre 2022.
#'
#' @param read_format (**opcional**) \code{"duckdb"} o \code{"tibble"} establece el formato
#' de lectura de la base de datos. En la mayoria de los casos \code{"tibble"} va a
#' resultar en un error de memoria. La opcion de \code{"duckdb"} siempre es mas rapida por lo cual
#' es el default.
#'
#' @param download_process (**opcional**)  Metodo para descargar ya sea `pins` o `download.file`.
#' Se recomienda `pins` pues guarda en memoria la fecha de la ultima descarga y analiza
#' si ha pasado mas de un dia desde la descarga. En caso afirmativo verifica si el
#' archivo ha cambiado y si hubo cambios entonces lo descarga.
#'
#' @param quiet (**opcional**) Variable para no mostrar mensajes
#'
#' @param pragma_memory_limit (**opcional**) Limite de memoria para el programa
#' (ver [PRAGMAS](https://duckdb.org/docs/sql/pragmas)). Cambialo a que sea mas o menos la mitad
#' de tu RAM. La forma mas sencilla es como una variable ambiental con
#' \code{Sys.setenv('pragma_memory_limit' = '1GB')} por ejemplo para un limite de 1 gigabyte.
#'
#' @param tblname (**opcional**)  Nombre de la tabla de `duckdb` donde guardar los datos por
#' default se llama `covidmx`. Solo es relevante si estas usando el mismo `dbdir` para otro
#' proyecto distinto.
#'
#' @param cache_datos (**opcional**) Direccion donde guardar los datos en memoria usando `pins`
#' para no tener que volver a descargarlos si nada ha cambiado
#'
#' @param cache_diccionario (**opcional**) Direccion donde guardar el diccionario en memoria
#' usando `pins` para no tener que volver a descargarlo si nada ha cambiado
#'
#' @param use_cache_on_failure  (**opcional**) Booleana. Establece que si no se pueden descargar
#' datos nuevos utilice los que tenga en memoria. Por default es `TRUE`.
#'
#' @param board_url_name (**opcional**) Establece el nombre del `pins::board_url` para
#' los datos abiertos (si ya usas pins para que no se empalme).
#' Por default se llama `datos_abiertos`
#'
#' @param board_url_name_dict (**opcional**) Establece el nombre del `pins::board_url` para los 
#' datos abiertos. Por default se llama `diccionario_covid`
#'
#' @param clear_zip (**opcional**) Si borrar los archivos `.zip` descargados para el diccionario 
#' y los datos abiertos. No se recomienda si estas usando `pins`. Ve la nota para mas informacion.
#'
#' @param clear_csv (**opcional**) Si borrar los archivos `.csv` que se generan despues de abrir 
#' el zip. El default es que si pues en general solo requieres el `duckdb`.
#'
#' @param use_dict (**opcional**) Si descargar el diccionario de `site.covid.dic`.
#'
#' @param unzip_command (**opcional**) Forma de extraer la base de datos de datos abiertos 
#' si `unzip` falla.
#' La forma de llamarla es con `system2(unzip_command, args = c(unzip_args, file_download_data))`.
#'
#' @param unzip_args (**opcional**) Argumentos de extraccion de la base de datos de datos abiertos 
#' si `unzip` falla.
#' La forma de llamarla es con `system2(unzip_command, args = c(unzip_args, file_download_data))`.
#'
#' @param unzip_args_dict (**opcional**) Lista de argumentos para usar `utils::unzip` en el 
#' diccionario de datos.
#'
#' @param check_unzip_install (**opcional**) Bandera de verificacion para checar si tienes 
#' lo necesario para unzippear los datos en el caso de que `unzip` no sirva.
#'
#' @param descarga_db_datos_abiertos_tbl_args (**opcional**) Lista con argumentos adicionales 
#' para el `pins::pin_download` de datos abiertos
#'
#' @param download_file_args (**opcional**) Lista de argumentos adicionales para `download.file` 
#' de los datos si se elige este metodo para descargar.
#'
#' @param descarga_db_diccionario_ssa_args (**opcional**) Lista con argumentos adicionales para el
#' `pins::pin_download` de datos abiertos
#'
#' @param download_file_args_dict (**opcional**) Lista de argumentos adicionales 
#' para `download.file` del diccionario si se elige este metodo de descarga.
#'
#' @param force_download (**opcional**) Analiza si cambio el pin y descarga datos nuevos en caso 
#' afirmativo aunque haya pasado menos de un dia.
#'
#' @param show_warnings (**opcional**) si arrojar `warnings`
#'
#' @param datos_abiertos_zip_paths (**opcional**)  Camino a los datos abiertos si ya los 
#' descargaste en `zip`
#'
#' @param datos_abiertos_unzipped_path (**opcional**)  Camino a los datos abiertos `csv` si ya 
#' los descargaste y descomprimiste el archivo `zip` en un `csv`
#'
#' @param datos_abiertos_tbl (**opcional**) Camino a un archivo `.duckdb` con los datos formateados
#'
#' @param diccionario_zip_path (**opcional**)  Camino al diccionario si ya losdescargaste en `zip`
#'
#' @param diccionario_unzipped_path (**opcional**)  Camino al diccionario `csv` si ya 
#' lo descargaste y descomprimiste el archivo `zip` en un `csv`
#'
#' @param diccionario (**opcional**)  Lo que resulta de realizar una descarga del diccionario
#' usando `descarga_diccionario`
#'
#' @param drv   (**opcional**) Un  driver para `dbConnect` (default `duckdb::duckdb()`)
#'
#' @param ...  (**opcional**) Parametros adicionales para `DBI::dbConnect`.
#'
#' @param colClasses  (**opcional**) Clases de la columna para leer en `duckdb::read_csv_duckdb()`.
#'
#' @return Lista de valores:
#' \itemize{
#'   \item dats        - Tabla conectada mediante `duckdb::dbConnect__duckdb_driver()` 
#'                       (si `duckdb`) o tibble (si `tibble`)
#'   \item disconnect  - Funcion para cerrar la conexion a la base de datos.
#'   \item dict        - Lista de `tibble`s con el diccionario de datos para cada variable
#' }
#' @examples
#' \donttest{
#' # Descarga de la base de datos junto con diccionario en duckdb y la guarda en
#' # un archivo temporal.
#' # Puede cambiarse el dlink por el adecuado o dejarse en blanco
#' # quita la opción de sites.covid t site.covid.dic para descargar los de la DGE. 
#' # Esto es solo un ejemplo.
#' file_duck <- tempfile(fileext = ".duckdb")
#' 
#' #Estos links deben omitirse en una corrida normal. Se incluyen por ahora como ejemplo
#' #pero las opciones site.covid.dic y sites.covid deben eliminarse de abajo.
#' dlink     <- "https://github.com/RodrigoZepeda/covidmx/raw/main/datos_abiertos_covid19.zip"
#' diclink   <- "https://github.com/RodrigoZepeda/covidmx/raw/main/diccionario_datos_covid19.zip"
#' 
#' #En el ejemplo de R por normas de CRAN tenemos que hacerlo así pero en tu
#' #computadora puedes solo usar descargar datos sin el if else
#' if (RCurl::url.exists(dlink) & RCurl::url.exists(diclink)){
#'   datos_covid <- descarga_datos_abiertos(
#'     dbdir = file_duck,
#'     sites.covid = dlink, 
#'     site.covid.dic = diclink,
#'     show_warnings = FALSE
#'   )
#'   # Luego haces algo con esos datos...
#'
#'   # Cuando terminas cierras la sesion:
#'   datos_covid$disconnect()
#'
#'   # Despues podras leerlos con read_datos_abiertos cuando quieras:
#'   datos_covid <- read_datos_abiertos(dbdir = file_duck, site.covid.dic = diclink)
#'   datos_covid$disconnect()
#'
#'   # Si no pones `dbdir` nota que los datos se guardan en un archivo temporal que se elimina
#'   # al cerrar tu sesion
#'   datos_covid <- descarga_datos_abiertos(sites.covid = dlink, show_warnings = FALSE,
#'                       site.covid.dic = diclink)
#'
#' } else {
#'   datos_covid <- datosabiertos
#' }
#' 
#' # Desconectamos
#' datos_covid$disconnect()
#' }
#' @encoding UTF-8
#' @seealso [read_datos_abiertos()]  [descarga_datos_red_irag()]
#' [descarga_datos_variantes_GISAID()] [casos()]
#'
#' @references
#'
#' Secretaría de Salud (2022). Datos Abiertos de COVID-19
#' URL: \url{https://www.gob.mx/salud/documentos/datos-abiertos-152127}
#'
#' @export
descarga_datos_abiertos <- function(dbdir = tempfile(fileext = ".duckdb"),
                                    sites.covid = get_sites_covid(),
                                    site.covid.dic = get_site_dic(),
                                    read_format = c("duckdb", "tibble"),
                                    drv = duckdb::duckdb(),
                                    pragma_memory_limit = Sys.getenv("pragma_memory_limit"),
                                    tblname = "covidmx",
                                    colClasses = get_col_class(),
                                    download_process = c("pins", "download.file"),
                                    unzip_command = Sys.getenv("unzip_command"),
                                    unzip_args = Sys.getenv("unzip_args"),
                                    unzip_args_dict = list("exdir" = ".", "overwrite" = TRUE),
                                    check_unzip_install = TRUE,
                                    clear_zip = (download_process[1] != "pins"),
                                    clear_csv = TRUE,
                                    use_dict = TRUE,
                                    datos_abiertos_zip_paths = NULL,
                                    datos_abiertos_unzipped_path = NULL,
                                    datos_abiertos_tbl = NULL, # FIXME
                                    diccionario_zip_path = NULL,
                                    diccionario_unzipped_path = NULL,
                                    diccionario = NULL,
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
                                    download_file_args_dict = download_file_args,
                                    descarga_db_datos_abiertos_tbl_args = list(),
                                    descarga_db_diccionario_ssa_args = list(),
                                    ...) {
  if (!quiet) {
    cli::cli_h1("Hola esto es lo que hare hoy para ti:")
    cli::cli_ol()
    cli::cli_li("Base de datos de covid-19 de la Direccion General de Epidemiologia:")
    ulid <- cli::cli_ul()
    if (is.null(datos_abiertos_zip_paths) & is.null(datos_abiertos_unzipped_path) &
      is.null(datos_abiertos_tbl)) {
      cli::cli_li("{.strong Descargar} las bases de datos abiertos")
    }
    if (is.null(datos_abiertos_unzipped_path) & is.null(datos_abiertos_tbl)) {
      cli::cli_li("{.strong Descomprimir} las bases de datos abiertos")
    }
    if (is.null(datos_abiertos_tbl)) {
      cli::cli_li("{.strong Consolidar} en una sola base de datos")
    }
    if (!is.null(datos_abiertos_tbl)) {
      cli::cli_alert_success("Ya lo tienes.")
    }
    cli::cli_end(ulid)
    if (use_dict) {
      cli::cli_li("Diccionario de datos de covid-19 de la Direccion General de Epidemiologia:")
      ulid <- cli::cli_ul()
      if (is.null(diccionario_zip_path) & is.null(diccionario_unzipped_path) & is.null(diccionario)) {
        cli::cli_li("{.strong Descargar} el diccionario")
      }
      if (is.null(diccionario_unzipped_path) & is.null(diccionario)) {
        cli::cli_li("{.strong Descomprimir} el diccionario")
      }
      if (is.null(diccionario)) {
        cli::cli_li("{.strong Consolidar} todos los diccionarios en una lista")
      }
      if (!is.null(diccionario)) {
        cli::cli_alert_success("Ya lo tienes.")
      }
      cli::cli_end(ulid)
      cli::cli_li("{.strong Unir} datos y diccionario en una sola lista")
    }
    cli::cli_end()
    cli::cli_h1("Comenzamos (por favor ten paciencia):")
  }

  # Descarga los datos abiertos con descarga_db
  datos_abiertos_tbl <- descarga_db(
    read_format = read_format,
    tblname     = tblname,
    pragma_memory_limit = pragma_memory_limit,
    drv                 = drv,
    dbdir               = dbdir,
    colClasses          = colClasses,
    download_process    = download_process,
    sites.covid         = sites.covid,
    unzip_command       = unzip_command,
    unzip_args          = unzip_args,
    check_unzip_install = check_unzip_install,
    clear_zip = clear_zip,
    clear_csv = clear_csv,
    force_download = force_download,
    show_warnings = show_warnings,
    datos_abiertos_zip_paths = datos_abiertos_zip_paths,
    datos_abiertos_unzipped_path = datos_abiertos_unzipped_path,
    datos_abiertos_tbl = datos_abiertos_tbl,
    quiet = quiet,
    board_url_name = board_url_name,
    use_cache_on_failure = use_cache_on_failure,
    cache = cache_datos,
    download_file_args = download_file_args,
    descarga_db_datos_abiertos_tbl_args = descarga_db_datos_abiertos_tbl_args,
    ...
  )


  if (use_dict) {
    # Descarga el diccionario
    diccionario <- descarga_diccionario(
      download_process = download_process,
      site.covid.dic = site.covid.dic,
      quiet = quiet,
      diccionario_zip_path = diccionario_zip_path,
      diccionario_unzipped_path = diccionario_unzipped_path,
      diccionario = diccionario,
      board_url_name_dict = board_url_name_dict,
      cache_diccionario = cache_diccionario,
      use_cache_on_failure = use_cache_on_failure,
      clear_zip = clear_zip,
      clear_csv = clear_csv,
      download_file_args_dict = download_file_args_dict,
      unzip_args_dict = unzip_args_dict,
      force_download = force_download,
      show_warnings = show_warnings,
      descarga_db_diccionario_ssa_args = descarga_db_diccionario_ssa_args
    )
  } else {
    diccionario <- NULL
  }

  # Pegamos todo
  datos_covid <- pega_db_datos_abiertos_tbl_y_diccionario(datos_abiertos_tbl = datos_abiertos_tbl, diccionario = diccionario)

  return(datos_covid)
}
