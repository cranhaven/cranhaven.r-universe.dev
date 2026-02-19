#' Auxiliares para la descarga de datos abiertos
#'
#' @description Conjunto de funciones para apoyar la descarga de datos abiertos de
#' la Direccion General de Epidemiologia (DGE)
#'
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
#' * [parse_db_datos_abiertos_tbl()] Genera una base de datos en `duckdb` (o `tibble`) con la informacion
#'
#' Si en algun momento se interrumpio la descarga o hubo problemas de conexion o detuviste
#' el proceso de generacion de la base de datos abiertos puedes llamar a las funciones
#' de [read_datos_abiertos()].
#'
#' @inheritParams descarga_datos_abiertos
#'
#' @return Lista de valores:
#' \itemize{
#'   \item dats        - Tabla conectada mediante `duckdb::dbConnect__duckdb_driver()` (si `duckdb`) o
#'                       tibble (si `tibble`)
#'   \item disconnect  - Funcion para cerrar la conexion a la base de datos.
#'   \item dict        - Lista de `tibble`s con el diccionario de datos para cada variable
#' }
#' @examples
#' \donttest{
#' #Estos links deben omitirse en una corrida normal. Se incluyen por ahora como ejemplo
#' #pero las opciones site.covid.dic y sites.covid deben eliminarse de abajo.
#' diclink   <- "https://github.com/RodrigoZepeda/covidmx/raw/main/diccionario_datos_covid19.zip"
#' dlink     <- "https://github.com/RodrigoZepeda/covidmx/raw/main/datos_abiertos_covid19.zip"
#' 
#' #' #En el ejemplo de R por normas de CRAN tenemos que hacerlo así pero en tu
#' #computadora puedes solo usar descargar datos sin el if else
#' if (RCurl::url.exists(dlink) & RCurl::url.exists(diclink)){
#'   
#'   # Descarga solo el diccionario no oficial (omite el site.covid.dic para el de DGE)
#'   diccionario <- descarga_diccionario(show_warnings = FALSE, site.covid.dic = diclink)
#'
#'   # O bien descarga solo los datos abiertos de ejemplo desde Github
#'   # omite el dlink (o cámbialo por el vínculo correcto) para descargar los datos de la DGE
#'   datos_abiertos <- descarga_db(sites.covid = dlink, show_warnings = FALSE)
#'
#'   # Pegalos en el formato que se necesita para el resto de funciones
#'   datos_covid <- pega_db_datos_abiertos_tbl_y_diccionario(datos_abiertos, diccionario)
#'
#'   # Desconectamos
#'   datos_covid$disconnect()
#'
#'   # Tambien puedes descargar paso por paso
#'   datos_abiertos <- descarga_db_datos_abiertos_tbl(
#'     sites.covid = dlink,
#'     show_warnings = FALSE
#'   ) |> # Descarga
#'   unzip_db_datos_abiertos_tbl() |> # Unzippea
#'   parse_db_datos_abiertos_tbl() # Duckdb
#'
#'   # O bien el diccionario
#'   diccionario <- descarga_db_diccionario_ssa(site.covid.dic = diclink) |> # Descarga
#'   unzip_db_diccionario_ssa() |> # Unzippea
#'   parse_db_diccionario_ssa() # Tibble
#'
#'   # Si descargaste cada uno por separado necesitas la funcion pega para
#'   # juntarlos en un unico objeto
#'   datos_covid <- pega_db_datos_abiertos_tbl_y_diccionario(datos_abiertos, diccionario)
#' }
#' }
#' @encoding UTF-8
#' @seealso [descarga_datos_abiertos()] [read_datos_abiertos()]  [descarga_datos_red_irag()]
#' [descarga_datos_variantes_GISAID()]
#' @export
#' @param cache  (**opcional**) parametro para el cache de `pins::board_url`
#' @param ...  (**opcional**) Parametros adicionales para `duckdb::dbConnect()` con 
#'  conexion de `duckdb::duckdb()`

descarga_db <- function(read_format = c("duckdb", "tibble"),
                        tblname = "covidmx",
                        pragma_memory_limit = Sys.getenv("pragma_memory_limit"),
                        drv = duckdb::duckdb(),
                        dbdir = tempfile(fileext = ".duckdb"),
                        colClasses = get_col_class(),
                        sites.covid = get_sites_covid(),
                        download_process = c("pins", "download.file"),
                        unzip_command = Sys.getenv("unzip_command"),
                        unzip_args = Sys.getenv("unzip_args"),
                        check_unzip_install = TRUE,
                        clear_zip = (download_process[1] != "pins"),
                        clear_csv = TRUE,
                        force_download = FALSE,
                        show_warnings = TRUE,
                        datos_abiertos_zip_paths = NULL,
                        datos_abiertos_unzipped_path = NULL,
                        datos_abiertos_tbl = NULL,
                        quiet = FALSE,
                        board_url_name = "datos_abiertos",
                        cache = NULL,
                        use_cache_on_failure = TRUE,
                        download_file_args = list(
                          method   = "curl",
                          destfile = tempfile(),
                          quiet    = quiet
                        ),
                        descarga_db_datos_abiertos_tbl_args = list(),
                        ...) {
  if (!(download_process[1] %in% c("pins", "download.file"))) {
    cli::cli_abort("Proceso de descarga invalido. Selecciona {.code pins} o {.code download.file}")
  }

  # Descargamos los datos de la ssa
  if (is.null(datos_abiertos_zip_paths) & is.null(datos_abiertos_unzipped_path) & is.null(datos_abiertos_tbl)) {
    if (!quiet) {
      cli::cli_alert_info("{.strong Descargando} la informacion...")
    }

    descarga_db_datos_abiertos_tbl_args <- list(
      "download_process"     = download_process,
      "sites.covid"          = sites.covid,
      "quiet"                = quiet,
      "board_url_name"       = board_url_name,
      "cache"                = cache,
      "use_cache_on_failure" = use_cache_on_failure,
      "force_download"       = force_download,
      "show_warnings"        = show_warnings,
      "download_file_args"   = download_file_args
    ) |>
      append(descarga_db_datos_abiertos_tbl_args)

    datos_abiertos_zip_paths <- do.call(
      descarga_db_datos_abiertos_tbl,
      descarga_db_datos_abiertos_tbl_args
    )
  }

  # Liberamos el zip
  if (!is.null(datos_abiertos_zip_paths) & is.null(datos_abiertos_unzipped_path) & is.null(datos_abiertos_tbl)) {
    if (!quiet) {
      cli::cli_alert_info("{.strong Descomprimiendo} los archivos {.file .zip}...")
    }

    datos_abiertos_unzipped_path <- unzip_db_datos_abiertos_tbl(
      datos_abiertos_zip_paths = datos_abiertos_zip_paths,
      unzip_command = unzip_command,
      unzip_args = unzip_args,
      clear_zip = clear_zip,
      quiet = quiet,
      check_unzip_install = check_unzip_install
    )
  }

  # Parseamos el file en tibble o duckdb
  if (!is.null(datos_abiertos_unzipped_path) & is.null(datos_abiertos_tbl)) {
    if (!quiet) {
      cli::cli_alert_info(
        "{.strong Consolidando} los archivos {.file .csv} en {.file {read_format[1]}}..."
      )
    }

    datos_abiertos_tbl <- parse_db_datos_abiertos_tbl(
      datos_abiertos_unzipped_path = datos_abiertos_unzipped_path,
      read_format = read_format,
      dbdir       = dbdir,
      drv         = drv,
      pragma_memory_limit = pragma_memory_limit,
      colClasses  = colClasses,
      tblname     = tblname,
      clear_csv   = clear_csv,
      quiet       = quiet,
      ...
    )
  } else if (!is.null(datos_abiertos_tbl)) {

    # Get file connection
    con <- duckdb::dbConnect(
      drv   = drv,
      dbdir = datos_abiertos_tbl,
      ...
    )

    # Memory limit
    if (pragma_memory_limit == "") {
      pragma_memory_limit <- "1GB"
    }


    DBI::dbExecute(con, paste0("PRAGMA memory_limit='", pragma_memory_limit, "'"))

    if (!quiet) {
      cli::cli_alert_info(
        "{.strong Conectando} a la tabla {.code {tblname}} creada en {.file duckdb}..."
      )
    }

    dats <- dplyr::tbl(con, tblname)

    # Formateo
    dats <- dats |>
      dplyr::mutate(dplyr::across(
        dplyr::all_of(c(
          "ORIGEN", "SECTOR", "SEXO",
          "TIPO_PACIENTE", "INTUBADO", "NEUMONIA", "EDAD", "NACIONALIDAD", "EMBARAZO",
          "HABLA_LENGUA_INDIG", "INDIGENA", "DIABETES", "EPOC", "ASMA", "INMUSUPR", "HIPERTENSION", "OTRA_COM",
          "CARDIOVASCULAR", "OBESIDAD", "RENAL_CRONICA", "TABAQUISMO", "OTRO_CASO", "TOMA_MUESTRA_LAB",
          "RESULTADO_LAB", "TOMA_MUESTRA_ANTIGENO", "RESULTADO_ANTIGENO", "CLASIFICACION_FINAL", "MIGRANTE",
          "UCI"
        )),
        ~ as.integer(.)
      )) |>
      dplyr::mutate(dplyr::across(dplyr::starts_with("FECHA"), ~ dplyr::if_else(. == "9999-99-99", NA_character_, .))) |>
      dplyr::mutate(dplyr::across(dplyr::starts_with("FECHA"), ~ dplyr::if_else(. == "-001-11-30", NA_character_, .))) |>
      dplyr::mutate(dplyr::across(dplyr::starts_with("FECHA"), ~ strptime(., "%Y-%m-%d")))

    # Creamos función de desconexión
    disconnect <- function(quiet = FALSE) {
      duckdb::dbDisconnect(con, shutdown = TRUE)
      if (!quiet) {
        cli::cli_alert_success("Desconectado")
      }
    }

    # Mensaje de desconexion
    if (!quiet) {
      cli::cli_alert_success(
        "{.strong Terminado}. No olvides desconectar la base con {.code datos_covid$disconnect()}
      al terminar de trabajar."
      )
    }

    datos_abiertos_tbl <- list("dats" = dats, "disconnect" = disconnect)
  }

  return(datos_abiertos_tbl)
}

#' @export
#' @rdname descarga_db
descarga_diccionario <- function(download_process = c("pins", "download.file"),
                                 site.covid.dic = get_site_dic(),
                                 quiet = FALSE,
                                 clear_zip = download_process[1] != "pins",
                                 clear_csv = TRUE,
                                 diccionario_zip_path = NULL,
                                 diccionario_unzipped_path = NULL,
                                 diccionario = NULL,
                                 board_url_name_dict = "diccionario_covid",
                                 cache_diccionario = NULL,
                                 use_cache_on_failure = TRUE,
                                 force_download = FALSE,
                                 show_warnings = TRUE,
                                 download_file_args_dict = list(
                                   method   = "curl",
                                   destfile = tempfile(),
                                   quiet    = quiet
                                 ),
                                 unzip_args_dict = list("exdir" = ".", "overwrite" = TRUE),
                                 descarga_db_diccionario_ssa_args = list()) {
  if (!(download_process[1] %in% c("pins", "download.file"))) {
    cli::cli_abort("Proceso de descarga invalido. Selecciona {.code pins} o {.code download.file}")
  }

  # Descargamos el diccionario
  if (is.null(diccionario_zip_path) & is.null(diccionario_unzipped_path) & is.null(diccionario)) {
    descarga_db_diccionario_ssa_args <- list(
      "download_process"        = download_process,
      "site.covid.dic"          = site.covid.dic,
      "quiet"                   = quiet,
      "board_url_name_dict"     = board_url_name_dict,
      "cache_diccionario"       = cache_diccionario,
      "force_download"          = force_download,
      "show_warnings"           = show_warnings,
      "use_cache_on_failure"    = use_cache_on_failure,
      "download_file_args_dict" = download_file_args_dict
    ) |>
      append(descarga_db_diccionario_ssa_args)

    diccionario_zip_path <- do.call(descarga_db_diccionario_ssa, descarga_db_diccionario_ssa_args)
  }

  # Liberamos el diccionario
  if (!is.null(diccionario_zip_path) & is.null(diccionario_unzipped_path) & is.null(diccionario)) {
    diccionario_unzipped_path <- unzip_db_diccionario_ssa(
      diccionario_zip_path = diccionario_zip_path,
      unzip_args_dict = unzip_args_dict,
      clear_zip = clear_zip
    )
  }

  # Leemos el diccionario
  if (!is.null(diccionario_unzipped_path)) {
    diccionario <- parse_db_diccionario_ssa(
      diccionario_unzipped_path = diccionario_unzipped_path,
      clear_csv = clear_csv
    )
  }

  return(diccionario)
}

#' @export
#' @rdname descarga_db
#' @param cache parametro para el cache de `pins::board_url`
#' @param ... Parametros adicionales para `pins::pin_download`
descarga_db_datos_abiertos_tbl <- function(download_process = c("pins", "download.file"),
                                           sites.covid = get_sites_covid(),
                                           quiet = FALSE,
                                           board_url_name = "datos_abiertos",
                                           cache = NULL,
                                           use_cache_on_failure = TRUE,
                                           force_download = FALSE,
                                           show_warnings = TRUE,
                                           download_file_args = list(
                                             method   = "curl",
                                             destfile = tempfile(),
                                             quiet    = quiet
                                           ),
                                           ...) {

  # Method for download
  download_process <- ifelse(download_process[1] == "download.file", "download.file", "pins")
  download_paths <- vector(mode = "list", length = length(sites.covid))

  if (!quiet) {
    cli::cli_progress_bar("Descargando", total = length(sites.covid), clear = TRUE)
  }

  for (sitenum in 1:length(sites.covid)) {
    if (!quiet) {
      cli::cli_progress_update()
    }

    site.covid <- sites.covid[sitenum]

    # Check site exists
    if (!RCurl::url.exists(site.covid)) {
      cli::cli_abort(
        "El sitio de {.strong {names(sites.covid)[sitenum]}} dado por:
        {.url {site.covid}}
        no existe o no puede ser encontrado. Verifica exista y tu conexion a Internet sea estable."
      )
    }


    if (download_process == "download.file") {

      # Attempt to download
      download_file_args <- append(list("url" = site.covid), download_file_args)
      do.call(download.file, download_file_args)

      # Return path
      download_paths[[sitenum]] <- download_file_args$destfile
    } else {

      # Attempt to create board
      bname <- paste0(board_url_name, "_", as.character(sitenum))
      names(site.covid) <- bname
      board_url_args <- list(
        "urls" = site.covid,
        "cache" = cache,
        "use_cache_on_failure" = use_cache_on_failure
      )
      board <- do.call(pins::board_url, board_url_args)

      # Obtenemos la diferencia de tiempo de cuando se bajo por vez ultia
      tdif <- pin_get_download_time(board, bname)

      if (!force_download & tdif < 0.9) {
        if (show_warnings) {
          cli::cli_warn(
            paste(
              "La descarga mas reciente de {.strong {names(sites.covid)[sitenum]}} fue",
              "hace {round(tdif,5)} dias. Como tiene menos de un dia usare esa.",
              "Escribe {.code force_download = TRUE} si quieres descargar de",
              "todas formas. Para desactivar este mensaje {.code show_warnings = FALSE.}"
            )
          )
        }

        # Lee de memoria
        download_paths[[sitenum]] <- pin_path_from_memory(board, bname)
      } else {
        # Descarga si cambio
        download_paths[[sitenum]] <- pins::pin_download(board = board, name = bname, ...)

        # Escribimos en el pin que ya descargamos
        pin_write_download_time(board, bname)
      }
    }
  }

  if (!quiet) {
    cli::cli_progress_done()
  }

  # Add names
  names(download_paths) <- names(sites.covid)

  return(download_paths)
}

#' @export
#' @rdname descarga_db
#' @param ... Parametros adicionales para `pins::pin_download`
descarga_db_diccionario_ssa <- function(download_process = c("pins", "download.file"),
                                        site.covid.dic = get_site_dic(),
                                        quiet = FALSE,
                                        board_url_name_dict = "diccionario_covid",
                                        cache_diccionario = NULL,
                                        use_cache_on_failure = TRUE,
                                        force_download = FALSE,
                                        show_warnings = TRUE,
                                        download_file_args_dict = list(
                                          method   = "curl",
                                          destfile = tempfile(),
                                          quiet    = quiet
                                        ),
                                        ...) {

  # Corremos el mismo programa solo que con la base de diccionario
  descarga_db_datos_abiertos_tbl(
    download_process = download_process,
    sites.covid      = site.covid.dic,
    board_url_name   = board_url_name_dict,
    cache            = cache_diccionario,
    use_cache_on_failure = use_cache_on_failure,
    download_file_args = download_file_args_dict,
    quiet              = quiet,
    force_download     = force_download,
    show_warnings      = show_warnings,
    ...
  )
}

#' @export
#' @rdname descarga_db
unzip_db_datos_abiertos_tbl <- function(datos_abiertos_zip_paths,
                                        unzip_command = Sys.getenv("unzip_command"),
                                        unzip_args = Sys.getenv("unzip_args"),
                                        check_unzip_install = TRUE,
                                        quiet = FALSE,
                                        clear_zip = FALSE) {
  if (!quiet) {
    cli::cli_progress_bar("Leyendo archivos zip",
      total = length(datos_abiertos_zip_paths),
      clear = TRUE
    )
  }

  csv_files <- vector(mode = "list", length = length(datos_abiertos_zip_paths))

  for (zipnum in 1:length(datos_abiertos_zip_paths)) {
    if (!quiet) {
      cli::cli_progress_update()
    }

    datos_abiertos_zip_path <- datos_abiertos_zip_paths[[zipnum]]

    # Unzip file
    filecon <- tryCatch(
      {
        unzip(datos_abiertos_zip_path, overwrite = TRUE)
        
        #Get unzipped file name if changes
        fname <- unzip(datos_abiertos_zip_path, list = T)$Name
        fname <- list.files(pattern = fname, full.names = T)[1]
      },
      warning = function(cond) {

        # Establecemos el comando para unzipear
        if (is.null(unzip_command) | unzip_command == "") {
          unzip_command <- ifelse(tolower(.Platform$OS.type) == "windows",
            "\"C:\\Program Files\\7-Zip\\7z.exe\"", "unzip"
          )
        }

        # Establecemos argumentos adicionales
        if (is.null(unzip_args) | unzip_args == "") {
          unzip_args <- ifelse(tolower(.Platform$OS.type) == "windows", "-x", "-o")
        }

        # Checamos que exusta la herramienta para unzippear
        if (check_unzip_install & stringr::str_detect(R.version$os, "darwin|linux")) {
          is_unzip <- system2("which", unzip_command, stdout = T, stderr = T)
          if (length(is_unzip) == 0) {
            cli::cli_abort(
              c(
                "Por favor instala unzip:",
                "+ {.strong [OSX]:} {.code brew install unzip}",
                "+ {.strong [Debian/Ubuntu]:} {.code apt install unzip}",
                "  o bien desde {.url http://infozip.sourceforge.net/UnZip.html}"
              )
            )
          }
        } else if (check_unzip_install & tolower(.Platform$OS.type) == "windows") {
          is_unzip <- shell(paste("if exist", unzip_command, "echo yes"), intern = T)
          if (is_unzip != "yes") {
            cli::cli_abort(
              c(
                paste(
                  "Por favor instala {.strong 7zip} de {.url https://www.7-zip.org/}",
                  "y en {.code unzip_command} pon el camino hacia el archivo {.strong 7z.exe}"
                ),
                paste(
                  "> {.emph Ejemplo:} {.code unzip_db_datos_abiertos_tbl(..., unzip_command",
                  "= 'C:\\Program Files\\7-Zip\\7z.exe')}"
                )
              )
            )
          }
        }

        # Unzippeamos
        fname <- system2(unzip_command, args = c("-l ", datos_abiertos_zip_path), stdout = TRUE)
        fname <- grep(".*.csv", fname, value = TRUE)
        fname <- sub(".*\\s","", fname)
        system2(unzip_command, args = c(unzip_args, datos_abiertos_zip_path), stdout = !quiet)
        fname <- list.files(pattern = fname[1], full.names = T)[1]
      },
      error = function(cond) {
        cli::cli_abort("No se puede leer {.file {datos_abiertos_zip_path}}")
      }
    )

    csv_files[[zipnum]] <- file.path(
      dirname(fname),
      paste0(names(datos_abiertos_zip_paths)[zipnum], ".csv")
    )
    file.rename(fname, csv_files[[zipnum]])

    if (clear_zip & file.exists(datos_abiertos_zip_path)) {
      file.remove(datos_abiertos_zip_path)
    }
  }

  if (!quiet) {
    cli::cli_progress_done()
  }

  names(csv_files) <- names(datos_abiertos_zip_paths)

  return(csv_files)
}

#' @export
#' @rdname descarga_db
unzip_db_diccionario_ssa <- function(diccionario_zip_path,
                                     unzip_args_dict = list("exdir" = ".", "overwrite" = TRUE),
                                     clear_zip = FALSE) {
  filenames <- unzip(zipfile = diccionario_zip_path[[1]], list = TRUE)
  fname <- filenames[which(stringr::str_detect(filenames$Name, "Cat.*logo.*")), "Name"]

  unzip_args <- append(list(
    "zipfile" = diccionario_zip_path[[1]],
    "files" = fname
  ), unzip_args_dict)
  filecon <- do.call(unzip, unzip_args)

  if (clear_zip & file.exists(diccionario_zip_path[[1]])) {
    unlink(diccionario_zip_path)
  }

  return(filecon)
}

#' @export
#' @rdname descarga_db
parse_db_diccionario_ssa <- function(diccionario_unzipped_path, clear_csv = FALSE) {
  if (!file.exists(diccionario_unzipped_path)) {
    cli::cli_abort("No puedo encontrar {.file {diccionario_unzipped_path}}")
  }

  diccionario <- list()

  diccionario <- diccionario |>
    append(
      list("ORIGEN" = readxl::read_excel(diccionario_unzipped_path,
        sheet = "Cat\u00e1logo ORIGEN",
        col_types = c("numeric", "text")
      ))
    ) |>
    append(
      list("SECTOR" = readxl::read_excel(diccionario_unzipped_path,
        sheet = "Cat\u00e1logo SECTOR",
        col_types = c("numeric", "text")
      ))
    ) |>
    append(
      list("SEXO" = readxl::read_excel(diccionario_unzipped_path,
        sheet = "Cat\u00e1logo SEXO",
        col_types = c("numeric", "text")
      ))
    ) |>
    append(
      list("PACIENTE" = readxl::read_excel(diccionario_unzipped_path,
        sheet = "Cat\u00e1logo TIPO_PACIENTE",
        col_types = c("numeric", "text")
      ))
    ) |>
    append(
      list("NACIONALIDAD" = readxl::read_excel(diccionario_unzipped_path,
        sheet = "Cat\u00e1logo NACIONALIDAD",
        col_types = c("numeric", "text")
      ))
    ) |>
    append(
      list("RESULTADO_LAB" = readxl::read_excel(diccionario_unzipped_path,
        sheet = "Cat\u00e1logo RESULTADO_LAB",
        col_types = c("numeric", "text")
      ))
    ) |>
    append(
      list("RESULTADO_ANTIGENO" = readxl::read_excel(diccionario_unzipped_path,
        sheet = "Cat\u00e1logo RESULTADO_ANTIGENO",
        col_types = c("numeric", "text")
      ))
    ) |>
    append(
      list("CLASIFICACION_FINAL" = readxl::read_excel(diccionario_unzipped_path,
        sheet = "Cat\u00e1logo CLASIFICACION_FINAL",
        col_types = c("numeric", "text", "text")
      ))
    ) |>
    append(
      list("MUNICIPIO_RES" = readxl::read_excel(diccionario_unzipped_path,
        sheet = "Cat\u00e1logo MUNICIPIOS",
        col_types = c("text", "text", "text")
      ))
    )

  # CATALOGO SI NO
  lista_si_no <- list(readxl::read_excel(diccionario_unzipped_path,
    sheet = "Cat\u00e1logo SI_NO",
    col_types = c("numeric", "text")
  ))

  for (variable in c(
    "INTUBADO", "NEUMONIA", "EMBARAZO", "HABLA LENGUA INDIGENA", "INDIGENA",
    "DIABETES", "EPOC", "ASMA", "INMUSUPR", "HIPERTENSION",
    "CARDIOVASCULAR", "OTRO_CASO", "TOMA_MUESTRA_LAB", "TOMA_MUESTRA_ANTIGENO",
    "OTRA_COMORBILIDAD", "OBESIDAD", "RENAL_CRONICA", "TABAQUISMO", "UCI"
  )) {
    names(lista_si_no) <- variable
    diccionario <- diccionario |> append(lista_si_no)
  }

  #> CATALOGO ENTIDAD
  lista_entidad <- list(readxl::read_excel(diccionario_unzipped_path,
    sheet = "Cat\u00e1logo de ENTIDADES",
    col_types = c("text", "text", "text")
  ))

  for (variable in c("ENTIDAD_UM", "ENTIDAD_RES", "ENTIDAD_NAC")) {
    names(lista_entidad) <- variable
    diccionario <- diccionario |> append(lista_entidad)
  }

  if (clear_csv & file.exists(diccionario_unzipped_path)) {
    unlink(diccionario_unzipped_path)
  }

  return(diccionario)
}

#' @export
#' @rdname descarga_db
parse_db_datos_abiertos_tbl <- function(datos_abiertos_unzipped_path,
                                        read_format = c("duckdb", "tibble"),
                                        pragma_memory_limit = Sys.getenv("pragma_memory_limit"),
                                        dbdir = tempfile(fileext = ".duckdb"),
                                        drv = duckdb::duckdb(),
                                        colClasses = get_col_class(),
                                        tblname = "covidmx",
                                        quiet = TRUE,
                                        clear_csv = FALSE,
                                        ...) {

  # Formato de lectura
  if (tolower(read_format[1]) %in% c("duckdb", "tibble")) {
    read_format <- tolower(read_format[1])
  } else {
    cli::cli_abort("{.code read_format} invalido. Selecciona {.code 'duckdb'} o {.code 'tibble'}")
  }

  # Check we have dbplyr
  if (!requireNamespace("dbplyr", quietly = TRUE) & read_format == "duckdb") {
    cli::cli_abort("Por favor instala {.code dbplyr} con {.code install.packages('dbplyr')}")
  }

  if (read_format == "tibble") {
    if (!quiet) {
      cli::cli_alert_info("{.strong Cargando} los datos en {.file tibble}...")
    }

    # Leemos el archivo
    dats <- readr::read_csv(unlist(datos_abiertos_unzipped_path),
      locale = readr::locale(encoding = "UTF-8"),
      trim_ws = TRUE,
      show_col_types = !quiet,
      progress = !quiet,
      col_types = readr::cols(
        .default              = readr::col_character(),
        FECHA_ACTUALIZACION   = readr::col_character(),
        ORIGEN                = readr::col_double(),
        SECTOR                = readr::col_double(),
        SEXO                  = readr::col_double(),
        TIPO_PACIENTE         = readr::col_double(),
        FECHA_INGRESO         = readr::col_character(),
        FECHA_SINTOMAS        = readr::col_character(),
        FECHA_DEF             = readr::col_character(),
        INTUBADO              = readr::col_double(),
        NEUMONIA              = readr::col_double(),
        EDAD                  = readr::col_double(),
        NACIONALIDAD          = readr::col_double(),
        EMBARAZO              = readr::col_double(),
        HABLA_LENGUA_INDIG    = readr::col_double(),
        INDIGENA              = readr::col_double(),
        DIABETES              = readr::col_double(),
        EPOC                  = readr::col_double(),
        ASMA                  = readr::col_double(),
        INMUSUPR              = readr::col_double(),
        HIPERTENSION          = readr::col_double(),
        OTRA_COM              = readr::col_double(),
        CARDIOVASCULAR        = readr::col_double(),
        OBESIDAD              = readr::col_double(),
        RENAL_CRONICA         = readr::col_double(),
        TABAQUISMO            = readr::col_double(),
        OTRO_CASO             = readr::col_double(),
        TOMA_MUESTRA_LAB      = readr::col_double(),
        RESULTADO_LAB         = readr::col_double(),
        TOMA_MUESTRA_ANTIGENO = readr::col_double(),
        RESULTADO_ANTIGENO    = readr::col_double(),
        CLASIFICACION_FINAL   = readr::col_double(),
        MIGRANTE              = readr::col_double(),
        UCI                   = readr::col_double()
      )
    ) |>
    dplyr::mutate(dplyr::across(dplyr::starts_with("FECHA"), ~ dplyr::if_else(. == "9999-99-99", NA_character_, .))) |>
    dplyr::mutate(dplyr::across(dplyr::starts_with("FECHA"), ~ dplyr::if_else(. == "-001-11-30", NA_character_, .))) |>
    dplyr::mutate(dplyr::across(dplyr::starts_with("FECHA"), ~ strptime(., "%Y-%m-%d")))

    if (!quiet) {
      cli::cli_alert_info(
        "{.strong Conectando} a la tabla {.code {tblname}} creada en {.file tibble}..."
      )
    }

    disconnect <- function(quiet = FALSE) {
      if (!quiet) {
        cli::cli_alert_success("Desconectado")
      }
    }
  } else {

    # Creamos la conexion de duck
    con <- duckdb::dbConnect(
      drv       = drv,
      dbdir     = dbdir,
      read_only = FALSE,
      ...
    )

    if (pragma_memory_limit == "") {
      pragma_memory_limit <- "1GB"
    }

    # Pragma memory limit
    DBI::dbExecute(con, paste0("PRAGMA memory_limit='", pragma_memory_limit, "'"))

    if (!quiet) {
      cli::cli_alert_info("{.strong Cargando} los datos en {.file duckdb}...")
    }

    duckdb::duckdb_read_csv(con, tblname, unlist(datos_abiertos_unzipped_path),
      colClasses = colClasses
    )

    if (!quiet) {
      cli::cli_alert_info(
        "{.strong Conectando} a la tabla {.code {tblname}} creada en {.file duckdb}..."
      )
    }

    dats <- dplyr::tbl(con, tblname)

    # Formateo
    dats <- dats |>
      dplyr::mutate(dplyr::across(
        dplyr::all_of(c(
          "ORIGEN", "SECTOR", "SEXO",
          "TIPO_PACIENTE", "INTUBADO", "NEUMONIA", "EDAD", "NACIONALIDAD", "EMBARAZO",
          "HABLA_LENGUA_INDIG", "INDIGENA", "DIABETES", "EPOC", "ASMA", "INMUSUPR", "HIPERTENSION", "OTRA_COM",
          "CARDIOVASCULAR", "OBESIDAD", "RENAL_CRONICA", "TABAQUISMO", "OTRO_CASO", "TOMA_MUESTRA_LAB",
          "RESULTADO_LAB", "TOMA_MUESTRA_ANTIGENO", "RESULTADO_ANTIGENO", "CLASIFICACION_FINAL", "MIGRANTE",
          "UCI"
        )),
        ~ as.integer(.)
      )) |>
      dplyr::mutate(dplyr::across(dplyr::starts_with("FECHA"), ~ dplyr::if_else(. == "9999-99-99", NA_character_, .))) |>
      dplyr::mutate(dplyr::across(dplyr::starts_with("FECHA"), ~ dplyr::if_else(. == "-001-11-30", NA_character_, .))) |>
      dplyr::mutate(dplyr::across(dplyr::starts_with("FECHA"), ~ strptime(., "%Y-%m-%d")))

    # Creamos funcion de desconexion
    disconnect <- function(quiet = FALSE) {
      duckdb::dbDisconnect(con, shutdown = TRUE)
      if (!quiet) {
        cli::cli_alert_success("Desconectado")
      }
    }
  }

  if (clear_csv) {
    if (!quiet) {
      cli::cli_alert_info("{.strong Eliminando} los archivos {.file .csv}")
    }

    for (fname in datos_abiertos_unzipped_path) {
      if (file.exists(fname)) {
        file.remove(fname)
      }
    }
  }

  # Mensaje de desconexion
  if (!quiet) {
    cli::cli_alert_success(
      "{.strong Terminado}. No olvides desconectar la base con {.code datos_covid$disconnect()}
      al terminar de trabajar."
    )
  }

  return(list(dats = dats, disconnect = disconnect))
}

#' @export
#' @rdname descarga_db
pega_db_datos_abiertos_tbl_y_diccionario <- function(datos_abiertos_tbl, diccionario) {
  return(append(datos_abiertos_tbl, list("dict" = diccionario)))
}
