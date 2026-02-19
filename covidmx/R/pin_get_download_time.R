# Import from pins the meta
pin_meta.pins_board_url <- utils::getFromNamespace("pin_meta.pins_board_url", "pins")

# Checks a pin's last download date
pin_get_download_time <- function(board, name) {

  # If hasn't been downloaded
  tdif <- Inf

  if (name %in% pins::pin_list(board)) {
    fdir <- pin_meta.pins_board_url(board, name)
    pd <- base::file.path(fdir$local$dir, "descarga.txt")

    if (file.exists(pd)) {
      # Leemos cuando fue la descarga
      fileConn <- file(pd)
      descarga <- base::as.POSIXlt(base::readLines(fileConn, n = 1))
      close(fileConn)

      # Obtenemos la diferencia de tiempo
      tdif <- base::difftime(base::as.POSIXlt(descarga), base::Sys.time(), units = "days")
      tdif <- base::abs(base::as.numeric(tdif))
    }
  }

  return(tdif)
}

# Función que devuelve el path al archivo principal de un pin
pin_path_from_memory <- function(board, name) {
  fdir <- pin_meta.pins_board_url(board, name)
  dfile <- base::file.path(fdir$local$dir, fdir$file)

  return(dfile)
}

# Función que escribe la fecha de la ultima descarga del pin
pin_write_download_time <- function(board, name) {
  if (name %in% pins::pin_list(board)) {
    pd <- base::file.path(pin_meta.pins_board_url(board, name)$local$dir, "descarga.txt")

    # Leemos el archivo y agregamos en la ultima linea la fecha de descarga
    fileConn <- base::file(pd)
    base::writeLines(format(Sys.time(), "%F %R %Z"), fileConn)
    base::close(fileConn)
  }
}

get_col_class <- function() {
  c(
    FECHA_ACTUALIZACION = "character",
    ID_REGISTRO = "character",
    ORIGEN = "character",
    SECTOR = "character",
    ENTIDAD_UM = "character",
    SEXO = "character",
    ENTIDAD_NAC = "character",
    ENTIDAD_RES = "character",
    MUNICIPIO_RES = "character",
    TIPO_PACIENTE = "character",
    FECHA_INGRESO = "character",
    FECHA_SINTOMAS = "character",
    FECHA_DEF = "character",
    INTUBADO = "character",
    NEUMONIA = "character",
    EDAD = "character",
    NACIONALIDAD = "character",
    EMBARAZO = "character",
    HABLA_LENGUA_INDIG = "character",
    INDIGENA = "character",
    DIABETES = "character",
    EPOC = "character",
    ASMA = "character",
    INMUSUPR = "character",
    HIPERTENSION = "character",
    OTRA_COM = "character",
    CARDIOVASCULAR = "character",
    OBESIDAD = "character",
    RENAL_CRONICA = "character",
    TABAQUISMO = "character",
    OTRO_CASO = "character",
    TOMA_MUESTRA_LAB = "character",
    RESULTADO_LAB = "character",
    TOMA_MUESTRA_ANTIGENO = "character",
    RESULTADO_ANTIGENO = "character",
    CLASIFICACION_FINAL = "character",
    MIGRANTE = "character",
    PAIS_NACIONALIDAD = "character",
    PAIS_ORIGEN = "character",
    UCI = "character"
  )
}

get_sites_covid <- function() {
  c(
    "2022" = paste0(
      "http://datosabiertos.salud.gob.mx/gobmx/salud",
      "/datos_abiertos/datos_abiertos_covid19.zip"
    ),
    "2021" = paste0(
      "https://datosabiertos.salud.gob.mx/gobmx/salud",
      "/datos_abiertos/historicos/2021/",
      "COVID19MEXICO2021.zip"
    ),
    "2020" = paste0(
      "https://datosabiertos.salud.gob.mx/gobmx/salud",
      "/datos_abiertos/historicos/2020/",
      "COVID19MEXICO2020.zip"
    )
  )
}

get_site_dic <- function() {
  paste0(
    "http://datosabiertos.salud.",
    "gob.mx/gobmx/salud/datos_a",
    "biertos/diccionario_datos_",
    "covid19.zip"
  )
}

#' @title Funcion para verificar la existencia de los sitios web de datos de covid
#'
#' @description La funcion recorre cada uno de los sitios y verifica su existencia.
#'
#' @param covid_data (**opcional**) Variable booleana `TRUE` si verifica los sitios de
#' datos y `FALSE` si no los verifica
#'
#' @param dictionnary  (**opcional**) Variable booleana `TRUE` si verifica los sitios del
#' diccionario y `FALSE` si no lo verifica
#'
#'
#' @return Devuelve `TRUE` si todos los sitios web existen, `FALSE`en caso de que no.
#'
#' @examples
#' # Verificamos que existan los sitios cambiando a TRUE cualquiera:
#' check_sites(covid_data = FALSE, dictionnary = FALSE)
#'
#' @export
check_sites <- function(covid_data = TRUE, dictionnary = TRUE) {
  sites <- c()

  if (covid_data) {
    sites <- c(sites, get_sites_covid())
  }

  if (dictionnary) {
    dictionnary <- c(sites, get_site_dic())
  }

  site_available <- c(TRUE)
  if (length(sites) > 0) {
    for (site in sites) {
      cli::cli_alert("Checando {.url {site}}")
      site_available <- c(site_available, RCurl::url.exists(site))
    }
  }

  if (all(site_available)) {
    cli::cli_alert_success("Los sitios de descarga fueron encontrados")
  }

  return(all(site_available))
}
