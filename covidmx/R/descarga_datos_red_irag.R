#' Descarga la base de datos de ocupacion hospitalaria de la red IRAG
#'
#' @description
#' `descarga_datos_red_irag` Lee los datos de ocupacion hospitalaria de la `RED IRAG`
#' disponibles en [https://www.gits.igg.unam.mx/red-irag-dashboard/reviewHome#](https://www.gits.igg.unam.mx/red-irag-dashboard/reviewHome#)
#' y analizados a traves de [RodrigoZepeda/CapacidadHospitalariaMX](https://github.com/RodrigoZepeda/CapacidadHospitalariaMX)
#'
#' @details
#'
#' Los datos de Red IRAG son descargados diariamente de manera automatica en Github:
#' [RodrigoZepeda/CapacidadHospitalariaMX](https://github.com/RodrigoZepeda/CapacidadHospitalariaMX)
#' y esta funcion los lee de ahi. Puede que esten un poco rezagados respecto a la pagina de la
#' `RED IRAG` ([https://www.gits.igg.unam.mx/red-irag-dashboard/reviewHome#](https://www.gits.igg.unam.mx/red-irag-dashboard/reviewHome#))
#' pero el rezago nunca es mayor a un dia.
#'
#' @param nivel (**opcional**) Regresa la ocupacion `"Estatal"`(default) o por `"Unidad Medica"`
#' @param quiet (**opcional**) booleana para no imprimir mensajes en la consola.
#' @param cache (**opcional**) cache para [pins::board_url()]. Representa el directorio donde
#' se almacenaran los datos descargados en formato de `pins`.
#' @param use_cache_on_failure (**opcional**)  parametro para [pins::board_url()]. En caso de
#' `TRUE` (default) si no puede descargar nueva informacion utiliza la que ya tiene en memoria
#' aunque sea vieja.
#' @param force_download (**opcional**)  analiza si cambio el pin y descarga datos nuevos en
#' caso afirmativo.
#' @param show_warnings  (**opcional**) si arrojar `warnings` o callar
#' @param ...  parametros adicionales para [pins::pin_download()].
#'
#' @return `tibble` con los datos de ocupacion hospitalaria
#' \itemize{
#'   \item `Unidad médica` - En caso `nivel = "Unidad Medica"` la unidad a la que pertenecen los datos
#'   \item `Institución`   - Institucion a la que pertenece la unidad medica
#'   \item `Estado`        - Entidad federativa de la informacion o de la unidad
#'   \item `CLUES`         - La Clave Unica de Establecimientos de Salud para la unidad (si `nivel = "Unidad Medica"`)
#'   \item `Fecha`         - La fecha a la cual corresponde dicha ocupacion
#'   \item `Actualizacion` - La fecha de actualizacion ultima de los datos.
#'   \item `Hospitalizados (%)`    - Porcentaje de ocupacion en camas de hospitalizacion.
#'   \item `Ventilación (%)`       - Porcentaje de ocupacion en ventiladores.
#'   \item `UCI y Ventilación (%)` - Porcentaje de ocupacion en unidades de cuidado intensivo con ventilacion.
#' }
#'
#' @examples
#' \donttest{
#' # Descarga de datos estatales
#' url_global <- paste0(
#'   "https://media.githubusercontent.com/media/RodrigoZepeda/",
#'   "CapacidadHospitalariaMX/master/processed/"
#' )
#' 
#' if (RCurl::url.exists(paste0(url_global, "HospitalizacionesMX_estatal.csv"))) {
#'   ocupacion_hospitalaria <- descarga_datos_red_irag("Estatal", show_warnings = FALSE)
#' }
#'
#' # También puedes hacer la descarga por unidad medica
#' # Descarga de datos por unidad medica
#' if (RCurl::url.exists(paste0(url_global, "HospitalizacionesMX_unidad_medica.csv"))) {
#'   ocupacion_unidad <- descarga_datos_red_irag("Unidad Medica", show_warnings = FALSE)
#' }
#' }
#'
#' @encoding UTF-8
#' @seealso [descarga_datos_variantes_GISAID()] [descarga_datos_abiertos()] [read_datos_abiertos()]
#'
#' @references
#'
#' Secretaría de Salud (2022). Sistema de Información de la Red IRAG
#' URL: \url{https://www.gits.igg.unam.mx/red-irag-dashboard/reviewHome}
#'
#' Zepeda-Tello, R. (2022). Descarga Automática de Datos de la Red IRAG
#' URL: \url{https://github.com/RodrigoZepeda/CapacidadHospitalariaMX}
#'
#' @export
descarga_datos_red_irag <- function(nivel = c("Estatal", "Unidad M\u00e9dica"),
                                    cache = NULL,
                                    use_cache_on_failure = TRUE,
                                    quiet = TRUE,
                                    force_download = FALSE,
                                    show_warnings = TRUE,
                                    ...) {
  github <- "https://media.githubusercontent.com/media/"
  cuenta <- "RodrigoZepeda/CapacidadHospitalariaMX/master/processed/"

  nivel <- ifelse(tolower(nivel[1]) == "estatal", "estatal", "unidad_medica")
  fname <- paste0(github, cuenta, "HospitalizacionesMX_", nivel[1], ".csv")

  if (!quiet) {
    cli::cli_alert("Descargando {nivel[1]} desde {.url {fname}}")
  }

  # Creamos el board
  board <- pins::board_url(
    urls = c(
      "estatal" = paste0(github, cuenta, "HospitalizacionesMX_estatal.csv"),
      "unidad_medica" = paste0(github, cuenta, "HospitalizacionesMX_unidad_medica.csv")
    ),
    cache = cache,
    use_cache_on_failure = use_cache_on_failure
  )

  # FIXME
  # This is a workaround as the pins package doesn't have metadata for downloads
  # Checamos si está descargado y cuándo lo descargaste si fue hace menos de un dia te
  # dejo con el mismo
  tdif <- pin_get_download_time(board, nivel[1])

  if (!force_download & tdif < 0.9) {
    if (show_warnings) {
      cli::cli_warn(
        "La descarga mas reciente de fue  hace {round(tdif,5)} dias. Como tiene menos de un
          dia usare esa. Escribe {.code force_download = TRUE} si quieres descargar de
          todas formas. Para desactivar este mensaje {.code show_warnings = FALSE.}"
      )
    }

    # Lee de memoria
    dfile <- pin_path_from_memory(board, nivel[1])
  } else {
    # Descarga si cambió
    dfile <- pins::pin_download(board = board, name = nivel[1], ...)
  }

  dats <- dfile |>
    readr::read_csv(
      locale = readr::locale(encoding = "UTF-8"),
      col_types = readr::cols(
        Estado        = readr::col_character(),
        Fecha         = readr::col_date(format = "%Y-%m-%d"),
        Actualizacion = readr::col_datetime(format = "%Y-%m-%dT%H:%M:%SZ")
      )
    )

  # Escribimos en el pin que ya descargamos
  pin_write_download_time(board, nivel[1])

  return(dats)
}
