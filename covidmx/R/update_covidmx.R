#' Actualiza el paquete covidmx
#'
#' @description Descarga e instala la version mas reciente de covidmx desde Github
#' \url{https://github.com/RodrigoZepeda/covidmx}
#'
#' @param quiet (**opcional**) Determina si instalar en silencio
#' @param force (**opcional**) Determina si forzar la reinstalacion
#' @param ...   (**opcional**) Parametros adicionales para [remotes::install_github()]
#'
#' @return Ningún valor. Se llama para actualizar el paquete a la versión más reciente desde Github.
#' @examples
#' \dontrun{
#' # Actualiza el paquete de coivdmx de Github | Updates the covidmx package from Github
#' update_covidmx()
#' }
#'
#' @note
#' Actualiza el paquete instalando todas las dependencias necesarias.
#' @export

update_covidmx <- function(quiet = FALSE, force = FALSE, ...) {
  if (requireNamespace("remotes", quietly = TRUE)) {

    # Verificamos la url exista
    url <- "https://github.com/RodrigoZepeda/covidmx"

    if (!RCurl::url.exists(url)) {
      cli::cli_abort(
        "No puedo encontrar el repositorio de Github en {.url {url}}"
      )
    }

    # Instalacion
    remotes::install_github("RodrigoZepeda/covidmx", quiet = quiet, force = force, ...)

    # Mensaje de reinicio
    cli::cli_alert_success(
      "Actualizaste exitosamente {.code covidmx}. Por favor reinicia la sesion actual de {.code R}"
    )
  } else {

    # Sugerimos instalar
    cli::cli_alert_warning(
      c(
        "Instala el paquete {.code remotes} con {.code install.packages('remotes')} para poder",
        " utilizar esta funcion"
      )
    )
  }
}
