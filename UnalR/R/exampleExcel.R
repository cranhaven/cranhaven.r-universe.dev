#' Obtenga la ruta de los archivos de Excel para la función `Agregar()`
#'
#' UnalR viene con algunos archivos de ejemplo en su directorio `inst/extdata`.
#' Esta función facilita el acceso a ellos.
#'
#' @param ruta Nombre del archivo. Si es `NULL`, se enumerarán los archivos de
#'   ejemplo.
#'
#' @returns
#' Cadena de caracteres indicando la ruta absoluta
#' (*no relativa a la carpeta en donde se ubique*) en donde se encuentra el archivo
#' especificado dentro del paquete.
#'
#' @examplesIf all(FALSE)
#' read_example("TestConsolidado1.xlsx")
#'
#' @export
read_example <- function(ruta = NULL) {
  if (is.null(ruta)) {
    dir(system.file("extdata", package = "UnalR"))
  } else {
    system.file("extdata", ruta, package = "UnalR", mustWork = TRUE)
  }
}
