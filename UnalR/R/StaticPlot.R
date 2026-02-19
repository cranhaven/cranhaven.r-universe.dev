#' Guarde y presente un widget HTML renderizado como una imagen estática
#'
#' Esta función permite representar un widget HTML como un objeto ráster
#' (*imagen de mapa de bits*), útil para reproducir gráficos interactivos en
#' archivos estáticos como un `.pdf` generado por `R Markdown`. Esta función usa
#' internamente los paquetes `webshot`, `htmlwidgets`, `png` y `grid` para poder
#' llevar a cabo su propósito.
#'
#' @param widgetHTML Widget `HTML` a ser mostrado de forma estática.
#' @param height Altura de la imagen estática a retornar.
#' @param primeraVez Si es `FALSE` (*valor predeterminado*) no se instalará `PhantomJS`.
#'   Éste es necesario instalarlo una única vez, por lo cual si es la primera vez
#'   que corre la función deberá indicar el argumento con el valor `TRUE`, después
#'   de esto omita este argumento y deje su valor por defecto.
#' @param ... Otros parámetros concernientes a la función [webshot()][webshot::webshot()],
#'   sin considerar los ya usados dentro de la función (`url`, `file`, `delay`, `zoom` y `vheight`).
#'
#' @details
#' No es necesario especificar el número del factor de zoom. El factor de zoom por
#' defecto es 5, el cual dará como resultado cinco veces más de píxeles vertical
#' y horizontalmente. Este valor fue seleccionado debido a que es el óptimo, un
#' valor mayor ocasiona un tiempo de ejecución excesivamente alto y poca ganancia
#' en cuanto a calidad, con un valor menor se tiene una pérdida considerable de
#' calidad.
#'
#' Si se especifican tanto el ancho como el alto, es probable que la imagen se
#' distorsione. Por lo tanto, el único argumento variable será la altura de la
#' imagen, dejando el ancho como un argumento adaptativo dependiendo del ancho
#' disponible.
#'
#' El tiempo de espera antes de tomar una captura de pantalla, en segundos, es de
#' \eqn{1s}. Este valor es debido a que se necesita un retraso mayor para que los
#' gráficos generados por `Highcharter` se muestren correctamente.
#'
#' @returns
#' Una imagen estática.
#'
#' @examplesIf all(FALSE)
#' misColores <- c(
#'   "#29ABE2", # AZUL CLARO  | Amazonia
#'   "#8CC63F", # VERDE       | Bogota
#'   "#CC241D", # ROJO        | Caribe
#'   "#0071BC", # AZUL VIVO   | Manizales
#'   "#F15A24", # NARANJA     | Medellin
#'   "#FBB03B", # AMARILLO    | Orinoquia
#'   "#93278F", # MORADO      | Palmira
#'   "#8A381A"  # GRIS        | Tumaco
#' )
#' figure <- Plot.Series(
#'   datos     = ejConsolidadoGrad,
#'   categoria = "SEDE_NOMBRE_ADM",
#'   colores   = misColores,
#'   libreria  = "highcharter"
#' )
#' StaticPlot(figure, primeraVez = TRUE)
#'
#' figure2 <- Plot.Torta(
#'   datos     = ejConsolidadoGrad,
#'   categoria = "SEXO",
#'   ano       = 2021,
#'   periodo   = 1,
#'   colores   = c("#116BEE", "#E62272"),
#'   titulo    = "DISTRIBUCI\u00d3N DE GRADUADOS POR SEXO",
#'   libreria  = "highcharter"
#' )
#' StaticPlot(figure2)
#'
#' @export
#'
#' @importFrom htmlwidgets saveWidget
#' @importFrom webshot webshot
#' @importFrom png readPNG
#' @importFrom grid grid.raster
StaticPlot <- function(widgetHTML, height = 500, primeraVez = FALSE, ...) {
  # https://stackoverflow.com/questions/57581268/embed-plotly-into-pdf-rmarkdown
  if (primeraVez) { webshot::install_phantomjs() }

  htmlwidgets::saveWidget(widget = widgetHTML, file = "Temp_Plot.html")
  webshot::webshot(url = "Temp_Plot.html", file = "Temp_Plot.png", delay = 1, zoom = 5, vheight = height, ...)

  StaticImage <- png::readPNG(source = "Temp_Plot.png")
  file.remove("Temp_Plot.html")
  file.remove("Temp_Plot.png")

  return(grid::grid.raster(StaticImage))
}
