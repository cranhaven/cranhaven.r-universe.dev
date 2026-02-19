#' Cree una serie de tiempo dinámica/estática y flexible con tres diferentes
#' paquetes
#'
#' Esta función proporciona excelentes herramientas y opciones para la visualización
#' de series de tiempo dinámicas con el objetivo de estudiar la evolución de una
#' o varias variables a lo largo del tiempo. Dicha serie interactiva se puede
#' representar usando tres diferentes librerías que son `Highcharter`, `Plotly`
#' y `Dygraph`, las cuales usan internamente `JavaScript`.
#'
#' @section Lista de argumentos de estilo:
#'
#' Sabemos que puede ser abrumador el número de argumentos dentro del parámetro
#' `estilo`, pero es necesario si queremos ofrecer al usuario la máxima
#' personalización dentro de cada función usando cualquier librería. Por tal
#' razón, a continuación, se detalla el listado completo de argumentos, usados
#' al especificar la librería y en qué función están presentes
#' (*marcado con una × si lo posee*).
#'
#' |   **Librería**  | **estilo$**                | [Plot.Series()] | [Plot.Barras()] | [Plot.Apiladas()] | [Plot.Boxplot()] | [Plot.Radar()] | [Plot.Treemap()] | [Plot.Torta()] | [Plot.Drilldown()] |
#' |:---------------:|----------------------------|:---------------:|:---------------:|:-----------------:|:----------------:|:--------------:|:----------------:|:--------------:|:------------------:|
#' |        —        | _gg.Tema_                  |        ×        |        ×        |         ×         |         ×        |                |                  |                |                    |
#' |        l        | _gg.Texto_                 |        ×        |        ×        |         ×         |         ×        |                |                  |                |                    |
#' |        l        | _gg.Legend_                |        ×        |                 |         ×         |         ×        |                |                  |                |                    |
#' |        l        | _gg.Linea_                 |        ×        |                 |                   |                  |                |                  |                |                    |
#' |        l        | _gg.Punto_                 |        ×        |                 |                   |                  |                |                  |                |                    |
#' |        l        | _gg.Bar_                   |                 |        ×        |         ×         |                  |                |                  |                |                    |
#' |        l        | _gg.VarWidth_              |                 |                 |                   |         ×        |                |                  |                |                    |
#' |        l        | _gg.OutShape_              |                 |                 |                   |         ×        |                |                  |                |                    |
#' |        l        | _gg.JitWidth_              |                 |                 |                   |         ×        |                |                  |                |                    |
#' |        l        | _gg.JitSize_               |                 |                 |                   |         ×        |                |                  |                |                    |
#' |        l        | _gg.Range_                 |                 |                 |                   |                  |        ×       |                  |                |                    |
#' |   **ggplot2**   | _gg.plty_                  |                 |                 |                   |                  |        ×       |                  |                |                    |
#' |        l        | _gg.plwd_                  |                 |                 |                   |                  |        ×       |                  |                |                    |
#' |        l        | _gg.cglwd_                 |                 |                 |                   |                  |        ×       |                  |                |                    |
#' |        l        | _gg.cglcol_                |                 |                 |                   |                  |        ×       |                  |                |                    |
#' |        l        | _gg.fontsize.title_        |                 |                 |                   |                  |                |         ×        |                |                    |
#' |        l        | _gg.fontsize.labels_       |                 |                 |                   |                  |                |         ×        |                |                    |
#' |        l        | _gg.fontcolor.labels_      |                 |                 |                   |                  |                |         ×        |                |                    |
#' |        l        | _gg.border.lwds_           |                 |                 |                   |                  |                |         ×        |                |                    |
#' |        l        | _gg.border.col_            |                 |                 |                   |                  |                |         ×        |                |                    |
#' |        l        | _gg.lowerbound.cex.labels_ |                 |                 |                   |                  |                |         ×        |                |                    |
#' |        l        | _gg.force.print.labels_    |                 |                 |                   |                  |                |         ×        |                |                    |
#' |        —        | _gg.overlap.labels_        |                 |                 |                   |                  |                |         ×        |                |                    |
#' |        »        | _hc.Tema_                  |        ×        |        ×        |         ×         |         ×        |                |         ×        |        ×       |          ×         |
#' |        l        | _hc.Credits_               |        ×        |        ×        |         ×         |         ×        |                |         ×        |        ×       |          ×         |
#' | **highcharter** | _hc.BoxInfo_               |        ×        |                 |                   |                  |                |                  |                |                    |
#' |        l        | _hc.Slider_                |        ×        |                 |                   |                  |                |                  |                |                    |
#' |        »        | _hc.borderRadius_          |                 |                 |                   |                  |                |         ×        |                |                    |
#' |        •        | _ply.Credits_              |        ×        |        ×        |         ×         |         ×        |        ×       |         ×        |        ×       |                    |
#' |        °        | _ply.Legend_               |                 |        ×        |                   |                  |                |                  |        ×       |                    |
#' |        °        | _ply.LegendPosition_       |        ×        |                 |         ×         |         ×        |        ×       |                  |                |                    |
#' |    **plotly**   | _ply.Interaction_          |        ×        |                 |                   |         ×        |                |                  |                |                    |
#' |        °        | _ply.Relleno_              |                 |                 |                   |                  |        ×       |                  |                |                    |
#' |        °        | _ply.Opacidad_             |                 |                 |                   |                  |        ×       |         ×        |                |                    |
#' |        •        | _ply.LegendTitle_          |                 |                 |                   |                  |        ×       |                  |                |                    |
#' |   **dygraphs**  | _dyg.LegendWidth_          |        ×        |                 |                   |                  |                |                  |                |                    |
#' |        »        | _dyg.Resaltar_             |        ×        |                 |                   |                  |                |                  |                |                    |
#' |        —        | _e.Tema_                   |                 |                 |                   |                  |        ×       |                  |                |                    |
#' |        l        | _e.Credits_                |                 |                 |                   |                  |        ×       |                  |                |                    |
#' |  **echarts4r**  | _e.Forma_                  |                 |                 |                   |                  |        ×       |                  |                |                    |
#' |        l        | _e.LegType_                |                 |                 |                   |                  |        ×       |                  |                |                    |
#' |        —        | _e.LegLoc_                 |                 |                 |                   |                  |        ×       |                  |                |                    |
#'
#' @param datos Un data frame, no un objeto clase serie de tiempo o vector numérico.
#' @param tiempo Lista de variable(s) tanto numéricas como categóricas que se
#'   concatenaran para crear un único periodo temporal (*ordenado ascendentemente*).
#' @param valores Variable numérica que contiene los valores que desea graficar.
#' @param categoria Una variable categórica dentro del data frame ingresado en `datos`.
#' @param colores Cadena de caracteres indicando los colores con los cuales se
#'   deben colorear cada una de las series correspondiente a cada nivel del
#'   argumento `categoria`. Si no se introduce algún vector se usará la paleta
#'   `rainbow` por defecto.
#' @param freqRelativa Si es `FALSE` (*valor predeterminado*) la serie graficada
#'   representará las frecuencias absolutas (*conteo*) más no las relativas (*porcentaje*).
#' @param invertir Si es `FALSE` (*valor predeterminado*) no se invertirá el eje
#'   `Y`. Establézcalo en `TRUE` si desea que en el eje `Y` el número más alto
#'   sea el más cercano al origen.
#' @param ylim Vector numérico que especifica el límite inferior y superior,
#'   respectivamente, del eje `Y`. Si no se introduce algún valor se mostrará
#'   todo el rango disponible para dicho eje.
#' @param titulo Cadena de caracteres indicando el título principal del plot.
#' @param labelX Cadena de caracteres indicando la etiqueta del eje `X`. Por
#'   defecto se emplea el rótulo "Periodo".
#' @param labelY Cadena de caracteres indicando la etiqueta del eje `Y`.
#' @param libreria Cadena de caracteres que indica el paquete con el cual se
#'   realizará la serie. Los valores permitidos son `"highcharter"`
#'   (*valor predeterminado*), `"plotly"` o `"dygraphs"`. Los valores se emparejarán
#'   parcialmente.
#' @param estilo Lista compuesta por varios parámetros, los cuales van a ser usados
#'   de acuerdo con la librería especificada para graficar la serie y cuyo objetivo
#'   es personalizar pequeños detalles de ésta.
#'   * `LegendTitle`: Cadena de caracteres indicando un título para la leyenda
#'     (\emph{diferentes niveles del argumento `categorias`}). Se utilizará tanto
#'     en el paquete `Highcharter` como en `Plotly`.
#'   * `hc.Tema`: Modifica el tema con el cual se creará la serie. Los posibles
#'     valores son un número entero entre \eqn{[1, 10]} el cual hace referencia
#'     a diferentes temas disponibles en dicha librería (`ffx`, `google`, `tufte`,
#'     `538`, `ggplot2`, `economist`, `sandsignika`, `ft`, `superheroes` y `flatdark`,
#'     respectivamente). El tema por defecto, al no ingresar valor alguno, es
#'     `hc_theme_flat()`.
#'   * `hc.Slider`: Si es `TRUE` agrega un deslizador/navegador dinámico en la
#'     parte inferior de la serie. Proporciona herramientas para acercar y alejar
#'     partes de la serie, así como para desplazarse por el conjunto de datos.
#'     El valor por defecto es `FALSE`.
#'   * `hc.BoxInfo`: Si es `TRUE` (\emph{valor predeterminado}) la información
#'     concerniente a cada punto se visualiza conjuntamente en un cuadro, o de
#'     forma individual (`FALSE`) al pasar el cursor sobre él.
#'   * `hc.Credits`: Cadena de caracteres indicando un subtítulo o etiqueta de
#'     créditos debajo del título principal.
#'   * `ply.LegendPosition`: Lista que especifica la posición y orientación de
#'     la leyenda. Los valores por defecto la ubican centrada verticalmente a la
#'     derecha del plot, es decir, `c(x = 1, y = 0.5, orientation = "v")`.
#'   * `ply.Interaction`: Cadena de caracteres que determina el modo de las
#'     interacciones de desplazamiento. Los valores permitidos son `"x unified"`
#'     (\emph{valor predeterminado}), `"y unified"`, `"closest"`, `"x"`, `"y"` y
#'     `FALSE`.
#'   * `ply.Credits`: Lista que especifica la posición y texto para añadir un
#'     subtítulo o etiqueta de créditos a la serie principal, por ejemplo,
#'     `c(x = 0.2, y = 1, text = "https://...")`.
#'   * `dyg.LegendWidth`: Número que indica el ancho (\emph{en píxeles}) que
#'     ocupará la leyenda. El valor por defecto es `250`.
#'   * `dyg.Resaltar`: Si es `FALSE` (\emph{valor predeterminado}) no se resaltará
#'     la serie en que se sitúa el cursor.
#'   * `gg.Tema`: Modifica el tema con el cual se creará la serie. Los posibles
#'     valores son un número entero entre \eqn{[1, 11]} el cual hace referencia
#'     a diferentes temas disponibles para `ggplot2` (`theme_light`, `theme_bw`,
#'     `theme_classic`, `theme_linedraw`, `theme_gray`, `theme_hc`, `theme_pander`,
#'     `theme_gdocs`, `theme_fivethirtyeight`, `theme_economist` y `theme_solarized`
#'     respectivamente).
#'     El tema por defecto, al no ingresar valor alguno, es el construido por el
#'     departamento `theme_DNPE`.
#'   * `gg.Legend`: Lista que especifica la posición y orientación de la leyenda.
#'     Los valores por defecto la ubican verticalmente a la derecha del plot.
#'     Algunos valores aceptados para `legend.position` son `"none"`, `"left"`,
#'     `"top"`, `"right"`, `"bottom"` y `c(CoordX, CoordY)`. Para `legend.direction`
#'     solo se acepta `"vertical"` u `"horizontal"`.
#'   * `gg.Linea`: Una lista de parámetros admitidos por la función [geom_line()][ggplot2::geom_line()]).
#'   * `gg.Punto`: Una lista de parámetros admitidos por la función [geom_point()][ggplot2::geom_point()])
#'   * `gg.Texto`: Una lista cuyos valores admitidos y usados son `subtitle`,
#'     `caption` y `tag`.
#'   * `gg.Repel`: Una lista de parámetros admitidos por la función [geom_text_repel()][ggrepel::geom_text_repel()])
#' @param estatico Si es `FALSE` (*valor predeterminado*) el gráfico a retornar
#'   será dinámico (*dependiendo de la librería seleccionada*), en caso contrario
#'   se retornará un gráfico estático construido con `ggplot2`.
#'
#' @details
#' Al usar el paquete `Highcharter` y usar las opciones de descarga, el nombre del
#' archivo descargado será la concatenación del plot graficado y la categoría usada,
#' así, por ejemplo, si se graficó la serie de tiempo para la categoría "Sede" el
#' nombre será `PlotSeries_Sede.png`.
#'
#' Tenga en cuenta que la librería `"dygraphs"` solo la podrá usar si dentro del
#' argumento tiempo ingresa las dos variables (`YEAR`, `SEMESTRE`) para asemejar
#' su estructura a los agregados clásicos. En caso contrario le arrojara un error.
#'
#' Recuerde que puede usar más temas (\emph{cualquiera de hecho}) de los que se
#' proporcionan para `ggplot2`. Por ejemplo, los de \href{https://github.com/hrbrmstr/hrbrthemes}{hrbrthemes}
#' o \href{https://github.com/ricardo-bion/ggtech}{ggtech}.
#'
#' @note
#' A continuación, se consolida en una tabla amigable el listado, uso y disposición
#' de todas las opciones para el parámetro `estilo`, dependiendo del tipo de gráfico
#' (*dinámico o estático*) y la librería usada (*en el caso de que sea dinámico*).
#'
#' | **PARÁMETRO** | **VALOR** | **PARÁMETRO** |   **VALOR**   |    **PARÁMETRO**   |
#' |:-------------:|:---------:|:-------------:|:-------------:|:------------------:|
#' |               |     *     |               |       •       |       gg.Tema      |
#' |               |     *     |               |       •       |      gg.Legend     |
#' |               |   _TRUE_  |               |       •       |      gg.Linea      |
#' |               |     *     |               |       •       |      gg.Punto      |
#' |               |     *     |               |       •       |      gg.Texto      |
#' |               |     *     |               |       •       |      gg.Repel      |
#' |               |     O     |       ~       | _highcharter_ |       hc.Tema      |
#' |  **estatico** |     O     |       _       |       ¦       |     hc.BoxInfo     |
#' |               |     O     |       _       |       ¦       |      hc.Slider     |
#' |               |     O     |       _       |       ¦       |     hc.Credits     |
#' |               |  _FALSE_  |  **libreria** |    _plotly_   | ply.LegendPosition |
#' |               |     O     |       _       |       °       |     ply.Credits    |
#' |               |     O     |       _       |       °       |   ply.Interaction  |
#' |               |     O     |       _       |   _dygraphs_  |   dyg.LegendWidth  |
#' |               |     O     |       ~       |       L       |    dyg.Resaltar    |
#'
#' @returns
#' Retorna la serie (*objeto widget de HTML*) creada. La clase del objeto retornado
#' será un "htmlwidget" y dependiendo de la librería usada pertenecerá adicionalmente
#' a la clase "highchart", "plotly" o "dygraphs".
#'
#' @examplesIf all(require("tibble"), require("dplyr"))
#  # Ejemplo generalizado (sin uso de un consolidado como input)
#' # library("tibble"); library("dplyr")
#' set.seed(42)
#' Blood <- tibble(
#'   Year    = rep(2000:2001, each = 100),
#'   Quarter = sample(c("I", "II", "III", "IV"), size = 200, replace = TRUE),
#'   Week    = sample(c("1rt", "2nd", "3rd"), size = 200, replace = TRUE),
#'   Group   = sample(
#'     c("O", "A", "B", "AB"), size = 200, prob = c(.5, .3, .16, .4), replace = TRUE
#'   ),
#'   RH      = sample(c("+", "-"), size = 200, replace = TRUE),
#'   Prevalence = round(runif(200)*100)
#' )
#' Plot.Series(
#'   datos     = Blood,
#'   tiempo    = vars(Year, Quarter, Week),
#'   valores   = Prevalence,
#'   categoria = RH,
#'   labelX    = ""
#' )
#' Plot.Series(
#'   datos     = Blood,
#'   tiempo    = vars(Year, Quarter),
#'   valores   = Prevalence,
#'   categoria = Group,
#'   libreria  = "plotly"
#' )
#' # ---------------------------------------------------------------------------
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
#' Msj <- "Distribuci\u00f3n de estudiantes graduados (desde el 2009-I al 2021-I) por sede."
#' Txt <- "EVOLUCI\u00d3N DEL N\u00daMERO DE GRADUADOS POR SEDE"
#' Plot.Series(
#'   datos        = ejConsolidadoGrad,
#'   categoria    = "SEDE_NOMBRE_ADM",
#'   freqRelativa = TRUE,
#'   ylim         = c(0, 75),
#'   colores      = misColores,
#'   titulo       = Txt,
#'   labelY       = "Frecuencia Relativa<br>(% de graduados)",
#'   libreria     = "highcharter",
#'   estilo       = list(LegendTitle = "SEDE:", hc.Tema = 10, hc.Slider = TRUE, hc.Credits = Msj)
#' )
#' # ---------------------------------------------------------------------------
#' Plot.Series(
#'   datos     = ejConsolidadoGrad,
#'   categoria = "SEDE_NOMBRE_ADM",
#'   invertir  = TRUE,
#'   colores   = misColores,
#'   titulo    = Txt,
#'   labelY    = "N\u00famero de Graduados",
#'   libreria  = "plotly",
#'   estilo    = list(
#'     LegendTitle = "SEDE:", ply.Interaction = "closest",
#'     ply.LegendPosition = list(x = 0.16, y = -0.25, orientation = "h"),
#'     ply.Credits = list(x = 0.5, y = 0.1, text = Msj)
#'   )
#' )
#' # ---------------------------------------------------------------------------
#' Plot.Series(
#'   datos     = ejConsolidadoGrad,
#'   categoria = "SEDE_NOMBRE_ADM",
#'   colores   = misColores,
#'   titulo    = Txt,
#'   labelY    = "N\u00famero de Graduados (k: miles)",
#'   libreria  = "dygraphs",
#'   estilo    = list(dyg.LegendWidth = 650, dyg.Resaltar = TRUE)
#' )
#'
#' @examplesIf require("dplyr")
#' # ---------------------------------------------------------------------------
#' # Agrupando para eliminar el semestre
#' # library("dplyr")
#' df <- ejConsolidadoGrad |> group_by(Variable, YEAR, Clase) |>
#'   summarise(Total = sum(Total, na.rm = TRUE), .groups = "drop")
#'
#' Msj <- "Comportamiento anual, considerando ambos semestres (exceptuando el caso del 2021)."
#' Plot.Series(
#'   datos     = df,
#'   categoria = "SEXO",
#'   ylim      = c(1000, 6000),
#'   colores   = c("#3360FF", "#F30081"),
#'   titulo    = "EVOLUCI\u00d3N DEL N\u00daMERO DE GRADUADOS POR SEXO",
#'   labelX    = "A\u00f1o",
#'   labelY    = "N\u00famero de Graduados",
#'   libreria  = "highcharter",
#'   estilo    = list(hc.Tema = 1, hc.Credits = Msj)
#' )
#'
#' @examplesIf all(require("ggplot2"), require("magick"), require("cowplot"))
#' # ---------------------------------------------------------------------------
#' # Ejemplo usando el caso estático (ggplot2)
#' # library("magick"); library("cowplot")
#' txtA <- "EVOLUCI\u00d3N DEL N.\u00ba DE GRADUADOS \u00d7 SEDE"
#' txtB <- "\nComportamiento anual (exceptuando el caso del 2021)."
#' fig1 <- Plot.Series(
#'   datos        = ejConsolidadoGrad,
#'   categoria    = "SEDE_NOMBRE_ADM",
#'   freqRelativa = FALSE,
#'   invertir     = FALSE,
#'   ylim         = c(100, 2000),
#'   colores      = misColores,
#'   titulo       = txtA,
#'   labelY       = "N\u00famero de Graduados",
#'   estatico     = TRUE,
#'   estilo       = list(
#'     LegendTitle = "SEDE:", gg.Tema = 8,
#'     gg.Legend = list(legend.position = "bottom", legend.direction = "horizontal"),
#'     gg.Linea  = list(linetype = 2, size = 0.1, arrow = grid::arrow()),
#'     gg.Punto  = list(alpha = 0.2, shape = 21, size = 2, stroke = 5),
#'     gg.Texto  = list(
#'       subtitle = txtB, caption = "\t\t Informaci\u00f3n Disponible desde 2009-1", tag = "\u00ae"
#'     )
#'   )
#' )
#' # A continuación, se detalla el caso en el que quiera adicionar un logo a 'fig1'
#' # library("ggplot2"); library("magick"); require("cowplot")
#' URL <- "https://upload.wikimedia.org/wikipedia/commons/1/1e/UNAL_Logosimbolo.svg"
#' LogoUN <- magick::image_read_svg(URL)
#' ggdraw() +
#'   draw_image(LogoUN, scale = 0.15, x = 0.15, hjust = 1, halign = 1, valign = 0) +
#'   draw_plot(fig1 + theme(legend.background = element_blank(),
#'                          panel.background = element_blank(),
#'                          plot.background = element_blank()
#'                          )
#'   )
#'
#' @examplesIf all(require("ggplot2"), require("ggrepel"))
#' # ---------------------------------------------------------------------------
#' # A continuación, se detalla el caso en el que quiera anotaciones textuales repulsivas
#' #   * (1) Espacio vacío que se debe respetar alrededor de la caja delimitadora
#' #   * (2) Espacio vacío que se debe respetar alrededor de cada punto
#' #   * (3) Entre más bajo más flechas, entre más distancia menos flechas
#' Plot.Series(
#'   datos        = ejConsolidadoGrad,
#'   categoria    = "SEDE_NOMBRE_ADM",
#'   freqRelativa = FALSE,
#'   invertir     = FALSE,
#'   ylim         = c(100, 2000),
#'   colores      = misColores,
#'   titulo       = "EVOLUCI\u00d3N DEL N.\u00ba DE GRADUADOS \u00d7 SEDE",
#'   labelY       = "N\u00famero de Graduados",
#'   estatico     = TRUE,
#'   estilo       = list(
#'     gg.Tema  = 1,
#'     gg.Repel = list(
#'       direction = "both", seed = 42, nudge_y = 0.25,
#'       arrow = arrow(length = unit(0.01, "npc")), segment.colour = "#4C716B",
#'       box.padding   = 0.5 ,     # (1)
#'       point.padding = 0.25,     # (2)
#'       min.segment.length = 0.45 # (3)
#'     )
#'   )
#' )
#'
#' @export
#'
#' @import highcharter
#' @import plotly
#' @import dygraphs
#' @rawNamespace import(ggplot2, except = last_plot)
#' @import dplyr
#' @importFrom scales percent label_percent
#' @importFrom tidyr pivot_wider pivot_longer
#' @importFrom xts xts
#' @importFrom zoo as.Date as.yearmon
#' @importFrom ggrepel geom_text_repel
#' @importFrom forcats as_factor fct_relevel
#' @importFrom methods missingArg
#' @importFrom grDevices rainbow
Plot.Series <- function(
    datos, tiempo, valores, categoria, freqRelativa = FALSE, invertir = FALSE,
    ylim, colores, titulo = "", labelX = "Periodo", labelY = "",
    libreria = c("highcharter", "plotly", "dygraphs"), estilo = NULL,
    estatico = FALSE
) {
  # COMANDOS DE VERIFICACIÓN Y VALIDACIÓN --------------------------------------
  if (missingArg(datos) || missingArg(categoria)) {
    stop("\u00a1Por favor introduzca un conjunto de datos y una categor\u00eda dentro de la columna 'Variable'!", call. = FALSE)
  }
  if (!all(is.logical(freqRelativa), is.logical(invertir), is.logical(estatico))) {
    stop("\u00a1Los argumentos 'freqRelativa', 'invertir' y 'estatico' deben ser un valor booleano (TRUE o FALSE)!", call. = FALSE)
  }
  if (!missingArg(ylim)) {
    if (!(is.numeric(ylim) && length(ylim) == 2)) {
      stop("\u00a1Por favor introduzca un vector de longitud dos que definen los l\u00edmites del eje Y!", call. = FALSE)
    }
    yLim <- ylim
  } else { yLim <- NULL }
  if (invertir) {
    Invertir <- "reversed"; ggInvertir <- "reverse"
    if (libreria != "highcharter") { yLim <- rev(yLim) }
  } else {
    Invertir <- NULL      ; ggInvertir <- "identity"
  }
  if (!all(is.character(titulo), is.character(labelX), is.character(labelY))) {
    stop("\u00a1Los argumentos 'titulo', 'labelX' y 'labelY' deben ser una cadena de texto!", call. = FALSE)
  }
  if (!estatico) {
    if (missingArg(libreria)) {
      warning("\u00a1Se usar\u00e1 la librer\u00eda 'highcharter' por defecto para realizar el plot!", call. = FALSE)
      libreria <- "highcharter"
    } else {
      libreria <- tolower(libreria)
      if (libreria %NotIN% c("highcharter", "plotly", "dygraphs")) {
        stop("\u00a1Por favor introduzca el nombre de una librer\u00eda v\u00e1lida (paquete usado para realizar la gr\u00e1fica)!", call. = FALSE)
      }
    }
  } else { libreria <- NULL }
  LegendTitle <- ifelse(is.null(estilo$LegendTitle), '', estilo$LegendTitle)

  # GENERACIÓN DEL DATAFRAME CON EL CUAL SE CREARÁ LA GRÁFICA ------------------
  if (all(missingArg(tiempo), missingArg(valores), !missingArg(categoria))) {
    if (!(toupper(categoria) %in% datos$Variable)) {
      stop("\u00a1Por favor introduzca una categor\u00eda que se encuentre dentro de la columna 'Variable'!", call. = FALSE)
    }
    if("SEMESTRE" %in% names(datos)) { tiempo <- c("YEAR", "SEMESTRE") } else { tiempo <- "YEAR"}
    datos <- datos |> ungroup() |> filter(Variable == categoria)
    tiempo    <- vars(!!!syms(tiempo))
    categoria <- sym("Clase")
    valores   <- sym("Total")
  } else {
    if (any(class(try(class(tiempo), silent = TRUE)) == "try-error")) { tiempo <- vars({{tiempo}}) }
  }

  datosCheck <- datos |> group_by(!!!tiempo, {{categoria}}) |>
    summarise({{valores}} := sum({{valores}}), .groups = "drop")
  if (nrow(datosCheck) != nrow(datos)) {
    msg <- "
    \u00a1Ha ingresado un dataframe que no est\u00e1 de forma condensada, es decir,
    para cada categor\u00eda existe m\u00e1s de un valor para un mismo punto del eje X!
    Se sumar\u00e1 los valores por defectos para dichos puntos que gocen de +1 valor
           "
    warning(msg, call. = FALSE)
    datos <- datosCheck
  }
  DataFrame <- datos |>
    arrange(!!!tiempo) |> mutate(Fecha = paste(!!!tiempo, sep = "-")) |>
    select(Fecha, {{categoria}}, {{valores}}) |> relocate(Fecha)

  tableHoriz <- DataFrame |>
    pivot_wider(
      names_from = {{categoria}}, values_from = {{valores}},
      values_fill = 0, values_fn = ~sum(.x, na.rm = TRUE)
    )
  categorias <- DataFrame |> select({{categoria}}) |> distinct() |> pull()

  if (length(categorias) == 1L) {
    Relativo <- DataFrame |> mutate(Relativo = {{valores}} / {{valores}} * 100) |> select(!({{valores}}))
  } else {
    Relativo <- tableHoriz |> select(!Fecha) |> PocentRelativo() |>
      as_tibble() |> mutate(Fecha = tableHoriz$Fecha) |>
      pivot_longer(cols = categorias, names_to = quo_name(enquo(categoria)), values_to = "Relativo")
  }
  TablaFinal <- DataFrame |> inner_join(Relativo, by = join_by(Fecha, {{categoria}}))
  TablaFinal <- TablaFinal |> rename(Clase := {{categoria}}, Total := {{valores}})
  TablaFinal$Fecha <- forcats::as_factor(TablaFinal$Fecha)
  # - . - . - . - . - . - . - . - . - . - . - . - . - . - . - . - . - . - . - .
  if (!(missingArg(colores) || length(colores) == length(categorias))) {
    stop(paste0(
      "\u00a1El n\u00famero de colores ingresados en el vector 'colores' no corresponde con el n\u00famero de categor\u00edas a colorear!",
      "\n\tNo. colores ingresados = ", length(colores), " != ", "No. de categor\u00edas = ", length(categorias)
      ), call. = FALSE
    )
  }
  if (missingArg(colores)) { colores <- rainbow(length(categorias), alpha = 0.7) }

  if (is.null(levels(datos |> select({{categoria}}) |> pull()))) {
    TablaFinal$Clase <- forcats::as_factor(TablaFinal$Clase)
    TablaFinal$Clase <- forcats::fct_relevel(TablaFinal$Clase, sort)
  } else {
    TablaFinal$Clase <- forcats::fct_relevel(TablaFinal$Clase, levels(datos |> select({{categoria}}) |> pull()))
  }

  # CREACIÓN DEL PLOT A RETORNAR -----------------------------------------------
  if (!estatico) {
    if (libreria == "highcharter") {
      # SEGREGACIÓN DEL CONDICIONAL DE FRECUENCIA ABSOLUTA O RELATIVA
      if (freqRelativa) {
        sufijoY    <- "{value}%"
        TablaFinal <- TablaFinal |> rename_at(vars(Relativo, Total), ~ c("Y", "Extra"))
        strFormat  <- '<span style="color:{series.color}">\u25CF </span><b>{series.name}: {point.y}%</b> ({point.Extra})<br/>'
      } else {
        sufijoY    <- "{value}"
        TablaFinal <- TablaFinal |> rename_at(vars(Total, Relativo), ~ c("Y", "Extra"))
        strFormat  <- '<span style="color:{series.color}">\u25CF </span><b>{series.name}: {point.y}</b> ({point.Extra}%)<br/>'
      }

      Spanish.Highcharter()
      if (!(missingArg(estilo) || is.null(estilo$hc.Tema))) {
        ThemeHC <- switch(
          estilo$hc.Tema,
          "1"  = hc_theme_ffx(),
          "2"  = hc_theme_google(),
          "3"  = hc_theme_tufte(),
          "4"  = hc_theme_538(),
          "5"  = hc_theme_ggplot2(),
          "6"  = hc_theme_economist(),
          "7"  = hc_theme_sandsignika(),
          "8"  = hc_theme_ft(),
          "9"  = hc_theme_superheroes(),
          "10" = hc_theme_flatdark()
        )
      } else { ThemeHC <- hc_theme_flat() }
      BoxInfo <- ifelse(!(missingArg(estilo) || is.null(estilo$hc.BoxInfo)), estilo$hc.BoxInfo, TRUE)

      PlotSeries <- TablaFinal |>
        hchart(
          type = "line", hcaes(x = Fecha, y = Y, group = Clase), color = colores,
          zoomType = list(enabled = FALSE), resetZoomButton = TRUE
        ) |>
        hc_chart(type = "datetime", zoomType = "x") |>
        hc_plotOptions(line = list(marker = list(enabled = FALSE, symbol = "square", radius = 1))) |>
        hc_title(text = titulo, style = list(fontWeight = "bold", fontSize = "22px", color = "#333333", useHTML = TRUE)) |>
        hc_xAxis(
          categories = as.list(levels(TablaFinal$Fecha)),
          title = list(text = labelX, offset = 70, style = list(fontWeight = "bold", fontSize = "18px", color = "black")),
          align = "center", lineColor = "#787878", opposite = FALSE,
          labels = list(style = list(fontWeight = "bold", color = "black", fontSize = "18px"))
        ) |>
        hc_yAxis(
          reversed = invertir, lineColor = "#787878", opposite = FALSE,
          lineWidth = 1, min = yLim[1], max = yLim[2],
          title = list(text = labelY, offset = 70, style = list(
            fontWeight = "bold", fontSize = "18px", color = "black"
            )
          ),
          labels = list(format = sufijoY, style = list(fontWeight = "bold", color = "black", fontSize = "18px"))
        ) |>
        # https://github.com/jbkunst/highcharter/issues/331
        hc_exporting(enabled = TRUE, filename = paste0("PlotSeries_", quo_name(enquo(categoria)))) |>
        hc_legend(
          enabled = TRUE, align = "center", verticalAlign = "bottom", layout = "horizontal",
          title = list(text = LegendTitle, style = list(textDecoration = "underline")),
          x = 42, y = 0, itemStyle = list(
            fontWeight = "bold", color = "black", fontSize = "18px"
          )
        ) |>
        hc_tooltip(
          crosshairs = TRUE, shared = BoxInfo, pointFormat = strFormat,
          backgroundColor = hex_to_rgba("#BAAEAE", 0.7),
          borderColor = "#6D6666", borderWidth = 5, useHTML = TRUE
        ) |>
        hc_add_theme(ThemeHC)

      if (!missingArg(estilo) && "hc.Slider" %in% names(estilo) && estilo$hc.Slider) {
        PlotSeries <- PlotSeries |>
          hc_navigator(
            height = 15, margin = 5, maskFill = "rgba(255,16,46,0.6)",
            enabled = TRUE, series = list(
              color = "#999999", lineWidth = 30, type = "areaspline", fillColor = "#999999"
            )
          ) |>
          hc_rangeSelector(
            enabled = TRUE, inputEnabled = FALSE, labelStyle = list(display = "none"),
            buttonPosition = list(align = "left"), floating = FALSE,
            buttons = list(list(type = "all", text = "Restaurar"))
          )
      }
      if (!(missingArg(estilo) || is.null(estilo$hc.Credits))) {
        PlotSeries <- PlotSeries |>
          hc_subtitle(text = estilo$hc.Credits, align = "left", style = list(color = "#2B908F", fontWeight = "bold"))
      }

    } else if (libreria == "plotly") {
      if (!(missingArg(estilo) || is.null(estilo$ply.LegendPosition))) {
        ParmsLegend <- estilo$ply.LegendPosition
      } else {
        ParmsLegend <- list(x = 1, y = 0.5, orientation = "v")
      }
      if (!(missingArg(estilo) || is.null(estilo$ply.Credits))) {
        ParmsCredits <- estilo$ply.Credits
      } else {
        ParmsCredits <- list(x = 0.2, y = 1, text = "")
      }
      Hovermode <- ifelse(!(missingArg(estilo) || is.null(estilo$ply.Interaction)), estilo$ply.Interaction, "x unified")

      FreqRelativa <- Relativo |> pivot_wider(names_from = {{categoria}}, values_from = Relativo)
      X <- tableHoriz$Fecha; PlotSeries <- plot_ly()
      for (i in seq_len(length(categorias))) {
        if (freqRelativa) {
          strFormat <- "%{y} (%{text})"; sufijoY <- "%"
          Y    <- FreqRelativa[[categorias[i]]]
          Text <- tableHoriz[[categorias[i]]]
        } else {
          strFormat <- "%{y} (%{text:.2s}%)"; sufijoY <- ""
          Y    <- tableHoriz[[categorias[i]]]
          Text <- FreqRelativa[[categorias[i]]]
        }
        PlotSeries <- PlotSeries |>
          add_trace(
            x = X, y = Y, text = Text, name = categorias[i], type = "scatter",
            mode = "markers+lines", line = list(color = colores[i], width = 3),
            marker = list(color = colores[i], size = 6, line = list(width = 1.2, color = "#787878")),
            hovertemplate = strFormat, textposition = "outside"
          )
      }
      # Arial | Open Sans | Courier New, monospace
      FamilyAxis  <- list(family = "Old Standard TT, serif", size = 16, color = "#525252")
      FamilyTitle <- list(family = "Open Sans", size = 24, color = "#333333")

      Title <- list(text = paste0("<b>", titulo, "</b>"), font = FamilyTitle, y = 0.96)
      Xaxis <- list(
        title          = labelX,
        zeroline       = FALSE,
        showline       = TRUE,
        showgrid       = FALSE,
        showticklabels = TRUE,
        linecolor      = "#787878",
        linewidth      = 2.5,
        autotick       = FALSE,
        ticks          = "outside",
        tickwidth      = 2.5,
        ticklen        = 10,
        tickcolor      = "#CCCCCC",
        tickangle      = -45,
        tickfont       = FamilyAxis
      )
      Yaxis <- list(
        title          = labelY,
        ticksuffix     = sufijoY,
        zeroline       = TRUE,
        autorange      = Invertir,
        range          = yLim,
        showline       = TRUE,
        showgrid       = TRUE,
        showticklabels = TRUE,
        linecolor      = "#787878",
        linewidth      = 3,
        tickfont       = FamilyAxis,
        separatethousands = TRUE
      )

      PlotSeries <- PlotSeries |>
        layout(
          title = Title, xaxis = Xaxis, yaxis = Yaxis,
          autosize = TRUE, showlegend = TRUE,
          legend = append(ParmsLegend, list(
            traceorder = "normal", title = list(text = paste0("<b>", LegendTitle, "</b>"))
            )
          ),
          hovermode = Hovermode,
          annotations = append(ParmsCredits, list(
            showarrow = FALSE, xref = "paper", yref = "paper",
            xanchor = "right", yanchor = "auto", xshift = 0, yshift = 0,
            font = list(size = 12, color = "#CCCCCC")
            )
          )
        ) |>
        config(locale = "es")

    } else if (libreria == "dygraphs") {
      if (freqRelativa) {
        with0 <- FALSE
        dygraphDF <- Relativo |> pivot_wider(names_from = {{categoria}}, values_from = Relativo)
      } else {
        with0 <- TRUE
        dygraphDF <- tableHoriz
      }

      LegendWidth <- ifelse(!(missingArg(estilo) || is.null(estilo$dyg.LegendWidth)), estilo$dyg.LegendWidth, 250)
      Periodos <- dygraphDF |> select(Fecha) |> distinct() |> pull()
      Periodos <- gsub("-2", "-7", Periodos)
      TableHorizontal <- dygraphDF
      TableHorizontal$Fecha <- zoo::as.Date(as.yearmon(Periodos, "%Y-%m"))
      TableHorizontal <- xts::xts(x = TableHorizontal[, -1], order.by = TableHorizontal$Fecha)

      getSemestre <- 'function(d) {
                      var monthNames = ["I", "", "", "", "", "","II", "", "", "", "", ""];
                      date = new Date(d);
                      if (date.getMonth() == 0 || date.getMonth() == 6) {
                        return date.getFullYear() + "-" + monthNames[date.getMonth()];
                      } else {
                        return "";
                      }
                   }'
      dyUnzoom <- function(dygraph) {
        dyPlugin(
          dygraph = dygraph, name = "Unzoom",
          path = system.file("plugins/unzoom.js", package = "dygraphs")
        )
      }

      PlotSeries <- dygraph(TableHorizontal, main = paste0("<span style='color:", "#333333", ";'>", titulo, "</span>")) |>
        dyOptions(
          drawPoints = TRUE, pointSize = 2, strokeWidth = 2, colors = colores,
          includeZero = with0, axisTickSize = 3, axisLineColor = "#787878",
          axisLabelColor = "#525252", axisLabelFontSize = 16,
          drawGrid = TRUE, gridLineColor = "lightblue"
        ) |>
        dyLegend(show = "always", width = LegendWidth, hideOnMouseOut = TRUE) |>
        dyAxis("x", label = labelX, axisLabelFormatter = JS(getSemestre), axisLineWidth = 4) |>
        dyAxis("y", label = labelY, axisLineWidth = 4) |>
        dyRangeSelector(height = 30, strokeColor = "") |>
        dyUnzoom()

      if (!(missingArg(estilo) || is.null(estilo$dyg.Resaltar))) {
        if (estilo$dyg.Resaltar) {
          PlotSeries <- PlotSeries |>
            dyHighlight(
              highlightCircleSize = 5, highlightSeriesBackgroundAlpha = 0.5,
              highlightSeriesOpts = list(strokeWidth = 2.5), hideOnMouseOut = TRUE
            )
        }
      }
    }
  } else {
    if (!(missingArg(estilo) || is.null(estilo$gg.Tema))) {
      ThemeGG <- switch(
        estilo$gg.Tema,
        "1"  = theme_light(),
        "2"  = theme_bw(),
        "3"  = theme_classic(),
        "4"  = theme_linedraw(),
        "5"  = theme_gray(),
        "6"  = ggthemes::theme_hc(),
        "7"  = ggthemes::theme_pander(),
        "8"  = ggthemes::theme_gdocs(),
        "9"  = ggthemes::theme_fivethirtyeight(),
        "10" = ggthemes::theme_economist(),
        "11" = ggthemes::theme_solarized()
      )
    } else { ThemeGG <- theme_DNPE() }


    if (freqRelativa) {
      TablaFinal <- TablaFinal |> rename_at(vars(Relativo, Total), ~ c("Y", "Extra"))
    } else {
      TablaFinal <- TablaFinal |> rename_at(vars(Total, Relativo), ~ c("Y", "Extra"))
    }
    if (!(missingArg(estilo) || is.null(estilo$gg.Legend))) {
      ParmsLegend <- estilo$gg.Legend
    } else {
      ParmsLegend <- list(legend.position = "right", legend.direction = "vertical")
    }
    if (!(missingArg(estilo) || is.null(estilo$gg.Linea))) {
      ParmsLine  <- estilo$gg.Linea
    } else {
      ParmsLine  <- list(linetype = "solid", linewidth = 1)
    }
    if (!(missingArg(estilo) || is.null(estilo$gg.Punto))) {
      ParmsPoint <- estilo$gg.Punto
    } else {
      ParmsPoint <- list(size = 2)
    }
    if (!(missingArg(estilo) || is.null(estilo$gg.Texto))) {
      ParmsLabs  <- estilo$gg.Texto
    } else {
      ParmsLabs  <- list(subtitle = NULL, caption = NULL, tag = NULL)
    }

    PlotSeries <- ggplot(data = TablaFinal, aes(x = Fecha, y = Y, group = Clase, color = Clase)) +
      do.call(geom_line, ParmsLine) + do.call(geom_point, ParmsPoint) +
      labs(
        x = labelX, y = br2addline(labelY), title = titulo, subtitle = ParmsLabs$subtitle,
        caption = ParmsLabs$caption, tag = ParmsLabs$tag, color = LegendTitle
      ) +
      scale_x_discrete(guide = guide_axis(angle = 45)) +
      scale_color_manual(values = colores) +
      ThemeGG + do.call(theme, ParmsLegend)


    scaleY   <- list(limits = yLim, trans = ggInvertir)
    if (!(missingArg(estilo) || is.null(estilo$gg.Repel))) {
      ParmsRepel <- estilo$gg.Repel
      FlagRepel  <- TRUE
    } else {
      ParmsRepel <- list(check_overlap = TRUE, position = position_dodge(width = 0), vjust = -0.5, size = 3)
      FlagRepel  <- FALSE
    }

    if (freqRelativa) {
      if (FlagRepel) {
        PlotSeries <- PlotSeries +
          do.call(ggrepel::geom_text_repel, append(ParmsRepel, list(aes(label = scales::percent(Y, scale = 1)))))
      } else {
        PlotSeries <- PlotSeries +
          do.call(geom_text, append(ParmsRepel, list(aes(label = scales::percent(Y, scale = 1)))))
      }
      PlotSeries <- PlotSeries + do.call(scale_y_continuous, append(scaleY, list(labels = scales::label_percent(scale = 1))))
    } else {
      if (FlagRepel) {
        PlotSeries <- PlotSeries +
          do.call(ggrepel::geom_text_repel, append(ParmsRepel, list(aes(label = Y))))
      } else {
        PlotSeries <- PlotSeries +
          do.call(geom_text, append(ParmsRepel, list(aes(label = Y))))
      }
      PlotSeries <- PlotSeries + do.call(scale_y_continuous, scaleY)
    }

  }

  return(PlotSeries)
}
