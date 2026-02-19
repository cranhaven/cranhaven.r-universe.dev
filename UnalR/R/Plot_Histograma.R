#' Cree un histograma que represente gráficamente la distribución de datos en un
#' conjunto, facilitando la comprensión de su frecuencia y patrones con dos
#' diferentes paquetes.
#'
#' Esta función proporciona excelentes herramientas y opciones para la visualización
#' de un histograma, con el objetivo de que pueda representar la distribución de
#' frecuencia de datos numéricos (*variable de un conjunto de datos*) en forma de
#' barras, donde cada barra representa la cantidad de veces que aparece un valor
#' o rango de valores. Dicho diagrama se puede representar usando dos diferentes
#' librerías que son `Highcharter` y `Plotly`, las cuales usan internamente `JavaScript`.
#'
#' @inheritParams Plot.Series
#' @param variable Una variable numérica dentro del data frame ingresado en `datos`.
#' @param color Cadena de caracteres indicando el color de relleno de las barras
#'   para todos los rangos de valores (intervalos).
#' @param bins Valor numérico que indica el número máximo de bins deseado. Este
#'   valor se utilizará en un algoritmo que decidirá el tamaño de bins óptimo para
#'   que el histograma visualice mejor la distribución de los datos.
#' @param density Si es `TRUE` se agregará la curva de densidad superpuesta al
#'   histograma. El valor por defecto es `FALSE`.
#' @param xlim Vector numérico que especifica el límite inferior y superior,
#'   respectivamente, del eje `X`. Si no se introduce algún valor se mostrará
#'   todo el rango disponible para dicho eje.
#' @param estilo Lista compuesta por varios parámetros, los cuales van a ser usados
#'   de acuerdo con la librería especificada para graficar el plot y cuyo objetivo
#'   es personalizar pequeños detalles de ésta.
#'   * `hc.Tema`, `ply.Credits`, `gg.Tema` y `gg.Texto`: Igual uso
#'     que en [Plot.Series()]
#'   * `ply.Density`: Una lista de parámetros admitidos por el argumento `line`
#'     de la función [add_lines()][plotly::add_lines()].
#'   * `gg.Hist`: Una lista de parámetros admitidos por la función [geom_histogram()][ggplot2::geom_histogram()].
#'   * `gg.Density`: Una lista de parámetros admitidos por la función [geom_density()][ggplot2::geom_density()].
#'
#' @details
#' * Al usar la librería `Highcharter` no se podrá superponer la curva de densidad,
#'   pues no se dispone de esta funcionalidad para dicho paquete.
#' * Si está usando el caso estático (`ggplot2`) y adicionalmente está graficando
#'   la curva de densidad, recuerde que el eje `Y` que predomina es el de la curva
#'   de densidad, por tal razón, si va a usar el argumento `ylim` debe recordar
#'   que quedara en la escala de \eqn{[0, 1]}.
#' * Tenga cuidado al usar el argumento `xlim` en el caso estático, ya que si uno
#'   de los bins se ve cortado (*no abarca el inicio y fin de éste*) no se graficará
#'   dicho intervalo.
#' * Al usar el paquete `Highcharter` y usar las opciones de descarga, el nombre
#'   del archivo descargado será la concatenación del plot graficado y la categoría
#'   usada, así, por ejemplo, si se graficó el diagrama de barras para la categoría
#'   "Nacionalidad" el nombre será `PlotHistograma__Nacionalidad.png`.
#'
#' @inheritSection Plot.Series Lista de argumentos de estilo
#'
#' @returns
#' Retorna el histograma (*objeto widget de HTML*) creado. La clase del objeto
#' retornado será un "htmlwidget" y dependiendo de la librería usada pertenecerá
#' adicionalmente a la clase "highchart" o "plotly".
#'
#' @examples
#' Txt <- "Datos Oficiales de la Prueba Saber Pro del año 2020"
#' Plot.Histograma(
#'   datos    = ejSaberPro2020,
#'   variable = PUNT_RAZO_CUANT,
#'   color    = "#12D640",
#'   bins     = 100,
#'   titulo   = "DISTRIBUCI\u00d3N EN EL PUNTAJE DE RAZONAMIENTO CUANTITATIVO",
#'   ylim     = c(0, 175),
#'   labelX   = "Puntaje en Matem\u00e1ticas",
#'   labelY   = "N\u00famero de Estudiantes",
#'   libreria = "highcharter",
#'   estilo   = list(hc.Tema  = 4)
#' )
#' # ---------------------------------------------------------------------------
#' Plot.Histograma(
#'   datos    = ejSaberPro2020,
#'   variable = PUNT_RAZO_CUANT,
#'   color    = "#B9ABD1",
#'   bins     = 80,
#'   density  = TRUE,
#'   titulo   = "DISTRIBUCI\u00d3N EN EL PUNTAJE\nDE RAZONAMIENTO CUANTITATIVO",
#'   ylim     = c(0, 400),
#'   labelX   = "Puntaje en Matem\u00e1ticas",
#'   labelY   = "N\u00famero de Estudiantes",
#'   libreria = "plotly",
#'   estilo   = list(
#'     ply.Credits = list(x = 0.5, y = 1.1, text = Txt),
#'     ply.Density = list(color = "#DD3380", width = 4, dash = "dot", opacity = 0.2)
#'   )
#' )
#' # ---------------------------------------------------------------------------
#' # Ejemplo usando el caso estático (ggplot2)
#' Plot.Histograma(
#'   datos    = ejSaberPro2020,
#'   variable = PUNT_RAZO_CUANT,
#'   density  = TRUE,
#'   titulo   = "DISTRIBUCI\u00d3N EN EL PUNTAJE DE RAZONAMIENTO CUANTITATIVO",
#'   labelX   = "Puntaje en Matem\u00e1ticas",
#'   labelY   = "N\u00famero de Estudiantes",
#'   estatico = TRUE,
#'   estilo   = list(
#'     gg.Tema    = 6,
#'     gg.Hist    = list(
#'       binwidth = 10, fill = "#FF4040", color = "#144169", alpha = 0.5, linetype = "dashed"
#'     ),
#'     gg.Density = list(color = "#20B2AA", fill = "#40E0D0", alpha = 0.4, lwd = 1.1, linetype = 2),
#'     gg.Texto   = list(subtitle = "\u00bb\u00bb\u00bb", tag = "\u00ae", caption  = Txt)
#'   )
#' )
#'
#' @export
#'
#' @import highcharter
#' @import plotly
#' @rawNamespace import(ggplot2, except = last_plot)
#' @import dplyr
#' @importFrom methods missingArg
#' @importFrom graphics hist
Plot.Histograma <- function(
    datos, variable, color, bins, density = FALSE,
    titulo = "", xlim, ylim, labelX = NULL, labelY = "Conteo",
    libreria = c("highcharter", "plotly"), estilo = NULL, estatico = FALSE) {

  # COMANDOS DE VERIFICACIÓN Y VALIDACIÓN
  if (missingArg(datos)) {
    stop("\u00a1Por favor introduzca un conjunto de datos!", call. = FALSE)
  }
  if (!all(is.character(titulo), is.character(labelY))) {
    stop("\u00a1El argumento 'titulo' y 'labelY' deben ser una cadena de texto!", call. = FALSE)
  }
  if (!all(is.logical(density), is.logical(estatico))) {
    stop("\u00a1Los argumentos 'density' y 'estatico' deben ser un valor booleano (TRUE o FALSE)!", call. = FALSE)
  }
  if (!estatico) {
    if (missingArg(libreria)) {
      warning("\u00a1Se usar\u00e1 la librer\u00eda 'highcharter' por defecto para realizar el plot!", call. = FALSE)
      libreria <- "highcharter"
    } else {
      libreria <- tolower(libreria)
      if (libreria %NotIN% c("highcharter", "plotly")) {
        stop("\u00a1Por favor introduzca el nombre de una librer\u00eda valida (paquete usado para realizar la gr\u00e1fica)!", call. = FALSE)
      }
    }
  }
  if (missingArg(bins)) {
    if (all(libreria == "highcharter", !estatico)) { bins <- "Sturges" } else { bins <- NULL }
    flagBins <- FALSE
  } else {
    if (!is.numeric(bins)) {
      stop("\u00a1El argumento 'bins' debe ser un valor num\u00e9rico!", call. = FALSE)
    }
    flagBins <- TRUE
  }
  if (!missingArg(xlim)) {
    if (!(is.numeric(xlim) && length(xlim) == 2)) {
      stop("\u00a1Por favor introduzca un vector de longitud dos que definen los l\u00edmites del eje Y!", call. = FALSE)
    }
    xLim <- xlim
  } else { xLim <- NULL }
  if (!missingArg(ylim)) {
    if (!(is.numeric(ylim) && length(ylim) == 2)) {
      stop("\u00a1Por favor introduzca un vector de longitud dos que definen los l\u00edmites del eje Y!", call. = FALSE)
    }
    yLim <- ylim
  } else { yLim <- NULL }

  NameVar <- quo_name(enquo(variable))
  if (is.null(labelX)) { labelX <- NameVar }

  # CREACIÓN DEL PLOT RETORNAR
  if (!estatico) {
    if (libreria == "highcharter") {
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

      if (flagBins) {
        PlotHistogram <- hchart(
          hist(datos |> select({{ variable }}) |> pull(), breaks = bins, plot = FALSE),
          color = color, name = NameVar
        )
      } else {
        PlotHistogram <- hchart(datos |> select({{ variable }}) |> pull(), color = color, name = NameVar)
      }
      PlotHistogram <- PlotHistogram |>
        hc_title(text = titulo, style = list(
          fontWeight = "bold", fontSize = "22px", color = "#333333", useHTML = TRUE
          )
        ) |>
        hc_yAxis(
          title = list(text = labelY, style = list(fontWeight = "bold", color = "black", fontSize = "18px")),
          min = yLim[1], max = yLim[2]
        ) |>
        hc_xAxis(
          title = list(text = labelX, style = list(fontWeight = "bold", color = "black", fontSize = "18px")),
          min = xLim[1], max = xLim[2]
        ) |>
        hc_exporting(enabled = TRUE, filename = paste0("PlotHistograma_", str_to_title("XX"))) |>
        hc_credits(enabled = TRUE, text = "DNPE", href = "http://estadisticas.unal.edu.co/home/") |>
        hc_add_theme(ThemeHC)

      if (!(missingArg(estilo) || is.null(estilo$hc.Credits))) {
        PlotHistogram <- PlotHistogram |>
          hc_subtitle(text = estilo$hc.Credits, align = "left", style = list(color = "#2B908F", fontWeight = "bold"))
      }
    } else if (libreria == "plotly") {
      # _  _  _  _  _  _  _  _  _  _  _  _  _  _  _  _  _  _  _  _  _  _  _  _
      if (!(missingArg(estilo) || is.null(estilo$ply.Credits))) {
        ParmsCredits <- estilo$ply.Credits
      } else {
        ParmsCredits <- list(x = 0.11, y = 1.1, text = "")
      }
      if (!(missingArg(estilo) || is.null(estilo$ply.Density))) {
        ParmsDensity <- estilo$ply.Density
      } else {
        ParmsDensity <- list(color = "#F66B33", width = 4, dash = "line", opacity = 0.2)
      }

      FamilyTitle <- list(family = "Open Sans", size = 24, color = "#333333")
      Title <- list(text = paste0("<b>", titulo, "</b>"), font = FamilyTitle, y = 0.95)
      if (titulo == "") {
        Margen <- NULL
      } else { Margen <- list(l = 50, r = 50, t = 110, b = 0) }
      # _  _  _  _  _  _  _  _  _  _  _  _  _  _  _  _  _  _  _  _  _  _  _  _
      PlotHistogram <- plot_ly(
        x = datos |> select({{ variable }}) |> pull(),
        type = "histogram", name = "Histogram", nbinsx = bins,
        marker = list(color = color, opacity = 0.7) # xbins = list(size = 1)
      )

      if (density) {
        Density <- density(datos |> select({{ variable }}) |> pull())
        PlotHistogram <- PlotHistogram |>
          add_lines(
            x = Density$x, y = Density$y, yaxis = "y2",
            name = "Density", marker = NULL, line = ParmsDensity
          )
      }

      PlotHistogram <- PlotHistogram |>
        layout(
          title  = Title,
          xaxis  = list(title = labelX, zeroline = FALSE, range = xLim),
          yaxis  = list(title = labelY, zeroline = FALSE, range = yLim),
          yaxis2 = list(overlaying = "y", side = "right", rangemode = "tozero"),
          autosize = TRUE, margin = Margen,
          annotations = append(ParmsCredits, list(
            showarrow = FALSE, xref = "paper", yref = "paper",
            xanchor = "right", yanchor = "auto", xshift = 0, yshift = 0,
            font = list(size = 12, color = "#2B908F")
            )
          )
        ) |>
        config(locale = "es")
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
    # _  _  _  _  _  _  _  _  _  _  _  _  _  _  _  _  _  _  _  _  _  _  _  _
    if (!(missingArg(estilo) || is.null(estilo$gg.Hist))) {
      ParmsHist <- append(list(bins = bins), estilo$gg.Hist)
    } else {
      ParmsHist <- list(bins = bins, color = "#2E2E30", fill = color, alpha = 0.7)
    }
    if (!(missingArg(estilo) || is.null(estilo$gg.Density))) {
      ParmsDensity <- estilo$gg.Density
    } else {
      ParmsDensity <- list(color = "#FF444A", fill = "#F66B33", alpha = 0.2)
    }
    if (!(missingArg(estilo) || is.null(estilo$gg.Texto))) {
      ParmsLabs <- estilo$gg.Texto
    } else {
      ParmsLabs <- list(subtitle = NULL, caption = NULL, tag = NULL)
    }

    PlotHistogram <- ggplot(data = datos, aes(x = {{variable}}))
    if (density) {
      ParmsHist <- append(list(aes(y = after_stat(density))), ParmsHist)
      PlotHistogram <- PlotHistogram +
        do.call(geom_histogram, ParmsHist) + do.call(geom_density, ParmsDensity)
    } else {
      PlotHistogram <- PlotHistogram + do.call(geom_histogram, ParmsHist)
    }
    PlotHistogram <- PlotHistogram +
      labs(
        title = titulo, subtitle = ParmsLabs$subtitle, x = labelX, y = labelY,
        caption = ParmsLabs$caption, tag = ParmsLabs$tag
      ) +
      scale_x_continuous(limits = xLim) + scale_y_continuous(limits = yLim) +
      ThemeGG
  }

  return(PlotHistogram)
}
