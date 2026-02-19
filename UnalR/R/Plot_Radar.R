#' Cree un gráfico de radar dinámico y flexible con dos diferentes paquetes
#'
#' Esta función proporciona excelentes herramientas y opciones para la visualización
#' de un gráfico de radar (*también conocido como gráfico de araña*) dinámico con
#' el objetivo de observar datos multivariados de forma bidimensional. Dicho radar
#' chart o spider plot se puede representar usando dos diferentes librerías que
#' son `Plotly` y `ECharts`, las cuales usan internamente `JavaScript`.
#'
#' @inheritParams Plot.Series
#' @inheritParams Plot.Barras
#' @param datos Un data frame, se espera en formato de microdatos no un agregado.
#' @param variables Lista (*ya sea creada con la sintaxis `base` o `tidy`*)
#'   con las variables numéricas (*mínimo tres para que se pueda realizar el gráfico*)
#'   dentro del data frame ingresado en `datos`.
#' @param estadistico Cadena de caracteres que indica el estadístico a graficar.
#'   Los valores permitidos son `"Promedio"` (*valor predeterminado*), `"Mediana"`,
#'   `"Varianza"`, `"SD"`, `"CV"`, `"Min"` y `"Max`".
#' @param colores Cadena de caracteres indicando los colores con los cuales se
#'   deben colorear cada una de las trazas correspondiente a cada nivel del
#'   argumento `categoria`. Si no se introduce algún vector se usará la paleta
#'   `rainbow` por defecto.
#' @param rango Vector numérico de longitud dos que indica el valor mínimo y máximo,
#'   respectivamente. Si no conoce el dominio del estadístico seleccionado omita
#'   éste parámetro, pues internamente se usará `c(0, NaN)` como rango.
#' @param libreria Cadena de caracteres que indica el paquete con el cual se
#'   realizará el radar. Los valores permitidos son `"plotly"` (*valor predeterminado*)
#'   o `"echarts"`. Los valores se emparejarán parcialmente.
#' @param estilo Lista compuesta por varios parámetros, los cuales van a ser usados
#'   de acuerdo con la librería especificada para graficar el radar y cuyo objetivo
#'   es personalizar pequeños detalles de éste.
#'   * `ply.LegendTitle`, `ply.LegendPosition` y `ply.Credits`: Igual uso que en
#'     [Plot.Series()]
#'   * `ply.Relleno`: Cadena de caracteres indicando cómo se debe rellenar el área,
#'     `toself` (\emph{valor predeterminado}) conecta los puntos de la traza de
#'     forma cerrada y superpone las áreas, mientras que `tonext` deja visible la
#'     capa más profunda y si se comparten áreas no las superpone; finalmente
#'     especifique `none` si desea ver únicamente los polígonos y que no se rellene
#'     el área dentro de ellos.
#'   * `ply.Opacidad`: Un número entre \eqn{[0, 1]} que indica la opacidad de los
#'     polígonos/trazos.
#'   * `e.Credits`: Cadena de caracteres indicando el subtítulo del gráfico principal.
#'     Para mayor información, consulte la función [e_title()][echarts4r::e_title()].
#'   * `e.Forma`: Cadena de caracteres indicando el tipo de renderizado del radar,
#'     los valores admitidos son `polygon` (\emph{valor predeterminado}) y `circle`.
#'     Para mayor información, consulte la función [e_radar_opts()][echarts4r::e_radar_opts()].
#'   * `e.Tema`: Modifica el tema con el cual se creará el gráfico. Los posibles
#'     valores son un número entero entre \eqn{[1, 14]} el cual hace referencia
#'     a diferentes temas disponibles en dicha librería (`helianthus`, `azul`,
#'     `inspired`, `macarons`, `westeros`, `walden`, `roma`, `royal`, `fruit`,
#'     `dark`, `chalk`, `purple-passion`, `vintage` y `essos` respectivamente).
#'     El tema por defecto se logra al no ingresar valor alguno. Para más información
#'     consulte [aquí](https://echarts4r.john-coene.com/articles/themes.html).
#'   * `e.LegType`: Cadena de caracteres indicando el tipo de leyenda, los valores
#'     admitidos son `plain` (\emph{valor predeterminado}) y `scroll`
#'     (\emph{útil cuando es necesario mostrar demasiados elementos}). Para mayor
#'     información consulte la función [e_legend()][echarts4r::e_legend()].
#'   * `e.LegLoc`: Valor numérico o cadena de caracteres indicando la distancia
#'     entre la leyenda y el lado derecho del contenedor, puede ser expresado como
#'     un valor puntual o un valor porcentual relativo al ancho del contenedor.
#'   * `gg.Range`: Valor booleano opcional, si se especifica en `TRUE` el rango
#'     a tomar será simétrico, en el sentido en que se tomará el mínimo y máximo
#'     global de todas las variables, uno mismo para cada una de ellas. Diferente
#'     a si se omite el parámetro `rango`, pues acá el mínimo y máximo varía para
#'     cada variable.
#'   * `gg.plty`: Tipo de línea para los datos del gráfico. Para más detalles
#'     consulte la función [radarchart()][fmsb::radarchart()].
#'   * `gg.plwd`: Ancho de la línea para los datos del gráfico. Para más detalles
#'     consulte la función [radarchart()][fmsb::radarchart()].
#'   * `gg.cglwd`: Ancho de la línea para las grillas del radar. Para más detalles
#'     consulte la función [radarchart()][fmsb::radarchart()].
#'   * `gg.cglcol`: Color de la línea para las grillas del radar. Para más detalles
#'     consulte la función [radarchart()][fmsb::radarchart()].
#'
#' @returns
#' Retorna el radar (*objeto widget de HTML*) creado. La clase del objeto retornado
#' será un "htmlwidget" y dependiendo de la librería usada pertenecerá
#' adicionalmente a la clase "plotly" o "echarts4r".
#'
#' @examplesIf require("dplyr")
#' # library(dplyr)
#' Plot.Radar(
#'   datos     = ejSaberPro2020,
#'   categoria = TIPO_COL,
#'   variables = vars(PUNT_LECT_CRIT, PUNT_RAZO_CUANT, PUNT_INGLES),
#'   colores   = c("#2ACE82", "#FE2667", "#32E7C8", "#FF8D00"),
#'   rango     = c(0, NaN),
#'   estilo    = list(ply.Relleno = "tonext")
#' )
#' # ---------------------------------------------------------------------------
#' Plot.Radar(
#'   datos     = ejSaberPro2020,
#'   categoria = SEDE_NOMBRE_ADM,
#'   variables = vars(
#'     PUNTAJE_GLOBAL, PUNT_RAZO_CUANT, PUNT_INGLES,
#'     PUNT_LECT_CRIT, PUNT_COMP_CIUD, PUNT_COMU_ESCR
#'   ),
#'   rango     = c(0, NaN)
#' )
#' # ---------------------------------------------------------------------------
#' Plot.Radar(
#'   datos     = ejSaberPro2020,
#'   categoria = SEDE_NOMBRE_ADM,
#'   variables = vars(
#'     PUNTAJE_GLOBAL, PUNT_RAZO_CUANT, PUNT_INGLES,
#'     PUNT_LECT_CRIT, PUNT_COMP_CIUD, PUNT_COMU_ESCR
#'   ),
#'   rango     = c(0, NaN),
#'   libreria  = "echarts"
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
#'   "#8A381A" # GRIS        | Tumaco
#' )
#' Msj <- "Gr\u00e1fico de radar para representar los puntajes multivariados de la prueba Saber Pro."
#'
#' Plot.Radar(
#'   datos       = ejSaberPro2020,
#'   categoria   = SEDE_NOMBRE_ADM,
#'   variables   = vars(
#'     PUNTAJE_GLOBAL, PUNT_RAZO_CUANT, PUNT_INGLES,
#'     PUNT_LECT_CRIT, PUNT_COMP_CIUD, PUNT_COMU_ESCR
#'   ),
#'   estadistico = "SD",
#'   colores     = misColores,
#'   rango       = c(0, NaN),
#'   titulo      = "SPIDER PLOT",
#'   libreria    = "plotly",
#'   estilo      = list(
#'     ply.LegendTitle = "SEDE:", ply.LegendPosition = list(x = 0, y = -0.15, orientation = "h"),
#'     ply.Relleno = "tonext", ply.Opacidad = 0.8, ply.Credits = list(x = 0.8, y = -0.1, text = Msj)
#'   )
#' )
#' # ---------------------------------------------------------------------------
#' Plot.Radar(
#'   datos       = ejSaberPro2020,
#'   categoria   = SEDE_NOMBRE_ADM,
#'   variables   = vars(
#'     PUNTAJE_GLOBAL, PUNT_RAZO_CUANT, PUNT_INGLES,
#'     PUNT_LECT_CRIT, PUNT_COMP_CIUD, PUNT_COMU_ESCR
#'   ),
#'   estadistico = "CV",
#'   colores     = misColores,
#'   rango       = c(0, 0.25),
#'   titulo      = "RADAR CHART",
#'   libreria    = "echarts",
#'   estilo      = list(
#'     e.Credits = Msj, e.Forma = "circle", e.Tema = 10,
#'     e.LegType = "scroll", e.LegLoc = 0
#'   )
#' )
#' # ---------------------------------------------------------------------------
#' # Ejemplo usando el caso estático (fmsb)
#' Plot.Radar(
#'   datos     = ejSaberPro2020,
#'   categoria = TIPO_COL,
#'   variables = vars(
#'     PUNT_RAZO_CUANT, PUNT_INGLES, PUNT_LECT_CRIT, PUNT_COMP_CIUD, PUNT_COMU_ESCR
#'   ),
#'   estadistico = "SD",
#'   colores  = c("#89D8FF", "#9CFF86", "#FFA568", "#FF7F7F"),
#'   titulo   = "RADAR CHART DE LA DESVIACI\u00d3N EST\u00c1NDAR\nPOR COMPONENTE EVALUADO",
#'   # rango    = c(10, 40),
#'   estatico = TRUE,
#'   estilo   = list(
#'     gg.Range = TRUE, gg.plty = 5, gg.plwd = 4, gg.cglwd = 2, gg.cglcol = "#856AA1"
#'   )
#' )
#'
#' @inheritSection Plot.Series Lista de argumentos de estilo
#'
#' @export
#'
#' @import plotly
#' @import echarts4r
#' @import fmsb
#' @import dplyr
#' @importFrom tidyr pivot_wider pivot_longer
#' @importFrom scales alpha
#' @importFrom methods missingArg
#' @importFrom grDevices rainbow
#' @importFrom graphics legend par
Plot.Radar <- function(
    datos, categoria, variables,
    estadistico = c("Promedio", "Mediana", "Varianza", "SD", "CV", "Min", "Max"),
    colores, rango, ordinal = FALSE, titulo = "", libreria = c("plotly", "echarts"),
    estilo = NULL, estatico = FALSE) {

  # COMANDOS DE VERIFICACIÓN Y VALIDACIÓN
  if (missingArg(datos) || missingArg(categoria) || missingArg(variables)) {
    stop("\u00a1Por favor introduzca un conjunto de datos, una categor\u00eda y una lista de variables num\u00e9ricas con las cuales se graficar\u00e1!", call. = FALSE)
  }
  if (!all(is.list(variables), length(variables) >= 3)) {
    stop("\u00a1El par\u00e1metro 'variables' debe ser una lista y adem\u00e1s contener tres o m\u00e1s variables cuantitativas!", call. = FALSE)
  }
  if (missingArg(estadistico)) {
    warning("\u00a1Se usar\u00e1 como estad\u00edstico la media muestral ('mean') por defecto!", call. = FALSE)
  }
  Statistic <- match.arg(estadistico)
  Function  <- switch(
    Statistic,
    Promedio = mean,
    Mediana  = median,
    Varianza = var,
    SD       = sd,
    CV       = cv,
    Min      = min,
    Max      = max
  )

  if (missingArg(rango)) {
    rango <- c(0, NaN); MaxMin <- FALSE
  } else {
    if (!all(is.numeric(rango), length(rango) == 2)) {
      stop("\u00a1El par\u00e1metro 'rango' debe ser un vector num\u00e9rico de longitud 2!", call. = FALSE)
    }
    MaxMin <- ifelse(any(is.nan(rango)), FALSE, TRUE)
  }
  if (!all(is.logical(ordinal), is.logical(estatico))) {
    stop("\u00a1Los argumentos 'ordinal' y 'estatico' deben ser un booleano (TRUE o FALSE)!", call. = FALSE)
  }
  if (!(is.character(titulo))) {
    stop("\u00a1El argumento 'titulo' debe ser una cadena de texto!", call. = FALSE)
  }
  if (!estatico) {
    if (missingArg(libreria)) {
      warning("\u00a1Se usar\u00e1 la librer\u00eda 'plotly' por defecto para realizar el plot!", call. = FALSE)
      libreria <- "plotly"
    } else {
      libreria <- tolower(libreria)
      if (libreria %NotIN% c("plotly", "echarts")) {
        stop("\u00a1Por favor introduzca el nombre de una librer\u00eda valida (paquete usado para realizar la gr\u00e1fica)!", call. = FALSE)
      }
    }
  }
  LegendTitle <- ifelse(is.null(estilo$ply.LegendTitle), "", estilo$ply.LegendTitle)

  # CREACIÓN DEL DATAFRAME CON EL CUAL SE CREARÁ LA GRÁFICA
  Groups  <- datos |> select({{categoria}}, !!!variables) |>
    group_by({{ categoria }}, .drop = FALSE)
  N       <- Groups |> summarise("n" = n())
  df_Full <- Groups |> summarise_all(Function, na.rm = TRUE) |> left_join(N)
  df      <- df_Full |> select(!c({{ categoria }}, n))
  categorias <- colnames(df)

  if (ordinal) {
    Orden <- rev(seq_len(nrow(df))); OrdenLegend <- "reversed"
  } else {
    Orden <- seq_len(nrow(df)); OrdenLegend <- "normal"
  }

  if (!(missingArg(colores) || length(colores) == nrow(df_Full[, 1]))) {
    stop(paste0(
      "\u00a1El n\u00famero de colores ingresados en el vector 'colores' no corresponde con el n\u00famero de categor\u00edas a colorear!",
      "\n\tNo. colores ingresados = ", length(colores), " != ", "No. de categor\u00edas = ", nrow(df_Full[, 1])
      ), call. = FALSE
    )
  }

  # CREACIÓN DEL PLOT RETORNAR
  df_echarts <- df_Full |> select(-n) |>
    pivot_longer(cols = !{{ categoria }}, values_to = "Estadistico") |>
    pivot_wider(names_from = {{ categoria }}, values_from = Estadistico) |>
    mutate_if(is.numeric, ~round(., 3))

  if (!estatico) {
    if (libreria == "plotly") {
      if (missingArg(colores)) {
        colores <- rainbow(nrow(df_Full[, 1]), alpha = 0.2)
      }
      if (!(missingArg(estilo) || is.null(estilo$ply.LegendPosition))) {
        ParmsLegend <- estilo$ply.LegendPosition
      } else {
        ParmsLegend <- list(x = 1, y = 0.5, orientation = "v")
      }
      if (!(missingArg(estilo) || is.null(estilo$ply.Credits))) {
        ParmsCredits <- estilo$ply.Credits
      } else {
        ParmsCredits <- list(x = 0, y = 0, text = "")
      }

      Relleno  <- ifelse(!(missingArg(estilo) || is.null(estilo$ply.Relleno)), estilo$ply.Relleno, "toself")
      Opacidad <- ifelse(!(missingArg(estilo) || is.null(estilo$ply.Opacidad)), estilo$ply.Opacidad, 1)

      PlotRadar <- plot_ly(type = "scatterpolar", fill = Relleno, mode = "markers+lines")
      for (i in Orden) {
        PlotRadar <- add_trace(
          PlotRadar,
          r = matrix(df[i, ]), theta = categorias, name = df_Full[i, 1],
          line = list(color = colores[i], width = 2),
          marker = list(color = colores[i], size = 6, line = list(width = 1, color = "#787878")),
          fillcolor = list(color = colores[i]), opacity = Opacidad,
          hoverinfo = "text",
          text = paste0(df_Full[i, 1], "<br> ", Statistic, ": ", round(df[i, ], 3), "<br> N: ", df_Full[i, ncol(df_Full)])
        )
      }

      # Arial | Open Sans | Courier New, monospace, Old Standard TT
      FamilyTitle  <- list(family = "Old Standard TT", size = 24, color = "#333333")
      FamilyLegend <- list(family = "Open Sans", size = 14, color = "#525252")
      Title  <- list(text = paste0("<b>", titulo, "</b>"), font = FamilyTitle, y = 0.995)
      Legend <- list(text = paste0("<b>", LegendTitle, "</b>"), font = FamilyLegend)

      PlotRadar <- PlotRadar |>
        layout(
          title = Title, autosize = TRUE, showlegend = TRUE,
          polar = list(radialaxis = list(visible = TRUE, range = rango)),
          legend = append(ParmsLegend, list(title = Legend, traceorder = OrdenLegend)),
          annotations = append(ParmsCredits, list(
            showarrow = FALSE, xref = "paper", yref = "paper",
            xanchor = "right", yanchor = "auto", xshift = 0, yshift = 0,
            font = list(size = 12, color = "#CCCCCC")
            )
          )
        ) |>
        config(locale = "es")
    } else if (libreria == "echarts") {
      if (!(missingArg(estilo) || is.null(estilo$e.Tema))) {
        Theme <- switch(
          estilo$e.Tema,
          "1"  = "helianthus",
          "2"  = "azul",
          "3"  = "inspired",
          "4"  = "macarons",
          "5"  = "westeros",
          "6"  = "walden",
          "7"  = "roma",
          "8"  = "royal",
          "9"  = "fruit",
          "10" = "dark",
          "11" = "chalk",
          "12" = "purple-passion",
          "13" = "vintage",
          "14" = "essos"
        )
        Flag <- TRUE
      } else { Flag <- FALSE }

      Subtitle <- ifelse(!(missingArg(estilo) || is.null(estilo$e.Credits)), estilo$e.Credits, "")
      Forma    <- ifelse(!(missingArg(estilo) || is.null(estilo$e.Forma))  , estilo$e.Forma  , "polygon")
      LegType  <- ifelse(!(missingArg(estilo) || is.null(estilo$e.LegType)), estilo$e.LegType, "plain")
      if (!(missingArg(estilo) || is.null(estilo$e.LegLoc))) {
        LegLoc <- estilo$e.LegLoc
      } else { LegLoc <- NULL }

      Columns <- colnames(df_echarts)[-1]
      Maximo  <- ifelse(is.nan(rango[2]), max(df_echarts[, -1]), rango[2])
      # https://echarts4r.john-coene.com/articles/get_started.html
      PlotRadar <- df_echarts |> e_charts_("name")
      for (i in seq_len(length(Columns))) {
        PlotRadar <- PlotRadar |>
          e_radar_(serie = Columns[i], max = Maximo, name = Columns[i], legend = TRUE)
      }

      PlotRadar <- PlotRadar |>
        e_radar_opts(shape = Forma) |>
        e_tooltip(trigger = "item") |>
        e_title(text = titulo, subtext = Subtitle) |>
        e_legend(right = LegLoc, type = LegType)

      if (Flag) { PlotRadar <- PlotRadar |> e_theme(Theme) }
      if (!missingArg(colores)) { PlotRadar <- PlotRadar |> e_color(colores) }
    }
    return(PlotRadar)
  } else {

    plty <- ifelse(
      !(missingArg(estilo) || is.null(estilo$gg.plty)), estilo$gg.plty, 1
    )
    plwd <- ifelse(
      !(missingArg(estilo) || is.null(estilo$gg.plwd)), estilo$gg.plwd, 2
    )
    cglwd <- ifelse(
      !(missingArg(estilo) || is.null(estilo$gg.cglwd)), estilo$gg.cglwd, 1
    )
    cglcol <- ifelse(
      !(missingArg(estilo) || is.null(estilo$gg.cglcol)), estilo$gg.cglcol, "#AAAAAA"
    )

    dfSpecial <- as.data.frame(df_echarts)
    rownames(dfSpecial) <- dfSpecial$name
    dfSpecial$name <- NULL
    dfFinal        <- as.data.frame(t(dfSpecial))
    namesLegend    <- rownames(dfFinal)

    if (!any(missingArg(estilo), is.null(estilo$gg.Range)) && estilo$gg.Range) {
      MaxMin    <- TRUE
      MaxGlobal <- round(max(dfFinal), 2)
      MinGlobal <- round(min(dfFinal), 2)
      dfFinal   <- addMaxMin(dfFinal, MinGlobal, MaxGlobal)
      namesLegend <- rownames(dfFinal[-c(1,2),])
    } else {
      if (!(missingArg(rango) || any(is.nan(rango)))) {
        dfFinal <- addMaxMin(dfFinal, rango[1], rango[2])
        namesLegend <- rownames(dfFinal[-c(1,2),])
      }
    }

    {
      parDefault <- par(no.readonly = TRUE)
      on.exit(par(parDefault))
      op <- par(mar = c(1, 1, 2.2, 1))
      radarchart(
        dfFinal, maxmin = MaxMin, title = titulo, pcol = colores,
        plty = plty, plwd = plwd, pfcol = scales::alpha(colores, 0.2),
        cglty = 1, cglwd = cglwd, cglcol = cglcol,
        axistype = 2, axislabcol = "#202020", vlcex = 0.8
      )
      legend(
        x = "bottom", horiz = TRUE, legend = namesLegend, bty = "n", pch = 20,
        col = colores, cex = 0.9, text.col = "black", pt.cex = 3
      )
      par(parDefault)
    }
    return(invisible(NULL))
  }
}
