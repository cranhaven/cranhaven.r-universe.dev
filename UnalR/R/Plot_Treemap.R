#' Cree un diagrama de árbol (*treemap*) dinámico y flexible con diversos paquetes
#'
#' Esta función proporciona excelentes herramientas y opciones para la visualización
#' de datos jerárquicos/estructurados como un conjunto de rectángulos anidados.
#' Cada grupo está representado por un rectángulo, cuya área (*tamaño*) es
#' proporcional a su frecuencia absoluta (*recuento*) y el color se usa para
#' mostrar otra dimensión numérica. Usando la interactividad, es posible representar
#' varias dimensiones/niveles: grupos, subgrupos, etc. Dicha gráfica se va a
#' representar usando la librería `Highcharter`, `Plotly`, `d3treeR`, entre otras,
#' las cuales usan internamente `JavaScript`.
#'
#' @inheritParams Plot.Series
#' @param datos Un data frame, no un vector numérico.
#' @param variables Una lista (*ya sea creada con la sintaxis `base` o `tidy`*)
#'   con las variables categóricas dentro del data frame ingresado en `datos` con
#'   las que se desea crear la jerarquía (*recuerde que esta se crea de izquierda
#'   a derecha, es decir la primera hace referencia al grupo, la segunda al subgrupo,
#'   etc.*).
#' @param atributo Una variable numérica dentro del data frame ingresado en `datos`.
#'   Este es opcional y solo aplica en el caso de un nivel, es decir, cuando se
#'   ingresa únicamente una variable.
#' @param textFreq Cadena de caracteres indicando el nombre que se le va a dar al
#'   recuento en cada uno de los grupos. Por defecto se emplea el rótulo "N".
#' @param metodo Cadena de caracteres indicando el diseño con el cual se realizará
#'   el gráfico (*en el caso de ingresar dos niveles o más*). Los valores permitidos
#'   son `"Classic"` (*valor predeterminado*), `"Classic2"`, `"Sunburst"` y
#'   `"Sunburst2"`, así se usará las funciones d3treeR::d3tree(), d3treeR::d3tree2(),
#'   [sunburst()][sunburstR::sunburst()] y [sund2b()][sunburstR::sund2b()]
#'   respectivamente.
#' @param estadistico Igual uso que en [Plot.Mapa()]
#' @param colores Igual uso que en [Plot.Series()], con algunos matices, cuando
#'   usamos `Highcharter` y nos encontramos en el caso de un nivel y especificamos
#'   un atributo es recomendable pasarle una escala de colores, pues con esto se
#'   construirá la barra horizontal. En el caso de usar el argumento `atributo`
#'   se puede ingresar el nombre de una paleta, por ejemplo "Set1".
#' @param libreria Igual uso que en [Plot.Torta()], con algunos matices, pues
#'   en el caso de ingresar más de una variable categórica se omitirá dicho
#'   argumento, ya que `metodo` tomará su lugar.
#' @param estilo Lista compuesta por varios parámetros, los cuales van a ser usados
#'   para graficar el treemap y cuyo objetivo es personalizar pequeños detalles
#'   de éste.
#'   * `hc.Tema` y `hc.Credits`: Igual uso que en [Plot.Series()]
#'   * `hc.borderRadius`: Un número entero positivo que indica el radio del borde
#'     de cada elemento. El valor por defecto es 0 (\emph{rectángulos}).
#'   * `ply.Opacidad`: Igual uso que en [Plot.Radar()]
#'   * `ply.Credits`: Igual uso que en [Plot.Series()]
#'   * `sun.Explanation`: Cadena de caracteres indicando qué es lo que se desea
#'     ver en el centro del sunburst al pasar el mouse por los diferentes anillos
#'     (\emph{niveles de la jerarquía}). Los valores permitidos son "All"
#'     (\emph{valor predeterminado}), "Count" y "Percent". Solo aplica para cuando
#'     `metodo = "Sunburst"`.
#'   * `sun.Color`: A diferencia del argumento `colores` acá puede pasar una paleta
#'     o vector de colores sin que se recorte (\emph{más no se recicle}) éste a
#'     la longitud de categorías del nodo padre. Su uso reemplaza el funcionamiento
#'     del argumento `colores`. Solo aplica para cuando `metodo = "Sunburst"`.
#'   * `sun.showLabels`: Si es `FALSE` (\emph{valor predeterminado}) no se mostrará
#'     etiquetas en los cortes. Solo aplica para cuando `metodo = "Sunburst2"`.
#'   * `sun.colorRoot`: Cadena de caracteres que indica el color del nodo raíz
#'     (\emph{root}). Puede indicar el color con el nombre (`"red"`), código
#'     hexadecimal (`"#FF0000"`) o RGB (`rgb(1, 0, 0)`). El valor por defecto es
#'     "rojo". Solo aplica para cuando `metodo = "Sunburst2"`.
#'   * `gg.fontsize.title`: Tamaño de la fuente del título. El valor por defecto
#'     es `14`. Para más detalles, consulte la función [treemap()][treemap::treemap()].
#'   * `gg.fontsize.labels`: Tamaño de la fuente de las etiquetas. Si ingresa un
#'     número especificará el tamaño para todos los niveles de agregación, por el
#'     contrario, si ingresa un vector podrá especificar el tamaño para cada nivel.
#'     El valor por defecto es `11`. Para más detalles, consulte la función [treemap()][treemap::treemap()].
#'   * `gg.fontcolor.labels`: Especifica los colores de la etiqueta. Ya sea una
#'     cadena de caracteres o un vector (*uno para cada nivel de agregación*).
#'     El valor por defecto es `NULL`. Para más detalles, consulte la función [treemap()][treemap::treemap()].
#'   * `gg.border.lwds`: Tamaño de las líneas de borde. Si ingresa un número
#'     especificará el grosor para todos los rectángulos, o un vector para
#'     especificar el grueso para cada nivel de agregación. Para más detalles,
#'     consulte la función [treemap()][treemap::treemap()].
#'   * `gg.border.col`: Color de los bordes dibujados alrededor de cada rectángulo,
#'     ya sea un valor único o un vector. El valor por defecto es `'#000000'`.
#'     Para más detalles, consulte la función [treemap()][treemap::treemap()].
#'   * `gg.lowerbound.cex.labels`: Número entre \eqn{[0, 1]}, 0 significa dibujar
#'     todas las etiquetas y 1 significa dibujar sólo las etiquetas si encajan
#'     (*considerando el `fontsize.labels`*). El valor por defecto es `0.4`.
#'     Para más detalles, consulte la función [treemap()][treemap::treemap()].
#'   * `gg.force.print.labels`: Si es `FALSE` (*valor predeterminado*) las
#'     etiquetas de datos no se ven obligadas a imprimirse si no encajan. Para
#'     más detalles consulte la función [treemap()][treemap::treemap()].
#'   * `gg.overlap.labels`: Número entre \eqn{[0, 1]}, que determina la tolerancia
#'     de superposición entre etiquetas. 0 significa que las etiquetas de los
#'     niveles inferiores no se imprimen si las etiquetas de los niveles superiores
#'     se superponen, 1 significa que las etiquetas siempre se imprimen. El valor
#'     por defecto es `0.5`. Para más detalles, consulte la función [treemap()][treemap::treemap()].
#'
#' @details
#' Si está trabajando en un `R Markdown` o un aplicativo `Shiny` no se puede usar
#' de forma conjunta el `método = Classic` (o `Classic2`) y `método = Sunburst`
#' (o `Sunburst2`), pues se trata de un problema interno, ya que usan versiones
#' diferentes de `d3`, puede darle seguimiento al problema
#' [aquí](https://github.com/timelyportfolio/sunburstR/issues/102). De igual forma,
#' si utiliza la librería `sunburstR` en algunas ocasiones se le verán afectadas
#' las tablas creadas con `DT`.
#'
#' @returns
#' Retorna el treemap (*objeto widget de HTML*) creado. La clase del objeto
#' retornado será un "htmlwidget" y dependiendo de la librería usada pertenecerá
#' adicionalmente a la clase "highchart", "plotly", " d3tree", "d3tree2", "sunburst"
#' o "sund2b".
#'
#' @examplesIf require("viridis")
#' library(viridis)
#' Msj <- "Acompa\u00f1ado del Estad\u00edstico seleccionado para la Variable Edad."
#' Plot.Treemap(
#'   datos       = ejGraduados,
#'   variables   = SEDE_NOMBRE_MAT,
#'   atributo    = EDAD_MOD,
#'   textFreq    = "Tamaño de la Muestra",
#'   estadistico = "Max",
#'   colores     = inferno(10),
#'   titulo      = "TOTAL DE GRADUADOS POR SEDE DE LA UNIVERSIDAD NACIONAL",
#'   libreria    = "highcharter",
#'   estilo      = list(hc.Tema = 7, hc.borderRadius = 20, hc.Credits = Msj)
#' )
#' # ---------------------------------------------------------------------------
#' Plot.Treemap(
#'   datos       = ejGraduados,
#'   variables   = FACULTAD,
#'   atributo    = EDAD_MOD,
#'   textFreq    = "n",
#'   estadistico = "CV",
#'   colores     = turbo(10, direction = -1),
#'   titulo      = "TOTAL DE GRADUADOS POR FACULTAD EN LA UNAL",
#'   libreria    = "plotly",
#'   estilo      = list(ply.Credits = list(x = 0.6, y = 1, text = Msj))
#' )
#'
#' @examplesIf all(FALSE)
#' # ---------------------------------------------------------------------------
#' # library(dplyr)
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
#' Plot.Treemap(
#'   datos     = ejGraduados,
#'   variables = vars(SEDE_NOMBRE_MAT, FACULTAD, PROGRAMA),
#'   metodo    = "Classic",
#'   colores   = misColores # "Set3"
#' )
#' Plot.Treemap(
#'   datos     = ejGraduados,
#'   variables = vars(SEDE_NOMBRE_MAT, FACULTAD, PROGRAMA),
#'   metodo    = "Classic2",
#'   colores   = "Set2"
#' )
#' Plot.Treemap(
#'   datos     = ejGraduados,
#'   variables = vars(SEDE_NOMBRE_MAT, FACULTAD, PROGRAMA),
#'   metodo    = "Sunburst",
#'   colores   = misColores,
#'   estilo    = list(sun.Explanation = "All")
#' )
#' Plot.Treemap(
#'   datos     = ejGraduados,
#'   variables = vars(SEDE_NOMBRE_MAT, FACULTAD, PROGRAMA),
#'   metodo    = "Sunburst",
#'   # colores   = misColores,
#'   estilo    = list(
#'     sun.Explanation = "All",
#'     sun.Color = list(range = c("#9E0142", "#D53E4F", "#F46D43", "#FDAE61",
#'                                "#FEE08B", "#FFFFBF", "#E6F598", "#ABDDA4",
#'                                "#66C2A5", "#3288BD", "#5E4FA2"
#'                                )
#'                      )
#'   )
#' )
#' Plot.Treemap(
#'   datos     = ejGraduados,
#'   variables = vars(SEDE_NOMBRE_MAT, FACULTAD, PROGRAMA),
#'   metodo    = "Sunburst2"
#' )
#' Plot.Treemap(
#'   datos     = ejGraduados,
#'   variables = vars(SEDE_NOMBRE_MAT, FACULTAD, PROGRAMA),
#'   metodo    = "Sunburst2",
#'   colores   = misColores,
#'   estilo    = list(sun.showLabels = TRUE, sun.colorRoot = "#EF0055")
#' )
#'
#' @examplesIf require("dplyr")
#' # ---------------------------------------------------------------------------
#' # Ejemplo usando el caso estático (treemap)
#' # library(dplyr)
#' Plot.Treemap(
#'   datos     = ejGraduados,
#'   variables = vars(SEDE_NOMBRE_MAT, FACULTAD),
#'   colores   = c("#FF3232", "#AFFF5E", "#FD6DB3", "#4CCAF2", "#FF9248", "#FBB03B"),
#'   titulo    = "TOTAL DE GRADUADOS \u00d7 SEDE",
#'   estatico  = TRUE,
#'   estilo    = list(
#'     gg.fontsize.title = 12, gg.fontsize.labels = c(15, 9),
#'     gg.fontcolor.labels = c("#FFFFFF", "#212020"),
#'     gg.border.lwds = c(4, 2), gg.border.col = c("#73095D", "#D60D4B"),
#'     gg.lowerbound.cex.labels = 0.3, gg.overlap.labels = 0.1
#'   )
#' )
#'
#' @inheritSection Plot.Series Lista de argumentos de estilo
#'
#' @export
#'
#' @import plotly
#' @import treemap
#' @import d3r
#' @import sunburstR
#' @import dplyr
#' @importFrom utils head
#' @importFrom methods missingArg
#' @importFrom grDevices rainbow
Plot.Treemap <- function(
    datos, variables, atributo, textFreq = "N",
    metodo = c("Classic", "Classic2", "Sunburst", "Sunburst2"),
    estadistico = c("Promedio", "Mediana", "Varianza", "SD", "CV", "Min", "Max"),
    colores, titulo = "", libreria = c("highcharter", "plotly"), estilo = NULL,
    estatico = FALSE) {

  if (missingArg(datos) || missingArg(variables)) {
    stop("\u00a1Por favor introduzca un conjunto de datos y una(s) variable(s) cualitativas con las cuales se crear\u00e1 la jerarqu\u00eda!", call. = FALSE)
  }
  if (!all(is.character(textFreq), is.character(titulo))) {
    stop("\u00a1El argumento 'textFreq' y 'titulo' deben ser una cadena de texto!", call. = FALSE)
  }
  if (length(rlang::quo_get_expr(enquo(variables))) <= 2) {
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
  Percentage <- function(x) { round(x / sum(x, na.rm = TRUE) * 100, 3) }

  Method    <- match.arg(metodo)
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

  Opacidad <- ifelse(!(missingArg(estilo) || is.null(estilo$ply.Opacidad)), estilo$ply.Opacidad, 1)

  # CREACIÓN DEL DATAFRAME CON EL CUAL SE CREARÁ LA GRÁFICA

  if (!estatico) {
    if (length(rlang::quo_get_expr(enquo(variables))) <= 2) {
      if (!missingArg(atributo)) {
        if (missingArg(estadistico)) {
          warning("\u00a1Se usar\u00e1 como estad\u00edstico la media muestral ('mean') por defecto!", call. = FALSE)
        }
        Caso <- "I"

        x <- rlang::quo_name(enquo(variables))
        y <- rlang::quo_name(enquo(atributo))

        Groups <- datos |> select({{ atributo }}, {{ variables }}) |>
          group_by({{ variables }}, .drop = FALSE)
        N  <- Groups |> summarise("n" = n())
        df <- Groups |>
          summarise_all(Function, na.rm = TRUE) |> left_join(N, by = x) |>
          rename_at(all_of(c(x, y)), ~ c("X", "Y")) |>
          mutate(Porcentaje = Percentage(n))

        if (missingArg(colores)) { colores <- rainbow(nrow(df), alpha = 0.6) }
        # -.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.
        TreemapHC <- df |>
          hchart("treemap", hcaes(x = X, value = n, color = Y), name = textFreq,
                 dataLabels = list(enabled = TRUE, format = "{point.name}<br/>{point.Porcentaje: .1f}%")
          ) |>
          hc_tooltip(pointFormat = paste0("{series.name}: {point.n} <br> <b>", Statistic, ": {point.Y: .2f}</b>"), useHTML = TRUE)
        # https://plotly.com/r/reference/treemap/#treemap-legendrank
        TreemapPy <- df %>%
          plot_ly(
            type = "treemap", name = "", parents = NA, values = ~n, labels = ~X,
            text = ~Y, opacity = Opacidad, textposition = "middle center",
            texttemplate = paste0(
              "<b>%{label}</b>\n ", textFreq, ": %{value} (<i>%{percentParent}</i>)\n",
              Statistic, ": %{text: .2f}"
            ),
            hovertemplate = "%{label}", marker = list(colors = colores), sort = TRUE
          )
      } else {
        df <- datos |> count({{ variables }}, sort = TRUE) |>
          mutate(Porcentaje = Percentage(n), X := {{ variables }}) |> arrange(X)
        if (missingArg(colores)) { colores <- rainbow(nrow(df), alpha = 0.6) }
        Caso <- "II"
        # -.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.
        TreemapHC <- df |>
          hchart("treemap", hcaes(x = X, value = n, color = n), name = textFreq,
                 dataLabels = list(enabled = TRUE, format = "{point.name}<br/>{point.Porcentaje: .1f}%")
          )
        TreemapPy <- df %>%
          plot_ly(
            type = "treemap", name = "", parents = NA, values = ~n, labels = ~X,
            opacity = Opacidad, textposition = "middle center",
            texttemplate = "<b>%{label}</b>\n ", textFreq, ": %{value} (<i>%{percentParent}</i>)\n",
            hovertemplate = "%{label}: %{value}", marker = list(colors = colores), sort = TRUE
          )
      }

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
        Borde <- ifelse(!(missingArg(estilo) || is.null(estilo$hc.borderRadius)), estilo$hc.borderRadius, 0)

        PlotTreemap <- TreemapHC
        if (Caso == "I") {
          PlotTreemap <- PlotTreemap |>
            hc_colorAxis(stops = color_stops(colors = colores)) |>
            hc_plotOptions(treemap = list(borderRadius = Borde))
        } else {
          PlotTreemap <- PlotTreemap |>
            hc_colorAxis(stops = color_stops(colors = colores)) |>
            hc_plotOptions(treemap = list(colorByPoint = TRUE, colors = colores, borderRadius = Borde))
        }

        PlotTreemap <- PlotTreemap |>
          hc_title(text = titulo, style = list(
            fontWeight = "bold", fontSize = "22px", color = "#333333", useHTML = TRUE
            )
          ) |>
          hc_exporting(enabled = TRUE, filename = paste0("PlotTreemap_", as_label(enquo(variables)))) |>
          hc_credits(enabled = TRUE, text = "DNPE", href = "http://estadisticas.unal.edu.co/home/") |>
          hc_add_theme(ThemeHC)

        if (!(missingArg(estilo) || is.null(estilo$hc.Credits))) {
          PlotTreemap <- PlotTreemap |>
            hc_subtitle(text = estilo$hc.Credits, align = "left", style = list(color = "#2B908F", fontWeight = "bold"))
        }
      } else if (libreria == "plotly") {
        if (!(missingArg(estilo) || is.null(estilo$ply.Credits))) {
          ParmsCredits <- estilo$ply.Credits
        } else { ParmsCredits <- list(x = 0, y = 0, text = "") }

        # Arial | Open Sans | Courier New, monospace, Old Standard TT
        FamilyTitle <- list(family = "Old Standard TT", size = 24, color = "#333333")
        Title <- list(text = paste0("<b>", titulo, "</b>"), font = FamilyTitle, y = 0.995)

        PlotTreemap <- TreemapPy |>
          layout(
            title = Title, autosize = TRUE, showlegend = TRUE,
            annotations = append(ParmsCredits, list(
              showarrow = FALSE, xref = "paper", yref = "paper",
              xanchor = "right", yanchor = "auto", xshift = 0, yshift = 0,
              font = list(size = 12, color = "#CCCCCC")
              )
            )
          ) |>
          config(locale = "es")
      }
    } else {
      df <- datos |> count(!!!variables, sort = TRUE)
      if (missingArg(colores)) {
        colores <- rainbow(nrow(unique(df[, 1])), alpha = 0.6, rev = TRUE)
      }

      Figure <- df |>
        treemap::treemap(index = head(names(df), -1), vSize = "n", type = "index", palette = colores, draw = FALSE)
      grDevices::dev.off()

      if (Method %in% c("Classic", "Classic2")) {
        switch(
          Method,
          Classic = {
            PlotTreemap <- d3tree(Figure, rootname = "General", width = "100%")
          },
          Classic2 = {
            PlotTreemap <- d3tree2(Figure, rootname = "General", width = "100%")
          }
        )
      } else {
        tmNest <- d3r::d3_nest(data = Figure$tm, value_cols = colnames(Figure$tm)[-(seq_len(length(variables)))])
        if (Method == "Sunburst") {
          if (!(missingArg(estilo) || is.null(estilo$sun.Color))) {
            Colores <- estilo$sun.Color
          } else {
            Colores <- htmlwidgets::JS("function(d){return d3.select(this).datum().data.color;}")
          }
          Dots <- list(
            data = tmNest, colors = Colores, valueField = "vSize",
            legend = FALSE, sumNodes = FALSE, withD3 = TRUE, width = "100%"
          )
          if (!(missingArg(estilo) || is.null(estilo$sun.Explanation))) {
            switch(
              estilo$sun.Explanation,
              Percent = { Dots <- append(Dots, list(percent = TRUE)) },
              Count   = { Dots <- append(Dots, list(percent = FALSE, count = TRUE)) },
              All = {
                Text <- "function(d) {
                          return d.data.name + '<br>' + d.value + ' (' + Math.round(d.value/this*1000)/10 + '%)'
                  }"
                Dots <- append(Dots, list(explanation = Text))
              }
            )
          }
          PlotTreemap <- do.call(sunburst, Dots)
        } else {
          textNode <- ifelse(!(missingArg(estilo) || is.null(estilo$sun.showLabels)), estilo$sun.showLabels, FALSE)
          colRoot  <- ifelse(!(missingArg(estilo) || is.null(estilo$sun.colorRoot)), estilo$sun.colorRoot, "#DD2626")

          Colores <- htmlwidgets::JS(paste0("function(name, d){return d.color || '", colRoot, "';}"))
          Text <- "function(nodedata, size, percent) {
                   return '<span style=\"font-weight: bold;\">' + nodedata.name + ':</span>' + ' ' + size
                }"
          PlotTreemap <- sunburstR::sund2b(
            data = tmNest, colors = Colores, valueField = "vSize",
            tooltip = sunburstR::sund2bTooltip(html = htmlwidgets::JS(Text)),
            showLabels = textNode, width = "100%"
          )
        }
      }
    }
  } else {
    df <- datos |> count(!!!variables, sort = TRUE)
    if (missingArg(colores)) {
      colores <- rainbow(nrow(unique(df[, 1])), alpha = 0.6, rev = TRUE)
    }

    fontsize.title  <- ifelse(
      missingArg(estilo) || is.null(estilo$gg.fontsize.title),
      14, estilo$gg.fontsize.title
    )
    if (missingArg(estilo) || is.null(estilo$gg.fontsize.labels)) {
      fontsize.labels <- 11
    } else { fontsize.labels <- estilo$gg.fontsize.labels }
    if (missingArg(estilo) || is.null(estilo$gg.fontcolor.labels)) {
      fontcolor.labels <- NULL
    } else { fontcolor.labels <- estilo$gg.fontcolor.labels }
    if (missingArg(estilo) || is.null(estilo$gg.border.lwds)) {
      border.lwds <- c(3, 2)
    } else { border.lwds <- estilo$gg.border.lwds }
    if (missingArg(estilo) || is.null(estilo$gg.border.col)) {
      border.col <- "#000000"
    } else { border.col <- estilo$gg.border.col }
    lowerbound.cex.labels <- ifelse(
      missingArg(estilo) || is.null(estilo$gg.lowerbound.cex.labels),
      0.4, estilo$gg.lowerbound.cex.labels
    )
    force.print.labels    <- ifelse(
      missingArg(estilo) || is.null(estilo$gg.force.print.labels),
      FALSE, estilo$gg.force.print.labels
    )
    overlap.labels <- ifelse(
      missingArg(estilo) || is.null(estilo$gg.overlap.labels),
      0.5, estilo$gg.overlap.labels
    )

    PlotTreemap <- treemap::treemap(
      df, index = head(names(df), -1), vSize = "n", type = "index",
      palette = colores, title = titulo, fontsize.title = fontsize.title,
      fontsize.labels = fontsize.labels, fontcolor.labels = fontcolor.labels,
      border.lwds = border.lwds, border.col = border.col,
      lowerbound.cex.labels = lowerbound.cex.labels, inflate.labels = FALSE,
      bg.labels = 200, force.print.labels = force.print.labels,
      overlap.labels = overlap.labels, draw = TRUE
    )
  }

  if (!estatico) { return(PlotTreemap) } else { return(invisible(PlotTreemap)) }
}
