#' Cree un gráfico circular/torta/pie dinámico/estático y flexible con dos
#' diferentes paquetes
#'
#' Esta función permite mostrar de forma interactiva (*y estática*) una descripción
#' compacta y general de una variable con sus respectivas categorías. Dicho diagrama
#' se puede representar usando dos diferentes librerías que son `Highcharter` y
#' `Plotly`, las cuales usan internamente `JavaScript`.
#'
#' @inheritParams Plot.Series
#' @param datos Un data frame, no un vector numérico.
#' @param ano Argument deprecated. This Argument still exist but will be removed
#'   in the next version.
#' @param periodo Argument deprecated. This Argument still exist but will be removed
#'   in the next version.
#' @param label Cadena de caracteres indicando la etiqueta a la que hace referencia
#'   el plot.
#' @param addPeriodo Argument deprecated. This Argument still exist but will be
#'   removed in the next version.
#' @param libreria Cadena de caracteres que indica el paquete con el cual se realizará
#'   el plot. Los valores permitidos son `"highcharter"` (*valor predeterminado*)
#'   y `"plotly"`. Los valores se emparejarán parcialmente.
#' @param estilo Lista compuesta por varios parámetros, los cuales van a ser usados
#'   de acuerdo con la librería especificada para graficar la torta y cuyo objetivo
#'   es personalizar pequeños detalles de ésta.
#'   * `LegendTitle`, `hc.Tema`, `hc.Credits` y `ply.Credits`: Igual uso que en
#'     [Plot.Series()]
#'   * `ply.Legend`: Igual uso que en [Plot.Barras()]
#'
#' @details
#' Al usar el paquete `Highcharter` y usar las opciones de descarga, el nombre del
#' archivo descargado será la concatenación del plot graficado y la categoría usada,
#' así, por ejemplo, si se graficó el diagrama de pie para la categoría "Sexo" el
#' nombre será `PlotTorta_Sexo.png`.
#'
#' @note
#' Los gráficos circulares son una forma muy mala de mostrar información. El ojo
#' es bueno para juzgar medidas lineales y malo para juzgar áreas relativas. Un
#' gráfico de barras o un gráfico de puntos es una forma preferible de mostrar
#' este tipo de datos.
#'
#' @inheritSection Plot.Series Lista de argumentos de estilo
#'
#' @returns
#' Retorna el diagrama circular (*objeto widget de HTML*) creado. La clase del
#' objeto retornado será un "htmlwidget" y dependiendo de la librería usada
#' pertenecerá adicionalmente a la clase "highchart" o "plotly".
#'
#' @examplesIf all(require("tibble"), require("dplyr"))
#' # Ejemplo generalizado (sin uso de un consolidado como input)
#' # library("tibble"); library("dplyr")
#' set.seed(42)
#' Blood <- tibble(
#'   Group = sample(c("O", "A", "B", "AB"), size = 200, prob = c(0.5, 0.3, 0.16, 0.4), replace = TRUE),
#'   Prevalence = round(runif(200)*100)
#' )
#' Plot.Torta(
#'   datos     = Blood     ,
#'   valores   = Prevalence,
#'   categoria = Group     ,
#'   colores   = c("#FF553D", "#A5FF67", "#40D2FF", "#FFDB5C"),
#'   label     = "No. of Prevalence"
#' )
#' Plot.Torta(
#'   datos     = Blood     ,
#'   valores   = Prevalence,
#'   categoria = Group     ,
#'   colores   = c("#FF553D", "#A5FF67", "#40D2FF", "#FFDB5C"),
#'   titulo    = "DISTRIBUTION OF BLOOD GROUPS",
#'   estatico  = TRUE,
#'   estilo    = list(gg.Tema = 6)
#' )
#' Plot.Torta(
#'   datos     = Blood     ,
#'   valores   = Prevalence,
#'   categoria = Group     ,
#'   colores   = c("#FF553D", "#A5FF67", "#40D2FF", "#FFDB5C"),
#'   titulo    = "DISTRIBUTION OF BLOOD GROUPS",
#'   estatico  = TRUE,
#'   estilo = list(
#'     gg.Tema  = 7, gg.Donut = TRUE, gg.Percent = FALSE,
#'     gg.Texto = list(
#'       subtitle = "Synthetic or fake data that resembles real-world data",
#'       caption  = "* Data Simulation",
#'       tag      = "\u00ae"
#'     )
#'   )
#' )
#' @examplesIf require("dplyr")
#' # ---------------------------------------------------------------------------
#' col <- c("#F15A24", "#8CC63F")
#' Msj <- "Distribuci\u00f3n de estudiantes graduados en el primer periodo acad\u00e9mico del 2021."
#' Txt <- "DISTRIBUCI\u00d3N DE GRADUADOS POR MODALIDAD DE FORMACI\u00d3N"
#' Plot.Torta(
#'   datos     = ejConsolidadoGrad |> filter(YEAR==2021, SEMESTRE==1),
#'   categoria = "TIPO_NIVEL",
#'   colores   = col,
#'   titulo    = paste(Txt, "(Periodo 2021-1)"),
#'   label     = "N\u00famero de Graduados",
#'   libreria  = "highcharter",
#'   estilo    = list(
#'     LegendTitle = "\u00c9sta es una descripci\u00f3n para la leyenda:",
#'     hc.Tema = 7, hc.Credits = Msj
#'   )
#' )
#' # ---------------------------------------------------------------------------
#' Msj <- "Distribuci\u00f3n hist\u00f3rica de estudiantes graduados (desde el 2009-I al 2021-I)."
#' Plot.Torta(
#'   datos     = ejConsolidadoGrad,
#'   categoria = "TIPO_NIVEL",
#'   colores   = col,
#'   titulo    = gsub("DOS POR", "DOS\nPOR", Txt),
#'   libreria  = "plotly",
#'   estilo    = list(
#'     ply.Legend = "inside", ply.Credits = list(
#'       x = 0.8, y = 1.1, text = paste0("<b>", Msj, "</b>")
#'     )
#'   )
#' )
#'
#' @export
#'
#' @import highcharter
#' @import plotly
#' @import dplyr
#' @importFrom methods missingArg
#' @importFrom grDevices rainbow
#' @importFrom lifecycle deprecate_warn
Plot.Torta <- function(
    datos, valores, categoria, ano, periodo, colores, titulo = "", label = "",
    addPeriodo = FALSE, libreria = c("highcharter", "plotly"), estilo = NULL,
    estatico = FALSE
) {
  # COMANDOS DE VERIFICACIÓN Y VALIDACIÓN --------------------------------------
  if (missingArg(datos) || missingArg(categoria)) {
    stop("\u00a1Por favor introduzca un conjunto de datos y una categor\u00eda dentro de la columna 'Variable'!", call. = FALSE)
  }
  if (!is.logical(estatico)) {
    stop("\u00a1El argumento 'estatico' debe ser un valor booleano (TRUE o FALSE)!", call. = FALSE)
  }
  if (!all(is.character(titulo), is.character(label))) {
    stop("\u00a1Los argumentos 'titulo' y 'label' deben ser una cadena de texto!", call. = FALSE)
  }
  if (!estatico) {
    if (missingArg(libreria)) {
      warning("\u00a1Se usar\u00e1 la librer\u00eda 'highcharter' por defecto para realizar el plot!", call. = FALSE)
      libreria <- "highcharter"
    } else {
      libreria <- tolower(libreria)
      if (libreria %NotIN% c("highcharter", "plotly")) {
        stop("\u00a1Por favor introduzca el nombre de una librer\u00eda v\u00e1lida (paquete usado para realizar la gr\u00e1fica)!", call. = FALSE)
      }
    }
  } else { libreria <- NULL }

  LegendTitle <- ifelse(is.null(estilo$LegendTitle), "", estilo$LegendTitle)

  # GENERACIÓN DEL DATAFRAME CON EL CUAL SE CREARÁ LA GRÁFICA ------------------
  if (all(missingArg(valores), !missingArg(categoria))) {
    if (!(toupper(categoria) %in% datos$Variable)) {
      stop("\u00a1Por favor introduzca una categor\u00eda que se encuentre dentro de la columna 'Variable'!", call. = FALSE)
    }
    datos <- datos |> ungroup() |> filter(Variable == categoria, is.na(Clase) != TRUE)
    categoria <- sym("Clase")
    valores   <- sym("Total")
  }
  categorias <- datos |> select({{categoria}}) |> distinct() |> pull()

  # Adición temporal (para dar un periodo de adaptación antes de la eliminación del argumento)
  if (any(!missingArg(ano), !missingArg(periodo), !missingArg(addPeriodo))) {
    if (!missingArg(ano)) {
      lifecycle::deprecate_warn(
        when = "1.0.1",
        what = "Plot.Torta(ano)",
        details = "Please remove the use of argument 'ano'. It's recommended to make the filter in the 'datos' input."
      )
    }
    if (!missingArg(periodo)) {
      lifecycle::deprecate_warn(
        when = "1.0.1",
        what = "Plot.Torta(periodo)",
        details = "Please remove the use of argument 'periodo'. It's recommended to make the filter in the 'datos' input."
      )
    }
    if (!missingArg(addPeriodo)) {
      lifecycle::deprecate_warn(
        when = "1.0.1",
        what = "Plot.Torta(addPeriodo)",
        details = "Please remove the use of argument 'addPeriodo'. It's recommended to add period in 'titulo'."
      )
    }
    if (!(missingArg(ano) || missingArg(periodo))) {
      titulo <- ifelse(!missingArg(titulo) && addPeriodo, paste0(titulo, " (Periodo ", ano, "-", periodo, ")"), titulo)
      datos <- datos |> filter(YEAR == ano, SEMESTRE == periodo)
    } else {
      if (missingArg(ano) && missingArg(periodo)) {
        datos <- datos
      } else if (missingArg(ano)) {
        datos <- datos
      } else {
        datos <- datos |> filter(YEAR == ano)
      }
    }
  }

  TablaFinal <- datos |> group_by({{categoria}}) |>
    summarise(Total = sum({{valores}}), .groups = "drop")
  TablaFinal <- TablaFinal |> rename(Clase := {{categoria}})

  if (!(missingArg(colores) || length(colores) == length(categorias))) {
    stop(paste0(
      "\u00a1El n\u00famero de colores ingresados en el vector 'colores' no corresponde con el n\u00famero de categor\u00edas a colorear!",
      "\n\tNo. colores ingresados = ", length(colores), " != ", "No. de categor\u00edas = ", length(categorias)
    ), call. = FALSE)
  }
  if (missingArg(colores)) { colores <- rainbow(length(categorias), alpha = 0.7) }

  # CREACIÓN DEL PLOT RETORNAR -------------------------------------------------
  if (!estatico) {
    if (libreria == "highcharter") {
      Spanish.Highcharter()
      if (!(missingArg(estilo) || is.null(estilo$hc.Tema))) {
        ThemeHC <- switch(estilo$hc.Tema,
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

      if (nrow(TablaFinal) == 1L) {
        TablaFinal$color <- colores
        TablaFinal <- rename(TablaFinal, name = Clase, y = Total)
        PlotTorta  <- highchart() |> hc_chart(type = "pie") |>
          hc_add_series(data = TablaFinal, name = label, showInLegend = TRUE)
      } else {
        PlotTorta <- TablaFinal |>
          hchart(type = "pie", hcaes(x = Clase, y = Total), name = label, showInLegend = TRUE) |>
          hc_plotOptions(pie = list(
            allowPointSelect = TRUE, colorByPoint = TRUE, colors = colores,
            dataLabels = list(
              enabled = TRUE, format = "<b>{point.name}</b>: {point.percentage:.1f} %",
              style = list(fontWeight = "bold", color = "black", fontSize = "18px")
              )
            )
          )
      }
      PlotTorta <- PlotTorta |>
        hc_title(text = titulo, style = list(fontWeight = "bold", fontSize = "22px", color = "#333333", useHTML = TRUE)) |>
        hc_exporting(enabled = TRUE, filename = paste0("PlotTorta_", quo_name(enquo(categoria)))) |>
        hc_credits(enabled = TRUE, text = "DNPE", href = "http://estadisticas.unal.edu.co/home/") |>
        hc_legend(
          enabled = TRUE, align = "center", verticalAlign = "bottom",
          title = list(text = LegendTitle, style = list(textDecoration = "underline")),
          itemStyle = list(fontWeight = "bold", color = "black", fontSize = "18px")
        ) |>
        hc_add_theme(ThemeHC)

      if (!(missingArg(estilo) || is.null(estilo$hc.Credits))) {
        PlotTorta <- PlotTorta |>
          hc_subtitle(text = estilo$hc.Credits, align = "left", style = list(color = "#2B908F", fontWeight = "bold"))
      }
    } else if (libreria == "plotly") {
      if (!(missingArg(estilo) || is.null(estilo$ply.Credits))) {
        ParmsCredits <- estilo$ply.Credits
      } else {
        ParmsCredits <- list(x = 0.2, y = 1, text = "")
      }

      FamilyTitle <- list(family = "Open Sans", size = 24, color = "#333333")
      Title  <- list(text = paste0("<b>", titulo, "</b>"), font = FamilyTitle, y = 0.95)
      Margen <- list(l = 50, r = 50, t = 110, b = 0)                  # l = left, r = right, t = top, b = bottom

      Clase <- TablaFinal$Clase
      Total <- TablaFinal$Total
      if (!missingArg(estilo) && estilo$ply.Legend == "inside") {
        PlotTorta <- plot_ly(
          labels = Clase, values = Total, type = "pie",
          textposition = "inside", textinfo = "label+value+percent",  # "label+percent"
          insidetextfont = list(color = "#FFFFFF", size = 20), hoverinfo = "label+value",
          insidetextorientation = "radial",                           # "horizontal", "radial", "tangential", "auto"
          marker = list(colors = colores, line = list(color = "#000000", width = 1.5))
        ) |>
          layout(title = Title, showlegend = FALSE, autosize = TRUE, margin = Margen)
      } else {
        PlotTorta <- plot_ly(
          labels = Clase, values = Total, type = "pie", textinfo = "label+percent",
          marker = list(colors = colores, line = list(color = "#FFFFFF", width = 1.5))
        ) |>
          layout(title = Title, showlegend = TRUE, autosize = TRUE, margin = Margen)
      }

      PlotTorta <- PlotTorta |>
        layout(
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
    TablaFinal$Prop <- TablaFinal$Total/sum(TablaFinal$Total)
    TablaFinal <- TablaFinal |> arrange(desc(Clase)) |>
      mutate(Lab_yPos = cumsum(Prop) - 0.5*Prop, Prop = round(Prop, 2))

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

    if (!(missingArg(estilo) || is.null(estilo$gg.Texto))) {
      ParmsLabs <- estilo$gg.Texto
    } else {
      ParmsLabs <- list(subtitle = NULL, caption = NULL, tag = NULL)
    }
    xHack <- ifelse(!(missingArg(estilo) || is.null(estilo$gg.Donut) || !estilo$gg.Donut), 2, "")

    PlotTorta <- ggplot(data = TablaFinal, aes(x = xHack, y = Prop, fill = Clase)) +
      geom_bar(width = 1, stat = "identity", color = "#000000") +
      coord_polar("y", start = 0) +
      labs(
        x = NULL, y = NULL, title = titulo, subtitle = ParmsLabs$subtitle,
        caption = ParmsLabs$caption, tag = ParmsLabs$tag
      ) +
      scale_fill_manual(values = colores) +
      ThemeGG + theme(
        axis.text.x  = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.y  = element_blank(),
        axis.ticks.y = element_blank()
      )
    if (!(missingArg(estilo) || is.null(estilo$gg.Percent) || estilo$gg.Percent)) {
      PlotTorta <- PlotTorta + geom_text(aes(y = Lab_yPos, label = Prop), color = "#000000")
    } else {
      PlotTorta <- PlotTorta + geom_text(aes(y = Lab_yPos, label = scales::percent(Prop, accuracy = 0.1)), color = "#000000")
    }
    if (!(missingArg(estilo) || is.null(estilo$gg.Donut) || !estilo$gg.Donut)) {
      PlotTorta <- PlotTorta + xlim(0.5, 2.5)
    }
  }
  return(PlotTorta)
}
