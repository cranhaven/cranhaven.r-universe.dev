#' Cree un gráfico de barras apiladas dinámico/estático y flexible
#'
#' Esta función proporciona excelentes herramientas y opciones para la visualización
#' de un gráfico de barras apiladas con el objetivo de mostrar el tamaño relativo
#' (*como porcentaje*) de una variable categórica, subdivididas por colores en
#' función de un subgrupo. Dicha gráfica se va a representar usando la librería
#' `Highcharter`, la cual usa internamente `JavaScript`.
#'
#' @inheritParams Plot.Torta
#' @inheritParams Plot.Series
#' @param ejeX Una variable categórica dentro del data frame ingresado en `datos`.
#' @param estilo Lista compuesta por varios parámetros, los cuales van a ser
#'   usados para graficar las barras apiladas y cuyo objetivo es personalizar
#'   pequeños detalles de éste.
#'   * `hc.Tema`, `hc.Credits`, `gg.Tema`, `gg.Legend` y `gg.Texto`: Igual uso
#'     que en [Plot.Series()]
#'   * `LegendTitle`: Cadena de caracteres indicando un título para la leyenda
#'     (\emph{diferentes niveles del argumento `categoria`}).
#'   * `ply.LegendPosition`: Igual uso que en [Plot.Series()]
#'   * `ply.Credits`: Igual uso que en [Plot.Series()]
#'   * `gg.Bar`: Igual uso que en [Plot.Barras()]
#'
#' @returns
#' Retorna el diagrama de barras apiladas (*objeto widget de HTML*) creado. La
#' clase del objeto retornado será un "htmlwidget" y adicionalmente pertenecerá
#' a la clase "highchart".
#'
#' @examplesIf all(require("tibble"), require("dplyr"))
#' # Ejemplo generalizado (sin uso de un consolidado como input)
#' # library("tibble"); library("dplyr")
#' set.seed(42)
#' Blood <- tibble(
#'   Quarter = sample(c("I", "II", "III", "IV"), size = 200, replace = TRUE),
#'   Group   = sample(
#'     c("O", "A", "B", "AB"), size = 200, prob = c(.5, .3, .16, .4), replace = TRUE
#'   ),
#'   Prevalence = round(runif(200)*100)
#' )
#' Plot.Apiladas(
#'   datos     = Blood     ,
#'   ejeX      = Quarter   ,
#'   valores   = Prevalence,
#'   categoria = Group     ,
#'   colores   = c("#FF553D", "#A5FF67", "#40D2FF", "#FFDB5C")
#' )
#' @examplesIf require("dplyr")
#' # ---------------------------------------------------------------------------
#' Txt <- "BARRAS APILADAS EN FUNCI\u00d3N DEL NIVEL ACAD\u00c9MICO Y EL A\u00d1O"
#' Msj <- paste(
#'   "Se considera \u00fanicamente los valores obtenidos en el primer periodo",
#'   "acad\u00e9mico de cada a\u00f1o."
#' )
#' Plot.Apiladas(
#'   datos     = ejConsolidadoGrad |> filter(YEAR %in% c(2018:2020), SEMESTRE == 1),
#'   categoria = "NIVEL",      # Pruebe también con alguna de -> unique(ejConsolidadoGrad$Variable)
#'   colores   = c("#FFA700", "#C10AA1", "#01CDFE", "#00FF44", "#FF0040"),
#'   titulo    = Txt,
#'   estilo    = list(LegendTitle = "NIVEL ACAD\u00c9MICO:", hc.Tema = 4, hc.Credits = Msj)
#' )
#' Plot.Apiladas(
#'   datos     = ejConsolidadoGrad |> filter(YEAR %in% c(2018:2020), SEMESTRE == 1),
#'   categoria = "AREAC_SNIES",
#'     colores   = c("#D2D4DC", "#FF8ABF", "#945BC2", "#D11879",
#'                   "#FF7F7F", "#FFA568", "#9CFF86", "#89D8FF"),
#'   titulo    = "BARRAS APILADAS EN FUNCI\u00d3N DEL \u00c1REA DEL SNIES",
#'   libreria  = "plotly",
#'   estilo    = list(
#'     LegendTitle = "NIVEL ACAD\u00c9MICO:",
#'     ply.Credits = list(x = 0.5, y = 1.5, text = gsub("l p", "l\np", Msj)),
#'     ply.LegendPosition = list(x = 0.04, y = -0.3, orientation = "h")
#'   )
#' )
#' # Ejemplo usando el caso estático (ggplot2)
#' Plot.Apiladas(
#'   datos     = ejConsolidadoGrad |> filter(YEAR %in% c(2019:2021), SEMESTRE == 1),
#'   categoria = "NIVEL",
#'   colores   = c("#FFA700", "#C10AA1", "#01CDFE", "#00FF44", "#FF0040"),
#'   titulo    = gsub("L AC", "L\nAC", Txt),
#'   estatico  = TRUE,
#'   estilo    = list(
#'     LegendTitle = "NIVEL ACAD\u00c9MICO:", gg.Tema = 8,
#'     gg.Legend = list(legend.position = "right", legend.direction = "vertical"),
#'     gg.Bar    = list(width = 0.6, color = "#000000"),
#'     gg.Texto  = list(
#'       subtitle = "\u00bb\u00bb\u00bb", tag = "\u00ae",
#'       caption  = "Informaci\u00f3n Disponible desde 2009-1"
#'     )
#'   )
#' )
#'
#' @inheritSection Plot.Series Lista de argumentos de estilo
#'
#' @export
#'
#' @import highcharter
#' @import plotly
#' @rawNamespace import(ggplot2, except = last_plot)
#' @import dplyr
#' @importFrom scales percent label_percent
#' @importFrom methods missingArg
#' @importFrom grDevices rainbow
Plot.Apiladas <- function(
    datos, ejeX, valores, categoria, ano, periodo, addPeriodo = FALSE, colores,
    titulo = "", libreria = c("highcharter", "plotly"), estilo = NULL, estatico = FALSE
) {

  # COMANDOS DE VERIFICACIÓN Y VALIDACIÓN --------------------------------------
  if (missingArg(datos) || missingArg(categoria)) {
    stop("\u00a1Por favor introduzca un conjunto de datos y una categor\u00eda!", call. = FALSE)
  }
  if (!is.character(titulo)) {
    stop("\u00a1El argumento 'titulo' deben ser una cadena de texto!", call. = FALSE)
  }
  if (!all(is.logical(addPeriodo), is.logical(estatico))) {
    stop("\u00a1Los argumentos 'addPeriodo' y 'estatico' deben ser un valor booleano (TRUE o FALSE)!", call. = FALSE)
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
  } else { libreria <- NULL }
  LegendTitle <- ifelse(is.null(estilo$LegendTitle), "", estilo$LegendTitle)

  # GENERACIÓN DEL DATAFRAME CON EL CUAL SE CREARÁ LA GRÁFICA ------------------
  if (all(missingArg(valores), !missingArg(categoria))) {
    if (!(toupper(categoria) %in% datos$Variable)) {
      stop("\u00a1Por favor introduzca una categor\u00eda que se encuentre dentro de la columna 'Variable'!", call. = FALSE)
    }
    datos <- datos |> ungroup() |> filter(Variable == categoria, is.na(Clase) != TRUE)
    ejeX      <- sym("YEAR")
    categoria <- sym("Clase")
    valores   <- sym("Total")
  } else {
    if (any(class(try(class(ejeX), silent = TRUE)) == "try-error")) { ejeX <- vars({{ejeX}}) }
  }
  categorias <- datos |> select({{categoria}}) |> distinct() |> pull()

  # Adición temporal (para dar un periodo de adaptación antes de la eliminación del argumento)
  if (any(!missingArg(ano), !missingArg(periodo), !missingArg(addPeriodo))) {
    if (!missingArg(ano)) {
      lifecycle::deprecate_warn(
        when = "1.0.1",
        what = "Plot.Apiladas(ano)",
        details = "Please remove the use of argument 'ano'. It's recommended to make the filter in the 'datos' input."
      )
    }
    if (!missingArg(periodo)) {
      lifecycle::deprecate_warn(
        when = "1.0.1",
        what = "Plot.Apiladas(periodo)",
        details = "Please remove the use of argument 'periodo'. It's recommended to make the filter in the 'datos' input."
      )
    }
    if (!missingArg(addPeriodo)) {
      lifecycle::deprecate_warn(
        when = "1.0.1",
        what = "Plot.Apiladas(addPeriodo)",
        details = "Please remove the use of argument 'addPeriodo'. It's recommended to add period in 'titulo'."
      )
    }
    if (!(missingArg(ano) || missingArg(periodo))) {
      titulo <- ifelse(!missingArg(titulo) && addPeriodo, paste0(titulo, " (Periodo ", ano, "-", periodo, ")"), titulo)
      datos  <- datos |> filter(YEAR %in% ano, SEMESTRE %in% periodo)
    } else {
      if (missingArg(ano) && missingArg(periodo)) {
        datos <- datos
      } else if (missingArg(ano)) {
        datos <- datos
      } else {
        datos <- datos |> filter(YEAR %in% ano)
      }
    }
  }

  datosCheck <- datos |> group_by(!!!ejeX, {{categoria}}) |>
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
  datos <- datos |> arrange(!!!ejeX) |> mutate(xAxis := paste(!!!ejeX, sep = "-"))

  df <- datos |> left_join(
    datos |> group_by(xAxis) |>
      summarise(sumYear = sum({{valores}}), .groups = "drop"), by = "xAxis"
  ) |> mutate(percent_xAxis = round({{valores}}/sumYear * 100, 4))
  df <- df |> rename(Clase := {{categoria}})

  if (!(missingArg(colores) || length(colores) == length(categorias))) {
    stop(paste0(
      "\u00a1El n\u00famero de colores ingresados en el vector 'colores' no corresponde con el n\u00famero de categor\u00edas a colorear!",
      "\n\tNo. colores ingresados = ", length(colores), " != ", "No. de categor\u00edas = ", length(categorias)
      ), call. = FALSE
    )
  }
  if (missingArg(colores)) { colores <- rainbow(length(categorias), alpha = 0.7) }

  # CREACIÓN DEL PLOT A RETORNAR -----------------------------------------------
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

      PlotApiladas <- df |> hchart("column", hcaes(x = "xAxis", y = "percent_xAxis", group = "Clase")) |>
        hc_plotOptions(column = list(stacking = "normal")) |>
        hc_title(text = titulo, style = list(
          fontWeight = "bold", fontSize = "22px", color = "#333333", useHTML = TRUE
          )
        ) |>
        hc_xAxis(type = "category", title = list(text = NULL), labels = list(
          style = list(fontWeight = "bold", color = "black", fontSize = "18px")
          )
        ) |>
        hc_yAxis(
          title = list(text = "Porcentaje", style = list(fontWeight = "bold", color = "black", fontSize = "18px")),
          labels = list(format = "{value}%", style = list(fontWeight = "bold", color = "black", fontSize = "18px")),
          min = 0, max = 100
        ) |>
        hc_colors(colores) |>
        hc_tooltip(pointFormat = '<span style="color:{series.color}">\u25CF </span><b>{series.name}:</b> {point.percent_xAxis:.2f}%<br/>') |>
        hc_exporting(
          enabled = TRUE,
          filename = paste0("PlotApiladas_", str_to_title(quo_name(enquo(categoria))))
        ) |>
        hc_credits(enabled = TRUE, text = "DNPE", href = "http://estadisticas.unal.edu.co/home/") |>
        hc_legend(
          enabled = TRUE, align = "center", verticalAlign = "bottom",
          title = list(text = LegendTitle, style = list(textDecoration = "underline")),
          itemStyle = list(fontWeight = "bold", color = "black", fontSize = "18px")
        ) |>
        hc_add_theme(ThemeHC)

      if (!(missingArg(estilo) || is.null(estilo$hc.Credits))) {
        PlotApiladas <- PlotApiladas |>
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
        ParmsCredits <- list(x = 0.11, y = 1.1, text = "")
      }

      FamilyTitle <- list(family = "Open Sans", size = 24, color = "#333333")
      Title <- list(text = paste0("<b>", titulo, "</b>"), font = FamilyTitle, y = 0.95)
      if (titulo == "") {
        Margen <- NULL
      } else { Margen <- list(l = 50, r = 50, t = 110, b = 0) }

      PlotApiladas <- plot_ly(data = df)
      for (i in seq_len(length(categorias))) {
        df_Temp <- df |> filter(Clase == categorias[i])
        PlotApiladas <- add_trace(
          PlotApiladas, data = df_Temp, x = ~factor(xAxis), y = ~percent_xAxis,
          color = ~Clase, name = categorias[i], type = "bar", orientation = "v",
          hovertemplate = ~paste0("(", xAxis, "): ", round(percent_xAxis, 2), "%"),
          marker = list(color = colores[i], line = list(color = "#3A4750", width = 1.5))
        )
      }
      PlotApiladas <- PlotApiladas |>
        layout(
          barmode = "stack", title = Title, xaxis = list(title = ""),
          yaxis = list(title = "Porcentaje", ticksuffix = "%"),
          showlegend = TRUE, legend = append(ParmsLegend, list(
            traceorder = "normal", title = list(text = paste0("<b>", LegendTitle, "</b>"))
            )
          ),
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
    df <- df |> mutate(percent_xAxis = round(percent_xAxis, 2))
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

    if (!(missingArg(estilo) || is.null(estilo$gg.Legend))) {
      ParmsLegend <- estilo$gg.Legend
    } else {
      ParmsLegend <- list(legend.position = "bottom", legend.direction = "horizontal")
    }
    if (!(missingArg(estilo) || is.null(estilo$gg.Bar))) {
      ParmsBar <- append(estilo$gg.Bar, list(stat = "identity"))
    } else {
      ParmsBar <- list(stat = "identity", width = 0.9)
    }
    if (!(missingArg(estilo) || is.null(estilo$gg.Texto))) {
      ParmsLabs <- estilo$gg.Texto
    } else {
      ParmsLabs <- list(subtitle = NULL, caption = NULL, tag = NULL)
    }

    PlotApiladas <- ggplot(data = df, aes(x = factor(xAxis), y = percent_xAxis, fill = Clase)) +
      do.call(geom_bar, ParmsBar) +
      labs(
        title = titulo, subtitle = ParmsLabs$subtitle, y = "Porcentaje", x = NULL,
        caption = ParmsLabs$caption, tag = ParmsLabs$tag, fill = LegendTitle
      ) +
      geom_text(
        aes(label = scales::percent(percent_xAxis, scale = 1)),
        position = position_stack(vjust = 0.5), size = 3
      ) +
      scale_fill_manual(values = colores) +
      scale_y_continuous(labels = scales::label_percent(scale = 1)) +
      ThemeGG + do.call(theme, ParmsLegend)
  }

  return(PlotApiladas)
}
