#' Cree un gráfico de barras que muestre la información de forma horizontal o
#' vertical, para variables nominales u ordinales con dos diferentes paquetes
#'
#' Esta función permite mostrar de forma interactiva (*y estática*) un gráfico
#' de barras verticales u horizontales cuya altura/longitud es proporcional al
#' valor de la variable (*categorías de una variable cualitativa*), lo anterior
#' para ayudar a la creación de informes descriptivos y analíticos. Dicho diagrama
#' se puede representar usando dos diferentes librerías que son `Highcharter` y
#' `Plotly`, las cuales usan internamente `JavaScript`.
#'
#' @inheritParams Plot.Torta
#' @inheritParams Plot.Series
#' @param vertical Si es `TRUE` (*valor predeterminado*) indicará que la orientación
#'   del gráfico será vertical.
#' @param ordinal Si es `TRUE` indicará que las categorías de la variable ingresada
#'   son ordinales (*no nominales*), esto con el fin de ordenar la disposición en
#'   el que se presentan en el eje del gráfico, el valor por defecto es `FALSE`.
#' @param labelEje Cadena de caracteres indicando la etiqueta del eje `X` o `Y`
#'   (*dependiendo de la orientación del gráfico*). Por defecto se emplea el rótulo
#'   `"Número de "`.
#' @param textInfo Cadena de caracteres que especifica el texto que se escribe
#'   dentro de la caja de información al posar el cursor en alguna barra en el
#'   gráfico, producido por `Highcharter`, el valor por defecto es igual al de
#'   `labelX`.
#' @param estilo Lista compuesta por varios parámetros, los cuales van a ser usados
#'   de acuerdo con la librería especificada para graficar el plot y cuyo objetivo
#'   es personalizar pequeños detalles de ésta.
#'   * `hc.Tema`, `hc.Credits`, `ply.Credits`, `gg.Tema` y `gg.Texto`: Igual uso
#'     que en [Plot.Series()]
#'   * `ply.Legend`: Por defecto la gráfica muestra la leyenda fuera del gráfico
#'     de pie, si se introduce la cadena de texto `"inside"` se resumirá toda la
#'     información dentro del pie.
#'   * `gg.Bar`: Una lista de parámetros admitidos por la función [geom_bar()][ggplot2::geom_bar()]).
#'
#' @details
#' Al usar el paquete `Highcharter` y usar las opciones de descarga, el nombre
#' del archivo descargado será la concatenación del plot graficado y la categoría
#' usada, así, por ejemplo, si se graficó el diagrama de barras para la categoría
#' "Nacionalidad" el nombre será `PlotBarras_Nacionalidad.png`.
#'
#' @inheritSection Plot.Series Lista de argumentos de estilo
#'
#' @returns
#' Retorna el diagrama de barras (*objeto widget de HTML*) creado. La clase del
#' objeto retornado será un "htmlwidget" y dependiendo de la librería usada
#' pertenecerá adicionalmente a la clase "highchart" o "plotly".
#'
#' @examplesIf all(require("tibble"), require("dplyr"))
#' # Ejemplo generalizado (sin uso de un consolidado como input)
#' # library("tibble"); library("dplyr")
#' set.seed(42)
#' Blood <- tibble(
#'   Group = sample(c("O", "A", "B", "AB"), size = 200, prob = c(0.5, 0.3, 0.16, 0.4), replace = TRUE),
#'   RH    = sample(c("+", "-"), size = 200, replace = TRUE),
#'   Prevalence = round(runif(200)*100)
#' )
#' Plot.Barras(
#'   datos     = Blood     ,
#'   valores   = Prevalence,
#'   categoria = Group     ,
#'   ordinal   = TRUE      ,
#'   colores   = c("#FF553D", "#A5FF67", "#40D2FF", "#FFDB5C"),
#'   labelY    = "Prevalence"
#' )
#' Plot.Barras(
#'   datos     = Blood     ,
#'   valores   = Prevalence,
#'   categoria = Group     ,
#'   colores   = c("#FF553D", "#A5FF67", "#40D2FF", "#FFDB5C"),
#'   labelY    = "Prevalence",
#'   libreria  = "plotly"
#' )
#' @examplesIf require("dplyr")
#' # ---------------------------------------------------------------------------
#' Msj <- "Ac\u00e1 puede ir m\u00e1s informaci\u00f3n acerca del gr\u00e1fico."
#' Plot.Barras(
#'   datos        = ejConsolidadoGrad |> filter(YEAR==2021, SEMESTRE==1),
#'   categoria    = "NIVEL",
#'   freqRelativa = TRUE,
#'   vertical     = TRUE,
#'   ordinal      = TRUE,
#'   colores      = c("#D7191C", "#FDAE61", "#FFFFBF", "#ABDDA4", "#2B83BA"),
#'   titulo       = "GRADUADOS DE ACUERDO CON EL NIVEL DE FORMACI\u00d3N (Periodo 2021-1)",
#'   labelY       = "Frecuencia Relativa<br>(% de graduados)",
#'   textInfo     = "Porcentaje de Graduados",
#'   libreria     = "highcharter",
#'   estilo       = list(hc.Tema = 2, hc.Credits = Msj)
#' )
#' # ---------------------------------------------------------------------------
#' Txt <- "DISTRIBUCI\u00d3N DEL N\u00daMERO DE GRADUADOS POR NIVEL"
#' Msj <- "A\u00f1o 2020, sin segregar por semestre (considerando ambos)."
#' Plot.Barras(
#'   datos     = ejConsolidadoGrad |> filter(YEAR == 2020),
#'   categoria = "NIVEL",
#'   vertical  = FALSE,
#'   ordinal   = FALSE,
#'   colores   = c("#66C2A5", "#FC8D62", "#8DA0CB", "#E78AC3", "#A6D854"),
#'   titulo    = Txt,
#'   labelY    = "N\u00famero de Graduados",
#'   libreria  = "plotly",
#'   estilo    = list(
#'     ply.Credits = list(x = 0.45, y = 1.1, text = Msj), ply.Legend = FALSE
#'   )
#' )
#' # ---------------------------------------------------------------------------
#' # Ejemplo usando el caso estático (ggplot2)
#' Plot.Barras(
#'   datos     = ejConsolidadoGrad |> filter(YEAR == 2020),
#'   categoria = "NIVEL",
#'   vertical  = FALSE,
#'   ordinal   = FALSE,
#'   colores   = c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3", "#FF7F00"),
#'   titulo    = gsub("DE GR", "DE\nGR", Txt),
#'   labelY    = "N\u00famero de Graduados",
#'   estatico  = TRUE,
#'   estilo    = list(
#'     gg.Tema  = 10,
#'     gg.Bar   = list(width = 0.2, color = "#000000"),
#'     gg.Texto = list(subtitle = gsub("A", "\nA", Msj),
#'                     caption  = "Informaci\u00f3n Disponible desde 2009-1",
#'                     tag      = "\u00ae"
#'     )
#'   )
#' )
#'
#' @export
#'
#' @import highcharter
#' @import plotly
#' @rawNamespace import(ggplot2, except = last_plot)
#' @import dplyr
#' @importFrom scales percent label_percent
#' @importFrom methods missingArg
#' @importFrom stats reorder
#' @importFrom grDevices rainbow
#' @importFrom lifecycle deprecate_warn
Plot.Barras <- function(
    datos, valores, categoria, ano, periodo, freqRelativa = FALSE, ylim, vertical = TRUE,
    ordinal = FALSE, colores, titulo = "", labelX = "", labelY = "N\u00famero de",
    labelEje, addPeriodo = FALSE, textInfo = labelY, libreria = c("highcharter", "plotly"),
    estilo = NULL, estatico = FALSE
) {

  # COMANDOS DE VERIFICACIÓN Y VALIDACIÓN --------------------------------------
  if (missingArg(datos) || missingArg(categoria)) {
    stop("\u00a1Por favor introduzca un conjunto de datos y una categor\u00eda dentro de la columna 'Variable'!", call. = FALSE)
  }
  if (!all(is.logical(freqRelativa), is.logical(vertical), is.logical(ordinal), is.logical(estatico))) {
    stop("\u00a1Los argumentos 'freqRelativa', 'vertical', 'ordinal' y 'estatico' deben ser un valor booleano (TRUE o FALSE)!", call. = FALSE)
  }
  if (!missingArg(ylim)) {
    if (!(is.numeric(ylim) && length(ylim) == 2)) {
      stop("\u00a1Por favor introduzca un vector de longitud dos que definen los l\u00edmites del eje Y!", call. = FALSE)
    }
    yLim <- ylim
  } else { yLim <- NULL }
  if (!all(is.character(titulo), is.character(labelX), is.character(labelY), is.character(textInfo))) {
    stop("\u00a1Los argumentos 'titulo', 'labelX', 'labelY' y 'textInfo' deben ser una cadena de texto!", call. = FALSE)
  }
  # Adición temporal (para dar un periodo de adaptación antes de la eliminación del argumento)
  if (!missing(labelEje)) {
    lifecycle::deprecate_warn(
      when = "1.0.0",
      what = "Plot.Barras(labelEje)",
      with = "Plot.Barras(labelY)",
      details = "Please replace the use of argument 'labelEje' with 'labelY'. Before the argument is removed."
    )
    labelY <- labelEje
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
        what = "Plot.Barras(ano)",
        details = "Please remove the use of argument 'ano'. It's recommended to make the filter in the 'datos' input."
      )
    }
    if (!missingArg(periodo)) {
      lifecycle::deprecate_warn(
        when = "1.0.1",
        what = "Plot.Barras(periodo)",
        details = "Please remove the use of argument 'periodo'. It's recommended to make the filter in the 'datos' input."
      )
    }
    if (!missingArg(addPeriodo)) {
      lifecycle::deprecate_warn(
        when = "1.0.1",
        what = "Plot.Barras(addPeriodo)",
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
    summarise(Total = sum({{valores}}), .groups = "drop") |>
    mutate(Relativo = round(Total / sum(Total) * 100, 1))
  TablaFinal <- TablaFinal |> rename(Clase := {{categoria}})

  if (!(missingArg(colores) || length(colores) == length(categorias))) {
    stop(paste0(
      "\u00a1El n\u00famero de colores ingresados en el vector 'colores' no corresponde con el n\u00famero de categor\u00edas a colorear!",
      "\n\tNo. colores ingresados = ", length(colores), " != ", "No. de categor\u00edas = ", length(categorias)
      ), call. = FALSE
    )
  }
  if (missingArg(colores)) { colores <- rainbow(length(categorias), alpha = 0.7) }

  if (!ordinal) {
    TablaFinal <- bind_cols(TablaFinal, "Colores" = colores)
    TablaFinal <- TablaFinal |> arrange(desc(Total))
    MyColors   <- TablaFinal$Colores
  } else { MyColors <- colores }

  # CREACIÓN DEL PLOT RETORNAR -------------------------------------------------
  if (!estatico) {
    if (libreria == "highcharter") {
      # SEGREGACIÓN DEL CONDICIONAL DE FRECUENCIA ABSOLUTA O RELATIVA
      if (freqRelativa) {
        sufijoY    <- "{value}%"
        sufijoBar  <- "{point.y}%"
        TablaFinal <- TablaFinal |> rename_at(vars(Relativo, Total), ~c("Y", "Extra"))
        strFormat  <- '<span style="color:{series.color}">\u25CF </span><b>{series.name}: {point.y}%</b> ({point.Extra})<br/>'
      } else {
        sufijoY    <- "{value}"
        sufijoBar  <- "{point.y}"
        TablaFinal <- TablaFinal |> rename_at(vars(Total, Relativo), ~c("Y", "Extra"))
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

      Orientacion <- ifelse(vertical, "column", "bar")
      PlotOptions <- list(
        tooltip = list(pointFormat = strFormat), colorByPoint = TRUE,
        colors = MyColors, dataLabels = list(
          enabled = TRUE, pointFormat = sufijoBar,
          style = list(fontWeight = "bold", color = "black", fontSize = "18px")
        )
      )
      if (nrow(TablaFinal) == 1L) {
        TablaFinal$color <- colores
        TablaFinal <- rename(TablaFinal, name = Clase, y = Y)
        PlotBarras <- highchart() |> hc_chart(type = Orientacion) |>
          hc_add_series(data = TablaFinal, name = textInfo, showInLegend = FALSE) |>
          hc_xAxis(
            categories = as.list(TablaFinal$name),
            title = list(text = labelX, style = list(
              fontWeight = "bold", color = "black", fontSize = "18px"
              )
            ),
            labels = list(style = list(fontWeight = "bold", color = "black", fontSize = "18px"))
          )
      } else {
        PlotBarras <- highchart() |>
          hc_add_series(
            TablaFinal, type = Orientacion, hcaes(x = Clase, y = Y),
            name = textInfo, showInLegend = FALSE
          ) |>
          hc_xAxis(
            categories = as.list(TablaFinal$Clase),
            title = list(text = labelX, style = list(
              fontWeight = "bold", color = "black", fontSize = "18px"
              )
            ),
            labels = list(style = list(fontWeight = "bold", color = "black", fontSize = "18px"))
          )
      }
      PlotBarras <- PlotBarras |>
        hc_plotOptions(bar = PlotOptions, column = PlotOptions) |>
        hc_title(
          text = titulo,
          style = list(fontWeight = "bold", fontSize = "22px", color = "#333333", useHTML = TRUE)
        ) |>
        hc_yAxis(
          min = yLim[1], max = yLim[2],
          title = list(text = labelY, style = list(
            fontWeight = "bold", color = "black", fontSize = "18px"
            )
          ),
          labels = list(format = sufijoY, style = list(
            fontWeight = "bold", color = "black", fontSize = "18px"
            )
          )
        ) |>
        hc_exporting(enabled = TRUE, filename = paste0("PlotBarras_", quo_name(enquo(categoria)))) |>
        hc_credits(enabled = TRUE, text = "DNPE", href = "http://estadisticas.unal.edu.co/home/")  |>
        hc_add_theme(ThemeHC)

      if (!(missingArg(estilo) || is.null(estilo$hc.Credits))) {
        PlotBarras <- PlotBarras |>
          hc_subtitle(text = estilo$hc.Credits, align = "left", style = list(color = "#2B908F", fontWeight = "bold"))
      }
    } else if (libreria == "plotly") {
      if (!(missingArg(estilo) || is.null(estilo$ply.Credits))) {
        ParmsCredits <- estilo$ply.Credits
      } else {
        ParmsCredits <- list(x = 0.11, y = 1.1, text = "")
      }
      ShowLeyenda <- ifelse(!(missingArg(estilo) || is.null(estilo$ply.Legend)), estilo$ply.Legend, TRUE)

      FamilyTitle <- list(family = "Open Sans", size = 24, color = "#333333")
      Title <- list(text = paste0("<b>", titulo, "</b>"), font = FamilyTitle, y = 0.95)
      if (titulo == "") {
        Margen <- NULL
      } else { Margen <- list(l = 50, r = 50, t = 110, b = 0) }

      # SEGREGACIÓN DEL CONDICIONAL DE FRECUENCIA ABSOLUTA O RELATIVA
      if (freqRelativa) {
        sufijoY <- "%"; comodin <- ""
        TablaFinal <- TablaFinal |> rename_at(vars(Relativo, Total), ~c("varNum", "Extra"))
      } else {
        sufijoY <- ""; comodin <- "%"
        TablaFinal <- TablaFinal |> rename_at(vars(Total, Relativo), ~c("varNum", "Extra"))
      }

      Extra <- TablaFinal$Extra
      if (vertical) {
        if (ordinal) {
          EjeX <- TablaFinal$Clase; EjeY <- TablaFinal$varNum
        } else {
          EjeX <- reorder(TablaFinal$Clase, TablaFinal$varNum); EjeY <- TablaFinal$varNum
        }
        PlotBarras <- plot_ly(
          x = EjeX, y = EjeY, type = "bar", color = EjeX, orientation = "v",
          hovertemplate = paste0(EjeY, sufijoY, " (", Extra, comodin, ")"),
          marker = list(color = colores, line = list(color = "#3A4750", width = 1.5))
        ) |>
          layout(
            title = Title, xaxis = list(title = labelX),
            yaxis = list(title = labelY, ticksuffix = sufijoY, range = yLim),
            showlegend = ShowLeyenda, autosize = TRUE, margin = Margen
          )
      } else {
        if (ordinal) {
          EjeX <- TablaFinal$varNum; EjeY <- TablaFinal$Clase
        } else {
          EjeX <- TablaFinal$varNum; EjeY <- reorder(TablaFinal$Clase, TablaFinal$varNum)
        }
        PlotBarras <- plot_ly(
            TablaFinal,
            x = EjeX, y = EjeY, type = "bar", color = ~Clase, orientation = "h",
            hovertemplate = paste0(EjeX, sufijoY, " (", Extra, comodin, ")"),
            marker = list(color = colores, line = list(color = "#3A4750", width = 1.5))
          ) |>
          layout(
            title = Title, xaxis = list(title = labelY, ticksuffix = sufijoY),
            yaxis = list(title = labelX), showlegend = ShowLeyenda, autosize = TRUE, margin = Margen
          )
      }

      PlotBarras <- PlotBarras |>
        layout(
          annotations = append(
            ParmsCredits, list(
              showarrow = FALSE, xref = "paper", yref = "paper", xanchor = "right",
              yanchor = "auto", xshift = 0, yshift = 0, font = list(size = 12, color = "#2B908F")
            )
          )
        ) |>
        config(locale = "es")
    }
  } else {
    # This Trick Update the Factor Levels (necesario para que el argumento 'ordinal' funcione)
    TablaFinal <- TablaFinal |> mutate(Clase = factor(Clase, levels = Clase))
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

    if (vertical) {
      geomText <- list(position = position_dodge(width = 0), vjust = -0.5, size = 3)
    } else {
      geomText <- list(position = position_dodge(width = 0), hjust = -0.2, size = 3)
    }
    if (freqRelativa) {
      TablaFinal <- TablaFinal |> rename_at(vars(Relativo, Total), ~ c("Y", "Extra"))
    } else {
      TablaFinal <- TablaFinal |> rename_at(vars(Total, Relativo), ~ c("Y", "Extra"))
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

    PlotBarras <- ggplot(data = TablaFinal, aes(x = Clase, y = Y, fill = Clase)) +
      do.call(geom_bar, ParmsBar) +
      labs(
        title = titulo, subtitle = ParmsLabs$subtitle, x = labelX, y = br2addline(labelY),
        caption = ParmsLabs$caption, tag = ParmsLabs$tag
      ) +
      scale_fill_manual(values = MyColors) +
      ThemeGG + theme(legend.position = "none")

    if (freqRelativa) {
      PlotBarras <- PlotBarras +
        do.call(geom_text, append(geomText, list(aes(label = scales::percent(Y, scale = 1))))) +
        do.call(scale_y_continuous, list(limits = yLim, labels = scales::label_percent(scale = 1)))
    } else {
      PlotBarras <- PlotBarras +
        do.call(geom_text, append(geomText, list(aes(label = Y)))) +
        do.call(scale_y_continuous, list(limits = yLim))
    }
    if (!vertical) { PlotBarras <- PlotBarras + coord_flip() }
  }

  return(PlotBarras)
}
