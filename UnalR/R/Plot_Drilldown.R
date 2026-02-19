#' Cree un gráfico profundo (*drill down*) de torta/barras dinámico y flexible
#'
#' Esta función proporciona excelentes herramientas y opciones para la visualización
#' de un gráfico drill down con el objetivo de poder inspeccionar los datos con
#' mayor nivel de detalle, sin la necesidad de navegar o salir de él, pudiendo
#' hacer clic en diversos elementos como columnas o sectores circulares. Dicha
#' gráfica se va a representar usando la librería `Highcharter`, la cual usa
#' internamente `JavaScript`.
#'
#' @param datos Un data frame, no un vector numérico.
#' @param varPrincipal Una variable categórica dentro del data frame ingresado
#'   en `datos`.
#' @param varSecundaria Otra variable categórica dentro del data frame ingresado
#'   en `datos`, diferente a la principal, pues se segregará a otros niveles.
#' @param ano Igual uso que en [Plot.Torta()]
#' @param periodo Igual uso que en [Plot.Torta()]
#' @param torta Si es `TRUE` (*valor predeterminado*) el primer nivel o gráfico
#'   principal será un diagrama de torta, defínalo en `FALSE` si desea que éste
#'   sea un gráfico de barras.
#' @param vertical Si es `TRUE` (*valor predeterminado*) indicará que tanto la
#'   orientación del gráfico principal como secundario será vertical. Solamente
#'   aplicará si el argumento `torta` es `FALSE`.
#' @param colores Cadena de caracteres indicando los colores con los cuales se
#'   deben colorear cada una de las trazas correspondiente a cada nivel del
#'   argumento `varPrincipal`. Si no se introduce algún vector se usará la paleta
#'   `rainbow` por defecto.
#' @param colores2 Igual que `colores` pero aplicado al gráfico secundario.
#' @param titulo Igual uso que en [Plot.Series()]
#' @param label Cadena de caracteres indicando el agregado al que hace referencia
#'   el gráfico. Por defecto no se emplea ningún rótulo.
#' @param textInfo Cadena de caracteres indicando el texto que aparecerá dentro
#'   de la caja de información al pasar el mouse por las diferentes columnas del
#'   gráfico de barras.
#' @param addPeriodo Igual uso que en [Plot.Torta()]
#' @param estilo Lista compuesta por varios parámetros, los cuales van a ser usados
#'   para graficar el drill down y cuyo objetivo es personalizar pequeños detalles
#'   de éste.
#'   * `LegendTitle`: Cadena de caracteres indicado un título para la leyenda
#'     (\emph{diferentes niveles del argumento `varPrincipal`}).
#'   * `hc.Tema` y `hc.Credits`: Igual uso que en [Plot.Series()]
#'
#' @returns
#' Retorna el diagrama drill down (*objeto widget de HTML*) creado. La clase del
#' objeto retornado será un "htmlwidget" y adicionalmente pertenecerá a la clase
#' "highchart".
#'
#' @examplesIf require("dplyr")
#' # library(dplyr)
#' df <- ejMiniConsolidadoAsp |>
#'   filter(Clase != "Sin Información", tolower(Clase) != "no aplica")
#' text <- "DISTRIBUCI\u00d3N DE ASPIRANTES A PREGRADO EN SITUACI\u00d3N DE DISCAPACIDAD"
#' Msj  <- paste(
#'   "Discapacidad: Deficiencia, limitaci\u00f3n de la actividad ",
#'   "y la restricci\u00f3n de la participaci\u00f3n."
#' )
#' Plot.Drilldown(
#'   datos         = df,
#'   varPrincipal  = "DISCAPACIDAD",
#'   varSecundaria = "TIPO_DISC",
#'   ano           = max(df$YEAR),
#'   periodo       = slice(df, n())$SEMESTRE,
#'   torta         = TRUE, # Pruebe poniendo ambos valores ahora en FALSE
#'   vertical      = TRUE,
#'   colores       = c("#FF0040", "#00FF40"),
#'   colores2      = c("#66C2A5", "#FC8D62", "#8DA0CB", "#E78AC3", "#A6D854", "#FFD92F"),
#'   titulo        = text,
#'   label         = "Aspirantes",
#'   textInfo      = "Aspirantes con discapacidades por tipo",
#'   addPeriodo    = TRUE,
#'   estilo        = list(hc.Tema = 7, hc.Credits = Msj)
#' )
#'
#' @export
#'
#' @import highcharter
#' @import dplyr
#' @importFrom forcats fct_drop
#' @importFrom methods missingArg
#' @importFrom grDevices rainbow
Plot.Drilldown <- function(
    datos, varPrincipal, varSecundaria, ano, periodo, torta = TRUE,
    vertical = TRUE, colores, colores2, titulo = "", label = "", textInfo = "",
    addPeriodo = TRUE, estilo = NULL) {

  # COMANDOS DE VERIFICACIÓN Y VALIDACIÓN
  if (any(missingArg(datos), missingArg(varPrincipal), missingArg(varSecundaria))) {
    stop("\u00a1Por favor introduzca un conjunto de datos, una variable principal y una secundaria (a desglosar)!", call. = FALSE)
  }
  class  <- toupper(varPrincipal)
  class2 <- toupper(varSecundaria)
  if (!all(class %in% datos$Variable, class2 %in% datos$Variable)) {
    stop("\u00a1Por favor aseg\u00farese que tanto la variable principal como la secundaria se encuentren dentro de la columna 'Variable'!", call. = FALSE)
  }
  if (!all(is.character(titulo), is.character(label), is.character(textInfo))) {
    stop("\u00a1Los argumentos 'titulo', 'label' y 'textInfo' deben ser una cadena de texto!", call. = FALSE)
  }
  LegendTitle <- ifelse(is.null(estilo$LegendTitle), "", estilo$LegendTitle)
  Outside <- paste("Total de", label); Inside <- paste("N\u00famero de", label)

  # CREACIÓN DEL DATAFRAME CON EL CUAL SE CREARÁ LA GRÁFICA
  if (all(missingArg(ano), missingArg(periodo))) {
    df <- ungroup(datos) |> filter(Variable == varPrincipal, !is.na(Clase))    |>
      group_by(Clase) |> summarise(Total = sum(Total)) |> arrange(desc(Total)) |>
      mutate(Clase = fct_drop(Clase), drilldown = c(NA, "S\u00ed"))

    df2 <- ungroup(datos) |> filter(Variable == varSecundaria, !is.na(Clase)) |>
      group_by(Clase) |> summarise(Total = sum(Total)) |> arrange(desc(Total))
  } else {
    titulo <- ifelse(!missingArg(titulo) && addPeriodo, paste0(titulo, " (Periodo ", ano, "-", periodo, ")"), titulo)
    df <- ungroup(datos) |>
      filter(Variable == varPrincipal, !is.na(Clase), YEAR == ano, SEMESTRE == periodo) |>
      arrange(desc(Total)) |> select(-Variable, -YEAR, -SEMESTRE) |>
      mutate(Clase = fct_drop(Clase), drilldown = c(NA, "S\u00ed"))

    df2 <- ungroup(datos) |>
      filter(Variable == varSecundaria, !is.na(Clase), YEAR == ano, SEMESTRE == periodo) |>
      arrange(desc(Total)) |> select(-Variable, -YEAR, -SEMESTRE)
  }
  categorias  <- df  |> select(Clase) |> distinct() |> pull()
  categorias2 <- df2 |> select(Clase) |> distinct() |> pull()

  # CREACIÓN DEL PLOT RETORNAR
  if (!any(missingArg(colores), length(colores) == length(categorias))) {
    stop(paste0(
      "\u00a1El n\u00famero de colores ingresados en el vector 'colores' no corresponde con el n\u00famero de categor\u00edas a colorear!",
      "\n\tNo. colores ingresados = ", length(colores), " != ", "No. de categor\u00edas = ", length(categorias)
    ), call. = FALSE)
  }
  if (missingArg(colores)) { colores <- rainbow(length(categorias), alpha = 0.7) }
  if (!(missingArg(colores2) || length(colores2) == length(categorias2))) {
    stop(paste0(
      "\u00a1El n\u00famero de colores ingresados en el vector 'colores2' no corresponde con el n\u00famero de categor\u00edas a colorear!",
      "\n\tNo. colores ingresados = ", length(colores2), " != ", "No. de categor\u00edas = ", length(categorias2)
    ), call. = FALSE)
  }
  if (missingArg(colores2)) { colores2 <- rainbow(length(categorias2), alpha = 0.7, rev = TRUE) }

  Activado <- ifelse(torta, TRUE, FALSE)
  optionsOutside <- list(
    colorByPoint = TRUE, colors = colores,
    dataLabels = list(enabled = TRUE, style = list(
      fontWeight = "bold", color = "black", fontSize = "18px"
      )
    )
  )
  optionsInside <- list(
    colorByPoint = TRUE, colors = colores2,
    dataLabels = list(enabled = TRUE, style = list(
      fontWeight = "bold", color = "black", fontSize = "18px"
      )
    )
  )

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

  if (torta) {
    InvOrientacion <- "column"

    PlotDrilldown <- df |>
      hchart(type = "pie", hcaes(x = Clase, y = Total), name = Outside, showInLegend = TRUE) |>
      hc_plotOptions(
        pie = list(
          allowPointSelect = TRUE, colorByPoint = TRUE, colors = colores,
          dataLabels = list(
            enabled = TRUE, format = "<b>{point.name}</b>: {point.percentage:.1f} %",
            style = list(fontWeight = "bold", color = "black", fontSize = "18px")
          )
        ),
        column = optionsInside
      )
  } else {
    Orientacion    <- ifelse(vertical, "column", "bar")
    InvOrientacion <- ifelse(vertical, "bar", "column")

    PlotDrilldown <- highchart() |>
      hc_add_series(df,
        type = Orientacion, colorByPoint = TRUE,
        hcaes(x = paste(Clase, "-", round(Total * 100 / sum(Total), 1), "%"), y = Total),
        name = Outside, showInLegend = FALSE
      )

    if (vertical) {
      PlotDrilldown <- PlotDrilldown |> hc_plotOptions(column = optionsOutside, bar = optionsInside)
    } else {
      PlotDrilldown <- PlotDrilldown |> hc_plotOptions(bar = optionsOutside, column = optionsInside)
    }
  }

  PlotDrilldown <- PlotDrilldown |>
    hc_title(text = titulo, style = list(fontWeight = "bold", fontSize = "22px", color = "#333333", useHTML = TRUE)) |>
    hc_xAxis(type = "category", labels = list(style = list(fontWeight = "bold", color = "black", fontSize = "18px"))) |>
    hc_yAxis(
      title = list(text = Inside, style = list(fontWeight = "bold", color = "black", fontSize = "18px")),
      labels = list(style = list(fontWeight = "bold", color = "black", fontSize = "18px"))
    ) |>
    hc_drilldown(
      drillUpButton = list(theme = list(fill = "#00C0FF", states = list(hover = list(fill = "#FFC000")))),
      activeDataLabelStyle = list(color = "#0080FF", textDecoration = "underline", fontStyle = "italic"),
      activeAxisLabelStyle = list(textDecoration = "none"),
      allowPointDrilldown = TRUE, series = list(list(
        id = "S\u00ed", name = textInfo,
        data = list_parse2(df2), type = InvOrientacion
      ))
    ) |>
    hc_exporting(enabled = TRUE, filename = paste0("PlotDrilldown_", str_to_title(varPrincipal))) |>
    hc_credits(enabled = TRUE, text = "DNPE", href = "http://estadisticas.unal.edu.co/home/") |>
    hc_legend(
      enabled = Activado, align = "center", verticalAlign = "bottom",
      title = list(text = LegendTitle, style = list(textDecoration = "underline")),
      itemStyle = list(fontWeight = "bold", color = "black", fontSize = "18px")
    ) |>
    hc_add_theme(ThemeHC)

  if (!any(missingArg(estilo), is.null(estilo$hc.Credits))) {
    PlotDrilldown <- PlotDrilldown |>
      hc_subtitle(text = estilo$hc.Credits, align = "left", style = list(color = "#2B908F", fontWeight = "bold"))
  }

  return(PlotDrilldown)
}
