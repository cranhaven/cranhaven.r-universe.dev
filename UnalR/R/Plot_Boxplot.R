#' Cree un diagrama de caja/boxplot dinámico y flexible con dos diferentes paquetes
#'
#' Esta función proporciona excelentes herramientas y opciones para la visualización
#' de un diagrama de caja y bigotes (*también conocido como boxplot*) dinámico
#' con el objetivo de representar gráficamente una serie numérica a través de sus
#' cuantiles. Dicho boxplot se puede representar usando dos diferentes librerías
#' que son `Highcharter` y `Plotly`, las cuales usan internamente `JavaScript`.
#'
#' @inheritParams Plot.Torta
#' @inheritParams Plot.Series
#' @param datos Un data frame, no un vector numérico.
#' @param variable Una variable numérica dentro del data frame ingresado en `datos`.
#' @param grupo1 Una variable categórica dentro del data frame ingresado en `datos`.
#' @param grupo2 Otra variable categórica dentro del data frame ingresado en `datos`
#'   por si se desea segregar por otra clase el grupo principal.
#' @param vertical Si es `TRUE` (*valor predeterminado*) indicará que la orientación
#'   del gráfico será vertical.
#' @param outliers Si es `TRUE` (*valor predeterminado*) se mostrarán los puntos
#'   correspondientes a los datos atípicos, defínalo en `FALSE` si desea ocultar
#'   dichos puntos.
#' @param jitter Si es `TRUE` se agregará las observaciones de cada grupo con un
#'   poco de ruido aleatorio encima de las cajas, útil para mostrar la distribución
#'   subyacente de los datos. El valor por defecto es `FALSE`.
#' @param violin Si es `TRUE` se acompañará el boxplot con su diagrama de densidad,
#'   logrando ser más informativo al mostrar la distribución completa de los datos.
#'   Solo aplica para la librería `"plotly"`.
#' @param numericalVars Una lista (*ya sea creada con la sintaxis `base` o `tidy`*)
#'   con las variables numéricas dentro del data frame ingresado en `datos` con
#'   las que se desea crear un botón dinámico para desplazarse entre ellas fijando
#'   el grupo ingresado en `grupo1`.
#' @param ylim Vector numérico que especifica el límite inferior y superior,
#'   respectivamente, del eje `Y`. Si no se introduce algún valor se mostrará todo
#'   el rango disponible para dicho eje.
#' @param colores Cadena de caracteres indicando los colores con los cuales se
#'   deben colorear cada una de las trazas correspondiente a cada nivel del argumento
#'   `grupo1`. Si no se introduce algún vector se usará la paleta `rainbow` por defecto.
#' @param sizeOutlier Valor numérico que indica el tamaño de los puntos considerados
#'   como atípicos, por defecto se tiene un valor específico al que se le sumará
#'   el ingresado acá.
#' @param colOutlier Cadena de caracteres indicando el color de los puntos
#'   considerados como atípicos, por defecto se pintarán de un azul rey.
#' @param textBox Cadena de caracteres indicando el nombre de la serie numérica
#'   con la que se construye las cajas, necesario únicamente si se especifica
#'   solamente el `grupo1`, para el caso en que se tenga dos grupos no tendrá
#'   ningún efecto en el plot.
#' @param estilo Lista compuesta por varios parámetros, los cuales van a ser usados
#'   de acuerdo con la librería especificada para graficar el boxplot y cuyo
#'   objetivo es personalizar pequeños detalles de éste.
#'   * `LegendTitle`, `hc.Tema` y `hc.Credits`: Igual uso que en [Plot.Series()]
#'   * `ply.Interaction`, `ply.LegendPosition` y `ply.Credits`: Igual uso que en
#'     [Plot.Series()]
#'   * `gg.Tema`, `gg.Legend` y `gg.Texto`: Igual uso que en [Plot.Series()]
#'   * `gg.VarWidth`: Si es `FALSE` (\emph{valor predeterminado}) se realizará
#'     un diagrama de caja estándar. Si es `TRUE`, las cajas se dibujan con anchos
#'     proporcionales a las raíces cuadradas del número de observaciones en los
#'     grupos. Para más información, consulte la función [geom_boxplot()][ggplot2::geom_boxplot()]).
#'   * `gg.OutShape`: Modifica la forma de los outliers (\emph{datos atípicos}).
#'     Los posibles valores son un número entero entre \eqn{[1, 23]}. Para más
#'     información consulte la función [geom_boxplot()][ggplot2::geom_boxplot()]).
#'   * `gg.JitWidth`: Valor numérico que indica el tamaño del ancho en el que los
#'     puntos pueden dispersarse en cada una de las cajas El valor por defecto es
#'     `0.25`. Para más información, consulte la función [geom_jitter()][ggplot2::geom_jitter()]).
#'   * `gg.JitSize`: Valor numérico que indica el tamaño de los jittered points.
#'     El valor por defecto es `0.4`. Para más información, consulte la función
#'     [geom_jitter()][ggplot2:: geom_jitter()]).
#'
#' @details
#' El argumento `numericalVars` funciona solamente con la librería `"plotly"`,
#' pues la función de crear los botones dinámicos es procedente de dicha librería.
#'
#' @returns
#' Retorna el boxplot (*objeto widget de HTML*) creado. La clase del objeto retornado
#' será un "htmlwidget" y dependiendo de la librería usada pertenecerá adicionalmente
#' a la clase "highchart" o "plotly".
#'
#' @examplesIf require("pals")
#' Txt <- "EVOLUCI\u00d3N DEL PUNTAJE EN EL EXAMEN DE ADMISI\u00d3N"
#' Msj <- "Aspirantes a pregrado (<i>no se incluye los datos at\u00edpicos</i>)"
#  # library(pals)
#' Plot.Boxplot(
#'   datos       = ejMiniAspirantesPre,
#'   variable    = PTOTAL,
#'   grupo1      = Serie,
#'   outliers    = FALSE,
#'   ylim        = c(0, 1000),
#'   colores     = pals::jet(30),
#'   sizeOutlier = 1,
#'   colOutlier  = "#FF3366",
#'   titulo      = Txt,
#'   labelY      = "Puntaje",
#'   textBox     = "Score",
#'   libreria    = "highcharter",
#'   estilo      = list(hc.Tema = 2, hc.Credits = Msj)
#' )
#'
#' @examples
#' # ---------------------------------------------------------------------------
#' Txt <- "EVOLUCI\u00d3N DEL PUNTAJE EN EL EXAMEN DE ADMISI\u00d3N"
#' Msj2 <- paste(
#'   "Aspirantes a pregrado",
#'   "(<i>cada periodo se encuentra segregado por el tipo de admisi\u00f3n</i>)"
#' )
#' Plot.Boxplot(
#'   datos    = ejMiniAspirantesPre,
#'   variable = PTOTAL,
#'   grupo1   = Serie,
#'   grupo2   = TIPO_INS,
#'   outliers = TRUE,
#'   ylim     = c(0, 1000),
#'   colores  = c("#00ABFF", "#F3224B", "#FCD116", "#29DF2C"),
#'   titulo   = Txt,
#'   labelY   = "Puntaje",
#'   libreria = "highcharter",
#'   estilo   = list(LegendTitle = "Programa:", hc.Tema = 6, hc.Credits = Msj2)
#' )
#' # ---------------------------------------------------------------------------
#' Plot.Boxplot(
#'   datos    = iris,
#'   variable = Sepal.Length,
#'   grupo1   = Species,
#'   violin   = TRUE,
#'   colores  = c("#FF1D58", "#FDB911", "#00E527"),
#'   titulo   = "BOXPLOT DE LA LONGITUD DEL S\u00c9PALO | IRIS DATASET",
#'   labelX   = "Especie",
#'   labelY   = "Longitud del S\u00e9palo",
#'   libreria = "plotly"
#' )
#' # ---------------------------------------------------------------------------
#' Plot.Boxplot(
#'   datos       = ejMiniAspirantesPre,
#'   variable    = PTOTAL,
#'   grupo1      = Serie,
#'   grupo2      = TIPO_INS,
#'   jitter      = TRUE,
#'   ylim        = c(0, 1000),
#'   colores     = c("#00ABFF", "#F3224B", "#FCD116", "#29DF2C"),
#'   sizeOutlier = 0,
#'   colOutlier  = "#D3D3D3",
#'   titulo      = Txt,
#'   labelY      = "Puntaje",
#'   libreria    = "plotly",
#'   estilo      = list(
#'     LegendTitle = "Programa:", ply.Interaction = "closest",
#'     ply.LegendPosition = list(x = 0.16, y = -0.25, orientation = "h"),
#'     ply.Credits = list(x = 0.4, y = 0.95, text = Msj2)
#'   )
#' ) -> Advertencia
#' suppressWarnings(print(Advertencia))
#'
#' @examplesIf require("dplyr")
#' # ---------------------------------------------------------------------------
#' # library(dplyr)
#' df <- ejSaberPro2020 |>
#'   select(SEDE_NOMBRE_ADM, PUNTAJE_GLOBAL, PUNT_RAZO_CUANT, PUNT_INGLES,
#'          PUNT_LECT_CRIT, PUNT_COMP_CIUD, PUNT_COMU_ESCR
#'          )
#' Numericas <- vars(PUNT_RAZO_CUANT, PUNT_INGLES, PUNT_LECT_CRIT, PUNT_COMP_CIUD, PUNT_COMU_ESCR)
#' # Numericas <- c("PUNT_RAZO_CUANT", "PUNT_INGLES", ... , "PUNT_COMU_ESCR")
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
#' Plot.Boxplot(
#'   datos         = df,
#'   variable      = PUNTAJE_GLOBAL,
#'   grupo1        = SEDE_NOMBRE_ADM,
#'   numericalVars = Numericas,
#'   colores       = misColores,
#'   libreria      = "plotly"
#' )
#'
#' @examplesIf require("pals")
#' # ---------------------------------------------------------------------------
#' # Ejemplo usando el caso estático (ggplot2)
#' # library(pals)
#' Plot.Boxplot(
#'   datos      = ejMiniAspirantesPre,
#'   variable   = PTOTAL,
#'   grupo1     = Serie,
#'   jitter     = TRUE,
#'   colores    = pals::turbo(30),
#'   colOutlier = "#CBB8FF",
#'   titulo     = gsub("L E", "L\nE", Txt),
#'   labelY     = "Puntaje",
#'   textBox    = "Score",
#'   estatico   = TRUE,
#'   estilo     = list(
#'     gg.Tema = 11, gg.Legend = list(legend.position = "none"),
#'     gg.Texto = list(
#'       subtitle = gsub("<|/|i>", "", Msj ),
#'       caption  = "Información Disponible desde 2008-1", tag = "\u00ae"
#'     ),
#'     gg.VarWidth = TRUE, gg.JitWidth = 0.08, gg.JitSize = 0.05
#'   )
#' )
#'
#' @examples
#' # ---------------------------------------------------------------------------
#' Plot.Boxplot(
#'   datos       = ejMiniAspirantesPre,
#'   variable    = PTOTAL,
#'   grupo1      = Serie,
#'   grupo2      = TIPO_INS,
#'   outliers    = TRUE,
#'   colores     = c("#00ABFF", "#F3224B", "#FCD116", "#29DF2C"),
#'   sizeOutlier = 2,
#'   colOutlier  = "#F5E8E8",
#'   titulo      = gsub("L E", "L\nE", Txt),
#'   labelY      = "Puntaje",
#'   textBox     = "Score",
#'   estatico    = TRUE,
#'   estilo      = list(
#'     LegendTitle = "NIVEL ACAD\u00c9MICO:", gg.Tema = 9, gg.OutShape = 21,
#'     gg.Legend = list(legend.position = "bottom", legend.direction = "horizontal"),
#'     gg.Texto = list(
#'       subtitle = gsub("<|/|i>", "", Msj2),
#'       caption  = "Información Disponible desde 2008-1", tag = "\u00ae"
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
#' @import dplyr
#' @importFrom cli cli_h1 bg_yellow col_black cli_alert_info cli_alert cli_text cli_alert_success symbol
#' @importFrom methods missingArg
#' @importFrom grDevices rainbow
Plot.Boxplot <- function(
  datos,
  variable,
  grupo1,
  grupo2,
  vertical = TRUE,
  outliers = TRUE,
  jitter = FALSE,
  violin = FALSE,
  numericalVars,
  ylim,
  colores,
  sizeOutlier = 0,
  colOutlier = "#08306B",
  titulo = "",
  labelX = "Periodo",
  labelY = "",
  textBox = "",
  libreria = c("highcharter", "plotly"),
  estilo = NULL,
  estatico = FALSE
) {
  # COMANDOS DE VERIFICACIÓN Y VALIDACIÓN
  if (missingArg(datos) || missingArg(variable) || missingArg(grupo1)) {
    stop(
      "\u00a1Por favor introduzca un conjunto de datos, una variable num\u00e9rica y un grupo con los cuales se graficar\u00e1!",
      call. = FALSE
    )
  }
  if (
    !all(
      is.logical(vertical),
      is.logical(outliers),
      is.logical(jitter),
      is.logical(violin),
      is.logical(estatico)
    )
  ) {
    stop(
      "\u00a1Los argumentos 'vertical', 'outliers', 'jitter', 'violin' y 'estatico' deben ser un valor booleano (TRUE o FALSE)!",
      call. = FALSE
    )
  }
  if (
    !all(
      is.character(titulo),
      is.character(labelX),
      is.character(labelY),
      is.character(textBox)
    )
  ) {
    stop(
      "\u00a1Los argumentos 'titulo', 'labelX', 'labelY' y 'textBox' deben ser una cadena de texto!",
      call. = FALSE
    )
  }
  if (!estatico) {
    if (missingArg(libreria)) {
      warning(
        "\u00a1Se usar\u00e1 la librer\u00eda 'highcharter' por defecto para realizar el plot!",
        call. = FALSE
      )
      libreria <- "highcharter"
    } else {
      libreria <- tolower(libreria)
      if (libreria %NotIN% c("highcharter", "plotly")) {
        stop(
          "\u00a1Por favor introduzca el nombre de una librer\u00eda valida (paquete usado para realizar la gr\u00e1fica)!",
          call. = FALSE
        )
      }
    }
  }
  if (!missingArg(ylim)) {
    if (!(is.numeric(ylim) && length(ylim) == 2)) {
      stop(
        "\u00a1Por favor introduzca un vector de longitud dos que definen los l\u00edmites del eje Y!",
        call. = FALSE
      )
    }
    yLim <- ylim
  } else {
    yLim <- NULL
  }

  # AJUSTES Y CONDICIONALES PRELIMINARES POR CONSIDERAR
  LegendTitle <- ifelse(is.null(estilo$LegendTitle), "", estilo$LegendTitle)
  Puntos <- ifelse(jitter, "all", ifelse(outliers, "outliers", FALSE))

  if (outliers) {
    OutShape <- ifelse(
      !(missingArg(estilo) || is.null(estilo$gg.OutShape)),
      estilo$gg.OutShape,
      19
    )
    atipicos <- list(
      outlier.color = colOutlier,
      outlier.size = sizeOutlier,
      outlier.shape = OutShape
    )
  } else {
    atipicos <- list(outlier.shape = NA)
  }
  VarWidth <- ifelse(
    !(missingArg(estilo) || is.null(estilo$gg.VarWidth)),
    estilo$gg.VarWidth,
    FALSE
  )

  datos <- datos |> mutate({{ grupo1 }} := factor({{ grupo1 }}))
  if (vertical) {
    miniDots <- list(
      data = datos,
      x = rlang::enquo(grupo1),
      y = rlang::enquo(variable)
    )
  } else {
    miniDots <- list(
      data = datos,
      y = rlang::enquo(grupo1),
      x = rlang::enquo(variable)
    )
  }

  # BUG: Ordenando los niveles del eje X, para permitir que sean factores con niveles personalizados
  if (is.null(levels(datos |> select({{ grupo1 }}) |> pull()))) {
    datos <- datos |> mutate({{ grupo1 }} := forcats::as_factor({{ grupo1 }}))
    datos <- datos |>
      mutate({{ grupo1 }} := forcats::fct_relevel({{ grupo1 }}, sort))
  } else {
    datos <- datos |>
      mutate(
        {{ grupo1 }} := forcats::fct_relevel(
          {{ grupo1 }},
          levels(datos |> select({{ grupo1 }}) |> pull())
        )
      )
  }

  if (!missingArg(grupo2)) {
    datos <- datos |> select({{ variable }}, {{ grupo1 }}, {{ grupo2 }})
    Levels <- datos |> select({{ grupo2 }}) |> distinct() |> pull()
    if (!(missingArg(colores) || length(colores) == length(Levels))) {
      stop(
        paste0(
          "\u00a1El n\u00famero de colores ingresados en el vector 'colores' no corresponde con el n\u00famero de categor\u00edas a colorear!",
          "\n\tNo. colores ingresados = ",
          length(colores),
          " != ",
          "No. de categor\u00edas = ",
          length(Levels)
        ),
        call. = FALSE
      )
    }
    if (missingArg(colores)) {
      colores <- rainbow(length(Levels), alpha = 0.7)
    }

    ColxPunto <- FALSE
    ShowLegend <- TRUE
    Intento <- try(
      data_to_boxplot(
        data = datos,
        variable = {{ variable }},
        group_var = {{ grupo1 }},
        group_var2 = {{ grupo2 }},
        add_outliers = outliers,
        color = colores
      ),
      silent = TRUE
    )
    if (any(class(Intento) == "try-error")) {
      dfBoxPlot <- data_to_boxplot(
        data = datos,
        variable = {{ variable }},
        group_var = {{ grupo1 }},
        group_var2 = {{ grupo2 }},
        add_outliers = FALSE,
        color = colores
      )
    } else {
      dfBoxPlot <- Intento
    }

    if (all(!estatico, libreria == "highcharter")) {
      rm <- dfBoxPlot |> mutate(l = lengths(data))
      if (length(unique(rm$l)) != 1) {
        allGrid <- expand_grid(
          G1 = datos |> select({{ grupo1 }}) |> distinct() |> pull(),
          G2 = datos |> select({{ grupo2 }}) |> distinct() |> pull()
        )
        allGrid <- allGrid |> rename({{ grupo1 }} := G1, {{ grupo2 }} := G2)
        dataComplete <- left_join(
          allGrid,
          datos,
          by = join_by({{ grupo1 }}, {{ grupo2 }})
        )
        varsInput <- datos |> select({{ grupo1 }}, {{ grupo2 }}) |> colnames()
        message({
          cli::cli_h1(cli::bg_yellow(cli::col_black("PRECAUCI\u00d3N")))
          cli::cli_alert_info(
            "Alerta: Inconsistencias encontradas en el dataframe ingresado.",
            wrap = TRUE
          )
          cli::cli_alert(
            "Su df no contiene informaci\u00f3n para todas las posibles combinaciones entre: {.val {varsInput}}.",
            wrap = TRUE
          )
          cli::cli_text(
            "{cli::symbol$star} Para que todo funcione correctamente con el paquete {.pkg highcharter} es necesario agregar:"
          )
          rowsAdd <- setdiff(dataComplete, datos)
          cli::cli_alert_success(
            "Se tuvo que agregar {nrow(rowsAdd)} fil{?a/as}."
          )
          # print(rowsAdd)
        })
        Intento <- try(
          data_to_boxplot(
            data = dataComplete,
            variable = {{ variable }},
            group_var = {{ grupo1 }},
            group_var2 = {{ grupo2 }},
            add_outliers = outliers,
            color = colores
          ),
          silent = TRUE
        )
        if (any(class(Intento) == "try-error")) {
          dfBoxPlot <- data_to_boxplot(
            data = dataComplete,
            variable = {{ variable }},
            group_var = {{ grupo1 }},
            group_var2 = {{ grupo2 }},
            add_outliers = FALSE,
            color = colores
          )
        } else {
          dfBoxPlot <- Intento
        }
      }
      rm(allGrid, dataComplete, rm, rowsAdd)
    }

    datos <- datos |> mutate({{ grupo2 }} := factor({{ grupo2 }}))
    ggBase <- list(
      datos,
      aes(x = {{ grupo1 }}, y = {{ variable }}, fill = {{ grupo2 }})
    )
    ggShow <- "right"
    # -.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.
    TypeGroup <- "group"
    if (!violin) {
      Dots <- list(
        type = "box",
        color = rlang::enquo(grupo2),
        colors = colores,
        boxpoints = Puntos,
        pointpos = 0,
        jitter = 0.4,
        marker = list(color = colOutlier, size = 2 + sizeOutlier)
      )
      Dots <- append(miniDots, Dots)
      tempBoxPlot <- do.call(ggplot, ggBase) +
        do.call(
          geom_boxplot,
          append(list(na.rm = TRUE, varwidth = VarWidth), atipicos)
        )
    } else {
      Dots <- list(
        type = "violin",
        color = rlang::enquo(grupo2),
        colors = colores,
        box = list(visible = FALSE),
        meanline = list(visible = TRUE)
      )
      Dots <- append(miniDots, Dots)
      tempBoxPlot <- do.call(ggplot, ggBase) + geom_violin(na.rm = TRUE)
    }
  } else {
    Levels <- datos |> select({{ grupo1 }}) |> distinct() |> pull()
    if (!(missingArg(colores) || length(colores) == length(Levels))) {
      stop(
        paste0(
          "\u00a1El n\u00famero de colores ingresados en el vector 'colores' no corresponde con el n\u00famero de categor\u00edas a colorear!",
          "\n\tNo. colores ingresados = ",
          length(colores),
          " != ",
          "No. de categor\u00edas = ",
          length(Levels)
        ),
        call. = FALSE
      )
    }
    if (missingArg(colores)) {
      colores <- rainbow(length(Levels), alpha = 0.7)
    }

    ColxPunto <- TRUE
    ShowLegend <- FALSE
    dfBoxPlot <- data_to_boxplot(
      data = datos,
      variable = {{ variable }},
      group_var = {{ grupo1 }},
      add_outliers = outliers,
      name = textBox
    )
    ggBase <- list(
      datos,
      aes(x = {{ grupo1 }}, y = {{ variable }}, fill = {{ grupo1 }})
    )
    ggShow <- "none"
    # -.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.
    TypeGroup <- NULL
    if (!violin) {
      Dots <- list(
        type = "box",
        color = rlang::enquo(grupo1),
        colors = colores,
        name = textBox,
        showlegend = FALSE,
        boxpoints = Puntos,
        pointpos = 0,
        jitter = 0.4,
        marker = list(color = colOutlier, size = 2 + sizeOutlier)
      )
      Dots <- append(miniDots, Dots)
      tempBoxPlot <- do.call(ggplot, ggBase) +
        do.call(
          geom_boxplot,
          append(list(na.rm = TRUE, varwidth = VarWidth), atipicos)
        )
    } else {
      Dots <- list(
        type = "violin",
        color = rlang::enquo(grupo1),
        colors = colores,
        name = textBox,
        showlegend = FALSE,
        box = list(visible = FALSE),
        meanline = list(visible = TRUE)
      )
      Dots <- append(miniDots, Dots)
      tempBoxPlot <- do.call(ggplot, ggBase) + geom_violin(na.rm = TRUE)
    }
  }

  # CREACIÓN DEL PLOT A RETORNAR -----------------------------------------------
  if (!estatico) {
    if (libreria == "highcharter") {
      Spanish.Highcharter()
      if (!(missingArg(estilo) || is.null(estilo$hc.Tema))) {
        ThemeHC <- switch(
          estilo$hc.Tema,
          "1" = hc_theme_ffx(),
          "2" = hc_theme_google(),
          "3" = hc_theme_tufte(),
          "4" = hc_theme_538(),
          "5" = hc_theme_ggplot2(),
          "6" = hc_theme_economist(),
          "7" = hc_theme_sandsignika(),
          "8" = hc_theme_ft(),
          "9" = hc_theme_superheroes(),
          "10" = hc_theme_flatdark()
        )
      } else {
        ThemeHC <- hc_theme_flat()
      }

      Orientacion <- ifelse(vertical, "column", "bar")
      PlotBoxPlot <- highchart() |>
        hc_chart(type = Orientacion) |>
        hc_xAxis(
          type = "category",
          title = list(
            text = labelX,
            offset = 70,
            style = list(
              fontWeight = "bold",
              fontSize = "18px",
              color = "black"
            )
          ),
          align = "center",
          lineColor = "#787878",
          opposite = FALSE,
          labels = list(
            style = list(
              fontWeight = "bold",
              color = "black",
              fontSize = "18px"
            )
          )
        ) |>
        hc_add_series_list(dfBoxPlot) |>
        hc_plotOptions(
          boxplot = list(colorByPoint = ColxPunto, colors = colores),
          series = list(
            marker = list(fillColor = colOutlier, radius = 1.5 + sizeOutlier)
          )
        ) |>
        hc_title(
          text = titulo,
          style = list(
            fontWeight = "bold",
            fontSize = "22px",
            color = "#333333",
            useHTML = TRUE
          )
        ) |>
        hc_yAxis(
          title = list(
            text = labelY,
            offset = 70,
            style = list(
              fontWeight = "bold",
              fontSize = "18px",
              color = "black"
            )
          ),
          lineColor = "#787878",
          opposite = FALSE,
          lineWidth = 1,
          min = yLim[1],
          max = yLim[2],
          labels = list(
            style = list(
              fontWeight = "bold",
              color = "black",
              fontSize = "18px"
            )
          )
        ) |>
        hc_credits(
          enabled = TRUE,
          text = "DNPE",
          href = "http://estadisticas.unal.edu.co/home/"
        ) |>
        # https://github.com/jbkunst/highcharter/issues/331
        hc_exporting(
          enabled = TRUE,
          filename = paste0("PlotBoxPlot_", as_label(enquo(grupo1)))
        ) |>
        hc_legend(
          enabled = ShowLegend,
          align = "center",
          verticalAlign = "bottom",
          layout = "horizontal",
          title = list(
            text = LegendTitle,
            style = list(textDecoration = "underline")
          ),
          x = 42,
          y = 0,
          itemStyle = list(
            fontWeight = "bold",
            color = "black",
            fontSize = "18px"
          )
        ) |>
        hc_add_theme(ThemeHC)

      if (!(missingArg(estilo) || is.null(estilo$hc.Credits))) {
        PlotBoxPlot <- PlotBoxPlot |>
          hc_subtitle(
            text = estilo$hc.Credits,
            align = "left",
            style = list(color = "#2B908F", fontWeight = "bold")
          )
      }
    } else if (libreria == "plotly") {
      if (!missingArg(numericalVars)) {
        df <- as.data.frame(datos)
        CrearBotones <- function(df, listVars) {
          lapply(
            listVars,
            FUN = function(varName, df) {
              boton <- list(
                method = "restyle",
                args = list("y", list(df[, varName])),
                label = sprintf("Mostrar: %s", varName)
              )
            },
            df
          )
        }
        Dots <- list(
          data = df,
          x = rlang::enquo(grupo1),
          y = rlang::enquo(variable),
          type = "violin",
          color = rlang::enquo(grupo1),
          colors = colores,
          box = list(visible = TRUE),
          meanline = list(visible = TRUE)
        )
      }
      # -.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.
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
      Hovermode <- ifelse(
        !(missingArg(estilo) || is.null(estilo$ply.Interaction)),
        estilo$ply.Interaction,
        ifelse(missingArg(grupo2), "x", "closest")
      )

      # Arial | Open Sans | Courier New, monospace
      FamilyAxis <- list(
        family = "Old Standard TT, serif",
        size = 16,
        color = "#525252"
      )
      FamilyTitle <- list(family = "Open Sans", size = 24, color = "#333333")

      Title <- list(
        text = paste0("<b>", titulo, "</b>"),
        font = FamilyTitle,
        y = 0.96
      )
      Xaxis <- list(
        title = paste0("<i>", labelX, "</i>"),
        zeroline = FALSE,
        showline = TRUE,
        showgrid = FALSE,
        linecolor = "#787878",
        linewidth = 2.5,
        autotick = FALSE,
        ticks = "outside",
        tickwidth = 2.5,
        ticklen = 10,
        tickcolor = "#CCCCCC",
        tickangle = -45,
        tickfont = FamilyAxis,
        showticklabels = TRUE
      )
      Yaxis <- list(
        title = labelY,
        zeroline = TRUE,
        showline = TRUE,
        showgrid = TRUE,
        linecolor = "#787878",
        linewidth = 3,
        range = yLim,
        tickfont = FamilyAxis,
        showticklabels = TRUE,
        separatethousands = TRUE
      )

      if (!vertical) {
        c <- Xaxis
        Xaxis <- Yaxis
        Yaxis <- c
      }

      PlotBoxPlot <- do.call(plot_ly, Dots) |>
        layout(
          boxmode = TypeGroup,
          violinmode = TypeGroup,
          title = Title,
          xaxis = Xaxis,
          yaxis = Yaxis,
          autosize = TRUE,
          showlegend = TRUE,
          legend = append(
            ParmsLegend,
            list(
              traceorder = "normal",
              title = list(text = paste0("<b>", LegendTitle, "</b>"))
            )
          ),
          hovermode = Hovermode,
          annotations = append(
            ParmsCredits,
            list(
              showarrow = FALSE,
              xref = "paper",
              yref = "paper",
              xanchor = "right",
              yanchor = "auto",
              xshift = 0,
              yshift = 0,
              font = list(size = 12, color = "#CCCCCC")
            )
          )
        ) |>
        config(locale = "es")

      if (!missingArg(numericalVars)) {
        PlotBoxPlot <- PlotBoxPlot |>
          layout(
            updatemenus = list(list(
              buttons = CrearBotones(df, vars2vec(numericalVars))
            ))
          )
      }
    }
  } else {
    if (!(missingArg(estilo) || is.null(estilo$gg.Tema))) {
      ThemeGG <- switch(
        estilo$gg.Tema,
        "1" = theme_light(),
        "2" = theme_bw(),
        "3" = theme_classic(),
        "4" = theme_linedraw(),
        "5" = theme_gray(),
        "6" = ggthemes::theme_hc(),
        "7" = ggthemes::theme_pander(),
        "8" = ggthemes::theme_gdocs(),
        "9" = ggthemes::theme_fivethirtyeight(),
        "10" = ggthemes::theme_economist(),
        "11" = ggthemes::theme_solarized()
      )
    } else {
      ThemeGG <- theme_DNPE()
    }

    if (!(missingArg(estilo) || is.null(estilo$gg.Legend))) {
      ParmsLegend <- estilo$gg.Legend
    } else {
      ParmsLegend <- list(
        legend.position = ggShow,
        legend.direction = "vertical"
      )
    }
    if (!(missingArg(estilo) || is.null(estilo$gg.Texto))) {
      ParmsLabs <- estilo$gg.Texto
    } else {
      ParmsLabs <- list(subtitle = NULL, caption = NULL, tag = NULL)
    }

    PlotBoxPlot <- tempBoxPlot +
      scale_fill_manual(values = colores) +
      labs(
        title = titulo,
        subtitle = ParmsLabs$subtitle,
        x = labelX,
        y = labelY,
        caption = ParmsLabs$caption,
        tag = ParmsLabs$tag,
        fill = LegendTitle
      ) +
      scale_x_discrete(guide = guide_axis(angle = 45)) +
      scale_y_continuous(limits = yLim) +
      ThemeGG +
      do.call(theme, ParmsLegend)

    if (jitter) {
      JitWidth <- ifelse(
        !(missingArg(estilo) || is.null(estilo$gg.JitWidth)),
        estilo$gg.JitWidth,
        0.25
      )
      JitSize <- ifelse(
        !(missingArg(estilo) || is.null(estilo$gg.JitSize)),
        estilo$gg.JitSize,
        0.4
      )
      PlotBoxPlot <- PlotBoxPlot +
        geom_jitter(width = JitWidth, size = JitSize, color = colOutlier)
    }

    if (!vertical) {
      PlotBoxPlot <- PlotBoxPlot + coord_flip()
    }
  }

  return(PlotBoxPlot)
}
