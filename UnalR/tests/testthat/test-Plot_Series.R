test_that("Plot.Series() captura de algunos valores claves a retornar", {
  # Creando el df a usar, específico para pruebas y generado minuciosamente
  df <- tibble::tibble(
    T1 = c( 1 ,  1 ,  1 ,  1  ,  2 ,  2  ,  2  ),
    T2 = c('I', 'I', 'I', 'II', 'I', 'II', 'II'),
    T3 = c( 0 ,  0 ,  0 ,  0  ,  0 ,  0  ,  1  ),
    ID = c('9A', '9A', '10A', '10A', '10A', '9A', '10A'),
    No = rep(1, 7)
  )
  df$ID   <- forcats::fct_relevel(df$ID, c("9A", "10A"))
  limitsY <- c(0, 3)
  # ------------------------------- HIGHCHARTER --------------------------------
  expect_warning(
    output_PlotSeries <- Plot.Series(
      datos     = df,
      tiempo    = dplyr::vars(T1, T2, T3),
      valores   = No,
      categoria = ID,
      invertir  = TRUE,
      ylim      = limitsY,
      colores   = c("#9CC414", "#0076A3"),
      titulo    = "Title",
      labelX    = "Time",
      libreria  = "highcharter"
    ), NULL
  )
  # Validando que se retorne el tipo de objeto deseado
  expect_type(output_PlotSeries, "list")
  expect_s3_class(output_PlotSeries, c("highchart", "htmlwidget"))
  # Validando algunos aspectos básicos que debe cumplir el plot
  #   * Valores booleanos que se definieron en los parámetros de la función
  expect_true(output_PlotSeries[["x"]][["hc_opts"]][["yAxis"]][["reversed"]])
  expect_true(output_PlotSeries[["x"]][["hc_opts"]][["plotOptions"]][["series"]][["showInLegend"]])
  #   * Valores accesibles y que corroboran aspectos claves fácilmente alterables
  expect_equal(output_PlotSeries[["x"]][["hc_opts"]][["chart"]][["type"]]        , "datetime")
  expect_equal(output_PlotSeries[["x"]][["hc_opts"]][["title"]][["text"]]        , "Title")
  expect_equal(output_PlotSeries[["x"]][["hc_opts"]][["yAxis"]][["min"]]         , limitsY[1])
  expect_equal(output_PlotSeries[["x"]][["hc_opts"]][["yAxis"]][["max"]]         , limitsY[2])
  expect_equal(output_PlotSeries[["x"]][["hc_opts"]][["exporting"]][["filename"]], "PlotSeries_ID")
  expect_equal(output_PlotSeries[["x"]][["hc_opts"]][["series"]][[1]][["name"]]  , "9A")
  expect_equal(output_PlotSeries[["x"]][["hc_opts"]][["series"]][[2]][["name"]]  , "10A")
  expect_equal(output_PlotSeries[["x"]][["hc_opts"]][["series"]][[1]][["color"]] , "#9CC414")
  expect_equal(output_PlotSeries[["x"]][["hc_opts"]][["series"]][[2]][["color"]] , "#0076A3")
  expect_equal(output_PlotSeries[["x"]][["conf_opts"]][["lang"]][["downloadCSV"]], "Descargar CSV")
  expect_equal(output_PlotSeries[["x"]][["hc_opts"]][["xAxis"]][["title"]][["text"]], "Time")
  expect_equal(output_PlotSeries[["x"]][["type"]], "chart")
  expect_identical(
    unlist(output_PlotSeries[["x"]][["hc_opts"]][["xAxis"]][["categories"]]),
    c("1-I-0", "1-II-0", "2-I-0", "2-II-0", "2-II-1")
  )
  # ---------------------------------- PLOTLY ----------------------------------
  expect_warning(
    output_PlotSeries <- Plot.Series(
      datos     = df,
      tiempo    = dplyr::vars(T1, T2, T3),
      valores   = No,
      categoria = ID,
      invertir  = TRUE,
      ylim      = limitsY,
      colores   = c("#9CC414", "#0076A3"),
      titulo    = "Title",
      labelX    = "Time",
      libreria  = "plotly"
    ), NULL
  )
  # Validando que se retorne el tipo de objeto deseado
  expect_type(output_PlotSeries, "list")
  expect_s3_class(output_PlotSeries, c("plotly", "htmlwidget"))
  # Validando algunos aspectos básicos que debe cumplir el plot
  #   * Configuración global especificada
  expect_equal(output_PlotSeries[["x"]][["config"]][["locale"]], "es")
  expect_equal(output_PlotSeries[["x"]][["layoutAttrs"]][[1]][["yaxis"]][["autorange"]], "reversed")
  #   * Validando la existencia y disposición de la leyenda
  expect_true(output_PlotSeries[["x"]][["layoutAttrs"]][[1]][["showlegend"]])
  expect_equal(output_PlotSeries[["x"]][["layoutAttrs"]][[1]][["legend"]][["orientation"]], "v")
  #   * Confirmando que los ejes correspondan al tipo/modo/color/orden
  expect_equal(output_PlotSeries[["x"]]$attr[2][[1]]$type      , "scatter")
  expect_equal(output_PlotSeries[["x"]]$attr[2][[1]]$mode      , "markers+lines")
  expect_equal(output_PlotSeries[["x"]]$attr[2][[1]]$line$color, "#9CC414")
  expect_equal(output_PlotSeries[["x"]]$attr[3][[1]]$type      , "scatter")
  expect_equal(output_PlotSeries[["x"]]$attr[3][[1]]$mode      , "markers+lines")
  expect_equal(output_PlotSeries[["x"]]$attr[3][[1]]$line$color, "#0076A3")
  expect_identical(levels(output_PlotSeries[["x"]]$attr[2][[1]]$name), c("9A", "10A"))
  expect_identical(levels(output_PlotSeries[["x"]]$attr[3][[1]]$name), c("9A", "10A"))
  expect_identical(
    output_PlotSeries[["x"]]$attr[2][[1]]$x, c("1-I-0", "1-II-0", "2-I-0", "2-II-0", "2-II-1")
  )
  expect_identical(
    output_PlotSeries[["x"]]$attr[3][[1]]$x, c("1-I-0", "1-II-0", "2-I-0", "2-II-0", "2-II-1")
  )
  #   * Corroborando que el título y eje conserven sus nombres ingresados
  expect_equal(output_PlotSeries[["x"]][["layoutAttrs"]][[1]][["title"]][["text"]], "<b>Title</b>")
  expect_equal(output_PlotSeries[["x"]][["layoutAttrs"]][[1]][["xaxis"]][["title"]], "Time")
  # --------------------------------- GGPPLOT2 ---------------------------------
  expect_warning(
    output_PlotSeries <- Plot.Series(
      datos     = df,
      tiempo    = dplyr::vars(T1, T2, T3),
      valores   = No,
      categoria = ID,
      ylim      = limitsY,
      colores   = c("#9CC414", "#0076A3"),
      titulo    = "Title",
      labelX    = "Time",
      labelY    = "Count",
      estatico  = TRUE,
      estilo    = list(LegendTitle = "Group:", gg.Tema = 1)
    ), NULL
  )
  expect_true(is_ggplot(output_PlotSeries))
  expect_equal(nrow(output_PlotSeries$data), 6)
  expect_equal(output_PlotSeries[["labels"]][["x"]], "Time")
  expect_equal(output_PlotSeries[["labels"]][["y"]], "Count")
  expect_equal(output_PlotSeries[["labels"]][["colour"]], "Group:")
  expect_equal(output_PlotSeries[["labels"]][["title"]] , "Title")
})
