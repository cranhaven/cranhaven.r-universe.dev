test_that("Plot.Boxplot() captura de algunos valores claves a retornar", {
  # Creando el df a usar, específico para pruebas y generado minuciosamente
  set.seed(42)
  df <- tibble::tibble(
    Continuous   = round(rnorm(70), 2),
    Categorical1 = rep(c('A', 'B', 'B', 'A', 'B', 'B', 'C'), 10),
    Categorical2 = rep(c('I', 'I', 'II', 'I', 'I', 'II', 'I'), 10)
  )
  df$Categorical1 <- forcats::fct_relevel(df$Categorical1, c("B", "A", "C"))
  df$Categorical2 <- forcats::fct_relevel(df$Categorical2, c("II", "I"))
  limitsY <- c(-3, 3)
  # ------------------------------- HIGHCHARTER --------------------------------
  output_PlotBoxplot <- Plot.Boxplot(
    datos    = df,
    variable = Continuous,
    grupo1   = Categorical1,
    grupo2   = Categorical2,
    outliers = TRUE,
    ylim     = limitsY,
    colores  = c("#292F6D", "#DD164C"),
    titulo   = "Title",
    labelX   = "xAxisText",
    libreria = "highcharter"
  )
  # Validando que se retorne el tipo de objeto deseado
  expect_type(output_PlotBoxplot, "list")
  expect_s3_class(output_PlotBoxplot, c("highchart", "htmlwidget"))
  # Validando algunos aspectos básicos que debe cumplir el plot
  #   * Valores booleanos que se definieron en los parámetros de la función
  expect_false(output_PlotBoxplot[["x"]][["hc_opts"]][["plotOptions"]][["boxplot"]][["colorByPoint"]])
  #   * Valores accesibles y que corroboran aspectos claves fácilmente alterables
  expect_equal(output_PlotBoxplot[["x"]][["hc_opts"]][["chart"]][["type"]]           , "column")
  expect_equal(output_PlotBoxplot[["x"]][["hc_opts"]][["title"]][["text"]]           , "Title")
  expect_equal(output_PlotBoxplot[["x"]][["hc_opts"]][["xAxis"]][["title"]][["text"]], "xAxisText")
  expect_equal(output_PlotBoxplot[["x"]][["hc_opts"]][["yAxis"]][["min"]]            , limitsY[1])
  expect_equal(output_PlotBoxplot[["x"]][["hc_opts"]][["yAxis"]][["max"]]            , limitsY[2])
  expect_equal(output_PlotBoxplot[["x"]][["hc_opts"]][["exporting"]][["filename"]]   , "PlotBoxPlot_Categorical1")
  expect_equal(output_PlotBoxplot[["x"]][["hc_opts"]][["series"]][[1]][["name"]]     , "II")
  expect_equal(output_PlotBoxplot[["x"]][["hc_opts"]][["series"]][[2]][["name"]]     , "I")
  expect_equal(output_PlotBoxplot[["x"]][["hc_opts"]][["series"]][[1]][["color"]]    , "#292F6D")
  expect_equal(output_PlotBoxplot[["x"]][["hc_opts"]][["series"]][[2]][["color"]]    , "#DD164C")
  expect_equal(output_PlotBoxplot[["x"]][["conf_opts"]][["lang"]][["downloadCSV"]]   , "Descargar CSV")
  expect_equal(output_PlotBoxplot[["x"]][["type"]], "chart")
  expect_identical(
    output_PlotBoxplot[["x"]][["hc_opts"]][["plotOptions"]][["boxplot"]][["colors"]],
    c("#292F6D", "#DD164C")
  )
  expect_snapshot(output_PlotBoxplot[["x"]][["hc_opts"]][["series"]])
})
