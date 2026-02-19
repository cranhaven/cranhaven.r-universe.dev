test_that("Tabla() captura de algunos valores claves a retornar", {
  # Creando el df a usar, específico para pruebas y generado minuciosamente
  df <- tibble::tibble(
    year   = rep(2024:2026, each = 4),
    season = rep(1:2, times = 6),
    abbrv  = c("A","A","A","A","A","B","C","D","A","C","B","D")
  )
  # --------------------------------- DINÁMICO ---------------------------------
  output_Tabla <- Tabla(
    datos          = df,
    filtros        = TRUE,
    encabezado     = "ENCABEZADO",
    leyenda        = "LEYENDA",
    ajustarNiveles = TRUE,
    scrollX        = FALSE,
    fillContainer  = FALSE,
    colorHead      = "#FF1234",
    estatico       = FALSE
  )
  # Validando que se retorne el tipo de objeto deseado
  expect_type(output_Tabla, "list")
  expect_s3_class(output_Tabla, c("datatables", "htmlwidget"))
  # Validando algunos aspectos básicos que debe cumplir la tabla
  #   * Número de filas y columnas inalterables
  expect_equal(nrow(output_Tabla[["x"]][["data"]]), 12)
  expect_equal(ncol(output_Tabla[["x"]][["data"]]), 3)
  #   * Extensiones y opciones generales del Javascript HTML
  expect_equal(output_Tabla[["x"]][["options"]][["dom"]], "Bfrtip")
  expect_contains(unlist(output_Tabla[["x"]][["extensions"]]), c("Buttons", "KeyTable"))
  #   * Por definición la 1ra columna se ordena desc y la 2da asc
  expect_equal(output_Tabla[["x"]][["options"]][["order"]][[1]][[2]], "desc")
  expect_equal(output_Tabla[["x"]][["options"]][["order"]][[2]][[2]], "asc")
  #   * Valores booleanos que se definieron en los parámetros de la función
  expect_false(output_Tabla[["x"]][["fillContainer"]])
  expect_true(output_Tabla[["x"]][["options"]][["autoWidth"]])
  expect_true(output_Tabla[["x"]][["options"]][["keys"]])
  expect_true(output_Tabla[["x"]][["options"]][["searchHighlight"]])
  expect_false(output_Tabla[["x"]][["options"]][["scrollX"]])
  #   * Corroborando el encabezado, leyenda y color de cabecera incrustados en etiquetas HTML
  expect_snapshot(output_Tabla[["x"]][["container"]])
  expect_snapshot(output_Tabla[["x"]][["caption"]])
  expect_snapshot(output_Tabla[["x"]][["options"]][["initComplete"]][1])

  # --------------------------------- ESTÁTICO ---------------------------------
  output_Tabla <- Agregar(
    datos = df, formula = abbrv ~ year + season,
    frecuencia = list("Year" = 2024:2026, "Period" = 1:2)
  ) %>%
    Tabla(
      .,
      rows        = vars(year, season),
      pivotCat    = Clase,
      pivotVar    = Total,
      columnNames = c("Time 1", "Time 2"),
      encabezado  = "LETTERS",
      leyenda     = "LEYENDA",
      colorHead   = "#AB1234",
      estatico    = TRUE
    )
  # Validando que se retorne el tipo de objeto deseado
  expect_type(output_Tabla, "list")
  expect_s3_class(output_Tabla, c("gt_tbl", "list"))
  # Validando algunos aspectos básicos que debe cumplir la tabla
  #   * Número de filas y columnas inalterables
  expect_equal(nrow(output_Tabla[["_data"]]), 6)
  expect_equal(ncol(output_Tabla[["_data"]]), 6)
  #   * Orden de los encabezados de las columnas
  expect_identical(
    output_Tabla[["_boxhead"]][["var"]],
    c("Time 1", "Time 2", "A", "B", "C", "D")
  )
  expect_contains(
    unlist(output_Tabla[["_boxhead"]][["column_label"]]),
    c("Time 1", "Time 2", "A", "B", "C", "D")
  )
  expect_identical(
    output_Tabla[["_spanners"]][["vars"]][[1]], c("A", "B", "C", "D")
  )
  #   * Corroborando el encabezado, leyenda y color de cabecera incrustados en etiquetas HTML
  expect_equal(output_Tabla[["_spanners"]][["spanner_label"]][[1]], "LETTERS")
  expect_equal(unlist(output_Tabla[["_source_notes"]]), "LEYENDA")
  expect_equal(output_Tabla[["_options"]][["value"]][[1]], "#AB1234")
})
