test_that("Positividad", {

  # Leemos los datos
  datos_covid <- covidmx::datosabiertos

  # Checamos positividad de pcr-----
  positividad_agrupados <- datos_covid |>
    positividad(
      entidades = "BAJA CALIFORNIA", tipo_prueba = "PCR",
      fill_NA = FALSE, remove_inconclusive = FALSE
    )

  numero_pruebas <- datos_covid |>
    numero_pruebas(entidades = "BAJA CALIFORNIA", tipo_prueba = "PCR", fill_zeros = TRUE)

  numero_pruebas_positivas <- datos_covid |>
    numero_pruebas(
      entidades = "BAJA CALIFORNIA", tipo_prueba = "PCR", fill_zeros = TRUE,
      .grouping_vars = "RESULTADO_LAB"
    )
  numero_pruebas_positivas <- numero_pruebas_positivas$numero_pruebas |>
    dplyr::filter(RESULTADO_LAB == 1)

  # Checamos # pruebas sea
  ntests <- numero_pruebas$numero_pruebas$n
  npos <- numero_pruebas_positivas$n

  expect_equal(positividad_agrupados$positividad$n_pruebas, ntests)

  # Checamos # pruebas positivas sea
  expect_equal(positividad_agrupados$positividad$n_positivos, npos)

  # Checamos positividad
  expect_equal(positividad_agrupados$positividad$Positividad, npos / ntests)

  # Checamos positividad de igg-----
  positividad_agrupados <- datos_covid |>
    positividad(
      entidades = "BAJA CALIFORNIA", tipo_prueba = "ANTIGENO",
      fill_NA = FALSE, remove_inconclusive = FALSE
    )

  numero_pruebas <- datos_covid |>
    numero_pruebas(entidades = "BAJA CALIFORNIA", tipo_prueba = "ANTIGENO", fill_zeros = TRUE)

  numero_pruebas_positivas <- datos_covid |>
    numero_pruebas(
      entidades = "BAJA CALIFORNIA", tipo_prueba = "ANTIGENO", fill_zeros = TRUE,
      .grouping_vars = "RESULTADO_ANTIGENO"
    )
  numero_pruebas_positivas <- numero_pruebas_positivas$numero_pruebas |>
    dplyr::filter(RESULTADO_ANTIGENO == 1)

  # Checamos # pruebas sea
  ntests <- numero_pruebas$numero_pruebas$n
  npos <- numero_pruebas_positivas$n

  expect_equal(positividad_agrupados$positividad$n_pruebas, ntests)

  # Checamos # pruebas positivas sea
  expect_equal(positividad_agrupados$positividad$n_positivos, npos)

  # Checamos positividad
  expect_equal(positividad_agrupados$positividad$Positividad, npos / ntests)

  # Checamos positividad de pcr e igg conjunta-----
  positividad_agrupados <- datos_covid |>
    positividad(
      entidades = "BAJA CALIFORNIA",
      fill_NA = FALSE, remove_inconclusive = FALSE,
      group_by_tipo_prueba = FALSE
    )

  numero_pruebas <- datos_covid |>
    numero_pruebas(
      entidades = "BAJA CALIFORNIA", group_by_tipo_prueba = FALSE,
      fill_zeros = FALSE
    )

  numero_pruebas_positivas <- datos_covid |>
    numero_pruebas(
      entidades = "BAJA CALIFORNIA", fill_zeros = FALSE,
      group_by_tipo_prueba = FALSE,
      .grouping_vars = c("RESULTADO_ANTIGENO", "RESULTADO_LAB")
    )
  numero_pruebas_positivas <- numero_pruebas_positivas$numero_pruebas |>
    dplyr::filter(RESULTADO_ANTIGENO == 1 | RESULTADO_LAB == 1) |>
    dplyr::group_by(!!as.symbol("FECHA_SINTOMAS")) |>
    dplyr::summarise(n = sum(n))

  # Checamos # pruebas sea
  ntests <- numero_pruebas$numero_pruebas$n
  npos <- numero_pruebas_positivas$n

  expect_equal(positividad_agrupados$positividad$n_pruebas, ntests)

  # Checamos # pruebas positivas sea
  expect_equal(positividad_agrupados$positividad$n_positivos, npos)

  # Checamos positividad
  expect_equal(positividad_agrupados$positividad$Positividad, npos / ntests)

  # Chequeo de que sólo lea el Baja California correcto-----
  positividad_agrupados <- datos_covid |>
    positividad(entidades = "BAJA CALIFORNIA")
  entidades <- unique(positividad_agrupados$positividad$ENTIDAD_FEDERATIVA)
  expect_true(length(entidades) == 1 & entidades == "BAJA CALIFORNIA")

  # Chequeo de que sólo lea el Baja California correcto-----
  positividad_agrupados <- datos_covid |>
    positividad(entidades = "BAJA CALIFORNIA SUR")
  entidades <- unique(positividad_agrupados$positividad$ENTIDAD_FEDERATIVA)
  expect_true(length(entidades) == 1 & entidades == "BAJA CALIFORNIA SUR")

  # Chequeo de que sólo lea el IMSS correcto 1----
  positividad_agrupados <- datos_covid |>
    positividad(tipo_sector = "IMSS", group_by_tipo_sector = TRUE, fill_NA = FALSE)
  sectores <- unique(positividad_agrupados$positividad$DESCRIPCION_TIPO_SECTOR)
  expect_true(length(sectores) == 1 & sectores == "IMSS")

  # Chequeo de que sólo lea el IMSS correcto 2----
  positividad_agrupados <- datos_covid |>
    positividad(
      tipo_sector = "IMSS-BIENESTAR", group_by_tipo_sector = TRUE,
      fill_NA = FALSE
    )
  sectores <- unique(positividad_agrupados$positividad$DESCRIPCION_TIPO_SECTOR)
  expect_true(length(sectores) == 1 & sectores == "IMSS-BIENESTAR")

  # Chequeo de que sólo lea el UCI correcto 1----
  positividad_agrupados <- datos_covid |>
    positividad(tipo_uci = "NO", group_by_tipo_uci = TRUE, fill_NA = FALSE)
  ucis <- unique(positividad_agrupados$positividad$DESCRIPCION_TIPO_UCI)
  expect_true(length(ucis) == 1 & ucis == "NO")

  # Chequeo de que sólo lea el UCI correcto 1----
  positividad_agrupados <- datos_covid |>
    positividad(
      tipo_uci = "NO ESPECIFICADO", group_by_tipo_uci = TRUE,
      fill_NA = FALSE
    )
  ucis <- unique(positividad_agrupados$positividad$DESCRIPCION_TIPO_UCI)
  expect_true(length(ucis) == 1 & ucis == "NO ESPECIFICADO")

  # Chequeo del list_name----
  positividad_prueba <- datos_covid |> positividad(list_name = "Prueba")
  expect_true("Prueba" %in% names(positividad_prueba))

  # Mensaje si ya eciste el nombre----
  positividad_prueba <- datos_covid |> positividad(list_name = "Prueba")
  expect_message(positividad(positividad_prueba, list_name = "Prueba"))

  # Error si no se selecciona un tipo de antigeno----
  expect_error(positividad(datos_covid, tipo_prueba = "Y3W9EPGFOI"))
  
  # Chequeo de que no salgan agrupados----
  positividad_agrupados <- datos_covid |> 
    positividad(group_by_tipo_paciente = TRUE,
                group_by_tipo_uci = TRUE,
                group_by_tipo_sector = TRUE,
                .grouping_vars = c("SEXO","DIABETES"))
  
  expect_true(!dplyr::is.grouped_df(positividad_agrupados$dats))
})
