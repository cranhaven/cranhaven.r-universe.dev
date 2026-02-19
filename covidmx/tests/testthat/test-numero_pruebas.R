test_that("Num pruebas", {

  # Leemos los datos
  datos_covid <- covidmx::datosabiertos

  # Antigeno-----
  numero_pruebas_agrupados <- datos_covid |>
    numero_pruebas(fill_zeros = FALSE, tipo_prueba = "Antigeno")

  # Agrupamos usando dplyr clásico
  igg <- datos_covid$dats |>
    dplyr::filter(!!as.symbol("TOMA_MUESTRA_ANTIGENO") == 1) |>
    dplyr::mutate(TIPO_PRUEBA = "ANTIGENO") |>
    dplyr::group_by_at("FECHA_SINTOMAS") |>
    dplyr::group_by_at("ENTIDAD_UM", .add = TRUE) |>
    dplyr::group_by_at("TIPO_PRUEBA", .add = TRUE) |>
    dplyr::tally() |>
    dplyr::left_join(datos_covid$dict$ENTIDAD_UM, by = c("ENTIDAD_UM" = "CLAVE_ENTIDAD")) |>
    dplyr::ungroup()

  # Verificamos igualdad
  expect_true(dplyr::all_equal(numero_pruebas_agrupados$numero_pruebas, igg))

  # PCR-----
  numero_pruebas_agrupados <- datos_covid |>
    numero_pruebas(fill_zeros = FALSE, tipo_prueba = "PCR")

  # Agrupamos usando dplyr clásico
  pcr <- datos_covid$dats |>
    dplyr::filter(!!as.symbol("TOMA_MUESTRA_LAB") == 1) |>
    dplyr::mutate(TIPO_PRUEBA = "PCR") |>
    dplyr::group_by_at("FECHA_SINTOMAS") |>
    dplyr::group_by_at("ENTIDAD_UM", .add = TRUE) |>
    dplyr::group_by_at("TIPO_PRUEBA", .add = TRUE) |>
    dplyr::tally() |>
    dplyr::left_join(datos_covid$dict$ENTIDAD_UM, by = c("ENTIDAD_UM" = "CLAVE_ENTIDAD")) |>
    dplyr::ungroup()

  # Verificamos igualdad
  expect_true(dplyr::all_equal(numero_pruebas_agrupados$numero_pruebas, pcr))

  # PCR y ANTIGENO-----
  numero_pruebas_agrupados <- datos_covid |>
    numero_pruebas(fill_zeros = FALSE)

  # Verificamos igualdad
  expect_true(dplyr::all_equal(numero_pruebas_agrupados$numero_pruebas, dplyr::bind_rows(pcr, igg)))


  # Chequeo de agrupacion sin entidad----
  numero_pruebas_agrupados <- datos_covid |>
    numero_pruebas(fill_zeros = FALSE, group_by_entidad = FALSE, tipo_prueba = "PCR")

  # Agrupamos usando dplyr clásico
  numero_pruebas_agrupados_al_natural <- datos_covid$dats |>
    dplyr::filter(!!as.symbol("TOMA_MUESTRA_LAB") == 1) |>
    dplyr::mutate(TIPO_PRUEBA = "PCR") |>
    dplyr::group_by_at("FECHA_SINTOMAS") |>
    dplyr::group_by_at("TIPO_PRUEBA", .add = TRUE) |>
    dplyr::tally()

  # Verificamos igualdad
  expect_true(dplyr::all_equal(numero_pruebas_agrupados$numero_pruebas, numero_pruebas_agrupados_al_natural))

  # Chequeo de agrupacion defuncion----
  numero_pruebas_agrupados <- datos_covid |>
    numero_pruebas(
      fill_zeros = FALSE, group_by_entidad = FALSE,
      fecha_tipo = "Defuncion", tipo_prueba = "PCR"
    )

  # Agrupamos usando dplyr clásico
  numero_pruebas_agrupados_al_natural <- datos_covid$dats |>
    dplyr::filter(!!as.symbol("TOMA_MUESTRA_LAB") == 1) |>
    dplyr::mutate(TIPO_PRUEBA = "PCR") |>
    dplyr::group_by_at("FECHA_DEF") |>
    dplyr::group_by_at("TIPO_PRUEBA", .add = TRUE) |>
    dplyr::tally()


  # Verificamos igualdad
  expect_true(dplyr::all_equal(numero_pruebas_agrupados$numero_pruebas, numero_pruebas_agrupados_al_natural))

  # Chequeo de agrupacion INGRESO----
  numero_pruebas_agrupados <- datos_covid |>
    numero_pruebas(fill_zeros = FALSE, group_by_entidad = FALSE, fecha_tipo = "Ingreso", tipo_prueba = "PCR")

  # Agrupamos usando dplyr clásico
  numero_pruebas_agrupados_al_natural <- datos_covid$dats |>
    dplyr::filter(!!as.symbol("TOMA_MUESTRA_LAB") == 1) |>
    dplyr::mutate(TIPO_PRUEBA = "PCR") |>
    dplyr::group_by_at("FECHA_INGRESO") |>
    dplyr::group_by_at("TIPO_PRUEBA", .add = TRUE) |>
    dplyr::tally()

  # Verificamos igualdad
  expect_true(dplyr::all_equal(numero_pruebas_agrupados$numero_pruebas, numero_pruebas_agrupados_al_natural))

  # Chequeo INGRESO si defunción----
  numero_pruebas_agrupados <- datos_covid |>
    numero_pruebas(
      fill_zeros = FALSE, group_by_entidad = TRUE,
      fecha_tipo = "Ingreso", defunciones = TRUE, tipo_prueba = "PCR"
    )

  # Agrupamos usando dplyr clásico
  numero_pruebas_agrupados_al_natural <- datos_covid$dats |>
    dplyr::filter(!!as.symbol("FECHA_DEF") > as.POSIXct("1999/01/01", format = "%Y/%m/%d")) |>
    dplyr::filter(!!as.symbol("TOMA_MUESTRA_LAB") == 1) |>
    dplyr::mutate(TIPO_PRUEBA = "PCR") |>
    dplyr::group_by_at("FECHA_INGRESO") |>
    dplyr::group_by_at("TIPO_PRUEBA", .add = TRUE) |>
    dplyr::group_by_at("ENTIDAD_UM", .add = TRUE) |>
    dplyr::tally() |>
    dplyr::left_join(datos_covid$dict$ENTIDAD_UM, by = c("ENTIDAD_UM" = "CLAVE_ENTIDAD"))

  # Verificamos igualdad
  expect_true(dplyr::all_equal(numero_pruebas_agrupados$numero_pruebas, numero_pruebas_agrupados_al_natural))

  # Chequeo sector----
  numero_pruebas_agrupados <- datos_covid |>
    numero_pruebas(
      fill_zeros = FALSE, tipo_sector = c("IMSS", "ISSSTE"), group_by_tipo_sector = T,
      tipo_prueba = "PCR"
    )

  # Agrupamos usando dplyr clásico
  numero_pruebas_agrupados_al_natural <- datos_covid$dats |>
    dplyr::filter(!!as.symbol("TOMA_MUESTRA_LAB") == 1) |>
    dplyr::mutate(TIPO_PRUEBA = "PCR") |>
    dplyr::group_by_at("FECHA_SINTOMAS") |>
    dplyr::group_by_at("ENTIDAD_UM", .add = TRUE) |>
    dplyr::group_by_at("SECTOR", .add = TRUE) |>
    dplyr::group_by_at("TIPO_PRUEBA", .add = TRUE) |>
    dplyr::tally() |>
    dplyr::left_join(datos_covid$dict$ENTIDAD_UM, by = c("ENTIDAD_UM" = "CLAVE_ENTIDAD")) |>
    dplyr::left_join(datos_covid$dict$SECTOR, by = c("SECTOR" = "CLAVE")) |>
    dplyr::filter(str_detect(!!as.symbol("DESCRIPCI\032N"), "^IMSS$|^ISSSTE$")) |>
    dplyr::rename(!!as.symbol("DESCRIPCION_TIPO_SECTOR") := !!as.symbol("DESCRIPCI\032N"))

  # Verificamos igualdad
  expect_true(dplyr::all_equal(numero_pruebas_agrupados$numero_pruebas, numero_pruebas_agrupados_al_natural))

  # Chequeo UCI----
  numero_pruebas_agrupados <- datos_covid |>
    numero_pruebas(
      tipo_uci = c("SI", "NO"), group_by_tipo_uci = TRUE, fill_zeros = F,
      tipo_prueba = "Antigeno"
    )

  # Agrupamos usando dplyr clásico
  numero_pruebas_agrupados_al_natural <- datos_covid$dats |>
    dplyr::filter(!!as.symbol("TOMA_MUESTRA_ANTIGENO") == 1) |>
    dplyr::mutate(TIPO_PRUEBA = "ANTIGENO") |>
    dplyr::group_by_at("FECHA_SINTOMAS") |>
    dplyr::group_by_at("TIPO_PRUEBA", .add = TRUE) |>
    dplyr::group_by_at("ENTIDAD_UM", .add = TRUE) |>
    dplyr::group_by_at("UCI", .add = TRUE) |>
    dplyr::tally() |>
    dplyr::left_join(datos_covid$dict$ENTIDAD_UM, by = c("ENTIDAD_UM" = "CLAVE_ENTIDAD")) |>
    dplyr::left_join(datos_covid$dict$UCI, by = c("UCI" = "CLAVE")) |>
    dplyr::filter(str_detect(!!as.symbol("DESCRIPCI\032N"), "^SI$|^NO$")) |>
    dplyr::rename(!!as.symbol("DESCRIPCION_TIPO_UCI") := !!as.symbol("DESCRIPCI\032N"))

  # Verificamos igualdad
  expect_true(dplyr::all_equal(numero_pruebas_agrupados$numero_pruebas, numero_pruebas_agrupados_al_natural))

  # Chequeo TIPO PACIENTE----
  numero_pruebas_agrupados <- datos_covid |>
    numero_pruebas(
      tipo_paciente = c("AMBULATORIO", "NO ESPECIFICADO"), group_by_tipo_paciente = T,
      fill_zeros = F, tipo_prueba = "Antigeno"
    )

  # Agrupamos usando dplyr clásico
  numero_pruebas_agrupados_al_natural <- datos_covid$dats |>
    dplyr::filter(!!as.symbol("TOMA_MUESTRA_ANTIGENO") == 1) |>
    dplyr::mutate(TIPO_PRUEBA = "ANTIGENO") |>
    dplyr::group_by_at("FECHA_SINTOMAS") |>
    dplyr::group_by_at("TIPO_PRUEBA", .add = TRUE) |>
    dplyr::group_by_at("ENTIDAD_UM", .add = TRUE) |>
    dplyr::group_by_at("TIPO_PACIENTE", .add = TRUE) |>
    dplyr::tally() |>
    dplyr::left_join(datos_covid$dict$ENTIDAD_UM, by = c("ENTIDAD_UM" = "CLAVE_ENTIDAD")) |>
    dplyr::left_join(datos_covid$dict$PACIENTE, by = c("TIPO_PACIENTE" = "CLAVE")) |>
    dplyr::filter(str_detect(!!as.symbol("DESCRIPCI\032N"), "^AMBULATORIO$|^NO ESPECIFICADO$")) |>
    dplyr::rename(!!as.symbol("DESCRIPCION_TIPO_PACIENTE") := !!as.symbol("DESCRIPCI\032N"))

  # Verificamos igualdad
  expect_true(dplyr::all_equal(numero_pruebas_agrupados$numero_pruebas, numero_pruebas_agrupados_al_natural))

  # Chequeo GROUPING VARS----
  numero_pruebas_agrupados <- datos_covid |>
    numero_pruebas(
      group_by_entidad = F, .grouping_vars = c("SEXO", "DIABETES"),
      fill_zeros = F, tipo_prueba = "Antigeno"
    )

  # Agrupamos usando dplyr clásico
  numero_pruebas_agrupados_al_natural <- datos_covid$dats |>
    dplyr::filter(!!as.symbol("TOMA_MUESTRA_ANTIGENO") == 1) |>
    dplyr::mutate(TIPO_PRUEBA = "ANTIGENO") |>
    dplyr::group_by_at("FECHA_SINTOMAS") |>
    dplyr::group_by_at("TIPO_PRUEBA", .add = TRUE) |>
    dplyr::group_by_at("SEXO", .add = TRUE) |>
    dplyr::group_by_at("DIABETES", .add = TRUE) |>
    dplyr::tally()

  # Verificamos igualdad
  expect_true(dplyr::all_equal(numero_pruebas_agrupados$numero_pruebas, numero_pruebas_agrupados_al_natural))

  # Chequeo de que sólo lea el Baja California correcto-----
  numero_pruebas_agrupados <- datos_covid |>
    numero_pruebas(entidades = "BAJA CALIFORNIA")
  entidades <- unique(numero_pruebas_agrupados$numero_pruebas$ENTIDAD_FEDERATIVA)
  expect_true(length(entidades) == 1 & entidades == "BAJA CALIFORNIA")

  # Chequeo de que sólo lea el Baja California correcto-----
  numero_pruebas_agrupados <- datos_covid |>
    numero_pruebas(entidades = "BAJA CALIFORNIA SUR")
  entidades <- unique(numero_pruebas_agrupados$numero_pruebas$ENTIDAD_FEDERATIVA)
  expect_true(length(entidades) == 1 & entidades == "BAJA CALIFORNIA SUR")

  # Chequeo de que sólo lea el IMSS correcto 1----
  numero_pruebas_agrupados <- datos_covid |>
    numero_pruebas(tipo_sector = "IMSS", group_by_tipo_sector = TRUE, fill_zeros = FALSE)
  sectores <- unique(numero_pruebas_agrupados$numero_pruebas$DESCRIPCION_TIPO_SECTOR)
  expect_true(length(sectores) == 1 & sectores == "IMSS")

  # Chequeo de que sólo lea el IMSS correcto 2----
  numero_pruebas_agrupados <- datos_covid |>
    numero_pruebas(tipo_sector = "IMSS-BIENESTAR", group_by_tipo_sector = TRUE, fill_zeros = FALSE)
  sectores <- unique(numero_pruebas_agrupados$numero_pruebas$DESCRIPCION_TIPO_SECTOR)
  expect_true(length(sectores) == 1 & sectores == "IMSS-BIENESTAR")

  # Chequeo de que sólo lea el UCI correcto 1----
  numero_pruebas_agrupados <- datos_covid |>
    numero_pruebas(tipo_uci = "NO", group_by_tipo_uci = TRUE, fill_zeros = FALSE)
  ucis <- unique(numero_pruebas_agrupados$numero_pruebas$DESCRIPCION_TIPO_UCI)
  expect_true(length(ucis) == 1 & ucis == "NO")

  # Chequeo de que sólo lea el UCI correcto 1----
  numero_pruebas_agrupados <- datos_covid |>
    numero_pruebas(tipo_uci = "NO ESPECIFICADO", group_by_tipo_uci = TRUE, fill_zeros = FALSE)
  ucis <- unique(numero_pruebas_agrupados$numero_pruebas$DESCRIPCION_TIPO_UCI)
  expect_true(length(ucis) == 1 & ucis == "NO ESPECIFICADO")

  # Chequeo de que el edad cut funcione-----
  numero_pruebas_agrupados <- datos_covid |>
    numero_pruebas(edad_cut = c(0, 1), fill_zeros = FALSE, tipo_prueba = "Antigeno")
  numero_pruebas_agrupados$numero_pruebas <- numero_pruebas_agrupados$numero_pruebas |>
    dplyr::select(-!!as.symbol("EDAD_CAT"))

  numero_pruebas_edad_cut <- datos_covid$dats |>
    dplyr::filter(EDAD >= 0 & EDAD <= 1) |>
    dplyr::filter(!!as.symbol("TOMA_MUESTRA_ANTIGENO") == 1) |>
    dplyr::mutate(TIPO_PRUEBA = "ANTIGENO") |>
    dplyr::group_by_at("FECHA_SINTOMAS") |>
    dplyr::group_by_at("TIPO_PRUEBA", .add = TRUE) |>
    dplyr::group_by_at("ENTIDAD_UM", .add = TRUE) |>
    dplyr::tally() |>
    dplyr::left_join(datos_covid$dict$ENTIDAD_UM, by = c("ENTIDAD_UM" = "CLAVE_ENTIDAD"))

  expect_true(dplyr::all_equal(numero_pruebas_agrupados$numero_pruebas, numero_pruebas_edad_cut))

  # Chequeo de que el fill zeros funcione-----
  numero_pruebas_agrupados <- datos_covid |>
    numero_pruebas(edad_cut = c(0, 1), fill_zeros = TRUE, tipo_prueba = "Antigeno")
  numero_pruebas_agrupados$numero_pruebas <- numero_pruebas_agrupados$numero_pruebas |>
    dplyr::select(-!!as.symbol("EDAD_CAT")) |>
    dplyr::arrange(FECHA_SINTOMAS, ENTIDAD_UM)

  numero_pruebas_edad_cut <- datos_covid$dats |>
    dplyr::filter(EDAD >= 0 & EDAD <= 1) |>
    dplyr::filter(!!as.symbol("TOMA_MUESTRA_ANTIGENO") == 1) |>
    dplyr::mutate(TIPO_PRUEBA = "ANTIGENO") |>
    dplyr::group_by_at("FECHA_SINTOMAS") |>
    dplyr::group_by_at("ENTIDAD_UM", .add = TRUE) |>
    dplyr::tally() |>
    dplyr::full_join(
      tidyr::expand_grid(
        FECHA_SINTOMAS = unique(datos_covid$dats$FECHA_SINTOMAS),
        ENTIDAD_UM = unique(datos_covid$dats$ENTIDAD_UM),
      ),
      by = c("FECHA_SINTOMAS", "ENTIDAD_UM")
    ) |>
    dplyr::left_join(datos_covid$dict$ENTIDAD_UM, by = c("ENTIDAD_UM" = "CLAVE_ENTIDAD")) |>
    dplyr::mutate(TIPO_PRUEBA = "ANTIGENO")

  numero_pruebas_edad_cut <- numero_pruebas_edad_cut |>
    dplyr::mutate(n = dplyr::if_else(is.na(as.numeric(n)), as.integer(0), as.integer(n))) |>
    dplyr::ungroup() |>
    dplyr::arrange(FECHA_SINTOMAS, ENTIDAD_UM)

  expect_true(dplyr::all_equal(numero_pruebas_agrupados$numero_pruebas, numero_pruebas_edad_cut))

  # Chequeo del list_name-----
  numero_pruebas_prueba <- datos_covid |> numero_pruebas(list_name = "Prueba")
  expect_true("Prueba" %in% names(numero_pruebas_prueba))

  # Chequeo de que no se repita el nombre-----
  numero_pruebas_prueba <- datos_covid |> numero_pruebas(list_name = "Prueba")
  expect_message(numero_pruebas(numero_pruebas_prueba, list_name = "Prueba"))

  # Chequeo de una entidad sin sentido-----
  expect_error(datos_covid |> numero_pruebas(entidades = c("2y48rogf", "gdivfk")))
})
