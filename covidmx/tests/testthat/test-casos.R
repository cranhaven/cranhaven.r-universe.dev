test_that("Casos", {

  # Leemos los datos
  datos_covid <- covidmx::datosabiertos

  # Vemos que la agrupacion coincida con la "normal"-----
  casos_agrupados <- datos_covid |>
    casos(fill_zeros = FALSE)

  # Agrupamos usando dplyr clásico
  casos_agrupados_al_natural <- datos_covid$dats |>
    dplyr::group_by_at("FECHA_SINTOMAS") |>
    dplyr::group_by_at("ENTIDAD_UM", .add = TRUE) |>
    dplyr::tally() |>
    dplyr::left_join(datos_covid$dict$ENTIDAD_UM, by = c("ENTIDAD_UM" = "CLAVE_ENTIDAD"))

  # Verificamos igualdad
  expect_true(dplyr::all_equal(casos_agrupados$casos, casos_agrupados_al_natural))

  # Chequeo de agrupacion sin entidad----
  casos_agrupados <- datos_covid |>
    casos(fill_zeros = FALSE, group_by_entidad = FALSE)

  # Agrupamos usando dplyr clásico
  casos_agrupados_al_natural <- datos_covid$dats |>
    dplyr::group_by_at("FECHA_SINTOMAS") |>
    dplyr::tally()

  # Verificamos igualdad
  expect_true(dplyr::all_equal(casos_agrupados$casos, casos_agrupados_al_natural))

  # Chequeo de agrupacion defuncion----
  casos_agrupados <- datos_covid |>
    casos(fill_zeros = FALSE, group_by_entidad = FALSE, fecha_tipo = "Defuncion")

  # Agrupamos usando dplyr clásico
  casos_agrupados_al_natural <- datos_covid$dats |>
    dplyr::group_by_at("FECHA_DEF") |>
    dplyr::tally()

  # Verificamos igualdad
  expect_true(dplyr::all_equal(casos_agrupados$casos, casos_agrupados_al_natural))

  # Chequeo de agrupacion INGRESO----
  casos_agrupados <- datos_covid |>
    casos(fill_zeros = FALSE, group_by_entidad = FALSE, fecha_tipo = "Ingreso")

  # Agrupamos usando dplyr clásico
  casos_agrupados_al_natural <- datos_covid$dats |>
    dplyr::group_by_at("FECHA_INGRESO") |>
    dplyr::tally()

  # Verificamos igualdad
  expect_true(dplyr::all_equal(casos_agrupados$casos, casos_agrupados_al_natural))

  # Chequeo INGRESO si defunción----
  casos_agrupados <- datos_covid |>
    casos(
      fill_zeros = FALSE, group_by_entidad = TRUE,
      fecha_tipo = "Ingreso", defunciones = TRUE
    )

  # Agrupamos usando dplyr clásico
  casos_agrupados_al_natural <- datos_covid$dats |>
    dplyr::filter(!!as.symbol("FECHA_DEF") > as.POSIXct("1999/01/01", format = "%Y/%m/%d")) |>
    dplyr::group_by_at("FECHA_INGRESO") |>
    dplyr::group_by_at("ENTIDAD_UM", .add = TRUE) |>
    dplyr::tally() |>
    dplyr::left_join(datos_covid$dict$ENTIDAD_UM, by = c("ENTIDAD_UM" = "CLAVE_ENTIDAD"))

  # Verificamos igualdad
  expect_true(dplyr::all_equal(casos_agrupados$casos, casos_agrupados_al_natural))

  # Chequeo sector----
  casos_agrupados <- datos_covid |>
    casos(fill_zeros = FALSE, tipo_sector = c("IMSS", "ISSSTE"), group_by_tipo_sector = T)

  # Agrupamos usando dplyr clásico
  casos_agrupados_al_natural <- datos_covid$dats |>
    dplyr::group_by_at("FECHA_SINTOMAS") |>
    dplyr::group_by_at("ENTIDAD_UM", .add = TRUE) |>
    dplyr::group_by_at("SECTOR", .add = TRUE) |>
    dplyr::tally() |>
    dplyr::left_join(datos_covid$dict$ENTIDAD_UM, by = c("ENTIDAD_UM" = "CLAVE_ENTIDAD")) |>
    dplyr::left_join(datos_covid$dict$SECTOR, by = c("SECTOR" = "CLAVE")) |>
    dplyr::filter(str_detect(!!as.symbol("DESCRIPCI\032N"), "^IMSS$|^ISSSTE$")) |>
    dplyr::rename(!!as.symbol("DESCRIPCION_TIPO_SECTOR") := !!as.symbol("DESCRIPCI\032N"))

  # Verificamos igualdad
  expect_true(dplyr::all_equal(casos_agrupados$casos, casos_agrupados_al_natural))

  # Chequeo UCI----
  casos_agrupados <- datos_covid |>
    casos(tipo_uci = c("SI", "NO"), group_by_tipo_uci = TRUE, fill_zeros = F)

  # Agrupamos usando dplyr clásico
  casos_agrupados_al_natural <- datos_covid$dats |>
    dplyr::group_by_at("FECHA_SINTOMAS") |>
    dplyr::group_by_at("ENTIDAD_UM", .add = TRUE) |>
    dplyr::group_by_at("UCI", .add = TRUE) |>
    dplyr::tally() |>
    dplyr::left_join(datos_covid$dict$ENTIDAD_UM, by = c("ENTIDAD_UM" = "CLAVE_ENTIDAD")) |>
    dplyr::left_join(datos_covid$dict$UCI, by = c("UCI" = "CLAVE")) |>
    dplyr::filter(str_detect(!!as.symbol("DESCRIPCI\032N"), "^SI$|^NO$")) |>
    dplyr::rename(!!as.symbol("DESCRIPCION_TIPO_UCI") := !!as.symbol("DESCRIPCI\032N"))

  # Verificamos igualdad
  expect_true(dplyr::all_equal(casos_agrupados$casos, casos_agrupados_al_natural))

  # Chequeo TIPO PACIENTE----
  casos_agrupados <- datos_covid |>
    casos(
      tipo_paciente = c("AMBULATORIO", "NO ESPECIFICADO"), group_by_tipo_paciente = T,
      fill_zeros = F
    )

  # Agrupamos usando dplyr clásico
  casos_agrupados_al_natural <- datos_covid$dats |>
    dplyr::group_by_at("FECHA_SINTOMAS") |>
    dplyr::group_by_at("ENTIDAD_UM", .add = TRUE) |>
    dplyr::group_by_at("TIPO_PACIENTE", .add = TRUE) |>
    dplyr::tally() |>
    dplyr::left_join(datos_covid$dict$ENTIDAD_UM, by = c("ENTIDAD_UM" = "CLAVE_ENTIDAD")) |>
    dplyr::left_join(datos_covid$dict$PACIENTE, by = c("TIPO_PACIENTE" = "CLAVE")) |>
    dplyr::filter(str_detect(!!as.symbol("DESCRIPCI\032N"), "^AMBULATORIO$|^NO ESPECIFICADO$")) |>
    dplyr::rename(!!as.symbol("DESCRIPCION_TIPO_PACIENTE") := !!as.symbol("DESCRIPCI\032N"))

  # Verificamos igualdad
  expect_true(dplyr::all_equal(casos_agrupados$casos, casos_agrupados_al_natural))

  # Chequeo tipo_clasificacion confirmados-----
  casos_agrupados <- datos_covid |>
    casos(group_by_entidad = F, fill_zeros = F, tipo_clasificacion = "Confirmados COVID")

  # Agrupamos usando dplyr clásico
  casos_agrupados_al_natural <- datos_covid$dats |>
    dplyr::filter(!!as.symbol("CLASIFICACION_FINAL") %in% c(1, 2, 3)) |>
    dplyr::group_by_at("FECHA_SINTOMAS") |>
    dplyr::tally()

  # Verificamos igualdad
  expect_true(dplyr::all_equal(casos_agrupados$casos, casos_agrupados_al_natural))

  # Chequeo tipo_clasificacion sospechosos-----
  casos_agrupados <- datos_covid |>
    casos(group_by_entidad = F, fill_zeros = F, tipo_clasificacion = "Sospechosos")

  # Agrupamos usando dplyr clásico
  casos_agrupados_al_natural <- datos_covid$dats |>
    dplyr::filter(!!as.symbol("CLASIFICACION_FINAL") == 6) |>
    dplyr::group_by_at("FECHA_SINTOMAS") |>
    dplyr::tally()

  # Verificamos igualdad
  expect_true(dplyr::all_equal(casos_agrupados$casos, casos_agrupados_al_natural))

  # Chequeo tipo_clasificacion negativo-----
  casos_agrupados <- datos_covid |>
    casos(group_by_entidad = F, fill_zeros = F, tipo_clasificacion = "Negativo")

  # Agrupamos usando dplyr clásico
  casos_agrupados_al_natural <- datos_covid$dats |>
    dplyr::filter(!!as.symbol("CLASIFICACION_FINAL") == 7) |>
    dplyr::group_by_at("FECHA_SINTOMAS") |>
    dplyr::tally()

  # Verificamos igualdad
  expect_true(dplyr::all_equal(casos_agrupados$casos, casos_agrupados_al_natural))

  # Chequeo tipo_clasificacion invakid-----
  casos_agrupados <- datos_covid |>
    casos(group_by_entidad = F, fill_zeros = F, tipo_clasificacion = "Invalido")

  # Agrupamos usando dplyr clásico
  casos_agrupados_al_natural <- datos_covid$dats |>
    dplyr::filter(!!as.symbol("CLASIFICACION_FINAL") == 4) |>
    dplyr::group_by_at("FECHA_SINTOMAS") |>
    dplyr::tally()

  # Verificamos igualdad
  expect_true(dplyr::all_equal(casos_agrupados$casos, casos_agrupados_al_natural))

  # Chequeo tipo_clasificacion invakid-----
  casos_agrupados <- datos_covid |>
    casos(group_by_entidad = F, fill_zeros = F, tipo_clasificacion = "No realizado")

  # Agrupamos usando dplyr clásico
  casos_agrupados_al_natural <- datos_covid$dats |>
    dplyr::filter(!!as.symbol("CLASIFICACION_FINAL") == 5) |>
    dplyr::group_by_at("FECHA_SINTOMAS") |>
    dplyr::tally()

  # Verificamos igualdad
  expect_true(dplyr::all_equal(casos_agrupados$casos, casos_agrupados_al_natural))

  # Chequeo GROUPING VARS----
  casos_agrupados <- datos_covid |>
    casos(
      group_by_entidad = F, .grouping_vars = c("SEXO", "DIABETES"),
      fill_zeros = F
    )

  # Agrupamos usando dplyr clásico
  casos_agrupados_al_natural <- datos_covid$dats |>
    dplyr::group_by_at("FECHA_SINTOMAS") |>
    dplyr::group_by_at("SEXO", .add = TRUE) |>
    dplyr::group_by_at("DIABETES", .add = TRUE) |>
    dplyr::tally()

  # Verificamos igualdad
  expect_true(dplyr::all_equal(casos_agrupados$casos, casos_agrupados_al_natural))

  # Chequeo de que sólo lea el Baja California correcto-----
  casos_agrupados <- datos_covid |>
    casos(entidades = "BAJA CALIFORNIA")
  entidades <- unique(casos_agrupados$casos$ENTIDAD_FEDERATIVA)
  expect_true(length(entidades) == 1 & entidades == "BAJA CALIFORNIA")

  # Chequeo de que sólo lea el Baja California correcto-----
  casos_agrupados <- datos_covid |>
    casos(entidades = "BAJA CALIFORNIA SUR")
  entidades <- unique(casos_agrupados$casos$ENTIDAD_FEDERATIVA)
  expect_true(length(entidades) == 1 & entidades == "BAJA CALIFORNIA SUR")

  # Chequeo de que sólo lea el IMSS correcto 1----
  casos_agrupados <- datos_covid |>
    casos(tipo_sector = "IMSS", group_by_tipo_sector = TRUE, fill_zeros = FALSE)
  sectores <- unique(casos_agrupados$casos$DESCRIPCION_TIPO_SECTOR)
  expect_true(length(sectores) == 1 & sectores == "IMSS")

  # Chequeo de que sólo lea el IMSS correcto 2----
  casos_agrupados <- datos_covid |>
    casos(tipo_sector = "IMSS-BIENESTAR", group_by_tipo_sector = TRUE, fill_zeros = FALSE)
  sectores <- unique(casos_agrupados$casos$DESCRIPCION_TIPO_SECTOR)
  expect_true(length(sectores) == 1 & sectores == "IMSS-BIENESTAR")

  # Chequeo de que sólo lea el UCI correcto 1----
  casos_agrupados <- datos_covid |>
    casos(tipo_uci = "NO", group_by_tipo_uci = TRUE, fill_zeros = FALSE)
  ucis <- unique(casos_agrupados$casos$DESCRIPCION_TIPO_UCI)
  expect_true(length(ucis) == 1 & ucis == "NO")

  # Chequeo de que sólo lea el UCI correcto 1----
  casos_agrupados <- datos_covid |>
    casos(tipo_uci = "NO ESPECIFICADO", group_by_tipo_uci = TRUE, fill_zeros = FALSE)
  ucis <- unique(casos_agrupados$casos$DESCRIPCION_TIPO_UCI)
  expect_true(length(ucis) == 1 & ucis == "NO ESPECIFICADO")

  # Chequeo de que el edad cut funcione-----
  casos_agrupados <- datos_covid |>
    casos(edad_cut = c(0, 1), fill_zeros = FALSE)
  casos_agrupados$casos <- casos_agrupados$casos |>
    dplyr::select(-!!as.symbol("EDAD_CAT"))

  casos_edad_cut <- datos_covid$dats |>
    dplyr::filter(EDAD >= 0 & EDAD <= 1) |>
    dplyr::group_by_at("FECHA_SINTOMAS") |>
    dplyr::group_by_at("ENTIDAD_UM", .add = TRUE) |>
    dplyr::tally() |>
    dplyr::left_join(datos_covid$dict$ENTIDAD_UM, by = c("ENTIDAD_UM" = "CLAVE_ENTIDAD"))

  expect_true(dplyr::all_equal(casos_agrupados$casos, casos_edad_cut))

  # Chequeo de que el fill zeros funcione-----
  casos_agrupados <- datos_covid |>
    casos(edad_cut = c(0, 1), fill_zeros = TRUE, entidades = c("BAJA CALIFORNIA", "ZACATECAS"))

  casos_agrupados$casos <- casos_agrupados$casos |>
    dplyr::select(-!!as.symbol("EDAD_CAT")) |>
    dplyr::arrange(FECHA_SINTOMAS, ENTIDAD_UM)

  casos_edad_cut <- datos_covid$dats |>
    dplyr::filter(EDAD >= 0 & EDAD <= 1) |>
    dplyr::group_by_at("FECHA_SINTOMAS") |>
    dplyr::group_by_at("ENTIDAD_UM", .add = TRUE) |>
    dplyr::tally() |>
    dplyr::full_join(
      tidyr::expand_grid(
        FECHA_SINTOMAS = unique(datos_covid$dats$FECHA_SINTOMAS),
        ENTIDAD_UM = datos_covid$dict$ENTIDAD_UM$CLAVE_ENTIDAD
      ),
      by = c("FECHA_SINTOMAS", "ENTIDAD_UM")
    ) |>
    dplyr::distinct() |>
    dplyr::left_join(datos_covid$dict$ENTIDAD_UM, by = c("ENTIDAD_UM" = "CLAVE_ENTIDAD")) |>
    dplyr::filter(as.numeric(ENTIDAD_UM) < 36) |>
    dplyr::filter(ENTIDAD_UM == "32" | ENTIDAD_UM == "02") |>
    dplyr::arrange(FECHA_SINTOMAS, ENTIDAD_UM)

  casos_edad_cut <- casos_edad_cut |>
    dplyr::mutate(n = dplyr::if_else(is.na(as.numeric(n)), as.integer(0), as.integer(n))) |>
    dplyr::ungroup() |>
    dplyr::arrange(FECHA_SINTOMAS, ENTIDAD_UM)

  expect_true(dplyr::all_equal(casos_agrupados$casos, casos_edad_cut))

  # Chequeo del list_name-----
  casos_prueba <- datos_covid |> casos(list_name = "Prueba")
  expect_true("Prueba" %in% names(casos_prueba))

  # Chequeo de una entidad sin sentido-----
  expect_error(datos_covid |> casos(entidades = c("2y48rogf", "gdivfk")))

  # Chequeo de repetidos----
  expect_message(casos(casos_prueba, list_name = "Prueba"))
  
  # Chequeo de que no salgan agrupados
  casos_agrupados <- datos_covid |> 
    casos(group_by_tipo_clasificacion = TRUE,
          group_by_tipo_paciente = TRUE,
          group_by_tipo_uci = TRUE,
          group_by_tipo_sector = TRUE,
          fill_zeros = FALSE,
          .grouping_vars = c("SEXO","DIABETES"))
  
  expect_true(!dplyr::is.grouped_df(casos_agrupados$dats))
})
