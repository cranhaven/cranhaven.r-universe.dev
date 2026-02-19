test_that("Download processes", {
  # Estos teste verifican que las bases descargadas tengan numero de filas > 0
  skip_if_offline()

  # Descarga de datos de red irag----
  redirag <- descarga_datos_red_irag("Estatal", quiet = T, cache = tempfile())
  expect_gt(nrow(redirag), 0)

  # Mensaje de descarga----
  mvdir <- tempfile()
  expect_message(descarga_datos_red_irag("Estatal", quiet = F, show_warnings = F, cache = mvdir))

  # Warning por nueva descarga red irag----
  expect_warning(descarga_datos_red_irag("Estatal", quiet = T, cache = mvdir))

  redirag <- descarga_datos_red_irag("Unidad Medica", quiet = T, show_warnings = F, cache = tempfile())
  expect_gt(nrow(redirag), 0)

  # Descarga de datos de GISAID----
  variantes <- descarga_datos_variantes_GISAID("nacional", quiet = T, show_warnings = F, cache = tempfile())
  expect_gt(nrow(variantes), 0)

  # GISAID CDMX----
  variantes <- descarga_datos_variantes_GISAID("cdmx", quiet = T, show_warnings = F, cache = tempfile())
  expect_gt(nrow(variantes), 0)

  # Mensaje de descarga----
  mvdir <- tempfile()
  expect_message(descarga_datos_variantes_GISAID("nacional", quiet = F, show_warnings = F, cache = mvdir))

  # Warning por nueva descarga red irag----
  expect_warning(descarga_datos_variantes_GISAID("nacional", quiet = T, cache = mvdir))
})
