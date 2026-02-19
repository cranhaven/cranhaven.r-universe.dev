test_that("duckdb", {
  setwd(tempdir())
  skip_on_cran()
  skip_if_offline()

  # Descarga con duckdb sin guardar
  dlink <- c("gtest" = "https://github.com/RodrigoZepeda/covidmx/raw/main/datos_abiertos_covid19.zip")
  datos_covid <- descarga_datos_abiertos(
    read_format = "duckdb", sites.covid = dlink, tblname = "test",
    quiet = TRUE, show_warnings = F, force_download = T,
    cache_datos = tempfile(), cache_diccionario = tempfile()
  )
  expect_gt(as.numeric(dplyr::collect(dplyr::tally(datos_covid$dats))[[1]]), 0)
  datos_covid$disconnect()

  # Descarga con duckdb y guardado
  tempduck <- tempfile(fileext = ".duckdb")
  datos_covid <- descarga_datos_abiertos(
    read_format = "duckdb", sites.covid = dlink,
    tblname = "test", quiet = TRUE, show_warnings = F,
    force_download = T, dbdir = tempduck,
    cache_datos = tempfile(), cache_diccionario = tempfile()
  )
  expect_gt(as.numeric(dplyr::collect(dplyr::tally(datos_covid$dats))[[1]]), 0)
  datos_covid$disconnect()

  # Lectura con duckdb
  skip_on_os("windows") # In windows throws reading error I don't have windows to check
  datos_covid <- read_datos_abiertos(
    datos_abiertos_path = tempduck, use_dict = FALSE,
    read_format = "duckdb", tblname = "test", quiet = TRUE,
    drv = duckdb::duckdb(dbdir = tempduck, read_only = TRUE),
    show_warnings = F, force_download = T
  )
  expect_gt(as.numeric(dplyr::collect(dplyr::tally(datos_covid$dats))[[1]]), 0)
  datos_covid$disconnect()
})
