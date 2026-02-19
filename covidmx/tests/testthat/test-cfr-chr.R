test_that("Test cfr and chr", {

  # Leemos los datos
  datos_covid <- covidmx::datosabiertos

  # Error de lista CFR---
  cfr_data <- cfr(datos_covid)
  expect_message(cfr(cfr_data))

  # Error de lista CHR---
  chr_data <- chr(datos_covid)
  expect_message(chr(chr_data))
})
