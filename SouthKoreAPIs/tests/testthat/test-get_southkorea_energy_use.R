# SouthKoreAPIs - Access South Korean Data via Public APIs and Curated Datasets
# Version 0.1.1
# Copyright (c) 2025 Renzo Caceres Rossi
# Licensed under the MIT License.
# See the LICENSE file in the root directory for full license text.

# get_southkorea_energy_use

library(testthat)

test_that("get_southkorea_energy_use() devuelve un tibble con la estructura correcta", {
  result <- get_southkorea_energy_use()

  # Verificar que el resultado no sea NULL
  expect_false(is.null(result))

  # Verificar que es un data.frame o tibble
  expect_s3_class(result, "data.frame")

  # Verificar que tiene exactamente las columnas esperadas
  expect_named(result, c("indicator", "country", "year", "value"))

  # Verificar tipos de datos
  expect_type(result$indicator, "character")
  expect_type(result$country, "character")
  expect_type(result$year, "integer")
  expect_type(result$value, "double")

  # Verificar rango de años
  expect_true(all(result$year >= 2010 & result$year <= 2022))

  # Verificar que el país es "Korea, Rep."
  expect_true(all(result$country == "Korea, Rep."))

  # Verificar que no hay valores NA
  expect_false(any(is.na(result)))

  # Verificar que hay 13 filas (2010–2022)
  expect_equal(nrow(result), 13)
})
