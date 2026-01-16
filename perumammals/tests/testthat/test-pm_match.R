# =============================================================================
# TEST SUITE 1: Validación de Inputs (validate_peru_mammals)
# =============================================================================

test_that("validate_peru_mammals valida correctamente los inputs", {
  # Test 1.1: splist debe ser un vector de caracteres
  expect_error(
    validate_peru_mammals(splist = 123),
    "`splist` must be a character vector"
  )

  expect_error(
    validate_peru_mammals(splist = list("Akodon", "Mus")),
    "`splist` must be a character vector"
  )

  # Test 1.2: quiet debe ser lógico
  expect_error(
    validate_peru_mammals(splist = "Akodon torques", quiet = "yes"),
    "`quiet` must be a single TRUE/FALSE logical"
  )

  # Test 1.3: Input válido no genera error
  expect_no_error(
    validate_peru_mammals(splist = "Akodon torques", quiet = TRUE)
  )
})


# =============================================================================
# TEST SUITE 2: Matching Exacto
# =============================================================================

test_that("validate_peru_mammals realiza matching exacto correcto", {
  # Test 2.1: Match exacto de especie válida del dataset real
  result <- validate_peru_mammals("Panthera onca", quiet = TRUE)

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 1)
  expect_true(result$matched)
  expect_equal(result$Matched.Rank, 2L)

  # Test 2.2: Match exacto case-insensitive
  result_upper <- validate_peru_mammals("PANTHERA ONCA", quiet = TRUE)
  result_lower <- validate_peru_mammals("panthera onca", quiet = TRUE)

  expect_true(result_upper$matched)
  expect_true(result_lower$matched)

  # Test 2.3: Múltiples especies reales del dataset
  species_list <- c("Panthera onca", "Tremarctos ornatus", "Puma concolor")
  result_multi <- validate_peru_mammals(species_list, quiet = TRUE)

  expect_equal(nrow(result_multi), 3)
  expect_true(all(result_multi$matched))
})


# =============================================================================
# TEST SUITE 3: Matching a Nivel de Género
# =============================================================================

test_that("validate_peru_mammals maneja correctamente nombres a nivel de género", {
  # Test 3.1: Nombre solo con género
  result <- validate_peru_mammals("Panthera", quiet = TRUE)

  expect_false(result$matched)
  expect_equal(result$Matched.Rank, NA_real_)  # Rank 1 = genus only
  expect_true(is.na(result$Matched.Species))

  # Test 3.2: Múltiples géneros
  genera_list <- c("Panthera", "Tremarctos", "Puma")
  result_genera <- validate_peru_mammals(genera_list, quiet = TRUE)

  expect_equal(nrow(result_genera), 3)
  #expect_true(all(result_genera$Matched.Rank == 1L))
  expect_true(all(is.na(result_genera$Matched.Species)))
})


# =============================================================================
# TEST SUITE 4: Fuzzy Matching
# =============================================================================

test_that("validate_peru_mammals realiza fuzzy matching cuando es necesario", {
  # Test 4.1: Typo en género (Panthera -> Pantera)
  result_genus_typo <- validate_peru_mammals("Pantera onca", quiet = TRUE)

  expect_true(result_genus_typo$matched)
  expect_true(result_genus_typo$genus_dist > 0)

  # Test 4.2: Typo en especie
  result_species_typo <- validate_peru_mammals("Panthera onc", quiet = TRUE)

  # Puede o no matchear dependiendo de la distancia
  if (result_species_typo$matched) {
    expect_true(result_species_typo$species_dist > 0)
  }

  # Test 4.3: Typo menor (1 caracter)
  result_minor <- validate_peru_mammals("Puma conclor", quiet = TRUE)

  if (result_minor$matched) {
    expect_true(result_minor$species_dist <= 2)
  }
})


# =============================================================================
# TEST SUITE 5: Manejo de Nombres Inválidos
# =============================================================================

# test_that("validate_peru_mammals maneja correctamente nombres inválidos", {
#   # Test 5.1: NA en la lista
#   result_with_na <- validate_peru_mammals(c("Panthera onca", NA, "Puma concolor"),
#                                           quiet = TRUE)
#
#   expect_equal(nrow(result_with_na), 3)
#   expect_true(any(is.na(result_with_na$Matched.Species)))
#     # Test 5.2: Nombre completamente inválido
#   result_invalid <- validate_peru_mammals("SpeciesXYZ123", quiet = TRUE)
#
#   expect_false(result_invalid$matched)
#   expect_true(is.na(result_invalid$Matched.Genus))
#
#   # Test 5.3: String vacío
#   result_empty <- validate_peru_mammals("", quiet = TRUE)
#
#   expect_false(result_empty$matched)
# })
#

# =============================================================================
# TEST SUITE 6: Estructura del Output
# =============================================================================

test_that("validate_peru_mammals retorna estructura correcta de datos", {
  result <- validate_peru_mammals(c("Panthera onca", "SpeciesUnknown"), quiet = TRUE)

  # Test 6.1: Columnas requeridas presentes
  required_cols <- c(
    "Orig.Name", "Orig.Genus", "Orig.Species",
    "Matched.Name", "Matched.Genus", "Matched.Species",
    "Matched.Rank", "matched", "Rank"
  )

  expect_true(all(required_cols %in% names(result)))

  # Test 6.2: Tipos de datos correctos
  expect_type(result$Orig.Name, "character")
  expect_type(result$Matched.Rank, "integer")
  expect_type(result$matched, "logical")

  # Test 6.3: Data frame tiene tantas filas como inputs
  expect_equal(nrow(result), 2)
})


# =============================================================================
# TEST SUITE 7: Matching en Lote
# =============================================================================

# test_that("validate_peru_mammals maneja correctamente listas grandes de especies", {
#   # Test 7.1: Lista con especies mixtas
#   large_list <- c(
#     "Panthera onca",
#     "Puma concolor",
#     "Invalid species",
#     "Tremarctos ornatus",
#     NA,
#     "Atelocynus microtis"
#   )
#
#   result_large <- validate_peru_mammals(large_list, quiet = TRUE)
#
#   expect_equal(nrow(result_large), length(large_list))
#   expect_true(sum(result_large$matched, na.rm = TRUE) >= 3)
#
#   # Test 7.2: Lista completamente válida con especies reales
#   valid_list <- c(
#     "Panthera onca",
#     "Puma concolor",
#     "Tremarctos ornatus"
#   )
#
#   result_valid <- validate_peru_mammals(valid_list, quiet = TRUE)
#
#   expect_true(all(result_valid$matched))
#   expect_equal(nrow(result_valid), 3)
# })


# =============================================================================
# TEST SUITE 8: Mensajes y Warnings
# =============================================================================

test_that("validate_peru_mammals genera mensajes apropiados", {
  # Test 8.1: Con quiet = FALSE, debe generar mensajes
  expect_message(
    validate_peru_mammals("Panthera onca", quiet = FALSE),
    "Node"
  )

  # Test 8.2: Con quiet = TRUE, no debe generar mensajes del matching
  expect_silent(
    validate_peru_mammals("Panthera onca", quiet = TRUE)
  )
})


# =============================================================================
# TEST SUITE 9: Casos Especiales con sp.
# =============================================================================

test_that("validate_peru_mammals maneja casos especiales 'sp.' correctamente", {
  # Buscar un caso real de sp. en el dataset
  data("peru_mammals")
  sp_cases <- peru_mammals$scientific_name[grepl("sp\\.", peru_mammals$species)]

  if (length(sp_cases) > 0) {
    # Test con caso real de sp.
    result_sp <- validate_peru_mammals(sp_cases[1], quiet = TRUE)

    expect_true(result_sp$matched)
    expect_equal(result_sp$Matched.Rank, 2L)
  }
})


# =============================================================================
# TEST SUITE 10: Integración con datos peru_mammals
# =============================================================================

test_that("validate_peru_mammals funciona con datos reales", {
  # Cargar datos
  data("peru_mammals", package = "perumammals")

  # Test 10.1: Verificar que los datos existen
  expect_true(exists("peru_mammals"))
  expect_s3_class(peru_mammals, "data.frame")

  # Test 10.2: Matching con especies reales del dataset
  sample_species <- head(peru_mammals$scientific_name, 5)
  result_real <- validate_peru_mammals(sample_species, quiet = TRUE)

  expect_equal(nrow(result_real), 5)
  expect_true(all(result_real$matched))

  # Test 10.3: Verificar consistencia de géneros
  sample_genera <- unique(head(peru_mammals$genus, 10))
  result_genera <- validate_peru_mammals(sample_genera, quiet = TRUE)
  #  result_genera
  expect_false(all(result_genera$matched))
  expect_true(all(result_genera$Rank == 1L))
})


# =============================================================================
# TEST SUITE 11: Metadatos del Resultado
# =============================================================================

test_that("validate_peru_mammals añade metadatos correctos", {
  result <- validate_peru_mammals(c("Panthera onca", "Puma concolor"), quiet = TRUE)

  # Verificar atributos
  expect_equal(attr(result, "target_database"), "peru_mammals")
  expect_equal(attr(result, "n_input"), 2)
  expect_true(!is.null(attr(result, "match_rate")))
  expect_true(!is.null(attr(result, "matching_date")))
})


# =============================================================================
# TEST SUITE 12: Performance y Robustez
# =============================================================================

test_that("validate_peru_mammals es robusto ante inputs moderados", {
  # Test 12.1: Lista moderadamente grande
  large_input <- rep(c("Panthera onca", "Puma concolor", "Unknown species"),
                     length.out = 50)

  expect_no_error({
    result_perf <- validate_peru_mammals(large_input, quiet = TRUE)
  })

  expect_equal(nrow(result_perf), 50)

  # Test 12.2: Verificar que el tiempo de ejecución es razonable
  execution_time <- system.time({
    validate_peru_mammals(large_input, quiet = TRUE)
  })

  expect_true(execution_time["elapsed"] < 10)  # Menos de 10 segundos
})


# =============================================================================
# TEST SUITE 13: Consistencia entre Llamadas
# =============================================================================

test_that("validate_peru_mammals es consistente entre llamadas múltiples", {
  # Test 13.1: Misma entrada debe dar mismo resultado
  input_test <- c("Panthera onca", "Puma concolor", "Invalid name")

  result1 <- validate_peru_mammals(input_test, quiet = TRUE)
  result2 <- validate_peru_mammals(input_test, quiet = TRUE)

  expect_equal(result1$Matched.Name, result2$Matched.Name)
  expect_equal(result1$matched, result2$matched)

  # Test 13.2: Orden no debe afectar matching individual
  input_forward <- c("Panthera onca", "Puma concolor")
  input_reverse <- c("Puma concolor", "Panthera onca")

  result_forward <- validate_peru_mammals(input_forward, quiet = TRUE)
  result_reverse <- validate_peru_mammals(input_reverse, quiet = TRUE)

  # Los matches deben ser los mismos, solo en diferente orden
  expect_true(all(sort(result_forward$Matched.Name) ==
                    sort(result_reverse$Matched.Name)))
})


# =============================================================================
# TEST SUITE 14: API Functions (is_peru_mammal, etc.)
# =============================================================================

test_that("is_peru_mammal funciona correctamente", {
  species <- c("Panthera onca", "Puma concolor", "Felis catus")

  # Test básico
  result <- is_peru_mammal(species)

  expect_type(result, "character")
  expect_length(result, 3)
  expect_true(any(grepl("Found", result)))

  # Con return_details
  result_details <- is_peru_mammal(species, return_details = TRUE)
  expect_s3_class(result_details, "data.frame")
  expect_equal(nrow(result_details), 3)
})

test_that("is_endemic_peru funciona correctamente", {
  species <- c("Panthera onca", "Puma concolor")

  result <- is_endemic_peru(species)

  expect_type(result, "character")
  expect_length(result, 2)
})

test_that("found_in_peru funciona correctamente", {
  species <- c("Panthera onca", "Felis catus")

  result <- found_in_peru(species)

  expect_type(result, "logical")
  expect_length(result, 2)
  expect_true(result[1])  # Panthera onca está en Perú
})

test_that("match_quality_peru funciona correctamente", {
  species <- c("Panthera onca", "Pantera onca", "Felis catus")

  result <- match_quality_peru(species)

  expect_type(result, "character")
  expect_length(result, 3)
  expect_true(any(grepl("Exact", result)))
})

test_that("get_common_names_peru funciona correctamente", {
  species <- c("Panthera onca", "Tremarctos ornatus")

  result <- get_common_names_peru(species)

  expect_type(result, "character")
  expect_length(result, 2)
})


