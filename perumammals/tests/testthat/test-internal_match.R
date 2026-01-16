test_that(".names_standardize maneja casos básicos correctamente", {

  # Test con nombres normales
  input <- c("Panthera onca", "TREMARCTOS ORNATUS", "puma_concolor")
  result <- .names_standardize(input)

  expect_equal(result[1], "PANTHERA ONCA")
  expect_equal(result[2], "TREMARCTOS ORNATUS")
  expect_equal(result[3], "PUMA CONCOLOR")
})

test_that(".names_standardize maneja casos con 'sp.' correctamente", {

  # Casos reales de peru_mammals
  input <- c("Akodon sp. Ancash", "Oligoryzomys sp. B", "Hadrosciurus sp. 3")
  result <- .names_standardize(input)

  expect_equal(result[1], "AKODON SP. ANCASH")
  expect_equal(result[2], "OLIGORYZOMYS SP. B")
  expect_equal(result[3], "HADROSCIURUS SP. 3")
})

test_that(".names_standardize elimina CF. y AFF.", {

  input <- c("Mazama cf. americana", "Odocoileus aff. virginianus")
  result <- .names_standardize(input)

  expect_equal(result[1], "MAZAMA AMERICANA")
  expect_equal(result[2], "ODOCOILEUS VIRGINIANUS")
})

test_that(".names_standardize maneja NAs correctamente", {

  input <- c("Panthera onca", NA, "Puma concolor")
  result <- .names_standardize(input)

  expect_equal(result[1], "PANTHERA ONCA")
  expect_true(is.na(result[2]))
  expect_equal(result[3], "PUMA CONCOLOR")
})

test_that(".splist_classify extrae componentes correctamente", {

  input <- c("Tremarctos ornatus", "Panthera onca (Linnaeus, 1758)")
  result <- .splist_classify(input)
  result

  # Verificar estructura
  expect_true(is.matrix(result))
  expect_equal(ncol(result), 4)  # Orig.Name, Orig.Genus, Orig.Species, Author
  expect_equal(nrow(result), 2)

})



test_that(".transform_split_classify asigna Rank correctamente", {

  input <- c("Panthera", "Panthera onca", "Akodon sp. Ancash")
  classified <- .splist_classify(input)
  result <- .transform_split_classify(classified)

  # Verificar Ranks
  expect_equal(result$Rank[1], 1L)  # Solo género
  expect_equal(result$Rank[2], 2L)  # Binomial normal
  expect_equal(result$Rank[3], 2L)  # Binomial con sp.
})

test_that(".transform_split_classify no tiene columnas de infraespecies", {

  input <- "Panthera onca"
  classified <- .splist_classify(input)
  result <- .transform_split_classify(classified)

  # No debe haber columnas de infraespecies
  expect_false("Orig.Infraspecies" %in% colnames(result))
  expect_false("Orig.Infra.Rank" %in% colnames(result))
  expect_false("Orig.Infraspecies_2" %in% colnames(result))
  expect_false("Orig.Infra.Rank_2" %in% colnames(result))
})

test_that(".check_binomial identifica nombres problemáticos", {

  input <- c("Panthera onca", "Tremarctos", NA, "Puma concolor")
  classified <- .splist_classify(input)

  # Capturar mensajes
  expect_message(
    problematic <- .check_binomial(classified, input),
    "genus level"
  )

  expect_message(
    problematic <- .check_binomial(classified, input),
    "NA value"
  )

  # Debe retornar posiciones 2 y 3
  expect_equal(sort(problematic), c(2, 3))
})

test_that(".check_binomial acepta casos con 'sp.'", {

  # Los casos con 'sp.' son válidos en peru_mammals
  input <- c("Akodon sp. Ancash", "Oligoryzomys sp. B", "Panthera onca")
  classified <- .splist_classify(input)

  problematic <- suppressMessages(.check_binomial(classified, input))

  # No debe haber problemas
  expect_length(problematic, 0)
})

test_that(".str_to_simple_cap formatea correctamente", {

  input <- c("PANTHERA ONCA", "tremarctos ornatus", "AKODON SP. ANCASH")
  result <- sapply(input, .str_to_simple_cap)
  expect_equal(as.character(result[1]), expected =  "Panthera onca")
  expect_equal(as.character(result[2]), "Tremarctos ornatus")
  expect_equal(as.character(result[3]), "Akodon sp. ancash")
})

test_that(".get_mammals_genus filtra correctamente", {

  # Crear data frame de prueba
  test_df <- data.frame(
    genus = c("Akodon", "Akodon", "Panthera", "Puma"),
    species = c("sp. Ancash", "sp. Villa", "onca", "concolor"),
    stringsAsFactors = FALSE
  )

  # Buscar Akodon
  result <- .get_mammals_genus("akodon", test_df)

  expect_equal(nrow(result), 2)
  expect_true(all(result$genus == "Akodon"))
  expect_equal(result$species, c("sp. Ancash", "sp. Villa"))
})

test_that(".get_mammals_genus es case-insensitive", {

  test_df <- data.frame(
    genus = c("Panthera"),
    species = c("onca"),
    stringsAsFactors = FALSE
  )

  # Probar con diferentes casos
  result1 <- .get_mammals_genus("panthera", test_df)
  result2 <- .get_mammals_genus("PANTHERA", test_df)
  result3 <- .get_mammals_genus("Panthera", test_df)

  expect_equal(nrow(result1), 1)
  expect_equal(nrow(result2), 1)
  expect_equal(nrow(result3), 1)
})

# Test de integración completo
test_that("Flujo completo funciona con casos reales de peru_mammals", {

  # Casos reales mezclados
  input <- c(
    "Tremarctos ornatus",      # Normal
    "PANTHERA ONCA",           # Mayúsculas
    "akodon sp. ancash",       # Caso especial en minúsculas
    "Oligoryzomys sp. B",      # Caso especial normal
    "Mazama",                  # Solo género
    NA                         # NA
  )

  # Paso 1: Estandarizar
  standardized <- .names_standardize(input)
  expect_true(!is.na(standardized[1]))
  expect_true(is.na(standardized[6]))

  # Paso 2: Clasificar
  classified <- .splist_classify(input)
  expect_equal(ncol(classified), 4)

  # Paso 3: Transformar
  transformed <- .transform_split_classify(classified)
  expect_true("Rank" %in% colnames(transformed))

  # Paso 4: Verificar
  problematic <- suppressMessages(.check_binomial(classified, input))
  expect_true(5 %in% problematic)  # Mazama (solo género)
  expect_true(6 %in% problematic)  # NA

  # Verificar que casos con sp. son válidos (Rank 2)
  expect_equal(transformed$Rank[3], 2L)  # Akodon sp. Ancash
  expect_equal(transformed$Rank[4], 2L)  # Oligoryzomys sp. B
})

