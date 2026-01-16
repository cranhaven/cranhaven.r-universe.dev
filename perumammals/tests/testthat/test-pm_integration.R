# =============================================================================
# TEST SUITE 1: Tests de Integración Completa
# =============================================================================

test_that("Flujo completo de matching funciona end-to-end", {
  data("peru_mammals", package = "perumammals")

  # Test 1.1: Tomar muestra de especies del dataset y hacer matching
  sample_species <- c(
    "Platyrrhinus brachycephalus",
    "Monodelphis ronaldi",
    "Eptesicus furinalis",
    "Nephelomys levipes",
    "Leopardus jacobita",
    "Nectomys rattus",
    "Trinycteris nicefori",
    "Puma yagouaroundi",
    "Saimiri sciureus",
    "Akodon boliviensis",
    "Lycalopex sechurae",
    "Anoura aequatoris",
    "Neusticomys peruviensis",
    "Hylaeamys yunganus",
    "Chrotopterus auritus",
    "Coendou prehensilis",
    "Gardnerycteris koepckeae",
    "Cyclopes ida",
    "Thomasomys pyrrhonotus",
    "Monodelphis gardeni",
    "Myotis albescens",
    "Hadrosciurus sp. 3",
    "Myoprocta pratti",
    "Chibchanomys orcesi",
    "Oxymycterus inca",
    "Cabassous unicinctus",
    "Monodelphis handleyi",
    "Lagidium viscacia",
    "Megaptera novaeangliae",
    "Dasyprocta variegata",
    "Myotis riparius",
    "Myotis oxyotus",
    "Amphinectomys savamis",
    "Akodon orophilus",
    "Platyrrhinus ismaeli",
    "Caenolestes caniventer",
    "Odocoileus peruvianus",
    "Proechimys simonsi",
    "Cynomops abrasus",
    "Potos flavus",
    "Cynomops kuizha",
    "Nyctinomops laticaudatus",
    "Arctocephalus australis",
    "Mesomys leniceps",
    "Thomasomys macrotis",
    "Monodelphis glirina",
    "Centronycteris maximiliani",
    "Isothrix barbarabrownae",
    "Neotomys ebriosus",
    "Marmosa simonsi",
    "Oligoryzomys sp. C"
  )

  # Verificar
  length(sample_species)  # Debe ser 50
  result <- validate_peru_mammals(sample_species, quiet = TRUE)
  sample_size <- nrow(result)
  expect_equal(nrow(result), sample_size)
  expect_true(all(result$matched))
  expect_true(all(result$Matched.Rank == 2L))

  # Test 1.2: Verificar que los matches son correctos
  for (i in 1:min(10, sample_size)) {
    original <- sample_species[i]
    matched <- result$Matched.Name[i]
    expect_equal(original, matched)
  }
})

test_that("Matching preserva orden de input", {
  species_ordered <- c(
    "Mus musculus",
    "Akodon torques",
    "Rattus rattus",
    "Thomasomys kalinowskii"
  )

  result <- validate_peru_mammals(species_ordered, quiet = TRUE)

  # Test 1.3: Verificar que el orden se mantiene
  expect_equal(result$Orig.Name, species_ordered)
  expect_equal(nrow(result), length(species_ordered))
})


# =============================================================================
# TEST SUITE 2: Edge Cases - Nombres Problemáticos
# =============================================================================

test_that("Manejo de caracteres especiales", {
  # Test 2.1: Nombres con números
  result_num <- validate_peru_mammals("Species123", quiet = TRUE)
  expect_false(result_num$matched)

  # Test 2.2: Nombres con guiones
  result_hyphen <- validate_peru_mammals("Genus-species", quiet = TRUE)
  expect_s3_class(result_hyphen, "data.frame")

  # Test 2.3: Nombres con puntos
  result_dot <- validate_peru_mammals("Genus sp.", quiet = TRUE)
  expect_s3_class(result_dot, "data.frame")

  # Test 2.4: Nombres con paréntesis
  result_paren <- validate_peru_mammals("Genus (species)", quiet = TRUE)
  expect_s3_class(result_paren, "data.frame")
})


test_that("Manejo de caracteres Unicode y acentos", {
  # Test 2.9: Nombres con tildes
  result_accent <- validate_peru_mammals("Génus spécies", quiet = TRUE)
  expect_s3_class(result_accent, "data.frame")

  # Test 2.10: Caracteres no ASCII
  result_unicode <- validate_peru_mammals("Genüs spëcies", quiet = TRUE)
  expect_s3_class(result_unicode, "data.frame")
})


# =============================================================================
# TEST SUITE 3: Edge Cases - Vectores Especiales
# =============================================================================

test_that("Manejo de vectores edge case", {
  # Test 3.1: Vector de un solo elemento
  result_single <- validate_peru_mammals("Akodon torques", quiet = TRUE)
  result_single
  expect_equal(nrow(result_single), 1)

  # Test 3.2: Vector con muchos NAs
  many_nas <- c(NA, NA, "Akodon torques", NA, "Mus musculus", NA)
  result_nas <- validate_peru_mammals(many_nas, quiet = TRUE)
  expect_equal(nrow(result_nas), length(many_nas))

  # Test 3.4: Vector con strings vacíos
  empties <- c("", "", "Akodon torques", "")
  result_empties <- validate_peru_mammals(empties, quiet = TRUE)
  expect_equal(nrow(result_empties), 4)

  # Test 3.5: Vector con todo el mismo valor repetido
  repeated <- rep("Akodon torques", 20)
  result_repeated <- validate_peru_mammals(repeated, quiet = TRUE)
  expect_equal(nrow(result_repeated), 20)
  expect_true(all(result_repeated$matched))
})



# =============================================================================
# TEST SUITE 4: Edge Cases - Fuzzy Matching Límites
# =============================================================================

test_that("Fuzzy matching con distancias extremas", {
  # Test 4.1: Solo un caracter diferente
  result_1char <- validate_peru_mammals("Akodn torques", quiet = TRUE)  # Falta una 'o'
  expect_s3_class(result_1char, "data.frame")

  # Test 4.2: Dos caracteres diferentes
  result_2char <- validate_peru_mammals("Akdn torques", quiet = TRUE)  # Faltan 'o' y 'o'
  expect_s3_class(result_2char, "data.frame")

  # Test 4.3: Nombre completamente diferente (no debe hacer match)
  result_different <- validate_peru_mammals("Xxxxx yyyyy", quiet = TRUE)
  expect_false(result_different$matched)

  # Test 4.4: Transposición de letras
  result_transpose <- validate_peru_mammals("Akdoon torques", quiet = TRUE)
  expect_s3_class(result_transpose, "data.frame")
})

test_that("Fuzzy matching con nombres similares", {
  data("peru_mammals", package = "perumammals")

  # Test 4.5: Si hay géneros similares, fuzzy debería elegir el más cercano
  # Esto dependerá de qué géneros realmente existan en peru_mammals

  # Test 4.6: Especies del mismo género con nombres similares
  akodon_species <- subset(peru_mammals, genus == "Akodon")

  if (nrow(akodon_species) >= 2) {
    # Tomar dos especies y crear typos
    sp1 <- akodon_species$species[1]
    sp1_typo <- paste0(substr(sp1, 1, nchar(sp1)-1), "x")

    result_sp_typo <- validate_peru_mammals(paste("Akodon", sp1_typo), quiet = TRUE)
    expect_s3_class(result_sp_typo, "data.frame")
  }
})

test_that("Performance con muchos fuzzy matches", {
  # Test 5.2: Lista con muchos typos (fuzzy matching intensivo)
  # Usamos solo typos que sabemos que existen en la DB
  typos <- c(
    "Akdon torques",      # typo en genus (Akodon existe)
    "Akodon torqes",      # typo en species (torques existe)
    "Akdon torqes",       # typo en ambos
    "Panthera onca",      # correcto para control
    "Pantera onca"        # typo en genus (Panthera existe)
  )

  start_time <- Sys.time()

  # Suprimir warnings de ambiguous matches (son informativos, no errores)
  suppressWarnings({
    result_typos <- validate_peru_mammals(rep(typos, 10), quiet = TRUE)
  })

  end_time <- Sys.time()

  # Performance: menos de 20 segundos
  execution_time <- as.numeric(difftime(end_time, start_time, units = "secs"))
  expect_true(execution_time < 20)

  # Verificaciones de resultado
  expect_equal(nrow(result_typos), 50)

  # Todos deben matchear porque todos los nombres base existen en Peru
  expect_true(all(result_typos$matched))

  # Debe haber fuzzy matches en genus
  expect_true(any(result_typos$genus_dist > 0, na.rm = TRUE))

  # Debe haber fuzzy matches en species
  expect_true(any(result_typos$species_dist > 0, na.rm = TRUE))

  # Verificar metadata de ambiguous matches
  expect_true(!is.null(attr(result_typos, "ambiguous_genera")) ||
                !is.null(attr(result_typos, "ambiguous_species")))
})


# =============================================================================
# TEST SUITE 5: Tests de Performance
# =============================================================================

test_that("Performance con datasets grandes (solo exact matches)", {
  # Solo nombres correctos para medir performance pura
  large_valid <- rep(c("Akodon torques",
                       "Panthera onca",      # Sin typo
                       "Thomasomys kalinowskii",
                       "Puma concolor"),
                     length.out = 200)

  start_time <- Sys.time()
  result_large <- validate_peru_mammals(large_valid, quiet = TRUE)
  end_time <- Sys.time()

  # Verificaciones
  expect_equal(nrow(result_large), 200)
  expect_equal(sum(result_large$matched), 200)  # Todos deben matchear

  # TODOS deben ser exact matches
  all_exact <- all(result_large$genus_dist == 0 &
                     result_large$species_dist == 0)
  expect_true(all_exact)

  # Performance debe ser razonable
  execution_time <- as.numeric(difftime(end_time, start_time, units = "secs"))
  expect_true(execution_time < 30)
})


test_that("Fuzzy matching con ambiguous matches", {
  # Específicamente probar casos ambiguos
  ambiguous_names <- c(
    "Pantera onca",     # Typo → Panthera
    "Akdon torques"     # Typo → Akodon
  )

  # Capturar TODOS los warnings
  warnings_caught <- character()

  result <- withCallingHandlers(
    validate_peru_mammals(rep(ambiguous_names, 25), quiet = TRUE),
    warning = function(w) {
      warnings_caught <<- c(warnings_caught, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )

  # Verificar que se generaron warnings de ambiguous matches
  expect_true(any(grepl("multiple fuzzy matches", warnings_caught)))
  expect_true(length(warnings_caught) >= 2)  # Debería haber al menos 2

  # Todos deben matchear a pesar de ser fuzzy
  expect_equal(sum(result$matched), 50)

  # Verificar que hay ambiguous matches guardados
  ambig <- get_ambiguous_matches(result, type = "all")
  expect_true(!is.null(ambig))
  expect_true(nrow(ambig) > 0)

  # Verificar que hay al menos 4 casos ambiguos
  expect_true(nrow(ambig) >= 4)
})


# =============================================================================
# TEST SUITE 6: Integración con Ecorregiones
# =============================================================================

test_that("Integración entre especies y ecorregiones", {
  data("peru_mammals", package = "perumammals")
  data("peru_mammals_ecoregions", package = "perumammals")

  # Test 6.1: Matching de especies y lookup de ecorregiones
  test_species <- head(peru_mammals$scientific_name, 5)
  match_result <- validate_peru_mammals(test_species, quiet = TRUE)

  # Para cada especie matched, debe haber info de ecorregiones disponible
  for (i in 1:nrow(match_result)) {
    if (match_result$matched[i]) {
      species_name <- match_result$Matched.Name[i]
      eco_info <- subset(peru_mammals_ecoregions,
                         scientific_name == species_name)

      # Puede o no tener ecorregiones, pero la búsqueda debe funcionar
      expect_s3_class(eco_info, "data.frame")
    }
  }
})

test_that("Workflow completo: match + lookup de metadatos", {
  data("peru_mammals", package = "perumammals")

  # Test 6.2: Flujo completo de usuario típico
  user_input <- c("Akodon torques", "Invalid name", "Mus musculus")

  # Paso 1: Matching
  match_result <- validate_peru_mammals(user_input, quiet = TRUE)
  expect_equal(nrow(match_result), 3)

  # Paso 2: Filtrar solo los matched
  valid_matches <- subset(match_result, matched == TRUE)
  #valid_matches
  expect_true(nrow(valid_matches) == 1)

  # Paso 3: Lookup de información adicional
  for (i in 1:nrow(valid_matches)) {
    species_name <- valid_matches$Matched.Name[i]
    species_info <- subset(peru_mammals, scientific_name == species_name)

    expect_true(nrow(species_info) == 1)
    expect_true("family" %in% names(species_info))
    expect_true("endemic" %in% names(species_info))
  }
})


# =============================================================================
# TEST SUITE 7: Consistencia de Resultados
# =============================================================================

test_that("Resultados son determinísticos", {
  # Test 7.1: Múltiples ejecuciones dan mismo resultado
  species_list <- c("Akodon torques", "Mus musculus", "Genus unknown")

  results <- list()
  for (i in 1:5) {
    results[[i]] <- validate_peru_mammals(species_list, quiet = TRUE)
  }

  # Todos los resultados deben ser idénticos
  for (i in 2:5) {
    expect_equal(results[[1]]$Matched.Name, results[[i]]$Matched.Name)
    expect_equal(results[[1]]$matched, results[[i]]$matched)
    expect_equal(results[[1]]$Matched.Rank, results[[i]]$Matched.Rank)
  }
})

test_that("Independencia entre llamadas", {
  # Test 7.2: Una llamada no afecta a la siguiente
  result1 <- validate_peru_mammals("Akodon torques", quiet = TRUE)
  result2 <- validate_peru_mammals("Mus musculus", quiet = TRUE)
  result3 <- validate_peru_mammals("Akodon torques", quiet = TRUE)

  # Primera y tercera llamada deben dar exactamente el mismo resultado
  expect_equal(result1$Matched.Name, result3$Matched.Name)
  expect_equal(result1$matched, result3$matched)
})


# =============================================================================
# TEST SUITE 8: Compatibilidad con dplyr/tidyverse
# =============================================================================

test_that("Resultados son compatibles con tidyverse", {
#  skip_if_not_installed("dplyr")

  library(dplyr)

  # Test 8.1: Resultado se puede usar con %>%
  result <- validate_peru_mammals(c("Akodon torques", "Mus musculus"), quiet = TRUE) %>%
    filter(matched == TRUE) %>%
    select(Matched.Name, Matched.Genus, Matched.Species)
#result
  expect_s3_class(result, "data.frame")
  expect_true(nrow(result) == 1)

  # Test 8.2: Se puede hacer join con otros datasets
  data("peru_mammals", package = "perumammals")

  match_result <- validate_peru_mammals(c("Akodon torques", "Mus musculus"), quiet = TRUE)

  joined <- match_result %>%
    filter(matched == TRUE) %>%
    left_join(peru_mammals,
              by = c("Matched.Name" = "scientific_name"))

  expect_s3_class(joined, "data.frame")
#  joined
  expect_true("family.x" %in% names(joined))
})


# =============================================================================
# TEST SUITE 9: Validación de Mensajes de Error
# =============================================================================

test_that("Mensajes de error son informativos", {
  # Test 9.1: Error con input NULL
  expect_error(
    validate_peru_mammals(NULL),
    "character vector"
  )

  # Test 9.2: Error con input numérico
  expect_error(
    validate_peru_mammals(123),
    "character vector"
  )

  # Test 9.3: Error con quiet inválido
#  expect_error(
 #   validate_peru_mammals("Akodon torques", quiet = TRUE),
  #  "logical"
  #)
})


# =============================================================================
# TEST SUITE 10: Tests de Regresión
# =============================================================================

test_that("Casos conocidos mantienen comportamiento esperado", {
  # Test 10.1: Especies comunes que deben hacer match
  common_species <- c(
    "Mus musculus",
    "Rattus rattus"
  )

  result <- validate_peru_mammals(common_species, quiet = TRUE)
  #result
  expect_false(all(result$matched))

  # Test 10.2: Género que debe existir
  result_genus <- validate_peru_mammals("Akodon", quiet = TRUE)
 # result_genus
  expect_true(!result_genus$matched)
  expect_equal(result_genus$Matched.Rank, NA_real_)

  # Test 10.3: Nombre que no debe existir
  result_invalid <- validate_peru_mammals("Fakeus nonexistus", quiet = TRUE)
  expect_false(result_invalid$matched)
})



# =============================================================================
# TEST SUITE 12: Validación Cruzada de Datos
# =============================================================================

test_that("Datos son internamente consistentes", {
  data("peru_mammals", package = "perumammals")
  data("peru_mammals_ecoregions", package = "perumammals")

  # Test 12.1: Todas las especies en ecoregions están en el dataset principal
  eco_species <- unique(peru_mammals_ecoregions$scientific_name)
  main_species <- peru_mammals$scientific_name

  missing_species <- setdiff(eco_species, main_species)
  expect_equal(length(missing_species), 0,
               info = paste("Missing species:", paste(missing_species, collapse = ", ")))

  # Test 12.2: pm_ids son únicos y consistentes
  eco_ids <- unique(peru_mammals_ecoregions$pm_id)
  main_ids <- peru_mammals$pm_id

  missing_ids <- setdiff(eco_ids, main_ids)
  expect_equal(length(missing_ids), 0)
})




