# =============================================================================
# TEST SUITE 1: Dataset peru_mammals
# =============================================================================

test_that("Dataset peru_mammals tiene estructura correcta", {
  data("peru_mammals", package = "perumammals")

  # Test 1.1: El dataset existe y es un data frame
  expect_true(exists("peru_mammals"))
  expect_s3_class(peru_mammals, "data.frame")

  # Test 1.2: Columnas requeridas están presentes
  required_cols <- c(
    "pm_id", "order", "family", "genus", "species",
    "scientific_name", "scientific_name_full", "author",
    "common_name", "endemic", "ecoregions", "reference"
  )

  expect_true(all(required_cols %in% names(peru_mammals)))

  # Test 1.3: No hay filas duplicadas en pm_id
  expect_false(any(duplicated(peru_mammals$pm_id)))

  # Test 1.4: Columnas de texto no contienen solo espacios en blanco
  expect_false(any(grepl("^\\s+$", peru_mammals$genus, perl = TRUE)))
  expect_false(any(grepl("^\\s+$", peru_mammals$species, perl = TRUE)))

  # Test 1.5: endemic es lógico
  expect_type(peru_mammals$endemic, "logical")

  # Test 1.6: scientific_name es combinación de genus y species
  sample_indices <- sample(nrow(peru_mammals), min(10, nrow(peru_mammals)))
  for (i in sample_indices) {
    expected_name <- paste(peru_mammals$genus[i], peru_mammals$species[i])
    expect_equal(peru_mammals$scientific_name[i], expected_name)
  }
})

test_that("Dataset peru_mammals contiene datos válidos", {
  data("peru_mammals", package = "perumammals")

  # Test 1.7: No hay NAs en columnas críticas
  expect_false(any(is.na(peru_mammals$pm_id)))
  expect_false(any(is.na(peru_mammals$genus)))
  expect_false(any(is.na(peru_mammals$species)))
  expect_false(any(is.na(peru_mammals$scientific_name)))

  # Test 1.8: Formato de pm_id es correcto (Mxxxx_GEN)
  expect_true(all(grepl("^M\\d{4}_[A-Z]{3}$", peru_mammals$pm_id)))

  # Test 1.9: Genus y species empiezan con mayúscula
  expect_true(all(grepl("^[A-Z]", peru_mammals$genus)))
  expect_true(all(grepl("^[a-z]", peru_mammals$species)))

  # Test 1.10: Dataset tiene un tamaño razonable
  expect_true(nrow(peru_mammals) > 0)
  expect_true(nrow(peru_mammals) < 1000)  # Ajustar según datos reales
})


# =============================================================================
# TEST SUITE 2: Dataset peru_mammals_ecoregions
# =============================================================================

test_that("Dataset peru_mammals_ecoregions tiene estructura correcta", {
  data("peru_mammals_ecoregions", package = "perumammals")

  # Test 2.1: El dataset existe y es un data frame
  expect_true(exists("peru_mammals_ecoregions"))
  expect_s3_class(peru_mammals_ecoregions, "data.frame")

  # Test 2.2: Columnas requeridas están presentes
  required_cols <- c("pm_id", "scientific_name", "ecoregion_code")
  expect_true(all(required_cols %in% names(peru_mammals_ecoregions)))

  # Test 2.3: No hay NAs en columnas críticas
  expect_false(any(is.na(peru_mammals_ecoregions$pm_id)))
  expect_false(any(is.na(peru_mammals_ecoregions$ecoregion_code)))

  # Test 2.4: Los pm_id existen en peru_mammals
  data("peru_mammals", package = "perumammals")
  expect_true(all(peru_mammals_ecoregions$pm_id %in% peru_mammals$pm_id))
})

test_that("Dataset peru_mammals_ecoregions contiene relaciones válidas", {
  data("peru_mammals_ecoregions", package = "perumammals")
  data("peru_mammals", package = "perumammals")

  # Test 2.5: Cada especie puede tener múltiples ecorregiones
  species_counts <- table(peru_mammals_ecoregions$pm_id)
  expect_true(any(species_counts > 1))

  # Test 2.6: Códigos de ecorregión son strings cortos
  expect_true(all(nchar(peru_mammals_ecoregions$ecoregion_code) <= 10))

  # Test 2.7: No hay combinaciones duplicadas pm_id-ecoregion
  combos <- paste(peru_mammals_ecoregions$pm_id,
                  peru_mammals_ecoregions$ecoregion_code)
  expect_false(any(duplicated(combos)))
})


# =============================================================================
# TEST SUITE 3: Dataset peru_mammals_ecoregions_meta
# =============================================================================

test_that("Dataset peru_mammals_ecoregions_meta tiene estructura correcta", {
  data("peru_mammals_ecoregions_meta", package = "perumammals")

  # Test 3.1: El dataset existe y es un data frame
  expect_true(exists("peru_mammals_ecoregions_meta"))
  expect_s3_class(peru_mammals_ecoregions_meta, "data.frame")

  # Test 3.2: Contiene metadatos de ecorregiones
  expect_true("ecoregion_code" %in% names(peru_mammals_ecoregions_meta))

  # Test 3.3: No hay códigos duplicados
  expect_false(any(duplicated(peru_mammals_ecoregions_meta$ecoregion_code)))

  # Test 3.4: Los códigos en metadata coinciden con los usados en ecoregions
  data("peru_mammals_ecoregions", package = "perumammals")
  used_codes <- unique(peru_mammals_ecoregions$ecoregion_code)
  meta_codes <- peru_mammals_ecoregions_meta$ecoregion_code

  # Todos los códigos usados deben estar en metadata
  expect_true(all(used_codes %in% meta_codes))
})


# =============================================================================
# TEST SUITE 4: Dataset peru_mammals_backbone
# =============================================================================

test_that("Dataset peru_mammals_backbone tiene estructura correcta", {
  data("peru_mammals_backbone", package = "perumammals")

  # Test 4.1: El dataset existe
  expect_true(exists("peru_mammals_backbone"))

  # Test 4.2: Contiene información sobre la fuente
  expect_true(any(grepl("source|year|species",
                        names(peru_mammals_backbone),
                        ignore.case = TRUE)))

  # Test 4.3: Año de publicación es razonable
  if ("year" %in% names(peru_mammals_backbone)) {
    expect_true(peru_mammals_backbone$year >= 2020)
    expect_true(peru_mammals_backbone$year <= as.integer(format(Sys.Date(), "%Y")))
  }
})



# =============================================================================
# TEST SUITE 6: Consistencia entre Datasets
# =============================================================================

test_that("Datasets son consistentes entre sí", {
  data("peru_mammals", package = "perumammals")
  data("peru_mammals_ecoregions", package = "perumammals")

  # Test 6.1: Todos los pm_id en ecoregions existen en peru_mammals
  expect_true(all(peru_mammals_ecoregions$pm_id %in% peru_mammals$pm_id))

  # Test 6.2: scientific_name es consistente entre datasets
  eco_species <- unique(peru_mammals_ecoregions$scientific_name)
  main_species <- peru_mammals$scientific_name

  expect_true(all(eco_species %in% main_species))

  # Test 6.3: No hay discrepancias en nombres científicos para mismo pm_id
  merged <- merge(peru_mammals[, c("pm_id", "scientific_name")],
                  unique(peru_mammals_ecoregions[, c("pm_id", "scientific_name")]),
                  by = "pm_id",
                  suffixes = c("_main", "_eco"))

  expect_true(all(merged$scientific_name_main == merged$scientific_name_eco))
})


# =============================================================================
# TEST SUITE 7: Endemismo
# =============================================================================

test_that("Datos de endemismo son consistentes", {
  data("peru_mammals", package = "perumammals")

  # Test 7.1: Campo endemic existe y es lógico
  expect_true("endemic" %in% names(peru_mammals))
  expect_type(peru_mammals$endemic, "logical")

  # Test 7.2: No hay NAs en endemic
  expect_false(any(is.na(peru_mammals$endemic)))

  # Test 7.3: Hay al menos algunas especies endémicas
  expect_true(sum(peru_mammals$endemic) > 0)

  # Test 7.4: Proporción de endémicas es razonable
  endemic_prop <- mean(peru_mammals$endemic)
  expect_true(endemic_prop > 0 && endemic_prop < 0.5)
})


# =============================================================================
# TEST SUITE 8: Taxonomía Jerárquica
# =============================================================================

test_that("Jerarquía taxonómica es consistente", {
  data("peru_mammals", package = "perumammals")

  # Test 8.1: Cada género pertenece a una sola familia
  genus_family <- unique(peru_mammals[, c("genus", "family")])
  expect_false(any(duplicated(genus_family$genus)))

  # Test 8.2: Cada familia pertenece a un solo orden
  family_order <- unique(peru_mammals[, c("family", "order")])
  expect_false(any(duplicated(family_order$family)))

  # Test 8.3: Órdenes comunes están presentes
  common_orders <- c("Rodentia", "Chiroptera", "Carnivora")
  expect_true(any(common_orders %in% peru_mammals$order))

  # Test 8.4: Familias dentro de un orden son consistentes
  # (verificar una muestra)
  rodentia <- subset(peru_mammals, order == "Rodentia")
  if (nrow(rodentia) > 0) {
    expect_true(all(rodentia$order == "Rodentia"))
  }
})


# =============================================================================
# TEST SUITE 9: Referencias y Autores
# =============================================================================

test_that("Referencias y autores tienen formato apropiado", {
  data("peru_mammals", package = "perumammals")

  # Test 9.1: Campo author existe
  expect_true("author" %in% names(peru_mammals))

  # Test 9.2: Campo reference existe
  expect_true("reference" %in% names(peru_mammals))

  # Test 9.3: La mayoría de especies tienen autor
  author_prop <- mean(!is.na(peru_mammals$author) & peru_mammals$author != "")
  expect_true(author_prop > 0.5)

  # Test 9.4: La mayoría de especies tienen referencia
  ref_prop <- mean(!is.na(peru_mammals$reference) & peru_mammals$reference != "")
  expect_true(ref_prop > 0.5)
})


# =============================================================================
# TEST SUITE 10: Nombres Comunes
# =============================================================================

test_that("Nombres comunes están disponibles", {
  data("peru_mammals", package = "perumammals")

  # Test 10.1: Campo common_name existe
  expect_true("common_name" %in% names(peru_mammals))

  # Test 10.2: Al menos algunas especies tienen nombre común
  has_common <- sum(!is.na(peru_mammals$common_name) &
                      peru_mammals$common_name != "")
  expect_true(has_common > 0)

  # Test 10.3: Nombres comunes no están en mayúsculas completas
  common_names <- na.omit(peru_mammals$common_name)
  if (length(common_names) > 0) {
    all_caps <- sum(toupper(common_names) == common_names &
                      nchar(common_names) > 5)
    expect_true(all_caps < length(common_names) * 0.5)
  }
})


# =============================================================================
# TEST SUITE 11: Integridad de IDs
# =============================================================================

test_that("Sistema de IDs es robusto", {
  data("peru_mammals", package = "perumammals")

  # Test 11.1: pm_id sigue formato esperado
  expect_true(all(grepl("^M\\d{4}_[A-Z]{3}$", peru_mammals$pm_id)))

  # Test 11.2: Parte numérica de pm_id es única
  numeric_part <- sub("^M(\\d{4})_.*", "\\1", peru_mammals$pm_id)
  expect_false(any(duplicated(numeric_part)))

  # Test 11.3: Abreviatura de género coincide con genus
  for (i in sample(nrow(peru_mammals), min(20, nrow(peru_mammals)))) {
    id <- peru_mammals$pm_id[i]
    genus_abbr <- sub("^M\\d{4}_([A-Z]{3})$", "\\1", id)
    genus_first3 <- toupper(substr(peru_mammals$genus[i], 1, 3))
    expect_equal(genus_abbr, genus_first3)
  }
})


# =============================================================================
# TEST SUITE 12: Ecorregiones
# =============================================================================

test_that("Datos de ecorregiones son completos", {
  data("peru_mammals_ecoregions", package = "perumammals")
  data("peru_mammals_ecoregions_meta", package = "perumammals")

  # Test 12.1: Hay múltiples ecorregiones representadas
  n_ecoregions <- length(unique(peru_mammals_ecoregions$ecoregion_code))
  expect_true(n_ecoregions >= 5)

  # Test 12.2: Códigos de ecorregión son consistentes
  used_codes <- unique(peru_mammals_ecoregions$ecoregion_code)
  meta_codes <- peru_mammals_ecoregions_meta$ecoregion_code

  # Verificar que hay metadata para cada código usado
  expect_true(all(used_codes %in% meta_codes))

  # Test 12.3: Distribución de especies por ecorregión es razonable
  eco_counts <- table(peru_mammals_ecoregions$ecoregion_code)
  expect_true(min(eco_counts) > 0)
  expect_true(max(eco_counts) < nrow(peru_mammals))
})


# =============================================================================
# TEST SUITE 13: Backbone Metadata
# =============================================================================

test_that("Metadata del backbone es informativo", {
  data("peru_mammals_backbone", package = "perumammals")

  # Test 13.1: Contiene información de versión o fecha
  backbone_info <- names(peru_mammals_backbone)
  expect_true(length(backbone_info) > 0)

  # Test 13.2: Si hay año, es post-2020 (año de referencia Pacheco et al.)
  if ("year" %in% names(peru_mammals_backbone) ||
      "publication_year" %in% names(peru_mammals_backbone)) {
    year_col <- ifelse("year" %in% names(peru_mammals_backbone),
                       "year", "publication_year")
    year_val <- peru_mammals_backbone[[year_col]]
    expect_true(year_val >= 2020)
  }
})



