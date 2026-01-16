# tests/testthat/test-data-peru_mammals.R

test_that("peru_mammals dataset has expected structure and size", {
  # objeto existe
  expect_true(exists("peru_mammals"))

  # clase y dimensiones básicas
  expect_s3_class(peru_mammals, "tbl_df")
  expect_equal(nrow(peru_mammals), 573L)

  # columnas esperadas
  expected_cols <- c(
    "pm_id",
    "order",
    "family",
    "genus",
    "species",
    "scientific_name",
    "scientific_name_full",
    "author",
    "common_name",
    "endemic",
    "ecoregions",
    "reference"
  )

  expect_true(all(expected_cols %in% colnames(peru_mammals)))

  # pm_id debe ser único y no NA
  expect_false(any(is.na(peru_mammals$pm_id)))
  expect_equal(anyDuplicated(peru_mammals$pm_id), 0L)

  # scientific_name no debe tener NA
  expect_false(any(is.na(peru_mammals$scientific_name)))

  # endemic debe ser lógico
  expect_type(peru_mammals$endemic, "logical")
})

test_that("peru_mammals_ecoregions is consistent with peru_mammals", {
  expect_true(exists("peru_mammals_ecoregions"))
  expect_s3_class(peru_mammals_ecoregions, "tbl_df")

  expected_cols <- c("pm_id", "scientific_name", "ecoregion_code")
  expect_true(all(expected_cols %in% colnames(peru_mammals_ecoregions)))

  # pm_id y scientific_name deben existir en peru_mammals
  expect_true(all(peru_mammals_ecoregions$pm_id %in% peru_mammals$pm_id))
  expect_true(all(peru_mammals_ecoregions$scientific_name %in% peru_mammals$scientific_name))

  # ecoregion_code no debe tener NA
  expect_false(any(is.na(peru_mammals_ecoregions$ecoregion_code)))
})

test_that("peru_mammals_ecoregions_meta matches ecoregion codes used", {
  expect_true(exists("peru_mammals_ecoregions_meta"))
  expect_s3_class(peru_mammals_ecoregions_meta, "tbl_df")

  expected_cols <- c("ecoregion_code", "ecoregion_label")
  expect_true(all(expected_cols %in% colnames(peru_mammals_ecoregions_meta)))

  # códigos deben ser únicos y no NA
  expect_false(any(is.na(peru_mammals_ecoregions_meta$ecoregion_code)))
  expect_equal(anyDuplicated(peru_mammals_ecoregions_meta$ecoregion_code), 0L)

  # todos los códigos usados en peru_mammals_ecoregions deben estar en meta
  codes_used <- sort(unique(peru_mammals_ecoregions$ecoregion_code))
  codes_meta <- sort(unique(peru_mammals_ecoregions_meta$ecoregion_code))
  expect_true(all(codes_used %in% codes_meta))
})

test_that("peru_mammals_backbone is coherent with peru_mammals", {
  expect_true(exists("peru_mammals_backbone"))
  expect_s3_class(peru_mammals_backbone, "tbl_df")

  # debería tener una sola fila
  expect_equal(nrow(peru_mammals_backbone), 1L)

  # columnas esperadas
  expected_cols <- c("source", "source_year", "n_species", "created_at")
  expect_true(all(expected_cols %in% colnames(peru_mammals_backbone)))

  # n_species debe coincidir con nrow(peru_mammals)
  expect_equal(peru_mammals_backbone$n_species[[1]], nrow(peru_mammals))

  # año de la fuente debería ser 2021
  expect_equal(peru_mammals_backbone$source_year[[1]], 2021L)
})

