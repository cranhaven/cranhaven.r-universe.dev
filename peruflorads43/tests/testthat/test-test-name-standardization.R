# ==============================================================================
# TEST SUITE 1: Name Standardization
# ==============================================================================
# Validates the .names_standardize() function behavior
# Based on issues found during development with hybrids, spaces, and case

test_that("Hybrid markers are removed correctly with warning", {
  input <- c(
    "x Lycida mathiasiae",
    "Cattleya x maxima"
  )

  expected <- c(
    "LYCIDA MATHIASIAE",
    "CATTLEYA MAXIMA"
  )

  # Captura el warning y, a la vez, asigna la salida a 'out'
  expect_warning(
    out <- peruflorads43:::.names_standardize(input),
    regexp = "The 'X' sign indicating hybrids have been removed",
    fixed  = FALSE
  )

  # Verifica el resultado (no vuelvas a llamar la función para evitar un segundo warning)
  expect_equal(out, expected)
})

test_that("Leading and trailing whitespace is removed", {
  # Issue discovered: "Deprea macrocalyx " had trailing space
  input <- c(
    " Cattleya maxima",
    "Polylepis incana ",
    "  Puya raimondii  "
  )

  expected <- c(
    "CATTLEYA MAXIMA",
    "POLYLEPIS INCANA",
    "PUYA RAIMONDII"
  )

  result <- peruflorads43:::.names_standardize(input)

  expect_equal(result, expected)
})

test_that("Infraspecific rank tags are standardized", {
  # Multiple forms of the same rank should be standardized
  input <- c(
    "Cattleya maxima var alba",
    "Cattleya maxima var. alba",
    "Cattleya maxima VAR alba",
    "Polylepis incana subsp. incana",
    "Polylepis incana ssp incana",
    "Polylepis incana ssp. incana",
    "Gentianella bicolor f. bicolor",
    "Gentianella bicolor forma bicolor",
    "Gentianella bicolor form. bicolor"
  )

  expected <- c(
    "CATTLEYA MAXIMA VAR. ALBA",
    "CATTLEYA MAXIMA VAR. ALBA",
    "CATTLEYA MAXIMA VAR. ALBA",
    "POLYLEPIS INCANA SUBSP. INCANA",
    "POLYLEPIS INCANA SUBSP. INCANA",
    "POLYLEPIS INCANA SUBSP. INCANA",
    "GENTIANELLA BICOLOR F. BICOLOR",
    "GENTIANELLA BICOLOR F. BICOLOR",
    "GENTIANELLA BICOLOR F. BICOLOR"
  )

  result <- peruflorads43:::.names_standardize(input)

  expect_equal(result, expected)
})

test_that("Multiple spaces are collapsed to single space", {
  input <- c(
    "Cattleya    maxima",
    "Polylepis  incana   var.  incana"
  )

  expected <- c(
    "CATTLEYA MAXIMA",
    "POLYLEPIS INCANA VAR. INCANA"
  )

  result <- peruflorads43:::.names_standardize(input)

  expect_equal(result, expected)
})

test_that("NA values are preserved", {
  input <- c("Cattleya maxima", NA, "Polylepis incana", NA)

  result <- peruflorads43:::.names_standardize(input)

  expect_length(result, 4)
  expect_true(is.na(result[2]))
  expect_true(is.na(result[4]))
  expect_false(is.na(result[1]))
  expect_false(is.na(result[3]))
})

test_that("Case conversion works correctly", {
  input <- c(
    "cattleya maxima",
    "POLYLEPIS INCANA",
    "Puya Raimondii",
    "gEnTiAnElLa BiCoLoR"
  )

  # All should be uppercase
  result <- peruflorads43:::.names_standardize(input)

  expect_true(all(result == toupper(result), na.rm = TRUE))
})
