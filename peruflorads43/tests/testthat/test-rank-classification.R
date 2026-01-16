# ==============================================================================
# TEST SUITE 2: Rank Classification
# ==============================================================================
# Validates .splist_classify() and .transform_split_classify()
# Based on critical rank validation issues found during development

test_that("Binomial names are classified as Rank 2", {
  input <- c(
    "Cattleya maxima",
    "Polylepis incana",
    "Puya raimondii"
  )

  result <- peruflorads43:::.splist_classify(input) |>
    peruflorads43:::.transform_split_classify()

  expect_equal(result$Rank, c(2L, 2L, 2L))
  expect_false(any(is.na(result$Orig.Genus)))
  expect_false(any(is.na(result$Orig.Species)))
  expect_true(all(is.na(result$Orig.Infraspecies)))
})

test_that("Trinomial names are classified as Rank 3", {
  # Issue discovered: Must have both Orig.Infra.Rank AND Orig.Infraspecies
  input <- c(
    "Cattleya maxima var. alba",
    "Polylepis incana subsp. incana",
    "Gentianella bicolor f. bicolor"
  )

  result <- peruflorads43:::.splist_classify(input) |>
    peruflorads43:::.transform_split_classify()

  expect_equal(result$Rank, c(3L, 3L, 3L))
  expect_false(any(is.na(result$Orig.Genus)))
  expect_false(any(is.na(result$Orig.Species)))
  expect_false(any(is.na(result$Orig.Infraspecies)))
  expect_false(any(is.na(result$Orig.Infra.Rank)))

  # Infraspecies_2 should be NA for Rank 3
  expect_true(all(is.na(result$Orig.Infraspecies_2)))
})

test_that("Quaternomial names are classified as Rank 4", {
  # Critical issue: Rank 4 requires BOTH levels of infraspecies
  input <- c(
    "Haageocereus acranthus subsp. olowinskianus f. deflexispinus"
  )

  result <- peruflorads43:::.splist_classify(input) |>
    peruflorads43:::.transform_split_classify()

  expect_equal(result$Rank, 4L)
  expect_false(is.na(result$Orig.Genus))
  expect_false(is.na(result$Orig.Species))
  expect_false(is.na(result$Orig.Infraspecies))
  expect_false(is.na(result$Orig.Infra.Rank))
  expect_false(is.na(result$Orig.Infraspecies_2))
  expect_false(is.na(result$Orig.Infra.Rank_2))

  expect_equal(result$Orig.Infra.Rank, "SUBSP.")
  expect_equal(result$Orig.Infra.Rank_2, "F.")
})

test_that("Genus-only names are classified as Rank 1", {
  # Issue discovered: "Microchilus" should be Rank 1
  input <- c("Microchilus", "Cattleya", "Polylepis")

  result <- peruflorads43:::.splist_classify(input) |>
    peruflorads43:::.transform_split_classify()

  expect_equal(result$Rank, c(1L, 1L, 1L))
  expect_false(any(is.na(result$Orig.Genus)))
  expect_true(all(is.na(result$Orig.Species)))
})

test_that("Authors are separated correctly", {
  input <- c(
    "Cattleya maxima Lindl.",
    "Polylepis incana Kunth",
    "Puya raimondii Harms"
  )

  result <- peruflorads43:::.splist_classify(input) |>
    peruflorads43:::.transform_split_classify()

  expect_equal(result$Author, c("LINDL.", "KUNTH", "HARMS"))
})

test_that("Multiple infraspecific categories are ordered correctly", {
  # First non-empty infraspecific category should be level 1
  # Second should be level 2
  input <- "Species alba var. rubra f. compacta"

  result <- peruflorads43:::.splist_classify(input) |>
    peruflorads43:::.transform_split_classify()

  expect_equal(result$Orig.Infraspecies, "RUBRA")
  expect_equal(result$Orig.Infra.Rank, "VAR.")
  expect_equal(result$Orig.Infraspecies_2, "COMPACTA")
  expect_equal(result$Orig.Infra.Rank_2, "F.")
})
