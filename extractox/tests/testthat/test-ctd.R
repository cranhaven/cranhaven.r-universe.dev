library(testthat)

input_terms <- c("50-00-0", "64-17-5", "methanal", "ethanol")

song <- c("bella", "ciao bella ciao", "bella ciao ciao ciao")

expected_columns <- c(
  "chemical_name", "chemical_id", "casrn", "gene_symbol",
  "gene_id", "organism", "organism_id", "pubmed_ids",
  "query"
)

# @@@@@@@@@@@@@
# extr_ctd ----
# @@@@@@@@@@@@@

Sys.sleep(3)

test_that("extr_ctd fetches valid expression data", {
  skip_on_cran()
  skip_if_offline()

  dat <- extr_ctd(
    input_terms = input_terms,
    report_type = "cgixns",
    category = "chem",
    action_types = "expression"
  )

  expect_true(is.data.frame(dat))

  expect_true(all(expected_columns %in% colnames(dat)))
  expect_gt(nrow(dat), 0)
})

Sys.sleep(3)

test_that("extr_ctd fetches other data", {
  skip_on_cran()
  skip_if_offline()


  dat <- extr_ctd(
    input_terms = input_terms,
    category = "chem",
    report_type = "genes_curated",
    input_term_search_type = "directAssociations",
    action_types = "ANY",
    ontology = c("go_bp", "go_cc")
  )
  expect_true(is.data.frame(dat))

  expect_true(all(expected_columns %in% colnames(dat)))
  expect_gt(nrow(dat), 0)
})

Sys.sleep(3)

test_that("extr_ctd no results", {
  skip_on_cran()
  skip_if_offline()

  expect_warning(
    {
      dat <- extr_ctd(
        input_terms = song,
        category = "chem",
        report_type = "genes_curated",
        input_term_search_type = "directAssociations",
        action_types = "ANY",
        ontology = c("go_bp", "go_cc")
      )
    },
    "Chemicals .*not found!"
  )

  expect_equal(nrow(dat), 3)
})

Sys.sleep(3)

test_that("extr_ctd no results with song (verbose = FALSE)", {
  skip_on_cran()
  skip_if_offline()

  # Expect no output and no error when verbose = FALSE
  expect_silent({
    dat <- extr_ctd(
      input_terms = song,
      category = "chem",
      report_type = "genes_curated",
      input_term_search_type = "directAssociations",
      action_types = "ANY",
      ontology = c("go_bp", "go_cc"),
      verbose = FALSE
    )
  })
})

# Check out
Sys.sleep(3)

test_that("extr_tetramer return NAS for unknown ids", {
  skip_on_cran()
  skip_if_offline()

  expect_warning(
    {
      dat <- extr_ctd(
        input_terms = c(
          "50-00-0", "64-17-5", "methanal", "ethanol", "bella", "ciao",
          "50-0000000"
        ), category = "chem", report_type = "genes_curated",
        input_term_search_type = "directAssociations",
        action_types = "ANY",
        ontology = c("go_bp", "go_cc")
      )
    },
    "Chemicals .*not found!"
  )

  expect_equal(sum(is.na(dat$gene_id)), 3)
})


# @@@@@@@@@@@@@@@@@@
# extr_tetramer ----
# @@@@@@@@@@@@@@@@@@

Sys.sleep(3)

test_that("extr_tetramer fetches tetramers data", {
  skip_on_cran()
  skip_if_offline()

  dat <- extr_tetramer(
    chem = c("50-00-0", "ethanol"),
    disease = "",
    gene = "",
    go = "",
    input_term_search_type = "directAssociations",
    qt_match_type = "equals"
  )
  expect_true(is.data.frame(dat))

  expected_columns <- c(
    "chemical", "chemical_id", "gene", "gene_id", "phenotype",
    "phenotype_id", "disease", "disease_id", "query"
  )

  expect_true(all(expected_columns %in% colnames(dat)))
  expect_gt(nrow(dat), 0)
})

Sys.sleep(3)

test_that("extr_tetramer no results", {
  skip_on_cran()
  skip_if_offline()

  expect_warning(
    {
      dat <- extr_tetramer(
        chem = song,
        disease = "",
        gene = "",
        go = "",
        input_term_search_type = "directAssociations",
        qt_match_type = "equals"
      )
    },
    "Chemicals .*not found!"
  )

  expect_true(is.data.frame(dat))
  expect_equal(nrow(dat), 3)
})

Sys.sleep(3)

test_that("extr_tetramer no results with song (verbose = FALSE)", {
  skip_on_cran()
  skip_if_offline()

  # Expect no output and no error when verbose = FALSE
  expect_silent({
    dat <- extr_tetramer(
      chem = song,
      disease = "",
      gene = "",
      go = "",
      input_term_search_type = "directAssociations",
      qt_match_type = "equals",
      verbose = FALSE
    )
  })
})
