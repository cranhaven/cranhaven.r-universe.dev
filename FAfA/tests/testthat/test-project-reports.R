test_that("project files preserve settings and optional data", {
  source_data <- data.frame(item_1 = 1:5, item_2 = 5:1)
  rules <- data.frame(
    Variable = "item_2",
    Minimum = 1,
    Maximum = 5,
    stringsAsFactors = FALSE
  )
  project <- FAfA:::create_fafa_project(
    source_data = source_data,
    active_data = source_data,
    inputs = list("efa_analysis-number_factor" = 1),
    module_state = list(recode = list(rules = rules, recoded_data = source_data)),
    factor_dictionary = list(F1 = names(source_data)),
    audit = FAfA:::empty_audit_log(),
    source_name = "example.csv",
    include_data = FALSE
  )

  expect_invisible(FAfA:::validate_fafa_project(project))
  expect_null(project$source_data)
  expect_null(project$active_data)
  expect_null(project$module_state$recode$recoded_data)
  expect_s3_class(project$module_state$recode$rules, "data.frame")
})

test_that("reproducible reports are written in each supported format", {
  source_data <- data.frame(item_1 = 1:5, item_2 = 5:1)
  audit <- FAfA:::append_audit_entry(
    FAfA:::empty_audit_log(), "EFA", "Analysis requested"
  )
  project <- FAfA:::create_fafa_project(
    source_data = source_data,
    active_data = source_data,
    inputs = list(
      "efa_analysis-number_factor" = 1,
      "efa_analysis-rotating_method" = "oblimin",
      "efa_analysis-fact_method" = "minres"
    ),
    module_state = list(),
    factor_dictionary = list(F1 = names(source_data)),
    audit = audit,
    include_data = TRUE
  )

  script_file <- tempfile(fileext = ".R")
  html_file <- tempfile(fileext = ".html")
  pdf_file <- tempfile(fileext = ".pdf")
  diagnostic_file <- tempfile(fileext = ".txt")

  writeLines(FAfA:::build_reproducible_script(project), script_file)
  FAfA:::write_fafa_html_report(html_file, project)
  FAfA:::write_fafa_pdf_report(pdf_file, project)
  FAfA:::write_fafa_diagnostic_report(diagnostic_file, audit)

  expect_true(all(file.exists(c(script_file, html_file, pdf_file, diagnostic_file))))
  expect_gt(file.info(pdf_file)$size, 1000)
  expect_match(paste(readLines(html_file), collapse = "\n"), "Reproducible R code")
  diagnostic_text <- paste(readLines(diagnostic_file), collapse = "\n")
  expect_match(diagnostic_text, "EFA.*Analysis requested")
  expect_false(grepl("item_1|item_2", diagnostic_text))
})

test_that("Turkish interface translations are available", {
  translations <- FAfA:::fafa_translations("tr")

  expect_equal(unname(translations[["Select Data"]]), "Veri Se\u00e7imi")
  expect_equal(unname(translations[["Project & Reports"]]), "Proje ve Raporlar")
  expect_length(FAfA:::fafa_translations("en"), 0)
})

test_that("critical browser translations use the preferred Turkish terms", {
  translation_file <- system.file(
    "app", "www", "fafa-project.js",
    package = "FAfA"
  )
  expect_true(nzchar(translation_file))

  translation_text <- paste(
    readLines(translation_file, encoding = "UTF-8", warn = FALSE),
    collapse = "\n"
  )
  expected_entries <- c(
    '"Exclude Variables": "De\u011fi\u015fken \u00c7\u0131karma"',
    '"Missing Values": "Kay\u0131p Veriler"',
    '"KMO Measure of Sampling Adequacy": "KMO \u00d6rneklem Yeterli\u011fi \u00d6l\u00e7\u00fcs\u00fc"',
    '"Message": "\u0130leti"',
    '"Reference:": "Kaynak:"',
    '"Overview": "Genel Bak\u0131\u015f"'
  )

  expect_true(all(vapply(
    expected_entries,
    function(entry) grepl(entry, translation_text, fixed = TRUE),
    logical(1)
  )))
})

test_that("session cache keys are stable and caches can be cleared", {
  cache <- FAfA:::new_session_cache()
  first_key <- FAfA:::session_cache_key("efa", 2, "oblimin")
  same_key <- FAfA:::session_cache_key("efa", 2, "oblimin")
  different_key <- FAfA:::session_cache_key("efa", 3, "oblimin")

  expect_identical(first_key, same_key)
  expect_false(identical(first_key, different_key))
  FAfA:::session_cache_set(cache, first_key, list(result = 1))
  expect_equal(FAfA:::session_cache_get(cache, first_key)$result, 1)
  FAfA:::clear_session_cache(cache)
  expect_null(FAfA:::session_cache_get(cache, first_key))
})
