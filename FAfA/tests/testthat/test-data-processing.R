test_that("missing-value cleaning preserves categorical columns", {
  source_data <- data.frame(
    item = c(1, 2, NA),
    group = c("control", "treatment", "NA"),
    stringsAsFactors = FALSE
  )

  result <- clean_missing_data(source_data, remove_na = FALSE)$cleaned_data

  expect_type(result$group, "character")
  expect_equal(result$group[1:2], c("control", "treatment"))
  expect_true(is.na(result$group[3]))
})

test_that("Turkish variable names become unique analysis-safe names", {
  source_data <- setNames(
    data.frame(a = 1, b = 2, c = 3),
    c(
      "\u00f6l\u00e7ek puan\u0131",
      "\u0130\u015f doyumu",
      "\u0130\u015f doyumu"
    )
  )

  result <- normalize_variable_names(source_data)

  expect_equal(names(result), c("olcek_puani", "Is_doyumu", "Is_doyumu.1"))
})

test_that("reverse scoring uses each item's observed scale", {
  source_data <- data.frame(
    item_1_5 = c(1, 2, 5, NA),
    item_0_4 = c(0, 1, 4, 2)
  )

  result <- reverse_score_variables(source_data, names(source_data))

  expect_equal(result$data$item_1_5, c(5, 4, 1, NA))
  expect_equal(result$data$item_0_4, c(4, 3, 0, 2))
  expect_equal(result$specifications$Formula, c("6 - item_1_5", "4 - item_0_4"))
})

test_that("fixed reverse-scoring limits are validated", {
  source_data <- data.frame(item = c(1, 2, 5))

  result <- reverse_score_variables(source_data, "item", lower = 1, upper = 5)

  expect_equal(result$data$item, c(5, 4, 1))
  expect_error(
    reverse_score_variables(source_data, "item", lower = 1, upper = 4),
    "outside 1-4"
  )
})

test_that("simple imputation methods preserve the dataset structure", {
  source_data <- data.frame(
    item = c(1, NA, 3),
    group = c("a", "b", "c"),
    check.names = FALSE
  )

  mean_result <- FAfA:::apply_imputation_method(source_data, "mean")
  listwise_result <- FAfA:::apply_imputation_method(source_data, "listwise")

  expect_equal(mean_result$item, c(1, 2, 3))
  expect_equal(mean_result$group, source_data$group)
  expect_equal(nrow(listwise_result), 2)
})

test_that("Stratified Alpha strata are generated from factor definitions", {
  dictionary <- list(
    F1 = c("item_1", "item_2"),
    F2 = c("item_3", "item_4", "item_5")
  )

  result <- FAfA:::build_stratified_alpha_spec(
    dictionary,
    paste0("item_", 1:5)
  )

  expect_equal(result$items, paste0("item_", 1:5))
  expect_equal(result$strata, "1,1,2,2,2")
  expect_error(
    FAfA:::build_stratified_alpha_spec(
      list(F1 = c("item_1", "item_2"), F2 = c("item_2", "item_3")),
      paste0("item_", 1:3)
    ),
    "only one"
  )
})

test_that("local stratified alpha follows the variance decomposition", {
  source_data <- data.frame(
    item_1 = c(1, 2, 3, 4, 5, 4),
    item_2 = c(2, 2, 4, 4, 5, 3),
    item_3 = c(5, 4, 3, 2, 1, 2),
    item_4 = c(3, 4, 2, 2, 1, 4)
  )

  result <- FAfA:::stratified_alpha_value(source_data, c(1, 1, 2, 2))
  subscale_errors <- vapply(list(1:2, 3:4), function(columns) {
    subscale <- source_data[columns]
    subscale_variance <- stats::var(rowSums(subscale))
    subscale_variance * (1 - FAfA:::cronbach_alpha_value(subscale))
  }, numeric(1))
  expected <- 1 - sum(subscale_errors) / stats::var(rowSums(source_data))

  expect_equal(result, expected, tolerance = 1e-12)
})

test_that("local item statistics provide the values used by weighting", {
  source_data <- data.frame(
    item_1 = c(1, 2, 3, 4, 5),
    item_2 = c(1, 2, 4, 4, 5),
    item_3 = c(5, 3, 2, 2, 1)
  )

  result <- FAfA:::corrected_item_statistics(source_data)
  expected_difficulty <- colMeans(source_data)
  expected_reliability <- vapply(seq_along(source_data), function(index) {
    item <- source_data[[index]]
    remaining_total <- rowSums(source_data[-index])
    population_sd <- sqrt(mean((item - mean(item))^2))
    stats::cor(item, remaining_total) * population_sd
  }, numeric(1))

  expect_equal(result$Difficulty, unname(expected_difficulty))
  expect_equal(result$Item.Rel.woi, expected_reliability, tolerance = 1e-12)
  expect_equal(names(FAfA:::item_weighting(source_data)), names(source_data))
})

test_that("local collinearity diagnostics retain VIF and condition indices", {
  source_data <- data.frame(
    item_1 = c(1, 2, 3, 4, 5, 6),
    item_2 = c(2, 1, 4, 3, 6, 5),
    item_3 = c(5, 2, 4, 1, 6, 3)
  )

  result <- FAfA:::collinearity_summary(source_data)
  expected_vif <- diag(solve(stats::cor(source_data)))
  design <- cbind(`(Intercept)` = 1, as.matrix(source_data))
  scaled_design <- sweep(design, 2, sqrt(colSums(design^2)), "/")
  eigenvalues <- eigen(crossprod(scaled_design), symmetric = TRUE)$values
  expected_indices <- sqrt(max(eigenvalues) / eigenvalues)

  expect_equal(result$VIF_min, min(expected_vif), tolerance = 1e-12)
  expect_equal(result$VIF_max, max(expected_vif), tolerance = 1e-12)
  expect_equal(result$CI_min, min(expected_indices), tolerance = 1e-12)
  expect_equal(result$CI_max, max(expected_indices), tolerance = 1e-12)
})

test_that("local moments match the descriptive definitions", {
  values <- c(1, 1, 2, 3, 5, NA)
  observed <- values[!is.na(values)]
  centered <- observed - mean(observed)
  second_moment <- mean(centered^2)

  expect_equal(
    FAfA:::standardized_moment(values, 3),
    mean(centered^3) / second_moment^(3 / 2),
    tolerance = 1e-12
  )
  expect_equal(
    FAfA:::standardized_moment(values, 4) - 3,
    mean(centered^4) / second_moment^2 - 3,
    tolerance = 1e-12
  )
})

test_that("assumption checks return Mardia rows in the expected structure", {
  set.seed(2026)
  source_data <- as.data.frame(matrix(stats::rnorm(240), ncol = 3))

  result <- FAfA:::assumptions(source_data)

  expect_equal(result$mvn_table$Test, c("Skewness", "Kurtosis"))
  expect_true(all(is.finite(result$mvn_table$Statistic)))
  expect_true(all(result$mvn_table[["p-value"]] >= 0 & result$mvn_table[["p-value"]] <= 1))
  expect_true(all(result$mvn_table$Result %in% c("YES", "NO")))
})

test_that("item-removal summary hides only genuinely duplicated columns", {
  duplicated_summary <- data.frame(
    ITERATION = 1:2,
    REMOVED_THIS_STEP = c("v1-v2", "v3"),
    REMOVED_ITEMS = c("v1-v2", "v3"),
    N_REMOVED = c(2, 1),
    check.names = FALSE
  )
  distinct_summary <- duplicated_summary
  distinct_summary$REMOVED_ITEMS <- c("v1-v2", "v1-v2-v3")

  compact <- FAfA:::prepare_itemrest_summary(duplicated_summary)
  detailed <- FAfA:::prepare_itemrest_summary(distinct_summary)
  turkish <- FAfA:::prepare_itemrest_summary(duplicated_summary, "tr")

  expect_false("REMOVED_THIS_STEP" %in% names(compact))
  expect_true(all(c("REMOVED_THIS_STEP", "REMOVED_ITEMS") %in% names(detailed)))
  expect_true("\u00c7IKARILAN_MADDELER" %in% names(turkish))
})

test_that("variance table retains proportions and adds percentage rows", {
  source_table <- rbind(
    `SS loadings` = c(F1 = 2.1, F2 = 1.4),
    `Proportion Var` = c(F1 = 0.35, F2 = 0.23),
    `Cumulative Var` = c(F1 = 0.35, F2 = 0.58)
  )

  result <- FAfA:::format_efa_variance_table(source_table)
  turkish <- FAfA:::format_efa_variance_table(source_table, "tr")

  expect_equal(unname(as.numeric(result["Proportion Var", ])), c(0.35, 0.23))
  expect_equal(unname(as.numeric(result["Proportion Var (%)", ])), c(35, 23))
  expect_true("A\u00e7\u0131klanan Varyans (%)" %in% rownames(turkish))
})

test_that("EFA heatmap displays correlation values by default", {
  report_data <- shiny::reactiveVal(data.frame(
    item_1 = c(1, 2, 3, 4, 5, 2),
    item_2 = c(2, 2, 4, 4, 5, 3),
    item_3 = c(5, 4, 4, 2, 1, 3)
  ))
  report_result <- shiny::reactiveVal(list())
  report_settings <- shiny::reactiveVal(list(cor_kind = "pea"))
  report_language <- shiny::reactiveVal("tr")

  shiny::testServer(
    efa_server_report,
    args = list(
      data = report_data,
      efa_output_reactive = report_result,
      efa_settings_reactive = report_settings,
      language = report_language
    ),
    {
      session$setInputs(heatmap_palette = "blue_red")
      session$flushReact()
      default_plot <- heatmap_plot()
      expect_true(any(vapply(
        default_plot$layers,
        function(layer) inherits(layer$geom, "GeomText"),
        logical(1)
      )))
      expect_no_error(ggplot2::ggplot_build(default_plot))

      session$setInputs(heatmap_show_values = FALSE)
      session$flushReact()
      hidden_plot <- heatmap_plot()
      expect_false(any(vapply(
        hidden_plot$layers,
        function(layer) inherits(layer$geom, "GeomText"),
        logical(1)
      )))
    }
  )
})

test_that("server-side interface text follows the selected language", {
  expect_equal(FAfA:::fafa_text("en", "Result", "Sonu\u00e7"), "Result")
  expect_equal(FAfA:::fafa_text("tr", "Result", "Sonu\u00e7"), "Sonu\u00e7")
})

test_that("Dynamic Fit Index settings and results are normalized", {
  expect_equal(
    FAfA:::parse_dynamic_mad("0.038, 0.05; 0.06"),
    c(0.038, 0.05, 0.06)
  )
  expect_error(FAfA:::parse_dynamic_mad("0.05, 2"), "between 0 and 1")

  mock_result <- list(cutoffs = structure(
    matrix(c("0.95", "0.06", "0.90", "0.08"), nrow = 2),
    dimnames = list(c("Consistent", "Close"), c("CFI", "RMSEA"))
  ))
  normalized <- FAfA:::normalize_dynamic_cutoffs(mock_result)
  expect_equal(names(normalized), c("Criterion", "CFI", "RMSEA"))
  expect_equal(normalized$Criterion, c("Consistent", "Close"))
})

test_that("Dynamic Fit Index discrepancy matrices are positive definite", {
  model_correlation <- matrix(
    c(
      1.00, 0.45, 0.30,
      0.45, 1.00, 0.35,
      0.30, 0.35, 1.00
    ),
    nrow = 3,
    byrow = TRUE,
    dimnames = list(paste0("x", 1:3), paste0("x", 1:3))
  )

  set.seed(42)
  result <- FAfA:::dynamic_misspecified_correlation(
    model_correlation,
    target_mad = 0.05
  )

  expect_equal(unname(diag(result$correlation)), rep(1, 3))
  expect_gt(min(eigen(result$correlation, symmetric = TRUE)$values), 0)
  expect_equal(result$achieved_mad, 0.05, tolerance = 0.01)
})

test_that("Dynamic Fit Index cutoff summaries report sensitivity and specificity", {
  consistent <- list(
    measures = data.frame(
      CFI = seq(0.94, 0.99, length.out = 50),
      RMSEA = seq(0.02, 0.07, length.out = 50),
      RCI = seq(0.03, 0.08, length.out = 50)
    ),
    achieved_mad = 0
  )
  misspecified <- list(list(
    measures = data.frame(
      CFI = seq(0.80, 0.94, length.out = 50),
      RMSEA = seq(0.07, 0.14, length.out = 50),
      RCI = seq(0.08, 0.15, length.out = 50)
    ),
    achieved_mad = 0.05
  ))

  result <- FAfA:::dynamic_cutoff_summary(
    consistent,
    misspecified,
    mad_values = 0.05
  )

  expect_equal(rownames(result), c("Consistent", "MAD 0.05"))
  expect_true(all(c(
    "CFI", "RMSEA", "CFI Sensitivity (%)", "RMSEA Specificity (%)"
  ) %in% names(result)))
})

test_that("conventional CFA cutoffs are summarized", {
  fit_table <- data.frame(
    Measure = c("CFI", "TLI (NNFI)", "RMSEA", "SRMR"),
    Value = c(0.96, 0.94, 0.05, 0.09)
  )
  summary <- FAfA:::conventional_fit_summary(fit_table)
  expect_equal(summary$Result, c("Meets", "Does not meet", "Meets", "Does not meet"))
})

test_that("APA 7 Word reports support direct and Shiny download paths", {
  skip_if_not_installed("officer")
  skip_if_not_installed("flextable")

  output_files <- c(tempfile(fileext = ".docx"), tempfile())
  for (output_file in output_files) {
    FAfA:::write_apa7_report(
      output_file,
      title = "Confirmatory Factor Analysis Report",
      subtitle = "FAfA - APA 7",
      sections = list(list(
        title = "Model Fit",
        text = "The model fit was evaluated with standard fit indices.",
        table = data.frame(Index = c("CFI", "RMSEA"), Value = c(0.96, 0.05)),
        note = "Values are rounded to three decimals."
      ))
    )

    expect_true(file.exists(output_file))
    expect_gt(file.info(output_file)$size, 1000)
    package_files <- utils::unzip(output_file, list = TRUE)$Name
    expect_true("word/document.xml" %in% package_files)
    expect_true("word/header1.xml" %in% package_files)

    document_connection <- unz(output_file, "word/document.xml")
    document_xml <- paste(
      readLines(document_connection, warn = FALSE, encoding = "UTF-8"),
      collapse = ""
    )
    close(document_connection)
    expect_match(document_xml, 'w:color w:val="000000"', fixed = TRUE)
    expect_false(grepl("<w14:textFill>", document_xml, fixed = TRUE))
  }
})

test_that("scree plot uses ordinary correlation-matrix eigenvalues", {
  source_data <- data.frame(
    item_1 = c(1, 2, 3, 4, 5),
    item_2 = c(1, 2, 4, 4, 5),
    item_3 = c(5, 4, 3, 2, 1)
  )

  result <- calculate_scree_eigenvalues(source_data)
  expected <- eigen(stats::cor(source_data), symmetric = TRUE)$values

  expect_equal(result$Component, 1:3)
  expect_equal(result$Eigenvalue, expected)
  expect_equal(sum(result$Eigenvalue), 3, tolerance = 1e-10)
})

test_that("scree plot rejects zero-variance variables", {
  source_data <- data.frame(item_1 = 1:5, item_2 = rep(1, 5))

  expect_error(
    calculate_scree_eigenvalues(source_data),
    "zero variance"
  )
})

test_that("Lubbe parallel analysis is reproducible and preserves the RNG state", {
  skip_if_not_installed("qgraph")

  set.seed(71)
  latent_1 <- stats::rnorm(120)
  latent_2 <- stats::rnorm(120)
  to_ordinal <- function(values) {
    as.integer(cut(
      values,
      breaks = stats::quantile(values, probs = seq(0, 1, length.out = 6)),
      include.lowest = TRUE
    ))
  }
  source_data <- data.frame(
    item_1 = to_ordinal(latent_1 + stats::rnorm(120, sd = 0.35)),
    item_2 = to_ordinal(latent_1 + stats::rnorm(120, sd = 0.35)),
    item_3 = to_ordinal(latent_2 + stats::rnorm(120, sd = 0.35)),
    item_4 = to_ordinal(latent_2 + stats::rnorm(120, sd = 0.35))
  )
  source_data$item_1 <- ordered(source_data$item_1)
  source_data$item_2[c(4, 19)] <- NA_integer_
  rng_before <- .Random.seed

  first <- FAfA:::lubbe_parallel_analysis(
    source_data,
    fa = "pc",
    n.iter = 12,
    quant = 0.95,
    seed = 902
  )
  expect_identical(.Random.seed, rng_before)
  second <- FAfA:::lubbe_parallel_analysis(
    source_data,
    fa = "pc",
    n.iter = 12,
    quant = 0.95,
    seed = 902
  )

  expect_equal(first, second)
  expect_length(first$pc.values, ncol(source_data))
  expect_length(first$pc.sim, ncol(source_data))
  expect_gte(first$ncomp, 0)
  expect_lte(first$ncomp, ncol(source_data))
  expect_equal(dim(first$values), c(12, ncol(source_data)))
})

test_that("factor retention exposes the Lubbe settings in its result", {
  skip_if_not_installed("qgraph")

  set.seed(19)
  source_data <- as.data.frame(replicate(4, sample(1:5, 100, replace = TRUE)))
  result <- FAfA:::factor_ret(
    source_data,
    method = "pa_lubbe",
    n.iter = 8,
    quant = 0.90,
    seed = 44
  )

  expect_match(rownames(result), "Lubbe, 2019", fixed = TRUE)
  expect_equal(result$Permutations, 8)
  expect_equal(result$Reference_Quantile, 0.90)
  expect_true(is.numeric(result$Suggested_Factors))
})

test_that("lavaan modifiers are removed from the factor dictionary", {
  syntax <- "F1 =~ 1*item1 + start(.7)*item2\nF1 =~ item3\nHO =~ F1 + F2"

  result <- parse_factor_dictionary(syntax)

  expect_equal(result$F1, c("item1", "item2", "item3"))
  expect_equal(result$HO, c("F1", "F2"))
})

test_that("CFA model builder supports cross-loadings and factor updates", {
  syntax <- FAfA:::set_lavaan_measurement("", "F1", c("m1", "m2"))
  syntax <- FAfA:::set_lavaan_measurement(syntax, "F2", c("m1", "m3"))
  dictionary <- parse_factor_dictionary(syntax)

  expect_equal(dictionary$F1, c("m1", "m2"))
  expect_equal(dictionary$F2, c("m1", "m3"))

  updated <- FAfA:::set_lavaan_measurement(syntax, "F1", c("m2", "m4"))
  expect_equal(sum(grepl("^F1[[:space:]]*=~", strsplit(updated, "\n")[[1]])), 1)
  expect_equal(parse_factor_dictionary(updated)$F1, c("m2", "m4"))
})

test_that("CFA model builder creates second-order syntax", {
  first_order <- paste(
    "F1 =~ m1 + m2 + m3",
    "F2 =~ m4 + m5 + m6",
    sep = "\n"
  )
  result <- FAfA:::build_second_order_syntax(
    first_order,
    "HO",
    c("F1", "F2")
  )

  expect_match(result, "HO =~ F1 + F2", fixed = TRUE)
  observed_dictionary <- FAfA:::observed_factor_dictionary(
    result,
    paste0("m", 1:6)
  )
  expect_equal(names(observed_dictionary), c("F1", "F2"))
  expect_error(
    FAfA:::build_second_order_syntax(first_order, "HO", "F1"),
    "at least two"
  )
})

test_that("CFA model builder creates orthogonal bifactor syntax", {
  dictionary <- list(
    F1 = c("m1", "m2", "m3"),
    F2 = c("m4", "m5", "m6")
  )
  first_order <- paste(vapply(names(dictionary), function(factor) {
    paste0(factor, " =~ ", paste(dictionary[[factor]], collapse = " + "))
  }, character(1)), collapse = "\n")

  result <- FAfA:::build_bifactor_syntax(
    first_order,
    dictionary,
    "G",
    c("F1", "F2"),
    orthogonal = TRUE
  )

  expect_match(result, "G =~ m1 + m2 + m3 + m4 + m5 + m6", fixed = TRUE)
  expect_match(result, "G ~~ 0*F1", fixed = TRUE)
  expect_match(result, "G ~~ 0*F2", fixed = TRUE)
  expect_match(result, "F1 ~~ 0*F2", fixed = TRUE)
  expect_silent(lavaan::lavaanify(result))

  correlated <- FAfA:::build_bifactor_syntax(
    result,
    dictionary,
    "G",
    c("F1", "F2"),
    orthogonal = FALSE
  )
  expect_false(grepl("~~ 0*", correlated, fixed = TRUE))
})
