# Input contract consistency tests

test_that("normalize_cluster_gene_list canonicalizes list inputs", {
  unnamed <- normalize_cluster_gene_list(list(c("G1"), c("G2")))
  expect_identical(names(unnamed), c("0", "1"))

  numeric_named <- normalize_cluster_gene_list(
    list("1" = c("A"), "2" = list(genes = c("B")))
  )
  expect_identical(names(numeric_named), c("1", "2"))

  non_numeric_named <- normalize_cluster_gene_list(
    list("t_cells" = c("CD3D"), "b_cells" = list(genes = c("MS4A1")))
  )
  expect_identical(names(non_numeric_named), c("t_cells", "b_cells"))
})

test_that("create_annotation_prompt accepts mixed list element formats", {
  prompt_result <- create_annotation_prompt(
    input = list(
      gs1 = c("CD4", "CD3D"),
      gs2 = list(genes = c("CD14"))
    ),
    tissue_name = "PBMC"
  )

  expect_true(all(c("gs1", "gs2") %in% names(prompt_result$gene_lists)))
  expect_match(prompt_result$prompt, "gs1: CD4, CD3D")
  expect_match(prompt_result$prompt, "gs2: CD14")
})

test_that("CacheManager generate_key supports character-vector list clusters", {
  cache_manager <- CacheManager$new(cache_dir = "temp")
  input_list <- list(
    "0" = c("CD3D", "CD3E"),
    "1" = list(genes = c("MS4A1"))
  )

  key_0 <- cache_manager$generate_key(input_list, c("gpt-5.5"), "0")
  key_1 <- cache_manager$generate_key(input_list, c("gpt-5.5"), "1")

  expect_true(is.character(key_0))
  expect_gt(nchar(key_0), 10)
  expect_false(identical(key_0, key_1))
})

test_that("compare_model_predictions pads unequal lengths without recycling", {
  res <- testthat::with_mocked_bindings({
    capture.output({
      result <- suppressMessages(compare_model_predictions(
        input = list("0" = list(genes = c("G1"))),
        tissue_name = "PBMC",
        models = c("m1", "m2"),
        api_keys = list(openai = "k")
      ))
    })
    result
  },
  get_api_key = function(model, api_keys) "k",
  get_provider = function(model) "openai",
  annotate_cell_types = function(input, tissue_name, model, api_key, top_gene_count = 10, ...) {
    if (identical(model, "m1")) c("A", "B", "C") else c("A", "B")
  },
  standardize_cell_type_names = function(predictions, models, api_keys, standardization_model = "x", base_urls = NULL) {
    predictions
  })

  expect_identical(dim(res$comparison_matrix), c(3L, 2L))
  expect_true(is.na(res$comparison_matrix[3, "m2"]))
  expect_true(is.na(res$consensus_predictions[3]))
  expect_true(is.na(res$consensus_proportions[3]))
  expect_true(is.na(res$entropies[3]))
  expect_identical(rownames(res$comparison_matrix), c("0", "..prediction_2", "..prediction_3"))
  expect_identical(names(res$consensus_predictions), c("0", "..prediction_2", "..prediction_3"))
  expect_false(is.nan(res$summary_stats$mean_consensus_proportion))
  expect_false(is.nan(res$summary_stats$mean_entropy))
})

test_that("standardize_cell_type_names ignores prose and accepts numbered mapping keys", {
  result <- testthat::with_mocked_bindings({
    suppressMessages(standardize_cell_type_names(
      predictions = list(m1 = c("1: T cell", "B cell")),
      models = "m1",
      api_keys = list(openai = "k"),
      standardization_model = "gpt-5.5"
    ))
  },
  get_api_key = function(...) "k",
  get_model_response = function(...) c(
    "Here is the requested mapping:",
    "1: T cell: CD4 T cell",
    "B cell: B cell",
    "Note: keep broad immune labels unchanged"
  ),
  log_warn = function(...) NULL)

  expect_identical(result$m1, c("CD4 T cell", "B cell"))
})

test_that("compare_model_predictions skips invalid models and uses valid remaining models", {
  res <- testthat::with_mocked_bindings({
    capture.output({
      result <- suppressWarnings(suppressMessages(compare_model_predictions(
        input = list("0" = list(genes = c("G1"))),
        tissue_name = "PBMC",
        models = c("bad-model", "m1", "m2"),
        api_keys = list(openai = "k")
      )))
    })
    result
  },
  get_api_key = function(model, api_keys) {
    if (identical(model, "bad-model")) stop("unknown model") else "k"
  },
  get_provider = function(model) {
    if (identical(model, "bad-model")) stop("unknown model") else "openai"
  },
  annotate_cell_types = function(input, tissue_name, model, api_key, top_gene_count = 10, ...) {
    c("T cell")
  },
  standardize_cell_type_names = function(predictions, models, api_keys, standardization_model = "x", base_urls = NULL) {
    predictions
  })

  expect_identical(colnames(res$comparison_matrix), c("m1", "m2"))
})

test_that("interactive_consensus_annotation supports mapped cluster selectors", {
  mocked_call <- function(selector) {
    testthat::with_mocked_bindings({
      suppressMessages(interactive_consensus_annotation(
        input = list(
          "1" = c("CD3D"),
          "2" = list(genes = c("MS4A1"))
        ),
        tissue_name = "PBMC",
        models = c("gpt-5.5", "grok-4.3"),
        api_keys = list(openai = "k", grok = "k"),
        use_cache = FALSE,
        clusters_to_analyze = selector
      ))
    },
    log_info = function(...) NULL,
    print_consensus_summary = function(...) NULL,
    get_initial_predictions = function(input, ...) {
      # Predictions keyed by original cluster names (no 0-based conversion)
      cluster_ids <- names(input)
      preds <- setNames(rep("T", length(cluster_ids)), cluster_ids)
      list(
        individual_predictions = list(m1 = as.list(preds), m2 = as.list(preds)),
        successful_models = c("m1", "m2")
      )
    },
    identify_controversial_clusters = function(input, ...) {
      list(
        consensus_results = list(),
        controversial_clusters = character(0),
        final_annotations = setNames(rep("X", length(input)), names(input))
      )
    },
    process_controversial_clusters = function(...) {
      list(discussion_logs = list(), final_annotations = list())
    },
    combine_results = function(initial_results, controversy_results, discussion_results) {
      list(final_annotations = controversy_results$final_annotations)
    })
  }

  # Cluster names are preserved as-is (no 0-based conversion)
  result_single <- mocked_call("1")
  expect_identical(names(result_single$final_annotations), "1")

  result_both <- mocked_call(c("1", "2"))
  expect_identical(sort(names(result_both$final_annotations)), c("1", "2"))
})

test_that("parse_consensus_response falls back to flexible parsing for short responses", {
  result <- testthat::with_mocked_bindings({
    parse_consensus_response(
      c(
        "Consensus Proportion = 0.75",
        "Entropy = 0.81",
        "B cell"
      )
    )
  },
  get_logger = function() {
    list(
      warn = function(...) NULL,
      info = function(...) NULL,
      debug = function(...) NULL,
      error = function(...) NULL
    )
  })

  expect_identical(result$reached, FALSE)
  expect_equal(result$consensus_proportion, 0.75)
  expect_equal(result$entropy, 0.81)
  expect_identical(result$majority_prediction, "B cell")
})

test_that("get_initial_predictions skips invalid model names without aborting", {
  result <- testthat::with_mocked_bindings({
    suppressWarnings(get_initial_predictions(
      input = list("0" = list(genes = c("CD3D"))),
      tissue_name = "PBMC",
      models = c("bad-model", "gpt-5.5"),
      api_keys = list(openai = "k"),
      top_gene_count = 10
    ))
  },
  get_provider = function(model) {
    if (identical(model, "bad-model")) stop("unknown model")
    "openai"
  },
  get_api_key = function(model, api_keys) {
    provider <- get_provider(model)
    if (provider %in% names(api_keys)) api_keys[[provider]] else NULL
  },
  annotate_cell_types = function(...) c("T cell"),
  log_info = function(...) NULL,
  log_warn = function(...) NULL)

  expect_identical(result$successful_models, "gpt-5.5")
  expect_true("gpt-5.5" %in% names(result$individual_predictions))
})

test_that("interactive_consensus_annotation consumes log_dir via initialize_logger", {
  seen_log_dir <- NULL

  testthat::with_mocked_bindings({
    suppressMessages(interactive_consensus_annotation(
      input = list("0" = list(genes = c("CD3D"))),
      tissue_name = "PBMC",
      models = c("gpt-5.5", "grok-4.3"),
      api_keys = list(openai = "k", grok = "k"),
      use_cache = FALSE,
      log_dir = "custom_logs_dir"
    ))
  },
  initialize_logger = function(log_dir = "logs") {
    seen_log_dir <<- log_dir
    invisible(NULL)
  },
  log_info = function(...) NULL,
  print_consensus_summary = function(...) NULL,
  get_initial_predictions = function(input, ...) {
    list(
      individual_predictions = list(m1 = c("0:T"), m2 = c("0:T")),
      successful_models = c("m1", "m2")
    )
  },
  identify_controversial_clusters = function(input, ...) {
    list(
      consensus_results = list(),
      controversial_clusters = character(0),
      final_annotations = setNames(rep("X", length(input)), names(input))
    )
  },
  process_controversial_clusters = function(...) {
    list(discussion_logs = list(), final_annotations = list())
  },
  combine_results = function(initial_results, controversy_results, discussion_results) {
    list(final_annotations = controversy_results$final_annotations)
  })

  expect_identical(seen_log_dir, "custom_logs_dir")
})

test_that("combine_results provides stable aliases for return fields", {
  result <- testthat::with_mocked_bindings({
    combine_results(
      initial_results = list(individual_predictions = list(m1 = "A")),
      controversy_results = list(
        consensus_results = list("0" = list(reached = TRUE)),
        controversial_clusters = c("1"),
        final_annotations = list("0" = "T cell")
      ),
      discussion_results = list(
        discussion_logs = list("1" = list(rounds = list())),
        final_annotations = list("1" = "B cell")
      )
    )
  },
  get_logger = function() list(session_id = "session-test"))

  expect_identical(result$voting_results, result$initial_results)
  expect_identical(result$discussion_results, result$discussion_logs)
  expect_identical(result$final_consensus, result$final_annotations)
})

capture_discussion_genes <- function(input, cluster_id = "0", top_gene_count = 5) {
  captured_cluster_genes <- NULL
  testthat::with_mocked_bindings({
    suppressWarnings(suppressMessages(facilitate_cluster_discussion(
      cluster_id = cluster_id,
      input = input,
      tissue_name = "PBMC",
      models = c("m1", "m2"),
      api_keys = list(openai = "k"),
      initial_predictions = list(m1 = c("0:T"), m2 = c("0:T")),
      top_gene_count = top_gene_count,
      max_rounds = 2
    )))
  },
  create_initial_discussion_prompt = function(cluster_id, cluster_genes, tissue_name, initial_predictions) {
    captured_cluster_genes <<- cluster_genes
    "prompt"
  },
  get_model_response = function(...) "CELL TYPE: T cell",
  check_consensus = function(...) {
    list(reached = TRUE, consensus_proportion = 1, entropy = 0, majority_prediction = "T cell")
  },
  get_api_key = function(model, api_keys) "k",
  get_provider = function(model) "openai",
  get_logger = function() {
    list(log_discussion = function(...) NULL)
  },
  log_warn = function(...) NULL,
  log_info = function(...) NULL)
  captured_cluster_genes
}

test_that("facilitate_cluster_discussion extracts data-frame genes by marker rank", {
  markers <- data.frame(
    cluster = c(0, 0, 0, 1),
    gene = c("low", "top", "negative", "other"),
    avg_log2FC = c(1, 3, -2, 5)
  )

  expect_identical(capture_discussion_genes(markers, cluster_id = "0", top_gene_count = 2), "top,low")
})

test_that("facilitate_cluster_discussion preserves data-frame row order without avg_log2FC", {
  markers <- data.frame(
    cluster = c("0", "0", "0"),
    gene = c("G1", "G2", "G3")
  )

  expect_identical(capture_discussion_genes(markers, cluster_id = "0", top_gene_count = 2), "G1,G2")
})

test_that("facilitate_cluster_discussion keeps fallback cluster_genes on extraction error", {
  captured_cluster_genes <- capture_discussion_genes(42, cluster_id = "0")
  expect_true(grepl("Error extracting genes", captured_cluster_genes, fixed = TRUE))
})

test_that("facilitate_cluster_discussion falls back for missing clusters and malformed target entries", {
  missing_cluster_genes <- capture_discussion_genes(list("1" = c("G1")), cluster_id = "0")
  malformed_entry_genes <- capture_discussion_genes(list("0" = list(markers = c("G1"))), cluster_id = "0")

  expect_true(grepl("Cluster '0' was not found", missing_cluster_genes, fixed = TRUE))
  expect_true(grepl("character vector of genes", malformed_entry_genes, fixed = TRUE))
})


test_that("facilitate_cluster_discussion ignores malformed unrelated list entries", {
  cluster_genes <- capture_discussion_genes(
    list("0" = c("G1", "G2"), "1" = list(markers = c("bad"))),
    cluster_id = "0"
  )

  expect_identical(cluster_genes, "G1,G2")
})

test_that("is_error_response detects error sentinels consistently", {
  expect_true(is_error_response("Error: request failed"))
  expect_true(is_error_response("  ERROR: request failed"))
  expect_true(is_error_response(c("metadata", "error : request failed")))
  expect_false(is_error_response("CELL TYPE: T cell"))
  expect_false(is_error_response(list(message = "Error: request failed")))
})

test_that("filter_valid_responses excludes case-insensitive error sentinels", {
  result <- testthat::with_mocked_bindings({
    filter_valid_responses(
      list(
        good = "CELL TYPE: T cell",
        bad_upper = "ERROR: provider failed",
        bad_spaced = " error : provider failed"
      ),
      cluster_id = "0"
    )
  },
  log_warn = function(...) NULL)

  expect_identical(names(result), "good")
})

test_that("annotate_cell_types supports custom providers", {
  custom_env <- new.env(parent = emptyenv())
  assign("customx", TRUE, envir = custom_env)

  result <- testthat::with_mocked_bindings({
    annotate_cell_types(
      input = list("0" = list(genes = c("CD3D", "CD3E"))),
      tissue_name = "PBMC",
      model = "custom-model",
      api_key = "k"
    )
  },
  get_provider = function(model) "customx",
  resolve_provider_base_url = function(provider, base_urls) NULL,
  create_annotation_prompt = function(input, tissue_name, top_gene_count = 10) {
    list(prompt = "prompt", expected_count = 1L, gene_lists = list("0" = "CD3D, CD3E"))
  },
  process_custom = function(prompt, model, api_key) "custom-ok",
  custom_providers = custom_env,
  log_info = function(...) NULL,
  log_debug = function(...) NULL)

  expect_identical(result, "custom-ok")
})

test_that("process_custom forwards model_config when provider supports it", {
  skip_if_not_installed("testthat", minimum_version = "3.2.0")
  original_providers <- as.list(custom_providers)
  original_models <- as.list(custom_models)
  rm(list = ls(envir = custom_providers), envir = custom_providers)
  rm(list = ls(envir = custom_models), envir = custom_models)
  on.exit({
    rm(list = ls(envir = custom_providers), envir = custom_providers)
    rm(list = ls(envir = custom_models), envir = custom_models)
    list2env(original_providers, envir = custom_providers)
    list2env(original_models, envir = custom_models)
  }, add = TRUE)

  captured_config <- NULL
  assign("customx",
         list(
           process_fn = function(prompt, model, api_key, model_config) {
             captured_config <<- model_config
             "custom-ok"
           },
           description = NULL,
           models = "custom-model"
         ),
         envir = custom_providers)
  assign("custom-model",
         list(provider = "customx", config = list(temperature = 0.2, max_tokens = 100)),
         envir = custom_models)

  result <- testthat::with_mocked_bindings({
    suppressMessages(process_custom("prompt", "custom-model", "k"))
  },
  get_logger = function() {
    list(
      info = function(...) NULL,
      error = function(...) NULL
    )
  })

  expect_identical(result, "custom-ok")
  expect_identical(captured_config, list(temperature = 0.2, max_tokens = 100))

  captured_config <- NULL
  custom_providers$customx$process_fn <- function(prompt, model, api_key, ...) {
    captured_config <<- list(...)[["model_config"]]
    "custom-ok"
  }

  result <- testthat::with_mocked_bindings({
    suppressMessages(process_custom("prompt", "custom-model", "k"))
  },
  get_logger = function() {
    list(
      info = function(...) NULL,
      error = function(...) NULL
    )
  })

  expect_identical(result, "custom-ok")
  expect_identical(captured_config, list(temperature = 0.2, max_tokens = 100))
})

test_that("check_consensus normalizes discussion cell type labels", {
  result <- testthat::with_mocked_bindings({
    suppressMessages(check_consensus(
      list(
        m1 = "CELL TYPE: T cell",
        m2 = "Reasoning\nCELL TYPE: T cell"
      ),
      controversy_threshold = 1,
      entropy_threshold = 0
    ))
  },
  get_logger = function() {
    list(
      info = function(...) NULL,
      debug = function(...) NULL,
      warn = function(...) NULL,
      error = function(...) NULL
    )
  })

  expect_true(result$reached)
  expect_identical(result$majority_prediction, "T cell")
})


test_that("consensus prompt documents inclusive consensus threshold", {
  prompt <- create_consensus_check_prompt(
    c("T cell", "B cell"),
    controversy_threshold = 0.5,
    entropy_threshold = 1
  )

  expect_true(grepl("Consensus Proportion >= 0.5", prompt, fixed = TRUE))
})


test_that("parse_consensus_response handles NA_character_ safely", {
  result <- testthat::with_mocked_bindings({
    parse_consensus_response(NA_character_)
  },
  get_logger = function() {
    list(
      warn = function(...) NULL,
      info = function(...) NULL,
      debug = function(...) NULL,
      error = function(...) NULL
    )
  })

  expect_identical(result, list(
    reached = FALSE,
    consensus_proportion = 0,
    entropy = 0,
    majority_prediction = "Unknown"
  ))
})

test_that("print_consensus_summary handles vector predictions without crashing", {
  results <- list(
    final_annotations = list("0" = c("Consensus A", "Consensus B")),
    controversial_clusters = c("0"),
    discussion_logs = list(
      "0" = list(initial_predictions = list(m1 = c("Pred A", "Pred B")))
    )
  )

  expect_no_error(capture.output(print_consensus_summary(results)))
})

test_that("QwenProcessor caches endpoint selection per API key", {
  original_cache <- as.list(.qwen_endpoint_cache)
  rm(list = ls(envir = .qwen_endpoint_cache), envir = .qwen_endpoint_cache)
  on.exit({
    rm(list = ls(envir = .qwen_endpoint_cache), envir = .qwen_endpoint_cache)
    list2env(original_cache, envir = .qwen_endpoint_cache)
  }, add = TRUE)

  key_a <- digest::digest("key-a", algo = "xxhash64")
  key_b <- digest::digest("key-b", algo = "xxhash64")
  assign(key_a, "https://intl.example.test", envir = .qwen_endpoint_cache)
  assign(key_b, "https://domestic.example.test", envir = .qwen_endpoint_cache)

  processor <- QwenProcessor$new()

  expect_identical(processor$get_working_api_url("key-a"), "https://intl.example.test")
  expect_identical(processor$get_working_api_url("key-b"), "https://domestic.example.test")
})


test_that("BaseAPIProcessor rejects non-scalar prompt cleanly", {
  TP <- R6::R6Class(
    "TP",
    inherit = BaseAPIProcessor,
    public = list(
      get_default_api_url = function() "x",
      make_api_call = function(chunk_content, model, api_key) list(),
      extract_response_content = function(response, model) "ok"
    )
  )

  p <- TP$new("tp")
  expect_error(
    p$process_request(c("a", "b"), "m", "k"),
    "Prompt is required but not provided"
  )
})

test_that("get_provider validates model as non-empty scalar", {
  expect_error(get_provider(character(0)), "model must be a non-empty character scalar")
  expect_error(get_provider(c("gpt-5.5", "grok-4.3")), "model must be a non-empty character scalar")
  expect_error(get_provider(NA_character_), "model must be a non-empty character scalar")
})

test_that("annotate_cell_types validates api_key scalar contract", {
  expect_error(
    annotate_cell_types(
      input = list("0" = list(genes = c("CD3D"))),
      tissue_name = "PBMC",
      model = "gpt-5.5",
      api_key = c("a", "b")
    ),
    "api_key must be a non-empty character scalar, or NA to return prompt only"
  )
})

test_that("interactive_consensus_annotation validates api_keys list contract", {
  expect_error(
    interactive_consensus_annotation(
      input = list("0" = list(genes = c("CD3D"))),
      tissue_name = "PBMC",
      models = c("gpt-5.5", "grok-4.3"),
      api_keys = "bad",
      use_cache = FALSE
    ),
    "api_keys must be a named, non-empty list"
  )
})

test_that("get_api_key ignores empty/NA/non-scalar keys and falls back correctly", {
  expect_null(get_api_key("gpt-5.5", list(openai = "")))
  expect_null(get_api_key("gpt-5.5", list(openai = "   ")))
  expect_null(get_api_key("gpt-5.5", list(openai = NA_character_)))
  expect_null(get_api_key("gpt-5.5", list(openai = c("a", "b"))))
  expect_identical(get_api_key("gpt-5.5", list(openai = "", "gpt-5.5" = "k2")), "k2")
  expect_identical(get_api_key("gpt-5.5", list(openai = "  k1  ")), "k1")
})

test_that("facilitate_cluster_discussion uses last available consensus on early break", {
  logger_env <- new.env(parent = emptyenv())
  logger_env$events <- list()
  logger <- list(
    log_discussion = function(cluster_id, event_type, data = NULL) {
      logger_env$events <- c(logger_env$events, list(list(event = event_type, data = data)))
    }
  )

  testthat::with_mocked_bindings({
    suppressMessages(facilitate_cluster_discussion(
      cluster_id = "0",
      input = list("0" = list(genes = c("G1"))),
      tissue_name = "PBMC",
      models = c("m1", "m2"),
      api_keys = list(openai = "k"),
      initial_predictions = list(m1 = c("0:T"), m2 = c("0:T")),
      top_gene_count = 5,
      max_rounds = 3
    ))
  },
  create_initial_discussion_prompt = function(...) "p1",
  create_discussion_prompt = function(...) "p2",
  get_model_response = local({
    cnt <- 0
    function(...) {
      cnt <<- cnt + 1
      if (cnt <= 2) "CELL TYPE: T cell" else if (cnt == 3) "CELL TYPE: T cell" else "Error: failed"
    }
  }),
  check_consensus = local({
    k <- 0
    function(...) {
      k <<- k + 1
      list(reached = FALSE, consensus_proportion = 0.5, entropy = 1.2, majority_prediction = paste0("M", k))
    }
  }),
  get_api_key = function(...) "k",
  get_provider = function(...) "openai",
  get_logger = function() logger,
  log_warn = function(...) NULL,
  log_info = function(...) NULL)

  end_events <- Filter(function(x) identical(x$event, "end"), logger_env$events)
  expect_equal(length(end_events), 1)
  expect_identical(end_events[[1]]$data$final_result, "M1")
})

test_that("process_controversial_clusters backtracks final prediction when last round lacks consensus", {
  out <- testthat::with_mocked_bindings({
    process_controversial_clusters(
      controversial_clusters = c("0"),
      input = list("0" = list(genes = c("G1"))),
      tissue_name = "PBMC",
      successful_models = c("m1", "m2"),
      api_keys = list(openai = "k"),
      individual_predictions = list(m1 = c("0:T"), m2 = c("0:T")),
      top_gene_count = 5,
      controversy_threshold = 0.7,
      entropy_threshold = 1.0,
      max_discussion_rounds = 3,
      cache_manager = list(
        generate_key = function(...) "k",
        save_to_cache = function(...) NULL,
        has_cache = function(...) FALSE
      ),
      use_cache = FALSE
    )
  },
  facilitate_cluster_discussion = function(...) {
    list(
      cluster_id = "0",
      rounds = list(
        list(consensus_result = list(majority_prediction = "T cell")),
        list(responses = list(m1 = "..."))
      )
    )
  },
  log_info = function(...) NULL,
  log_warn = function(...) NULL)

  expect_identical(out$final_annotations[["0"]], "T cell")
})

test_that("facilitate_cluster_discussion logs consensus and end on first-round early return", {
  logger_env <- new.env(parent = emptyenv())
  logger_env$events <- list()
  logger <- list(
    log_discussion = function(cluster_id, event_type, data = NULL) {
      logger_env$events <- c(logger_env$events, list(list(event = event_type, data = data)))
    }
  )

  testthat::with_mocked_bindings({
    facilitate_cluster_discussion(
      cluster_id = "0",
      input = list("0" = list(genes = c("G1"))),
      tissue_name = "PBMC",
      models = c("m1", "m2"),
      api_keys = list(openai = "k"),
      initial_predictions = list(m1 = c("0:T"), m2 = c("0:T")),
      top_gene_count = 5,
      max_rounds = 3
    )
  },
  create_initial_discussion_prompt = function(...) "p1",
  create_discussion_prompt = function(...) "p2",
  get_model_response = function(...) "Error: failed",
  check_consensus = function(...) list(reached = FALSE, consensus_proportion = 0, entropy = 0, majority_prediction = "Unknown"),
  get_api_key = function(...) "k",
  get_provider = function(...) "openai",
  get_logger = function() logger,
  log_warn = function(...) NULL,
  log_info = function(...) NULL)

  event_names <- vapply(logger_env$events, function(x) x$event, character(1))
  expect_true("consensus" %in% event_names)
  expect_true("end" %in% event_names)
})

test_that("BaseAPIProcessor marks semantic error result as failed API call", {
  logger_env <- new.env(parent = emptyenv())
  logger_env$api_call_success <- logical(0)
  logger_env$failed_audit_logs <- 0L

  logger <- list(
    info = function(...) NULL,
    debug = function(...) NULL,
    error = function(...) NULL,
    log_api_call = function(provider, model, duration, success = TRUE, tokens = NULL) {
      logger_env$api_call_success <- c(logger_env$api_call_success, success)
    },
    log_api_request_response = function(...) {
      logger_env$failed_audit_logs <- logger_env$failed_audit_logs + 1L
    }
  )

  testthat::with_mocked_bindings({
    TestProcessor <- R6::R6Class("TestProcessor",
      inherit = BaseAPIProcessor,
      public = list(
        initialize = function() super$initialize("testp", NULL),
        get_default_api_url = function() "https://example.test",
        make_api_call = function(chunk_content, model, api_key) stop("simulated failure"),
        extract_response_content = function(response, model) "unused"
      )
    )

    proc <- TestProcessor$new()
    expect_error(proc$process_request("prompt", "test-model", "test-key"),
                 "simulated failure")
  },
  get_logger = function() logger)

  expect_true(length(logger_env$api_call_success) >= 1)
  expect_identical(tail(logger_env$api_call_success, 1), FALSE)
  # Failed requests should still produce audit logs for traceability
  expect_true(logger_env$failed_audit_logs >= 1L)
})

test_that("initialize_logger preserves logger configuration and regenerates unique session IDs", {
  initialize_logger("logs")
  configure_logger(level = "DEBUG", console_output = FALSE, json_format = FALSE)
  logger_cfg <- get_logger()
  logger_cfg$max_log_size <- 42
  logger_cfg$max_log_files <- 7

  initialize_logger("logs")
  logger_after_reinit <- get_logger()
  expect_identical(logger_after_reinit$log_level, "DEBUG")
  expect_identical(logger_after_reinit$enable_console, FALSE)
  expect_identical(logger_after_reinit$enable_json, FALSE)
  expect_identical(logger_after_reinit$max_log_size, 42)
  expect_identical(logger_after_reinit$max_log_files, 7)

  id1 <- logger_after_reinit$session_id
  initialize_logger("logs")
  id2 <- get_logger()$session_id
  expect_false(identical(id1, id2))
})
