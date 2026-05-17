# PRA Agent Evaluation Framework using vitals
#
# This script defines evaluation tasks for systematically measuring and
# improving the PRA chat agent's tool selection accuracy and response quality.
#
# Usage:
#   source(system.file("eval/pra_eval.R", package = "PRA"))
#   results <- run_pra_eval(model = "llama3.2")
#
#   # Repeated trials with confidence intervals
#   results <- run_pra_eval(model = "llama3.2", n_trials = 3)
#
#   # Compare models
#   results <- run_pra_comparison(models = c("llama3.2", "qwen2.5"))
#
# Requirements: vitals (>= 0.2.0), ellmer, PRA (with Ollama running)

# ============================================================================
# Evaluation Dataset: 25 scenarios across 3 tiers
# ============================================================================

#' Build the PRA evaluation dataset
#'
#' @return A tibble with columns: input, target, tier, expected_tools,
#'   expected_keywords
pra_eval_dataset <- function() {
  tibble::tibble(
    input = c(
      # -- Tier 1: Single-tool (8 scenarios) --
      paste0(
        "Run a Monte Carlo simulation with 10000 iterations for a 3-task project: ",
        "Task A has a normal distribution with mean 10 and sd 2, ",
        "Task B has a triangular distribution with min 5, mode 10, max 15, ",
        "Task C has a uniform distribution between 8 and 12."
      ),
      "What is the Second Moment Method estimate for a project with task means [10, 15, 20, 8] and variances [4, 9, 16, 2]?",
      paste0(
        "Calculate EVM metrics for my project: BAC = $500,000, ",
        "cumulative planned schedule is [0.2, 0.4, 0.6, 0.8, 1.0], ",
        "we are at period 3, actual percent complete is 55%, ",
        "cumulative actual costs are [$95000, $200000, $310000]."
      ),
      paste0(
        "I have a risk event with 2 root causes. ",
        "Prior probabilities: P(C1) = 0.3, P(C2) = 0.2. ",
        "Conditional: P(R|C1) = 0.8, P(R|C2) = 0.6. ",
        "Background: P(R|not C1) = 0.2, P(R|not C2) = 0.4. ",
        "What is the overall risk probability?"
      ),
      paste0(
        "Calculate EVM metrics: BAC = $100,000, schedule [0.25, 0.5, 0.75, 1.0], ",
        "period 2, actual percent complete is 40%, ",
        "cumulative actual costs [$22000, $48000]. ",
        "What is the schedule variance?"
      ),
      paste0(
        "I have a dependency matrix for my project: ",
        "[[1,1,0,0],[0,1,1,0],[0,0,1,1],[0,0,0,1]]. ",
        "Show me the parent DSM."
      ),
      paste0(
        "Fit a sigmoidal learning curve (logistic model) to this data: ",
        "x = [1,2,3,4,5,6,7,8], y = [2,8,25,50,72,88,95,98]. ",
        "Predict the value at x = 10."
      ),
      paste0(
        "Run a Monte Carlo simulation for 2 tasks only: ",
        "Task 1 is uniform(5, 15), Task 2 is normal(20, 3). ",
        "Use 5000 simulations."
      ),

      # -- Tier 2: Multi-tool chain (9 scenarios) --
      paste0(
        "Simulate a 3-task project (A normal(10,2), B triangular(5,10,15), C uniform(8,12)) ",
        "with 10000 iterations and then calculate the contingency reserve at 95% confidence."
      ),
      paste0(
        "My project has BAC=$100,000, schedule [0.1,0.3,0.6,0.8,1.0], ",
        "period 3, 50% complete, costs [12000,28000,65000]. ",
        "Run a full EVM analysis and tell me what the forecast completion cost will be."
      ),
      paste0(
        "I have 2 root causes with P(C1)=0.3, P(C2)=0.2, ",
        "P(R|C1)=0.8, P(R|C2)=0.6, P(R|~C1)=0.2, P(R|~C2)=0.4. ",
        "After investigation, C1 was observed to have occurred but C2 is unknown. ",
        "What is the updated risk probability?"
      ),
      paste0(
        "Run a sensitivity analysis on a 3-task project: ",
        "Task A normal(10,2), Task B triangular(5,10,15), Task C uniform(8,12). ",
        "Which task drives the most variance?"
      ),
      paste0(
        "Fit a pearl learning curve to this data: ",
        "x = [1,2,3,4,5,6,7,8,9,10], y = [5,15,40,60,70,75,80,85,90,95]. ",
        "Then predict the value at x = 12."
      ),
      paste0(
        "Run a Monte Carlo simulation for tasks: A normal(15,3), B triangular(8,12,20), ",
        "C uniform(5,10). Then run a sensitivity analysis to find the biggest risk driver."
      ),
      paste0(
        "Simulate a 4-task project: A normal(10,2), B triangular(5,10,15), ",
        "C uniform(8,12), D normal(20,5). Then calculate the contingency at P80 ",
        "and P95 confidence levels."
      ),
      paste0(
        "I have a 3x3 dependency matrix [[1,1,0],[1,1,1],[0,1,1]]. ",
        "Show me the parent DSM, then compute the grandparent DSM."
      ),
      paste0(
        "Calculate the prior risk probability for 3 causes: P(C1)=0.4, P(C2)=0.1, P(C3)=0.3, ",
        "P(R|C1)=0.9, P(R|C2)=0.5, P(R|C3)=0.7, ",
        "P(R|~C1)=0.1, P(R|~C2)=0.5, P(R|~C3)=0.3. ",
        "Then update: C1 occurred, C2 did not occur, C3 is unknown."
      ),

      # -- Tier 3: Open-ended / conceptual (8 scenarios) --
      paste0(
        "My construction project has 5 work packages. ",
        "I think there's significant schedule risk. ",
        "How should I assess the schedule uncertainty?"
      ),
      paste0(
        "My project's CPI is 0.85 and SPI is 0.92. ",
        "We're at period 4 of 8. Should I be concerned? What should I do next?"
      ),
      "How do Bayesian risk methods differ from Monte Carlo simulation for project risk?",
      "What is the difference between the Second Moment Method and Monte Carlo simulation?",
      "What is contingency reserve and how is it calculated from simulation results?",
      "Explain what earned value management is and define CPI and SPI.",
      "What sigmoidal learning curve models does PRA support and when should I use each?",
      "How does sensitivity analysis help identify schedule risk drivers?"
    ),

    target = c(
      # Tier 1 targets
      "Calls mcs_tool with correct distributions and returns percentile results",
      "Calls smm_tool with means [10,15,20,8] and variances [4,9,16,2]",
      "Calls evm_analysis_tool with bac=500000, correct schedule, period=3, 0.55, costs",
      "Calls risk_prob_tool with correct cause probs and conditional probabilities",
      "Calls evm_analysis_tool and reports SV=-10000 (EV=40000, PV=50000)",
      "Calls parent_dsm_tool with the 4x4 matrix",
      "Calls fit_and_predict_sigmoidal_tool with logistic model, x data, y data, predict_x=10",
      "Calls mcs_tool with 2 tasks and 5000 simulations",

      # Tier 2 targets
      "Calls mcs_tool then contingency_tool with phigh=0.95",
      "Calls evm_analysis_tool and interprets EAC forecasts",
      "Calls risk_post_prob_tool with observed=[1, null]",
      "Calls sensitivity_tool with correct distributions",
      "Calls fit_and_predict_sigmoidal_tool with pearl model and predict_x=[12]",
      "Calls mcs_tool then sensitivity_tool",
      "Calls mcs_tool then contingency_tool (possibly twice for P80 and P95)",
      "Calls parent_dsm_tool then grandparent_dsm_tool",
      "Calls risk_prob_tool then risk_post_prob_tool with observed=[1,0,null]",

      # Tier 3 targets
      "Recommends MCS approach and asks for distribution estimates per work package",
      "Interprets CPI<1 and SPI<1 as behind schedule and over budget, suggests corrective action",
      "Explains Bayesian is for root-cause analysis, MCS for full distribution simulation",
      "Explains SMM is analytical approximation, MCS is full simulation with distributions",
      "Explains contingency as difference between percentile and base estimate from MCS",
      "Defines EVM, CPI = EV/AC measures cost efficiency, SPI = EV/PV measures schedule efficiency",
      "Lists logistic, gompertz, weibull, pearl models; explains S-curve shape and use cases",
      "Explains sensitivity analysis identifies which tasks contribute most to total variance"
    ),

    tier = c(
      rep("single_tool", 8),
      rep("multi_tool", 9),
      rep("open_ended", 8)
    ),

    expected_tools = list(
      # Tier 1
      "mcs_tool",
      "smm_tool",
      "evm_analysis_tool",
      "risk_prob_tool",
      "evm_analysis_tool",
      "parent_dsm_tool",
      "fit_and_predict_sigmoidal_tool",
      "mcs_tool",

      # Tier 2
      c("mcs_tool", "contingency_tool"),
      "evm_analysis_tool",
      "risk_post_prob_tool",
      "sensitivity_tool",
      "fit_and_predict_sigmoidal_tool",
      c("mcs_tool", "sensitivity_tool"),
      c("mcs_tool", "contingency_tool"),
      c("parent_dsm_tool", "grandparent_dsm_tool"),
      c("risk_prob_tool", "risk_post_prob_tool"),

      # Tier 3 — no specific tools expected; scored on content quality
      character(0),
      character(0),
      character(0),
      character(0),
      character(0),
      character(0),
      character(0),
      character(0)
    ),

    # Keywords that open-ended responses should contain for quality scoring
    expected_keywords = list(
      # Tier 1 — not used (tool accuracy matters)
      character(0), character(0), character(0), character(0),
      character(0), character(0), character(0), character(0),

      # Tier 2 — not used
      character(0), character(0), character(0), character(0),
      character(0), character(0), character(0), character(0), character(0),

      # Tier 3 — keywords for quality scoring
      c("monte carlo", "simulation", "distribution"),
      c("cpi", "spi", "behind", "over budget", "corrective"),
      c("bayesian", "root cause", "monte carlo", "simulation"),
      c("second moment", "analytical", "monte carlo", "simulation"),
      c("contingency", "percentile", "p50", "p80", "p95", "reserve"),
      c("earned value", "cpi", "spi", "cost", "schedule"),
      c("sigmoidal", "learning curve", "logistic", "gompertz"),
      c("sensitivity", "variance", "risk driver", "tornado")
    )
  )
}

# ============================================================================
# Package environment for tracking tool calls across solver invocations
# ============================================================================

.pra_eval_env <- new.env(parent = emptyenv())
.pra_eval_env$tool_calls <- list()

# ============================================================================
# Custom Solver
# ============================================================================

#' Create a PRA solver for vitals evaluation
#'
#' @param model Ollama model name
#' @param rag Whether to enable RAG
#' @return A solver function compatible with vitals (>= 0.2.0)
pra_solver <- function(model = "llama3.2", rag = TRUE) {
  force(model)
  force(rag)

  function(input) {
    n <- length(input)
    results <- character(n)
    solver_chats <- vector("list", n)

    # Reset tool tracking
    .pra_eval_env$tool_calls <- vector("list", n)

    for (i in seq_len(n)) {
      chat <- PRA::pra_chat(model = model, rag = rag)

      # Track which tools are called via chat turns
      tools_called <- character(0)

      response <- tryCatch({
        resp <- chat$chat(input[i])
        # Extract tool calls from chat turns
        turns <- tryCatch(chat$get_turns(), error = function(e) list())
        for (turn in turns) {
          tryCatch({
            if (!is.null(turn@role) && turn@role == "assistant") {
              for (content in turn@contents) {
                if (any(grepl("ToolRequest", class(content)))) {
                  tools_called <- c(tools_called, content@name)
                }
              }
            }
          }, error = function(e) NULL)
        }
        resp
      },
      error = function(e) paste0("ERROR: ", e$message)
      )

      results[i] <- response
      solver_chats[[i]] <- chat
      .pra_eval_env$tool_calls[[i]] <- tools_called
    }

    list(result = results, solver_chat = solver_chats)
  }
}

# ============================================================================
# Custom Scorer: Tool Selection + Content Quality (vitals 0.2.0 interface)
# ============================================================================

#' Score tool selection accuracy and open-ended response quality
#'
#' For tool-calling scenarios: checks if expected tools were called.
#' For open-ended scenarios: checks for domain-relevant keywords in the response.
#'
#' @param dataset The evaluation dataset (must have expected_tools and
#'   expected_keywords columns)
#' @return A scorer function
tool_selection_scorer <- function(dataset) {
  expected <- dataset$expected_tools
  keywords <- dataset$expected_keywords
  force(expected)
  force(keywords)

  function(samples, ...) {
    n <- nrow(samples)
    scores <- character(n)
    explanations <- character(n)

    for (i in seq_len(n)) {
      exp_tools <- expected[[i]]
      actual_tools <- .pra_eval_env$tool_calls[[i]]
      if (is.null(actual_tools)) actual_tools <- character(0)

      result_text <- samples$result[i]

      if (length(exp_tools) == 0) {
        # Open-ended: score on keyword coverage
        kws <- keywords[[i]]
        if (length(kws) == 0 || grepl("^ERROR:", result_text)) {
          if (nchar(result_text) > 50 && !grepl("^ERROR:", result_text)) {
            scores[i] <- "C"
            explanations[i] <- "Substantive response (no keywords defined)"
          } else {
            scores[i] <- "I"
            explanations[i] <- "Empty or error response"
          }
        } else {
          result_lower <- tolower(result_text)
          matched <- vapply(kws, function(kw) grepl(kw, result_lower, fixed = TRUE), logical(1))
          hit_rate <- mean(matched)

          if (hit_rate >= 0.5) {
            scores[i] <- "C"
            explanations[i] <- paste0("Keyword coverage: ", sum(matched), "/",
                                       length(kws), " (",
                                       paste(kws[matched], collapse = ", "), ")")
          } else if (hit_rate > 0) {
            scores[i] <- "P"
            explanations[i] <- paste0("Partial keyword coverage: ", sum(matched), "/",
                                       length(kws), " (",
                                       paste(kws[matched], collapse = ", "),
                                       "; missing: ", paste(kws[!matched], collapse = ", "), ")")
          } else {
            scores[i] <- "I"
            explanations[i] <- paste0("No keywords matched from: ",
                                       paste(kws, collapse = ", "))
          }
        }
      } else if (all(exp_tools %in% actual_tools)) {
        scores[i] <- "C"
        explanations[i] <- paste("All expected tools called:",
                                  paste(actual_tools, collapse = ", "))
      } else if (any(exp_tools %in% actual_tools)) {
        scores[i] <- "P"
        missing <- setdiff(exp_tools, actual_tools)
        explanations[i] <- paste("Partial: called",
                                  paste(intersect(exp_tools, actual_tools), collapse = ", "),
                                  "but missing", paste(missing, collapse = ", "))
      } else {
        scores[i] <- "I"
        explanations[i] <- paste("Expected:",
                                  paste(exp_tools, collapse = ", "),
                                  "| Got:",
                                  if (length(actual_tools) > 0) paste(actual_tools, collapse = ", ") else "(none)")
      }
    }

    tibble::tibble(
      score = factor(scores, levels = c("I", "P", "C"),
                     labels = c("Incorrect", "Partial", "Correct"),
                     ordered = TRUE),
      explanation = explanations
    )
  }
}

# ============================================================================
# Runner with repeated trials support
# ============================================================================

#' Run the PRA evaluation
#'
#' @param model Ollama model name
#' @param rag Whether to enable RAG
#' @param scorer_type "tool" for tool selection, "model" for model-graded QA
#' @param n_trials Number of repeated evaluation runs (default 1). Multiple
#'   trials enable confidence interval estimation.
#' @return A list with: task (vitals Task from last trial), summary (per-tier
#'   accuracy), trial_scores (accuracy per trial for CI calculation)
run_pra_eval <- function(model = "llama3.2", rag = TRUE,
                         scorer_type = "tool", n_trials = 1L) {
  if (!requireNamespace("vitals", quietly = TRUE)) {
    stop("Package 'vitals' is required. Install with: install.packages('vitals')")
  }

  dataset <- pra_eval_dataset()
  solver <- pra_solver(model = model, rag = rag)

  scorer <- if (scorer_type == "model") {
    vitals::model_graded_qa()
  } else {
    tool_selection_scorer(dataset)
  }

  trial_scores <- numeric(n_trials)
  tier_scores <- list()
  last_task <- NULL

  for (trial in seq_len(n_trials)) {
    if (n_trials > 1) message("=== Trial ", trial, " of ", n_trials, " ===")

    task <- vitals::Task$new(
      dataset = dataset,
      solver = solver,
      scorer = scorer,
      name = paste0("pra_eval_", model, if (rag) "_rag" else "_norag",
                     if (n_trials > 1) paste0("_t", trial) else "")
    )

    task$eval()
    last_task <- task

    # Extract scores from the task
    samples <- tryCatch(task$get_samples(), error = function(e) NULL)
    if (!is.null(samples)) {
      correct <- sum(samples$score == "Correct", na.rm = TRUE)
      total <- nrow(samples)
      trial_scores[trial] <- correct / total

      # Per-tier breakdown
      for (t in unique(dataset$tier)) {
        idx <- which(dataset$tier == t)
        tier_correct <- sum(samples$score[idx] == "Correct", na.rm = TRUE)
        tier_total <- length(idx)
        tier_key <- paste0(t, "_trial_", trial)
        tier_scores[[tier_key]] <- tier_correct / tier_total
      }
    }
  }

  # Build summary
  summary <- summarize_eval(trial_scores, tier_scores, dataset, n_trials, model, rag)

  list(
    task = last_task,
    summary = summary,
    trial_scores = trial_scores
  )
}

#' Summarize evaluation results with per-tier accuracy and optional CIs
#'
#' @param trial_scores Numeric vector of per-trial overall accuracy
#' @param tier_scores Named list of per-tier per-trial accuracies
#' @param dataset The evaluation dataset
#' @param n_trials Number of trials
#' @param model Model name
#' @param rag RAG setting
#' @return A list with formatted summary
summarize_eval <- function(trial_scores, tier_scores, dataset, n_trials, model, rag) {
  tiers <- unique(dataset$tier)

  cat("\n")
  cat("========================================\n")
  cat("  PRA Evaluation Summary\n")
  cat("========================================\n")
  cat("Model:  ", model, "\n")
  cat("RAG:    ", if (rag) "enabled" else "disabled", "\n")
  cat("Trials: ", n_trials, "\n")
  cat("Scenarios: ", nrow(dataset), "\n")
  cat("----------------------------------------\n")

  overall_mean <- mean(trial_scores)
  cat(sprintf("Overall accuracy: %.1f%%", overall_mean * 100))

  if (n_trials >= 3) {
    ci <- compute_ci(trial_scores)
    cat(sprintf(" (95%% CI: %.1f%% - %.1f%%)", ci[1] * 100, ci[2] * 100))
  }
  cat("\n\n")

  cat("Per-tier breakdown:\n")
  tier_summary <- list()
  for (t in tiers) {
    tier_vals <- unlist(tier_scores[grepl(paste0("^", t, "_trial_"), names(tier_scores))])
    tier_mean <- mean(tier_vals)
    tier_n <- sum(dataset$tier == t)
    cat(sprintf("  %-15s %5.1f%% (%d scenarios)", t, tier_mean * 100, tier_n))

    if (n_trials >= 3 && length(tier_vals) >= 3) {
      ci <- compute_ci(tier_vals)
      cat(sprintf("  [95%% CI: %.1f%% - %.1f%%]", ci[1] * 100, ci[2] * 100))
    }
    cat("\n")

    tier_summary[[t]] <- list(mean = tier_mean, n = tier_n, values = tier_vals)
  }
  cat("========================================\n\n")

  list(
    model = model,
    rag = rag,
    n_trials = n_trials,
    overall_mean = overall_mean,
    overall_scores = trial_scores,
    per_tier = tier_summary
  )
}

#' Compute 95% confidence interval using t-distribution
#' @param x Numeric vector of scores
#' @return Length-2 vector (lower, upper)
compute_ci <- function(x) {
  n <- length(x)
  if (n < 2) return(c(NA, NA))
  m <- mean(x)
  se <- stats::sd(x) / sqrt(n)
  t_crit <- stats::qt(0.975, df = n - 1)
  c(max(0, m - t_crit * se), min(1, m + t_crit * se))
}

# ============================================================================
# Comparative evaluation
# ============================================================================

#' Run comparative evaluation across models and RAG settings
#'
#' @param models Character vector of model names to compare
#' @param rag_options Logical vector of RAG settings to test (default both)
#' @param n_trials Number of trials per configuration (default 1)
#' @return List of evaluation result objects
run_pra_comparison <- function(models = c("llama3.2", "qwen2.5"),
                               rag_options = c(TRUE, FALSE),
                               n_trials = 1L) {
  if (!requireNamespace("vitals", quietly = TRUE)) {
    stop("Package 'vitals' is required. Install with: install.packages('vitals')")
  }

  results <- list()
  for (m in models) {
    for (r in rag_options) {
      label <- paste0(m, if (r) "_rag" else "_norag")
      message("\n>>> Evaluating: ", label, " <<<\n")
      tryCatch({
        results[[label]] <- run_pra_eval(model = m, rag = r, n_trials = n_trials)
      }, error = function(e) {
        message("  Failed: ", e$message)
      })
    }
  }

  # Print comparison table
  if (length(results) > 1) {
    cat("\n========================================\n")
    cat("  Comparison Summary\n")
    cat("========================================\n")
    cat(sprintf("%-25s %10s %12s %12s %12s\n",
                "Configuration", "Overall", "Single-tool", "Multi-tool", "Open-ended"))
    cat(strrep("-", 75), "\n")
    for (label in names(results)) {
      r <- results[[label]]$summary
      cat(sprintf("%-25s %9.1f%%",
                  label, r$overall_mean * 100))
      for (t in c("single_tool", "multi_tool", "open_ended")) {
        if (!is.null(r$per_tier[[t]])) {
          cat(sprintf(" %11.1f%%", r$per_tier[[t]]$mean * 100))
        } else {
          cat("          NA")
        }
      }
      cat("\n")
    }
    cat("========================================\n")
  }

  message("\nEvaluation complete. Use vitals::vitals_view() to inspect results.")
  results
}
