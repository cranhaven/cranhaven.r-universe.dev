#' Create PRA Tool Definitions for LLM Agent
#'
#' Creates a list of ellmer tool objects that wrap PRA's exported functions for
#' use with an LLM agent. Each tool includes a description that helps the LLM
#' select the appropriate analysis method and properly format parameters.
#'
#' @return A list of ellmer tool objects.
#'
#' @details
#' Tool wrappers handle serialization between the LLM (JSON strings) and R
#' (lists, matrices, data.frames). Complex inputs like task distribution lists
#' and correlation matrices are accepted as JSON strings and deserialized
#' internally.
#'
#' Large output vectors (e.g., Monte Carlo simulation samples) are summarized
#' to mean, sd, and key percentiles rather than returning the full vector to
#' the LLM. The full results are stored in the package environment for use by
#' downstream tools (e.g., contingency analysis needs the full distribution).
#'
#' When used with shinychat, tool results include rich HTML display with inline
#' plots via `ellmer::ContentToolResult`.
#'
#' @examples
#' \dontrun{
#' tools <- pra_tools()
#' chat <- ellmer::chat_ollama(model = "llama3.2")
#' for (tool in tools) chat$register_tool(tool)
#' }
#'
#' @export
pra_tools <- function() {
  if (!requireNamespace("ellmer", quietly = TRUE)) {
    stop("Package 'ellmer' is required for pra_tools(). Install with: install.packages('ellmer')")
  }

  list(
    # ---- Monte Carlo Simulation ----
    ellmer::tool(
      mcs_tool,
      "Run a Monte Carlo simulation to estimate total project cost or duration. Use when the user provides task-level uncertainty distributions (normal, triangular, uniform) and wants the range of possible project outcomes. Returns mean, standard deviation, and key percentiles (P5, P50, P95). Follow up with contingency_tool or sensitivity_tool.",
      arguments = list(
        num_sims = ellmer::type_integer("Number of simulations to run, typically 10000."),
        task_dists_json = ellmer::type_string(
          "JSON array of task distributions. Each element is an object with 'type' and parameters. Example: [{\"type\":\"normal\",\"mean\":10,\"sd\":2},{\"type\":\"triangular\",\"a\":5,\"b\":10,\"c\":15},{\"type\":\"uniform\",\"min\":8,\"max\":12}]"
        ),
        cor_mat_json = ellmer::type_string(
          "Optional JSON 2D array for the correlation matrix. Use 'null' or omit if tasks are independent. Example for 3 tasks: [[1,0.5,0.3],[0.5,1,0.4],[0.3,0.4,1]]"
        )
      ),
      annotations = ellmer::tool_annotations(title = "Monte Carlo Simulation")
    ),

    # ---- Second Moment Method ----
    ellmer::tool(
      smm_tool,
      "Quick analytical estimate of total project mean and variance using the Second Moment Method. Use when you need a fast approximation without running a full simulation, or when only means and variances are known (not full distributions).",
      arguments = list(
        mean_json = ellmer::type_string("JSON array of task mean values. Example: [10, 12, 8]"),
        var_json = ellmer::type_string("JSON array of task variance values. Example: [4, 9, 2]"),
        cor_mat_json = ellmer::type_string(
          "Optional JSON 2D array for the correlation matrix. Use 'null' or omit if tasks are independent."
        )
      ),
      annotations = ellmer::tool_annotations(title = "Second Moment Method")
    ),

    # ---- Contingency ----
    ellmer::tool(
      contingency_tool,
      "Calculate the contingency reserve from Monte Carlo simulation results. MUST be called after mcs_tool. Returns the dollar/time buffer needed between a base estimate (P50) and a high-confidence estimate (P95).",
      arguments = list(
        phigh = ellmer::type_number("High percentile for contingency (default 0.95)."),
        pbase = ellmer::type_number("Base percentile (default 0.50).")
      ),
      annotations = ellmer::tool_annotations(title = "Contingency Analysis")
    ),

    # ---- Sensitivity ----
    ellmer::tool(
      sensitivity_tool,
      "Identify which tasks contribute most to total project uncertainty. Returns sensitivity values for each task. Higher values mean the task is a bigger driver of total variance.",
      arguments = list(
        task_dists_json = ellmer::type_string("JSON array of task distributions (same format as mcs_tool)."),
        cor_mat_json = ellmer::type_string("Optional JSON 2D array for the correlation matrix. Use 'null' or omit if independent.")
      ),
      annotations = ellmer::tool_annotations(title = "Sensitivity Analysis")
    ),

    # ---- EVM: Full Analysis (composite) ----
    ellmer::tool(
      evm_analysis_tool,
      "Run a complete Earned Value Management analysis in one call. Computes PV, EV, AC, SV, CV, SPI, CPI, EAC (all 3 methods), ETC, VAC, and TCPI. Use this instead of individual EVM tools. Returns a full dashboard table.",
      arguments = list(
        bac = ellmer::type_number("Budget at Completion (total planned budget)."),
        schedule_json = ellmer::type_string("JSON array of cumulative planned completion fractions. Example: [0.1, 0.2, 0.4, 0.7, 1.0]"),
        time_period = ellmer::type_integer("Current time period (1-based index into schedule)."),
        actual_per_complete = ellmer::type_number("Actual percent complete as a decimal (e.g., 0.35 for 35%)."),
        actual_costs_json = ellmer::type_string("JSON array of cumulative actual costs per period. Example: [10000, 22000, 37000]"),
        cumulative = ellmer::type_enum("Whether costs are already cumulative ('true') or per-period ('false'). Default 'true'.", values = c("true", "false"))
      ),
      annotations = ellmer::tool_annotations(title = "Earned Value Management")
    ),

    # ---- Bayesian: Prior Risk Probability ----
    ellmer::tool(
      risk_prob_tool,
      "Calculate the prior probability of a risk event from root causes. Use when you have identified root causes with estimated probabilities and conditional risk probabilities.",
      arguments = list(
        cause_probs_json = ellmer::type_string("JSON array of root cause probabilities. Example: [0.3, 0.2]"),
        risks_given_causes_json = ellmer::type_string("JSON array of P(Risk|Cause). Example: [0.8, 0.6]"),
        risks_given_not_causes_json = ellmer::type_string("JSON array of P(Risk|Not Cause). Example: [0.2, 0.4]")
      ),
      annotations = ellmer::tool_annotations(title = "Bayesian Risk (Prior)")
    ),

    # ---- Bayesian: Posterior Risk Probability ----
    ellmer::tool(
      risk_post_prob_tool,
      "Update risk probability after observing whether causes occurred. Use after risk_prob_tool when new information is available about root causes.",
      arguments = list(
        cause_probs_json = ellmer::type_string("JSON array of root cause probabilities."),
        risks_given_causes_json = ellmer::type_string("JSON array of P(Risk|Cause)."),
        risks_given_not_causes_json = ellmer::type_string("JSON array of P(Risk|Not Cause)."),
        observed_causes_json = ellmer::type_string("JSON array of observed cause states: 1 (occurred), 0 (did not occur), null (unknown). Example: [1, null]")
      ),
      annotations = ellmer::tool_annotations(title = "Bayesian Risk (Posterior)")
    ),

    # ---- Bayesian: Prior Cost Distribution ----
    ellmer::tool(
      cost_pdf_tool,
      "Simulate prior cost distribution from risk probabilities. Returns summary statistics (mean, sd, percentiles) of simulated project costs.",
      arguments = list(
        num_sims = ellmer::type_integer("Number of simulations (typically 10000)."),
        risk_probs_json = ellmer::type_string("JSON array of risk probabilities (one per risk)."),
        means_given_risks_json = ellmer::type_string("JSON array of mean additional cost if risk occurs."),
        sds_given_risks_json = ellmer::type_string("JSON array of SD of additional cost if risk occurs."),
        base_cost = ellmer::type_number("Baseline project cost before risk impacts (default 0).")
      ),
      annotations = ellmer::tool_annotations(title = "Prior Cost Distribution")
    ),

    # ---- Bayesian: Posterior Cost Distribution ----
    ellmer::tool(
      cost_post_pdf_tool,
      "Simulate posterior cost distribution after observing risk outcomes. Use after observing which risks actually occurred.",
      arguments = list(
        num_sims = ellmer::type_integer("Number of simulations."),
        observed_risks_json = ellmer::type_string("JSON array of observed risk states: true (occurred), false (did not), null (unknown)."),
        means_given_risks_json = ellmer::type_string("JSON array of mean additional cost if risk occurs."),
        sds_given_risks_json = ellmer::type_string("JSON array of SD of additional cost if risk occurs."),
        base_cost = ellmer::type_number("Baseline project cost (default 0).")
      ),
      annotations = ellmer::tool_annotations(title = "Posterior Cost Distribution")
    ),

    # ---- Learning Curves: Fit and Predict (composite) ----
    ellmer::tool(
      fit_and_predict_sigmoidal_tool,
      "Fit a sigmoidal learning curve model and generate predictions. Use when the user has historical time/completion data and wants to model the learning curve. Models: pearl, gompertz, logistic.",
      arguments = list(
        x_json = ellmer::type_string("JSON array of x values (time or unit number). Example: [1,2,3,4,5,6,7,8,9,10]"),
        y_json = ellmer::type_string("JSON array of y values (cost, completion, etc.). Example: [5,15,40,60,70,75,80,85,90,95]"),
        model_type = ellmer::type_enum("Sigmoidal model type.", values = c("pearl", "gompertz", "logistic")),
        predict_x_json = ellmer::type_string("JSON array of x values to predict at. If omitted, predicts at the original x values."),
        conf_level = ellmer::type_number("Confidence level for prediction intervals (e.g., 0.95). Omit for no intervals.")
      ),
      annotations = ellmer::tool_annotations(title = "Learning Curve")
    ),

    # ---- DSM: Parent ----
    ellmer::tool(
      parent_dsm_tool,
      "Compute the resource-task dependency structure matrix (DSM). Shows which tasks share resources and are therefore coupled. Input is a resources x tasks matrix.",
      arguments = list(
        matrix_json = ellmer::type_string("JSON 2D array for a resources x tasks matrix. Example: [[1,0,1,0],[0,1,0,1],[1,0,1,1]]")
      ),
      annotations = ellmer::tool_annotations(title = "Parent DSM")
    ),

    # ---- DSM: Grandparent ----
    ellmer::tool(
      grandparent_dsm_tool,
      "Compute the risk-based task dependency structure matrix. Shows how tasks are coupled through shared risk-resource pathways. Requires both a resource-task matrix (S) and a risk-resource matrix (R).",
      arguments = list(
        s_matrix_json = ellmer::type_string("JSON 2D array for the resource-task matrix S (resources x tasks)."),
        r_matrix_json = ellmer::type_string("JSON 2D array for the risk-resource matrix R (risks x resources).")
      ),
      annotations = ellmer::tool_annotations(title = "Grandparent DSM")
    )
  )
}

# Suppress R CMD check note for 'x' used in graphics::curve() expressions
utils::globalVariables("x")

# ============================================================================
# /command registry — direct tool invocation without LLM
# ============================================================================

#' Get the slash-command registry
#'
#' Returns a named list of command definitions. Each entry has:
#' \describe{
#'   \item{title}{Display name}
#'   \item{description}{One-line summary}
#'   \item{args}{Named list of argument specs (name, type, required, description, example)}
#'   \item{fn}{Function to call with parsed arguments}
#' }
#' @return Named list of command definitions.
#' @keywords internal
pra_command_registry <- function() {
  list(
    mcs = list(
      title = "Monte Carlo Simulation",
      description = "Run a Monte Carlo simulation for task-level uncertainty.",
      args = list(
        list(
          name = "tasks", type = "json", required = TRUE,
          description = "Task distributions as JSON array",
          example = '[{"type":"normal","mean":10,"sd":2},{"type":"triangular","a":5,"b":10,"c":15},{"type":"uniform","min":8,"max":12}]'
        ),
        list(
          name = "n", type = "integer", required = FALSE,
          description = "Number of simulations (default: 10000)",
          example = "10000"
        )
      ),
      fn = function(args) {
        n <- if (!is.null(args$n)) as.integer(args$n) else 10000L
        mcs_tool(n, args$tasks)
      }
    ),
    smm = list(
      title = "Second Moment Method",
      description = "Quick analytical estimate of total project mean and variance.",
      args = list(
        list(
          name = "means", type = "json", required = TRUE,
          description = "JSON array of task mean values",
          example = "[10, 15, 20, 8]"
        ),
        list(
          name = "variances", type = "json", required = TRUE,
          description = "JSON array of task variance values",
          example = "[4, 9, 16, 2]"
        )
      ),
      fn = function(args) {
        smm_tool(args$means, args$variances)
      }
    ),
    contingency = list(
      title = "Contingency Analysis",
      description = "Calculate contingency reserve from the last MCS run.",
      args = list(
        list(
          name = "phigh", type = "number", required = FALSE,
          description = "High percentile (default: 0.95)",
          example = "0.95"
        ),
        list(
          name = "pbase", type = "number", required = FALSE,
          description = "Base percentile (default: 0.50)",
          example = "0.50"
        )
      ),
      fn = function(args) {
        ph <- if (!is.null(args$phigh)) as.numeric(args$phigh) else 0.95
        pb <- if (!is.null(args$pbase)) as.numeric(args$pbase) else 0.50
        contingency_tool(ph, pb)
      }
    ),
    sensitivity = list(
      title = "Sensitivity Analysis",
      description = "Identify which tasks drive the most variance.",
      args = list(
        list(
          name = "tasks", type = "json", required = TRUE,
          description = "Task distributions as JSON array (same format as /mcs)",
          example = '[{"type":"normal","mean":10,"sd":2},{"type":"uniform","min":8,"max":12}]'
        )
      ),
      fn = function(args) {
        sensitivity_tool(args$tasks)
      }
    ),
    evm = list(
      title = "Earned Value Management",
      description = "Run a complete EVM analysis (PV, EV, AC, SV, CV, SPI, CPI, EAC, TCPI).",
      args = list(
        list(
          name = "bac", type = "number", required = TRUE,
          description = "Budget at Completion",
          example = "500000"
        ),
        list(
          name = "schedule", type = "json", required = TRUE,
          description = "Cumulative planned completion fractions",
          example = "[0.2, 0.4, 0.6, 0.8, 1.0]"
        ),
        list(
          name = "period", type = "integer", required = TRUE,
          description = "Current time period (1-based)",
          example = "3"
        ),
        list(
          name = "complete", type = "number", required = TRUE,
          description = "Actual percent complete (0-1 or 0-100)",
          example = "0.35"
        ),
        list(
          name = "costs", type = "json", required = TRUE,
          description = "Cumulative actual costs per period",
          example = "[90000, 195000, 310000]"
        )
      ),
      fn = function(args) {
        evm_analysis_tool(
          bac = args$bac,
          schedule_json = args$schedule,
          time_period = args$period,
          actual_per_complete = args$complete,
          actual_costs_json = args$costs,
          cumulative = "true"
        )
      }
    ),
    risk = list(
      title = "Bayesian Risk (Prior)",
      description = "Calculate prior risk probability from root causes.",
      args = list(
        list(
          name = "causes", type = "json", required = TRUE,
          description = "Root cause probabilities",
          example = "[0.3, 0.2]"
        ),
        list(
          name = "risk_given", type = "json", required = TRUE,
          description = "P(Risk | Cause) for each cause",
          example = "[0.8, 0.6]"
        ),
        list(
          name = "risk_not_given", type = "json", required = TRUE,
          description = "P(Risk | Not Cause) for each cause",
          example = "[0.2, 0.4]"
        )
      ),
      fn = function(args) {
        risk_prob_tool(args$causes, args$risk_given, args$risk_not_given)
      }
    ),
    risk_post = list(
      title = "Bayesian Risk (Posterior)",
      description = "Update risk probability after observing causes.",
      args = list(
        list(
          name = "causes", type = "json", required = TRUE,
          description = "Root cause probabilities",
          example = "[0.3, 0.2]"
        ),
        list(
          name = "risk_given", type = "json", required = TRUE,
          description = "P(Risk | Cause) for each cause",
          example = "[0.8, 0.6]"
        ),
        list(
          name = "risk_not_given", type = "json", required = TRUE,
          description = "P(Risk | Not Cause) for each cause",
          example = "[0.2, 0.4]"
        ),
        list(
          name = "observed", type = "json", required = TRUE,
          description = "Observed causes: 1 (occurred), 0 (not), null (unknown)",
          example = "[1, null]"
        )
      ),
      fn = function(args) {
        risk_post_prob_tool(args$causes, args$risk_given, args$risk_not_given, args$observed)
      }
    ),
    learning = list(
      title = "Learning Curve",
      description = "Fit a sigmoidal learning curve (pearl/gompertz/logistic) and predict.",
      args = list(
        list(
          name = "x", type = "json", required = TRUE,
          description = "X values (time or unit number)",
          example = "[1,2,3,4,5,6,7,8,9,10]"
        ),
        list(
          name = "y", type = "json", required = TRUE,
          description = "Y values (cost, completion, etc.)",
          example = "[5,15,40,60,70,75,80,85,90,95]"
        ),
        list(
          name = "model", type = "string", required = TRUE,
          description = "Model type: pearl, gompertz, or logistic",
          example = "logistic"
        ),
        list(
          name = "predict", type = "json", required = FALSE,
          description = "X values to predict at (default: original x values)",
          example = "[12, 15]"
        )
      ),
      fn = function(args) {
        fit_and_predict_sigmoidal_tool(args$x, args$y, args$model, args$predict)
      }
    ),
    dsm = list(
      title = "Parent DSM",
      description = "Compute resource-task dependency structure matrix (S * S').",
      args = list(
        list(
          name = "matrix", type = "json", required = TRUE,
          description = "Resource-task matrix as JSON 2D array",
          example = "[[1,0,1],[0,1,0],[1,0,1]]"
        )
      ),
      fn = function(args) {
        parent_dsm_tool(args$matrix)
      }
    )
  )
}

#' Format help text for a single command
#' @keywords internal
format_command_help <- function(cmd_name, cmd) {
  arg_lines <- vapply(cmd$args, function(a) {
    req <- if (a$required) " (required)" else " (optional)"
    paste0("  **", a$name, "**", req, " - ", a$description, "\n    Example: `", a$example, "`")
  }, character(1))

  paste0(
    "### /", cmd_name, " - ", cmd$title, "\n\n",
    cmd$description, "\n\n",
    "**Arguments:**\n\n",
    paste(arg_lines, collapse = "\n\n"), "\n\n",
    "**Usage:**\n```\n/", cmd_name,
    paste0(" ", vapply(cmd$args, function(a) {
      if (a$required) {
        paste0(a$name, "=", a$example)
      } else {
        paste0("[", a$name, "=", a$example, "]")
      }
    }, character(1)), collapse = ""),
    "\n```"
  )
}

#' Format the /help overview
#' @keywords internal
format_help_overview <- function() {
  registry <- pra_command_registry()
  lines <- vapply(names(registry), function(name) {
    cmd <- registry[[name]]
    paste0("  **/", name, "** - ", cmd$description)
  }, character(1))

  paste0(
    "### Available Commands\n\n",
    "Type a command to run a tool directly (no LLM needed). ",
    "Type `/command` with no arguments to see detailed help.\n\n",
    paste(lines, collapse = "\n\n"),
    "\n\n  **/help** - Show this list\n\n",
    "You can also type natural language questions to use the AI agent."
  )
}

#' Parse and execute a slash command
#'
#' @param input The raw user input string starting with "/".
#' @return A list with `ok` (logical) and `result` (character or ContentToolResult).
#' @keywords internal
execute_command <- function(input) {
  input <- trimws(input)
  registry <- pra_command_registry()

  # Parse: /command arg1=val1 arg2=val2
  # Or:    /command (no args -> show help)
  # Or:    /help
  parts <- strsplit(input, "\\s+", perl = TRUE)[[1]]
  cmd_name <- sub("^/", "", tolower(parts[1]))

  # /help

  if (cmd_name == "help") {
    return(list(ok = TRUE, result = format_help_overview()))
  }

  # Unknown command
  if (!cmd_name %in% names(registry)) {
    suggestions <- names(registry)
    return(list(ok = FALSE, result = paste0(
      "Unknown command: **/", cmd_name, "**\n\n",
      "Available commands: ", paste0("`/", suggestions, "`", collapse = ", "),
      "\n\nType `/help` for details."
    )))
  }

  cmd <- registry[[cmd_name]]

  # No args -> show detailed help for this command
  if (length(parts) == 1) {
    return(list(ok = TRUE, result = format_command_help(cmd_name, cmd)))
  }

  # Parse key=value arguments
  arg_strings <- parts[-1]
  parsed_args <- list()
  parse_errors <- character(0)

  for (s in arg_strings) {
    if (!grepl("=", s)) {
      parse_errors <- c(parse_errors, paste0("Invalid argument `", s, "` - use `name=value` format."))
      next
    }
    eq_pos <- regexpr("=", s)
    key <- substr(s, 1, eq_pos - 1)
    val <- substr(s, eq_pos + 1, nchar(s))
    parsed_args[[key]] <- val
  }

  if (length(parse_errors) > 0) {
    return(list(ok = FALSE, result = paste0(
      "**Parse errors:**\n\n", paste("- ", parse_errors, collapse = "\n"), "\n\n",
      format_command_help(cmd_name, cmd)
    )))
  }

  # Check required arguments
  required <- vapply(cmd$args, function(a) if (a$required) a$name else NA_character_, character(1))
  required <- required[!is.na(required)]
  missing <- setdiff(required, names(parsed_args))

  if (length(missing) > 0) {
    missing_help <- vapply(missing, function(m) {
      a <- cmd$args[[which(vapply(cmd$args, function(x) x$name, character(1)) == m)]]
      paste0("  **", m, "** - ", a$description, "\n    Example: `", m, "=", a$example, "`")
    }, character(1))

    return(list(ok = FALSE, result = paste0(
      "**Missing required argument", if (length(missing) > 1) "s" else "", ":**\n\n",
      paste(missing_help, collapse = "\n\n"),
      "\n\n**Full usage:**\n\n",
      format_command_help(cmd_name, cmd)
    )))
  }

  # Execute
  tryCatch(
    {
      result <- cmd$fn(parsed_args)
      list(ok = TRUE, result = result)
    },
    error = function(e) {
      list(ok = FALSE, result = paste0(
        "**Error running /", cmd_name, ":** ", e$message, "\n\n",
        "Check your arguments and try again. Type `/", cmd_name,
        "` for usage details."
      ))
    }
  )
}

# ============================================================================
# Internal tool wrapper functions
# ============================================================================

# Package-level environment to store full simulation results for chaining
.pra_agent_env <- new.env(parent = emptyenv())

#' @keywords internal
check_package <- function(pkg, purpose = NULL) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    msg <- sprintf("Package '%s' is required", pkg)
    if (!is.null(purpose)) msg <- paste0(msg, " for ", purpose)
    stop(paste0(msg, ". Install with: install.packages('", pkg, "')"), call. = FALSE)
  }
}

#' @keywords internal
parse_json_param <- function(json_str, param_name, tool_name) {
  if (is.null(json_str) || json_str == "null" || json_str == "") {
    return(NULL)
  }
  check_package("jsonlite")
  tryCatch(
    jsonlite::fromJSON(json_str, simplifyVector = TRUE),
    error = function(e) {
      stop(sprintf(
        "Invalid JSON for '%s' in %s: %s\nReceived: %s",
        param_name, tool_name, e$message, json_str
      ), call. = FALSE)
    }
  )
}

#' @keywords internal
parse_json <- function(json_str) {
  check_package("jsonlite")
  if (is.null(json_str) || json_str == "null" || json_str == "") {
    return(NULL)
  }

  # Clean common LLM formatting mistakes before parsing
  cleaned <- json_str

  # Remove smart quotes that some models produce
  cleaned <- gsub("\u201c|\u201d", "\"", cleaned)
  cleaned <- gsub("\u2018|\u2019", "'", cleaned)

  # Strip leading/trailing whitespace

  cleaned <- trimws(cleaned)

  # If it looks like a bare comma-separated list (no brackets), wrap in []
  if (!grepl("^\\s*[\\[{]", cleaned) && grepl("^\\s*-?[0-9]", cleaned)) {
    cleaned <- paste0("[", cleaned, "]")
  }

  # Try strict parse first, then fallback with cleanup
  tryCatch(
    jsonlite::fromJSON(cleaned, simplifyVector = TRUE),
    error = function(e) {
      # Try removing trailing commas: [1, 2,] -> [1, 2]
      cleaned2 <- gsub(",\\s*([\\]\\}])", "\\1", cleaned)
      # Try replacing single quotes with double quotes
      cleaned2 <- gsub("'", "\"", cleaned2)
      tryCatch(
        jsonlite::fromJSON(cleaned2, simplifyVector = TRUE),
        error = function(e2) {
          # Last resort: try to extract numbers from the string
          nums <- regmatches(cleaned, gregexpr("-?[0-9]*\\.?[0-9]+", cleaned))[[1]]
          if (length(nums) > 0) {
            as.numeric(nums)
          } else {
            stop("Cannot parse JSON: ", json_str, call. = FALSE)
          }
        }
      )
    }
  )
}

#' Coerce an input to an R object
#'
#' Accepts either a JSON string (from LLM tool calls) or a native R object
#' (from direct R usage). If the input is a single character string, it is
#' parsed as JSON. Otherwise it is returned as-is.
#' @keywords internal
as_r_input <- function(x) {
  if (is.null(x)) {
    return(NULL)
  }
  if (is.character(x) && length(x) == 1) {
    parse_json(x)
  } else {
    x
  }
}

#' Coerce input to a numeric vector
#'
#' Parses JSON input and ensures the result is a numeric vector. Handles
#' cases where LLMs send string representations of numbers.
#' @keywords internal
as_numeric_input <- function(x) {
  val <- as_r_input(x)
  if (is.null(val)) {
    return(NULL)
  }
  as.numeric(val)
}

#' Validate task distributions parsed from JSON
#' @keywords internal
validate_task_dists <- function(task_dists, tool_name = "mcs_tool") {
  if (!is.list(task_dists) || length(task_dists) == 0) {
    stop(tool_name, ": task_dists_json must be a non-empty JSON array of distribution objects.", call. = FALSE)
  }
  valid_types <- c("normal", "triangular", "uniform")
  required_params <- list(
    normal = c("mean", "sd"),
    triangular = c("a", "b", "c"),
    uniform = c("min", "max")
  )
  for (i in seq_along(task_dists)) {
    d <- task_dists[[i]]
    dtype <- d$type
    if (is.null(dtype) || !dtype %in% valid_types) {
      stop(
        sprintf(
          "%s: Task %d has invalid type '%s'. Must be one of: %s",
          tool_name, i, as.character(dtype), paste(valid_types, collapse = ", ")
        ),
        call. = FALSE
      )
    }
    missing <- setdiff(required_params[[dtype]], names(d))
    if (length(missing) > 0) {
      stop(
        sprintf(
          "%s: Task %d (%s) is missing required parameters: %s",
          tool_name, i, dtype, paste(missing, collapse = ", ")
        ),
        call. = FALSE
      )
    }
  }
  invisible(TRUE)
}

#' @noRd
format_result_table <- function(...) {
  pairs <- list(...)
  nms <- names(pairs)
  vals <- vapply(pairs, function(v) {
    if (is.numeric(v)) format(round(v, 4), big.mark = ",", scientific = FALSE) else as.character(v)
  }, character(1))
  max_n <- max(nchar(nms))
  lines <- paste0("  ", formatC(nms, width = -max_n), "  ", vals)
  paste(lines, collapse = "\n")
}

#' @keywords internal
summarize_distribution <- function(x) {
  pcts <- stats::quantile(x, c(0.05, 0.10, 0.25, 0.50, 0.75, 0.90, 0.95))
  list(
    mean = mean(x),
    sd = stats::sd(x),
    min = min(x),
    max = max(x),
    percentiles = list(
      P5  = unname(pcts[1]),
      P10 = unname(pcts[2]),
      P25 = unname(pcts[3]),
      P50 = unname(pcts[4]),
      P75 = unname(pcts[5]),
      P90 = unname(pcts[6]),
      P95 = unname(pcts[7])
    )
  )
}

#' @noRd
format_distribution <- function(x) {
  s <- summarize_distribution(x)
  paste0(
    "Summary Statistics:\n",
    format_result_table(
      Mean = s$mean, SD = s$sd, Min = s$min, Max = s$max
    ),
    "\n\nPercentiles:\n",
    format_result_table(
      P5 = s$percentiles$P5, P10 = s$percentiles$P10,
      P25 = s$percentiles$P25, P50 = s$percentiles$P50,
      P75 = s$percentiles$P75, P90 = s$percentiles$P90,
      P95 = s$percentiles$P95
    )
  )
}

#' @keywords internal
parse_task_dists <- function(x) {
  if (is.list(x) && !is.data.frame(x) && length(x) > 0 && is.list(x[[1]])) {
    return(x) # Already a list-of-lists from direct R call
  }
  raw <- as_r_input(x)
  if (is.data.frame(raw)) {
    lapply(seq_len(nrow(raw)), function(i) as.list(raw[i, , drop = FALSE]))
  } else {
    raw
  }
}

#' Save a plot to a temp file and return path
#' @keywords internal
save_pra_plot <- function(plot_fn, name, width = 700, height = 450) {
  plot_path <- file.path(tempdir(), paste0(
    "pra_plot_", name, "_",
    format(Sys.time(), "%H%M%S"), ".png"
  ))
  grDevices::png(plot_path, width = width, height = height, res = 100)
  tryCatch(plot_fn(), finally = grDevices::dev.off())
  plot_path
}

#' Generate a base64-encoded HTML img tag from a plot function
#' @keywords internal
plot_to_html <- function(plot_fn, width = 700, height = 450) {
  check_package("base64enc")
  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)
  grDevices::png(tmp, width = width, height = height, res = 100)
  tryCatch(plot_fn(), finally = grDevices::dev.off())
  raw_bytes <- readBin(tmp, "raw", file.info(tmp)$size)
  b64 <- base64enc::dataURI(raw_bytes, mime = "image/png")
  paste0('<img src="', b64, '" style="max-width:100%;border-radius:8px;margin-top:8px;">')
}

#' Build an HTML table from name-value pairs
#' @keywords internal
html_result_table <- function(...) {
  pairs <- list(...)
  nms <- names(pairs)
  vals <- vapply(pairs, function(v) {
    if (is.numeric(v)) format(round(v, 4), big.mark = ",", scientific = FALSE) else as.character(v)
  }, character(1))
  rows <- paste0(
    "<tr><td style='padding:4px 12px;font-weight:600;'>", nms,
    "</td><td style='padding:4px 12px;'>", vals, "</td></tr>"
  )
  paste0(
    "<table style='border-collapse:collapse;margin:8px 0;font-size:0.9em;'>",
    paste(rows, collapse = ""), "</table>"
  )
}

#' Build a ContentToolResult or fall back to plain text
#' @keywords internal
tool_result <- function(text, title = NULL, html = NULL) {
  # Try to return rich ContentToolResult for shinychat display
  if (!is.null(html)) {
    tryCatch(
      {
        display <- list(html = html)
        if (!is.null(title)) display$title <- title
        ellmer::ContentToolResult(value = text, extra = list(display = display))
      },
      error = function(e) {
        # Fall back to plain text if ContentToolResult not available
        text
      }
    )
  } else {
    text
  }
}

# ---- MCS ----
#' @keywords internal
mcs_tool <- function(num_sims, task_dists_json, cor_mat_json = "null") {
  num_sims <- as.integer(unlist(num_sims))[1]
  if (is.na(num_sims) || num_sims < 1) {
    stop("mcs_tool: num_sims must be a positive integer.", call. = FALSE)
  }
  task_dists <- parse_task_dists(task_dists_json)
  validate_task_dists(task_dists, "mcs_tool")
  cor_mat <- as_r_input(cor_mat_json)
  if (!is.null(cor_mat)) cor_mat <- as.matrix(as.data.frame(cor_mat))
  result <- mcs(num_sims, task_dists, cor_mat)
  .pra_agent_env$last_mcs <- result

  s <- summarize_distribution(result$total_distribution)

  # Plain text for LLM
  text <- paste0(
    "Monte Carlo Simulation Results (n = ", format(num_sims, big.mark = ","), "):\n\n",
    format_distribution(result$total_distribution)
  )

  # Rich HTML display
  html <- tryCatch(
    {
      stats_html <- html_result_table(
        Mean = s$mean, `Std Dev` = s$sd, Min = s$min, Max = s$max,
        P5 = s$percentiles$P5, P25 = s$percentiles$P25,
        P50 = s$percentiles$P50, P75 = s$percentiles$P75,
        P90 = s$percentiles$P90, P95 = s$percentiles$P95
      )
      plot_img <- plot_to_html(function() {
        graphics::hist(result$total_distribution,
          freq = FALSE, breaks = 50,
          main = "Monte Carlo Simulation Results",
          xlab = "Total Project Duration/Cost",
          col = "#18bc9c80", border = "white"
        )
        graphics::curve(stats::dnorm(x, mean = result$total_mean, sd = result$total_sd),
          add = TRUE, col = "#2c3e50", lwd = 2
        )
        graphics::abline(
          v = stats::quantile(result$total_distribution, c(0.50, 0.95)),
          col = c("#3498db", "#e74c3c"), lty = 2, lwd = 1.5
        )
        graphics::legend("topright",
          legend = c("Normal fit", "P50", "P95"),
          col = c("#2c3e50", "#3498db", "#e74c3c"),
          lty = c(1, 2, 2), lwd = c(2, 1.5, 1.5),
          cex = 0.8, bg = "white"
        )
      })
      paste0(stats_html, plot_img)
    },
    error = function(e) NULL
  )

  tool_result(text, title = "Monte Carlo Simulation", html = html)
}

# ---- SMM ----
#' @keywords internal
smm_tool <- function(mean_json, var_json, cor_mat_json = "null") {
  means <- as_numeric_input(mean_json)
  vars <- as_numeric_input(var_json)
  cor_mat <- as_r_input(cor_mat_json)
  if (!is.null(cor_mat)) cor_mat <- as.matrix(as.data.frame(cor_mat))
  result <- smm(means, vars, cor_mat)

  text <- paste0(
    "Second Moment Method Results:\n\n",
    format_result_table(
      `Total Mean` = result$total_mean,
      `Total Variance` = result$total_var,
      `Total Std Dev` = result$total_std
    )
  )

  html <- tryCatch(
    {
      html_result_table(
        `Total Mean` = result$total_mean,
        `Total Variance` = result$total_var,
        `Total Std Dev` = result$total_std
      )
    },
    error = function(e) NULL
  )

  tool_result(text, title = "Second Moment Method", html = html)
}

# ---- Contingency ----
#' @keywords internal
contingency_tool <- function(phigh = 0.95, pbase = 0.50) {
  phigh <- as.numeric(phigh)
  pbase <- as.numeric(pbase)
  if (is.null(.pra_agent_env$last_mcs)) {
    return("Error: Run mcs_tool first. Contingency requires Monte Carlo simulation results.")
  }
  result <- contingency(.pra_agent_env$last_mcs, phigh, pbase)

  text <- paste0(
    "Contingency Analysis:\n\n",
    format_result_table(
      `Base Percentile` = paste0("P", pbase * 100),
      `High Percentile` = paste0("P", phigh * 100),
      `Contingency Reserve` = result
    )
  )

  html <- tryCatch(
    {
      html_result_table(
        `Base Percentile` = paste0("P", pbase * 100),
        `High Percentile` = paste0("P", phigh * 100),
        `Contingency Reserve` = result
      )
    },
    error = function(e) NULL
  )

  tool_result(text, title = "Contingency Analysis", html = html)
}

# ---- Sensitivity ----
#' @keywords internal
sensitivity_tool <- function(task_dists_json, cor_mat_json = "null") {
  task_dists <- parse_task_dists(task_dists_json)
  validate_task_dists(task_dists, "sensitivity_tool")
  cor_mat <- as_r_input(cor_mat_json)
  if (!is.null(cor_mat)) cor_mat <- as.matrix(as.data.frame(cor_mat))
  result <- sensitivity(task_dists, cor_mat)
  names(result) <- paste("Task", seq_along(result))

  vals <- vapply(result, function(v) format(round(v, 4), big.mark = ","), character(1))
  lines <- paste0("  ", formatC(names(result), width = -10), vals)
  text <- paste0(
    "Sensitivity Analysis (variance contribution per task):\n\n",
    paste(lines, collapse = "\n")
  )

  html <- tryCatch(
    {
      # Table
      tbl_args <- as.list(result)
      names(tbl_args) <- names(result)
      tbl_html <- do.call(html_result_table, tbl_args)

      # Tornado chart
      plot_img <- plot_to_html(function() {
        sorted <- sort(result)
        graphics::barplot(sorted,
          horiz = TRUE, las = 1,
          main = "Sensitivity Analysis",
          xlab = "Variance Contribution",
          col = grDevices::colorRampPalette(c("#18bc9c", "#e74c3c"))(length(sorted)),
          border = "white"
        )
      })
      paste0(tbl_html, plot_img)
    },
    error = function(e) NULL
  )

  tool_result(text, title = "Sensitivity Analysis", html = html)
}

# ---- EVM: Composite analysis ----
#' @keywords internal
evm_analysis_tool <- function(bac, schedule_json, time_period, actual_per_complete,
                              actual_costs_json, cumulative = "true") {
  # Coerce scalar params - LLMs may send strings, arrays, or JSON fragments
  bac <- as.numeric(unlist(bac))[1]
  time_period <- as.integer(unlist(time_period))[1]
  actual_per_complete <- as.numeric(unlist(actual_per_complete))[1]
  # Auto-convert integer-like percentages to decimals (e.g. 35 -> 0.35)
  if (!is.na(actual_per_complete) && actual_per_complete > 1 &&
    actual_per_complete <= 100 && actual_per_complete == round(actual_per_complete)) {
    actual_per_complete <- actual_per_complete / 100
  }
  if (is.na(bac) || bac <= 0) {
    stop("evm_analysis_tool: bac must be a positive number.", call. = FALSE)
  }
  if (is.na(time_period) || time_period < 1) {
    stop("evm_analysis_tool: time_period must be a positive integer.", call. = FALSE)
  }
  if (is.na(actual_per_complete) || actual_per_complete < 0 || actual_per_complete > 1) {
    stop("evm_analysis_tool: actual_per_complete must be between 0 and 1.", call. = FALSE)
  }
  schedule <- as_numeric_input(schedule_json)
  if (time_period > length(schedule)) {
    stop(sprintf(
      "evm_analysis_tool: time_period (%d) exceeds schedule length (%d).",
      time_period, length(schedule)
    ), call. = FALSE)
  }
  actual_costs <- as_numeric_input(actual_costs_json)
  cum <- if (is.logical(cumulative)) cumulative else cumulative == "true"

  # Core metrics
  pv_val <- pv(bac, schedule, time_period)
  ev_val <- ev(bac, actual_per_complete)
  ac_val <- ac(actual_costs, time_period, cum)

  # Variances and indices
  sv_val <- sv(ev_val, pv_val)
  cv_val <- cv(ev_val, ac_val)
  spi_val <- spi(ev_val, pv_val)
  cpi_val <- cpi(ev_val, ac_val)

  # Forecasts
  eac_typical <- eac(bac, "typical", cpi = cpi_val)
  eac_atypical <- eac(bac, "atypical", ac = ac_val, ev = ev_val)
  eac_combined <- eac(bac, "combined", cpi = cpi_val, ac = ac_val, ev = ev_val, spi = spi_val)
  etc_val <- etc(bac, ev_val, cpi_val)
  vac_val <- vac(bac, eac_typical)
  tcpi_val <- tcpi(bac, ev_val, ac_val, "bac")

  text <- paste0(
    "Earned Value Management Analysis:\n\n",
    "Core Metrics:\n",
    format_result_table(`Planned Value (PV)` = pv_val, `Earned Value (EV)` = ev_val, `Actual Cost (AC)` = ac_val),
    "\n\nVariances:\n",
    format_result_table(`Schedule Variance (SV)` = sv_val, `Cost Variance (CV)` = cv_val),
    "\n\nPerformance Indices:\n",
    format_result_table(`Schedule Performance Index (SPI)` = spi_val, `Cost Performance Index (CPI)` = cpi_val),
    "\n\nForecasts:\n",
    format_result_table(
      `EAC (Typical)` = eac_typical, `EAC (Atypical)` = eac_atypical,
      `EAC (Combined)` = eac_combined, `Estimate to Complete (ETC)` = etc_val,
      `Variance at Completion (VAC)` = vac_val, `TCPI (to meet BAC)` = tcpi_val
    )
  )

  html <- tryCatch(
    {
      core_tbl <- html_result_table(
        `Planned Value (PV)` = pv_val, `Earned Value (EV)` = ev_val, `Actual Cost (AC)` = ac_val
      )
      perf_tbl <- html_result_table(
        `Schedule Variance (SV)` = sv_val, `Cost Variance (CV)` = cv_val,
        `SPI` = spi_val, `CPI` = cpi_val
      )
      forecast_tbl <- html_result_table(
        `EAC (Typical)` = eac_typical, `EAC (Atypical)` = eac_atypical,
        `EAC (Combined)` = eac_combined, `ETC` = etc_val,
        `VAC` = vac_val, `TCPI` = tcpi_val
      )

      # Bar chart comparing PV, EV, AC
      plot_img <- plot_to_html(function() {
        vals <- c(pv_val, ev_val, ac_val)
        nms <- c("PV", "EV", "AC")
        cols <- c("#3498db", "#18bc9c", "#e74c3c")
        graphics::barplot(vals,
          names.arg = nms, col = cols, border = "white",
          main = "EVM Core Metrics", ylab = "Value",
          ylim = c(0, max(vals) * 1.15)
        )
        graphics::text(seq(0.7, by = 1.2, length.out = 3), vals,
          labels = format(round(vals), big.mark = ","),
          pos = 3, cex = 0.85, font = 2
        )
      })

      paste0(
        "<h5>Core Metrics</h5>", core_tbl,
        "<h5>Performance</h5>", perf_tbl,
        "<h5>Forecasts</h5>", forecast_tbl,
        plot_img
      )
    },
    error = function(e) NULL
  )

  tool_result(text, title = "Earned Value Management", html = html)
}

# ---- EVM: Individual tools ----
#' @keywords internal
pv_tool <- function(bac, schedule_json, time_period) {
  schedule <- as_numeric_input(schedule_json)
  val <- pv(bac, schedule, time_period)
  format_result_table(`Planned Value (PV)` = val)
}

#' @keywords internal
ev_tool <- function(bac, actual_per_complete) {
  val <- ev(bac, actual_per_complete)
  format_result_table(`Earned Value (EV)` = val)
}

#' @keywords internal
ac_tool <- function(actual_costs_json, time_period, cumulative = "true") {
  actual_costs <- as_numeric_input(actual_costs_json)
  cum <- if (is.logical(cumulative)) cumulative else cumulative == "true"
  val <- ac(actual_costs, time_period, cum)
  format_result_table(`Actual Cost (AC)` = val)
}

#' @keywords internal
sv_tool <- function(ev, pv) {
  val <- sv(ev, pv)
  format_result_table(`Schedule Variance (SV)` = val)
}

#' @keywords internal
cv_tool <- function(ev, ac) {
  val <- cv(ev, ac)
  format_result_table(`Cost Variance (CV)` = val)
}

#' @keywords internal
spi_tool <- function(ev, pv) {
  val <- spi(ev, pv)
  format_result_table(`Schedule Performance Index (SPI)` = val)
}

#' @keywords internal
cpi_tool <- function(ev, ac) {
  val <- cpi(ev, ac)
  format_result_table(`Cost Performance Index (CPI)` = val)
}

#' @keywords internal
eac_tool <- function(bac, method = "typical", cpi = NULL, ac = NULL, ev = NULL, spi = NULL) {
  val <- eac(bac, method, cpi, ac, ev, spi)
  paste0(
    format_result_table(`Estimate at Completion (EAC)` = val),
    "\n  Method                          ", method
  )
}

#' @keywords internal
etc_tool <- function(bac, ev, cpi = NULL) {
  val <- etc(bac, ev, cpi)
  format_result_table(`Estimate to Complete (ETC)` = val)
}

#' @keywords internal
vac_tool <- function(bac, eac) {
  val <- vac(bac, eac)
  format_result_table(`Variance at Completion (VAC)` = val)
}

#' @keywords internal
tcpi_tool <- function(bac, ev, ac, target = "bac", eac = NULL) {
  val <- tcpi(bac, ev, ac, target, eac)
  paste0(
    format_result_table(`To-Complete Performance Index (TCPI)` = val),
    "\n  Target                                     ", target
  )
}

# ---- Bayesian tools ----
#' @keywords internal
risk_prob_tool <- function(cause_probs_json, risks_given_causes_json, risks_given_not_causes_json) {
  cause_probs <- as_numeric_input(cause_probs_json)
  rgc <- as_numeric_input(risks_given_causes_json)
  rgnc <- as_numeric_input(risks_given_not_causes_json)
  val <- risk_prob(cause_probs, rgc, rgnc)

  text <- paste0(
    "Bayesian Risk Analysis (Prior):\n\n",
    format_result_table(
      `Risk Probability` = val,
      `Risk Percentage` = paste0(round(val * 100, 2), "%"),
      `Number of Causes` = length(cause_probs)
    )
  )

  html <- tryCatch(
    {
      html_result_table(
        `Risk Probability` = val,
        `Risk Percentage` = paste0(round(val * 100, 2), "%"),
        `Number of Causes` = length(cause_probs)
      )
    },
    error = function(e) NULL
  )

  tool_result(text, title = "Bayesian Risk (Prior)", html = html)
}

#' @keywords internal
risk_post_prob_tool <- function(cause_probs_json, risks_given_causes_json,
                                risks_given_not_causes_json, observed_causes_json) {
  cause_probs <- as_numeric_input(cause_probs_json)
  rgc <- as_numeric_input(risks_given_causes_json)
  rgnc <- as_numeric_input(risks_given_not_causes_json)
  observed <- as_r_input(observed_causes_json)
  observed <- ifelse(is.na(observed), NA, as.logical(observed))
  val <- risk_post_prob(cause_probs, rgc, rgnc, observed)
  obs_labels <- ifelse(is.na(observed), "Unknown", ifelse(observed, "Occurred", "Did not occur"))
  obs_lines <- paste0("  Cause ", seq_along(observed), ": ", obs_labels)

  text <- paste0(
    "Bayesian Risk Analysis (Posterior):\n\n",
    format_result_table(
      `Posterior Risk Probability` = val,
      `Posterior Risk Percentage` = paste0(round(val * 100, 2), "%")
    ),
    "\n\nObservations:\n", paste(obs_lines, collapse = "\n")
  )

  html <- tryCatch(
    {
      obs_args <- as.list(stats::setNames(obs_labels, paste("Cause", seq_along(observed))))
      paste0(
        html_result_table(
          `Posterior Probability` = val,
          `Posterior Percentage` = paste0(round(val * 100, 2), "%")
        ),
        "<h5>Observations</h5>",
        do.call(html_result_table, obs_args)
      )
    },
    error = function(e) NULL
  )

  tool_result(text, title = "Bayesian Risk (Posterior)", html = html)
}

#' @keywords internal
cost_pdf_tool <- function(num_sims, risk_probs_json, means_given_risks_json,
                          sds_given_risks_json, base_cost = 0) {
  num_sims <- as.integer(unlist(num_sims))[1]
  base_cost <- as.numeric(unlist(base_cost))[1]
  risk_probs <- as_numeric_input(risk_probs_json)
  means <- as_numeric_input(means_given_risks_json)
  sds <- as_numeric_input(sds_given_risks_json)
  result <- cost_pdf(num_sims, risk_probs, means, sds, base_cost)
  .pra_agent_env$last_cost_pdf <- result

  text <- paste0(
    "Prior Cost Distribution (n = ", format(num_sims, big.mark = ","),
    ", base cost = ", format(base_cost, big.mark = ",", scientific = FALSE), "):\n\n",
    format_distribution(result)
  )

  html <- tryCatch(
    {
      s <- summarize_distribution(result)
      tbl <- html_result_table(
        Mean = s$mean, `Std Dev` = s$sd, P50 = s$percentiles$P50,
        P90 = s$percentiles$P90, P95 = s$percentiles$P95
      )
      tbl
    },
    error = function(e) NULL
  )

  tool_result(text, title = "Prior Cost Distribution", html = html)
}

#' @keywords internal
cost_post_pdf_tool <- function(num_sims, observed_risks_json, means_given_risks_json,
                               sds_given_risks_json, base_cost = 0) {
  num_sims <- as.integer(unlist(num_sims))[1]
  base_cost <- as.numeric(unlist(base_cost))[1]
  observed <- as_r_input(observed_risks_json)
  observed <- ifelse(is.na(observed), NA, as.logical(observed))
  means <- as_numeric_input(means_given_risks_json)
  sds <- as_numeric_input(sds_given_risks_json)
  result <- cost_post_pdf(num_sims, observed, means, sds, base_cost)
  .pra_agent_env$last_cost_post_pdf <- result

  text <- paste0(
    "Posterior Cost Distribution (n = ", format(num_sims, big.mark = ","),
    ", base cost = ", format(base_cost, big.mark = ",", scientific = FALSE), "):\n\n",
    format_distribution(result)
  )

  html <- tryCatch(
    {
      s <- summarize_distribution(result)
      html_result_table(
        Mean = s$mean, `Std Dev` = s$sd, P50 = s$percentiles$P50,
        P90 = s$percentiles$P90, P95 = s$percentiles$P95
      )
    },
    error = function(e) NULL
  )

  tool_result(text, title = "Posterior Cost Distribution", html = html)
}

# ---- Sigmoidal (composite) ----
#' @keywords internal
fit_and_predict_sigmoidal_tool <- function(x_json, y_json, model_type,
                                           predict_x_json = NULL, conf_level = NULL) {
  x <- as_numeric_input(x_json)
  y <- as_numeric_input(y_json)
  data <- data.frame(x = x, y = y)

  fit <- fit_sigmoidal(data, "x", "y", model_type)
  coefficients <- stats::coef(fit)

  predict_x <- as_numeric_input(predict_x_json)
  if (is.null(predict_x)) predict_x <- x

  if (is.null(conf_level) || is.na(conf_level)) {
    preds <- predict_sigmoidal(fit, predict_x, model_type)
  } else {
    preds <- predict_sigmoidal(fit, predict_x, model_type, conf_level)
  }

  # Format coefficients
  coef_lines <- paste0(
    "  ", formatC(names(coefficients), width = -10),
    format(round(unname(coefficients), 6), big.mark = ",")
  )

  # Format predictions table
  pred_df <- data.frame(x = predict_x, predicted = round(preds, 4))
  pred_lines <- paste(utils::capture.output(print(pred_df, row.names = FALSE)), collapse = "\n")

  text <- paste0(
    "Learning Curve Fit (", model_type, " model):\n\n",
    "Coefficients:\n", paste(coef_lines, collapse = "\n"),
    "\n\nPredictions:\n", pred_lines
  )

  html <- tryCatch(
    {
      # Coefficients table
      coef_args <- as.list(unname(coefficients))
      names(coef_args) <- names(coefficients)
      coef_tbl <- do.call(html_result_table, coef_args)

      # Learning curve plot with data + fitted curve
      plot_img <- plot_to_html(function() {
        graphics::plot(x, y,
          pch = 19, col = "#2c3e50",
          main = paste0("Learning Curve (", model_type, ")"),
          xlab = "x", ylab = "y"
        )
        fine_x <- seq(min(x), max(x), length.out = 200)
        fine_y <- predict_sigmoidal(fit, fine_x, model_type)
        graphics::lines(fine_x, fine_y, col = "#18bc9c", lwd = 2)
        if (length(predict_x) > 0) {
          graphics::points(predict_x, preds, pch = 4, col = "#e74c3c", cex = 1.2, lwd = 2)
        }
        graphics::legend("bottomright",
          legend = c("Data", "Fitted curve", "Predictions"),
          col = c("#2c3e50", "#18bc9c", "#e74c3c"),
          pch = c(19, NA, 4), lty = c(NA, 1, NA), lwd = c(NA, 2, 2),
          cex = 0.8, bg = "white"
        )
      })

      paste0("<h5>Coefficients</h5>", coef_tbl, plot_img)
    },
    error = function(e) NULL
  )

  tool_result(text, title = paste0("Learning Curve (", model_type, ")"), html = html)
}

# ---- DSM tools ----
#' @keywords internal
parent_dsm_tool <- function(matrix_json) {
  mat <- as_r_input(matrix_json)
  if (!is.matrix(mat)) mat <- as.matrix(as.data.frame(mat))
  result <- parent_dsm(mat)
  result_mat <- result$matrix
  lines <- paste(utils::capture.output(print(result_mat)), collapse = "\n")

  text <- paste0("Parent DSM (Resource-Task Dependencies):\n\n", lines)

  html <- tryCatch(
    {
      paste0(
        "<pre style='font-size:0.85em;background:#f8f9fa;padding:10px;border-radius:6px;'>",
        lines, "</pre>"
      )
    },
    error = function(e) NULL
  )

  tool_result(text, title = "Parent DSM", html = html)
}

#' @keywords internal
grandparent_dsm_tool <- function(s_matrix_json, r_matrix_json) {
  s_mat <- as_r_input(s_matrix_json)
  if (!is.matrix(s_mat)) s_mat <- as.matrix(as.data.frame(s_mat))
  r_mat <- as_r_input(r_matrix_json)
  if (!is.matrix(r_mat)) r_mat <- as.matrix(as.data.frame(r_mat))
  result <- grandparent_dsm(s_mat, r_mat)
  result_mat <- result$matrix
  lines <- paste(utils::capture.output(print(result_mat)), collapse = "\n")

  text <- paste0("Grandparent DSM (Risk-Resource-Task Dependencies):\n\n", lines)

  html <- tryCatch(
    {
      paste0(
        "<pre style='font-size:0.85em;background:#f8f9fa;padding:10px;border-radius:6px;'>",
        lines, "</pre>"
      )
    },
    error = function(e) NULL
  )

  tool_result(text, title = "Grandparent DSM", html = html)
}

# ===========================================================================
# /command framework — deterministic tool calls bypassing the LLM
# ===========================================================================

#' PRA Slash-Command Registry
#'
#' Returns a named list of command definitions. Each command has a title,
#' description, argument specs (name, type, required, help), usage examples,
#' and an executor function that calls the underlying PRA tool wrapper.
#'
#' @return A named list of command definitions.
#' @keywords internal
pra_command_registry <- function() {
  list(
    mcs = list(
      title = "Monte Carlo Simulation",
      description = "Run a Monte Carlo simulation for project cost/duration uncertainty.",
      args = list(
        list(
          name = "n", type = "integer", required = FALSE, default = 10000L,
          help = "Number of simulations (default: 10000)"
        ),
        list(
          name = "tasks", type = "json", required = TRUE, default = NULL,
          help = "JSON array of task distributions, e.g. [{\"type\":\"normal\",\"mean\":10,\"sd\":2}]"
        )
      ),
      examples = c(
        '/mcs tasks=[{"type":"normal","mean":10,"sd":2},{"type":"triangular","a":5,"b":10,"c":15}]',
        '/mcs n=5000 tasks=[{"type":"uniform","min":8,"max":12}]'
      ),
      fn = function(args) {
        n <- if (!is.null(args$n)) as.integer(args$n) else 10000L
        mcs_tool(num_sims = n, task_dists_json = args$tasks)
      }
    ),
    smm = list(
      title = "Second Moment Method",
      description = "Quick analytical estimate of total project mean and variance.",
      args = list(
        list(
          name = "means", type = "json", required = TRUE, default = NULL,
          help = "JSON array of task means, e.g. [10, 12, 8]"
        ),
        list(
          name = "vars", type = "json", required = TRUE, default = NULL,
          help = "JSON array of task variances, e.g. [4, 9, 2]"
        )
      ),
      examples = c(
        "/smm means=[10,12,8] vars=[4,9,2]"
      ),
      fn = function(args) {
        smm_tool(mean_json = args$means, var_json = args$vars)
      }
    ),
    contingency = list(
      title = "Contingency Reserve",
      description = "Calculate contingency reserve from the last MCS result. Run /mcs first.",
      args = list(
        list(
          name = "phigh", type = "number", required = FALSE, default = 0.95,
          help = "High confidence percentile (default: 0.95)"
        ),
        list(
          name = "pbase", type = "number", required = FALSE, default = 0.50,
          help = "Base percentile (default: 0.50)"
        )
      ),
      examples = c(
        "/contingency",
        "/contingency phigh=0.90 pbase=0.50"
      ),
      fn = function(args) {
        ph <- if (!is.null(args$phigh)) as.numeric(args$phigh) else 0.95
        pb <- if (!is.null(args$pbase)) as.numeric(args$pbase) else 0.50
        contingency_tool(phigh = ph, pbase = pb)
      }
    ),
    sensitivity = list(
      title = "Sensitivity Analysis",
      description = "Variance contribution per task (which tasks drive the most risk).",
      args = list(
        list(
          name = "tasks", type = "json", required = TRUE, default = NULL,
          help = "JSON array of task distributions (same format as /mcs)"
        )
      ),
      examples = c(
        '/sensitivity tasks=[{"type":"normal","mean":10,"sd":2},{"type":"triangular","a":5,"b":10,"c":15}]'
      ),
      fn = function(args) {
        sensitivity_tool(task_dists_json = args$tasks)
      }
    ),
    evm = list(
      title = "Earned Value Management",
      description = "Full EVM analysis: PV, EV, AC, variances, indices, and forecasts.",
      args = list(
        list(
          name = "bac", type = "number", required = TRUE, default = NULL,
          help = "Budget at Completion"
        ),
        list(
          name = "schedule", type = "json", required = TRUE, default = NULL,
          help = "JSON array of cumulative planned % complete, e.g. [0.2, 0.4, 0.6, 0.8, 1.0]"
        ),
        list(
          name = "period", type = "integer", required = TRUE, default = NULL,
          help = "Current time period (1-based index into schedule)"
        ),
        list(
          name = "complete", type = "number", required = TRUE, default = NULL,
          help = "Actual percent complete (0-1), e.g. 0.35"
        ),
        list(
          name = "costs", type = "json", required = TRUE, default = NULL,
          help = "JSON array of cumulative actual costs, e.g. [90000, 195000, 310000]"
        )
      ),
      examples = c(
        "/evm bac=500000 schedule=[0.2,0.4,0.6,0.8,1.0] period=3 complete=0.35 costs=[90000,195000,310000]"
      ),
      fn = function(args) {
        evm_analysis_tool(
          bac = as.numeric(args$bac),
          schedule_json = args$schedule,
          time_period = as.integer(args$period),
          actual_per_complete = as.numeric(args$complete),
          actual_costs_json = args$costs
        )
      }
    ),
    risk = list(
      title = "Bayesian Risk (Prior)",
      description = "Calculate prior risk probability from root causes using Bayes' theorem.",
      args = list(
        list(
          name = "causes", type = "json", required = TRUE, default = NULL,
          help = "JSON array of cause probabilities, e.g. [0.3, 0.2]"
        ),
        list(
          name = "given", type = "json", required = TRUE, default = NULL,
          help = "JSON array of P(Risk | Cause), e.g. [0.8, 0.6]"
        ),
        list(
          name = "not_given", type = "json", required = TRUE, default = NULL,
          help = "JSON array of P(Risk | not Cause), e.g. [0.2, 0.4]"
        )
      ),
      examples = c(
        "/risk causes=[0.3,0.2] given=[0.8,0.6] not_given=[0.2,0.4]"
      ),
      fn = function(args) {
        risk_prob_tool(
          cause_probs_json = args$causes,
          risks_given_causes_json = args$given,
          risks_given_not_causes_json = args$not_given
        )
      }
    ),
    risk_post = list(
      title = "Bayesian Risk (Posterior)",
      description = "Update risk probability after observing which causes occurred.",
      args = list(
        list(
          name = "causes", type = "json", required = TRUE, default = NULL,
          help = "JSON array of cause probabilities"
        ),
        list(
          name = "given", type = "json", required = TRUE, default = NULL,
          help = "JSON array of P(Risk | Cause)"
        ),
        list(
          name = "not_given", type = "json", required = TRUE, default = NULL,
          help = "JSON array of P(Risk | not Cause)"
        ),
        list(
          name = "observed", type = "json", required = TRUE, default = NULL,
          help = "JSON array of observations: 1 = occurred, 0 = did not, null = unknown. e.g. [1, null]"
        )
      ),
      examples = c(
        "/risk_post causes=[0.3,0.2] given=[0.8,0.6] not_given=[0.2,0.4] observed=[1,null]"
      ),
      fn = function(args) {
        risk_post_prob_tool(
          cause_probs_json = args$causes,
          risks_given_causes_json = args$given,
          risks_given_not_causes_json = args$not_given,
          observed_causes_json = args$observed
        )
      }
    ),
    learning = list(
      title = "Learning Curve Fit",
      description = "Fit a sigmoidal learning curve (logistic, Gompertz, or Pearl) and predict.",
      args = list(
        list(
          name = "x", type = "json", required = TRUE, default = NULL,
          help = "JSON array of x values (e.g. time periods)"
        ),
        list(
          name = "y", type = "json", required = TRUE, default = NULL,
          help = "JSON array of y values (e.g. cumulative output)"
        ),
        list(
          name = "model", type = "string", required = TRUE, default = NULL,
          help = "Model type: 'logistic', 'gompertz', or 'pearl'"
        ),
        list(
          name = "predict", type = "json", required = FALSE, default = NULL,
          help = "Optional JSON array of x values to predict at"
        )
      ),
      examples = c(
        "/learning x=[1,2,3,4,5] y=[5,15,40,70,90] model=logistic",
        "/learning x=[1,2,3,4,5] y=[5,15,40,70,90] model=gompertz predict=[6,7,8]"
      ),
      fn = function(args) {
        fit_and_predict_sigmoidal_tool(
          x_json = args$x, y_json = args$y,
          model_type = args$model, predict_x_json = args$predict
        )
      }
    ),
    dsm = list(
      title = "Design Structure Matrix",
      description = "Compute parent DSM (resource-task dependencies) from an input matrix.",
      args = list(
        list(
          name = "matrix", type = "json", required = TRUE, default = NULL,
          help = "JSON 2D array, e.g. [[1,0],[1,1]]"
        )
      ),
      examples = c(
        "/dsm matrix=[[1,1,0],[0,1,1],[1,0,1]]"
      ),
      fn = function(args) {
        parent_dsm_tool(matrix_json = args$matrix)
      }
    )
  )
}

#' Format Help for a Single Command
#'
#' @param cmd_name Character command name (without /).
#' @param cmd Command definition from the registry.
#' @return A markdown-formatted help string.
#' @keywords internal
format_command_help <- function(cmd_name, cmd) {
  # Arguments
  arg_lines <- vapply(cmd$args, function(a) {
    req_tag <- if (a$required) {
      " *(required)*"
    } else {
      paste0(" *(default: ", deparse(a$default), ")*")
    }
    paste0("- **", a$name, "**", req_tag, " \u2014 ", a$help)
  }, character(1))

  # Examples
  ex_lines <- paste0("  ", cmd$examples)

  paste0(
    "### /", cmd_name, " \u2014 ", cmd$title, "\n\n",
    cmd$description, "\n\n",
    "**Arguments:**\n", paste(arg_lines, collapse = "\n"), "\n\n",
    "**Examples:**\n", paste(ex_lines, collapse = "\n")
  )
}

#' Format Help Overview of All Commands
#'
#' @return A markdown-formatted overview of all available /commands.
#' @keywords internal
format_help_overview <- function() {
  registry <- pra_command_registry()
  lines <- vapply(names(registry), function(nm) {
    cmd <- registry[[nm]]
    paste0("- **/", nm, "** \u2014 ", cmd$title)
  }, character(1))

  paste0(
    "## PRA Commands\n\n",
    "Type a command to run an analysis directly. ",
    "Type `/help <command>` for detailed usage.\n\n",
    paste(lines, collapse = "\n"), "\n\n",
    "Type `/help` for this overview."
  )
}

#' Route User Input to the Appropriate Handler
#'
#' Classifies user input into one of three modes and returns a structured
#' routing decision. This function encapsulates the three-mode routing
#' architecture used by both the Shiny app and programmatic interfaces.
#'
#' @param input Character string of user input.
#' @return A list with:
#'   \describe{
#'     \item{mode}{Character: `"command"`, `"tool"`, or `"rag"`}
#'     \item{reason}{Character: brief explanation of the routing decision}
#'   }
#'   For `"command"` mode, also includes `command` (the command name).
#'
#' @details
#' Routing logic:
#' \enumerate{
#'   \item Input starting with `/` is routed to the **command** handler
#'         for deterministic execution (no LLM involved).
#'   \item Input containing numerical data patterns (distributions, arrays,
#'         dollar amounts, percentages with surrounding context) is routed
#'         to the **tool** handler where the LLM selects and calls tools.
#'   \item All other input (conceptual questions, explanations) is routed
#'         to the **rag** handler where the LLM answers from the knowledge
#'         base context.
#' }
#'
#' Note: modes `"tool"` and `"rag"` both go through the LLM, but the
#' distinction affects how the query is framed (tool-calling emphasis vs.
#' RAG-context emphasis). The LLM ultimately decides whether to call a
#' tool, but the routing hint improves reliability with smaller models.
#'
#' @examples
#' \dontrun{
#' route_input("/mcs tasks=[...]")
#' # list(mode = "command", reason = "...", command = "mcs")
#'
#' route_input("Simulate 3 tasks: Normal(10,2)...")
#' # list(mode = "tool", reason = "...")
#'
#' route_input("What is earned value?")
#' # list(mode = "rag", reason = "...")
#' }
#'
#' @keywords internal
route_input <- function(input) {
  input <- trimws(input)

  # Mode 1: Slash commands — deterministic
  if (grepl("^/", input)) {
    cmd_name <- sub("^/(\\S+).*", "\\1", input)
    return(list(
      mode = "command",
      reason = paste0("Input starts with /", cmd_name),
      command = tolower(cmd_name)
    ))
  }

  # Heuristic patterns for numerical/computational data
  numerical_patterns <- c(
    "\\b(normal|triangular|uniform)\\s*\\(",          # distribution specs
    "\\[\\s*[\\d.,\\s]+\\]",                          # numeric arrays [1, 2, 3]
    "\\$[\\d,]+",                                      # dollar amounts $500,000
    "\\bBAC\\b.*\\d",                                  # BAC with numbers
    "\\bP\\([A-Z]",                                    # P(C1), P(Risk|...)
    "\\bmean\\s*[=:]\\s*\\d",                          # mean=10 or mean: 10
    "\\bsd\\s*[=:]\\s*\\d",                            # sd=2
    "\\bvariance[s]?\\s*\\[",                          # variances [...]
    "\\bschedule\\s*\\[",                              # schedule [...]
    "\\bcosts?\\s*\\[",                                # costs [...]
    "\\bmatrix\\s*\\[",                                # matrix [...]
    "\\d+\\s*(simulations|iterations)"                 # 10000 simulations
  )

  for (pat in numerical_patterns) {
    if (grepl(pat, input, ignore.case = TRUE, perl = TRUE)) {
      return(list(
        mode = "tool",
        reason = paste0("Numerical data detected (pattern: ", pat, ")")
      ))
    }
  }

  # Mode 3: RAG — conceptual/explanatory
  list(
    mode = "rag",
    reason = "No numerical data or /command detected; routing to RAG"
  )
}

#' Parse key=value argument string with bracket-aware splitting
#'
#' Splits a string like `n=5000 tasks=[{"type": "normal", "mean": 10}]` into
#' a named list `list(n = "5000", tasks = '[{"type": "normal", "mean": 10}]')`.
#' Tracks bracket/brace/quote nesting so that spaces inside `[]`, `{}`, or `""`
#' are not treated as argument separators.
#'
#' @param arg_string Character string of arguments (without the command name).
#' @param known_args Character vector of recognized argument names.
#' @return A named list of parsed argument values (all character strings).
#' @keywords internal
parse_command_args <- function(arg_string, known_args) {
  if (is.null(arg_string) || nchar(trimws(arg_string)) == 0) return(list())

  chars <- strsplit(arg_string, "")[[1]]
  n <- length(chars)

  # State machine: find top-level `key=value` boundaries
  # A key= boundary is a known arg name followed by = at bracket depth 0, outside quotes
  depth_sq <- 0L  # [ ] nesting
  depth_cb <- 0L  # { } nesting
  in_quote <- FALSE

  # First pass: mark each character position with its nesting depth and quote state
  top_level <- logical(n)  # TRUE if character is at depth 0 and not in quotes
  for (i in seq_len(n)) {
    ch <- chars[i]
    if (in_quote) {
      if (ch == '"' && (i == 1L || chars[i - 1L] != "\\")) in_quote <- FALSE
    } else {
      if (ch == '"') {
        in_quote <- TRUE
      } else if (ch == '[') {
        depth_sq <- depth_sq + 1L
      } else if (ch == ']') {
        depth_sq <- max(0L, depth_sq - 1L)
      } else if (ch == '{') {
        depth_cb <- depth_cb + 1L
      } else if (ch == '}') {
        depth_cb <- max(0L, depth_cb - 1L)
      }
    }
    top_level[i] <- !in_quote && depth_sq == 0L && depth_cb == 0L
  }

  # Second pass: find `key=` patterns at top level
  # Sort known_args by length descending to match longest first
  known_args <- known_args[order(nchar(known_args), decreasing = TRUE)]
  boundaries <- list()  # list of list(name, value_start)

  for (nm in known_args) {
    nm_len <- nchar(nm)
    # Search for `nm=` at top level, preceded by start-of-string or whitespace
    for (pos in seq_len(n - nm_len)) {
      if (!top_level[pos]) next
      candidate <- paste0(chars[pos:(pos + nm_len - 1L)], collapse = "")
      if (candidate != nm) next
      eq_pos <- pos + nm_len
      if (eq_pos > n || chars[eq_pos] != "=") next
      # Check preceding character: must be start of string or whitespace
      if (pos > 1 && !grepl("\\s", chars[pos - 1L])) next
      boundaries[[length(boundaries) + 1L]] <- list(
        name = nm, key_start = pos, value_start = eq_pos + 1L
      )
    }
  }

  if (length(boundaries) == 0) return(list())

  # Sort boundaries by position
  starts <- vapply(boundaries, function(b) b$key_start, integer(1))
  boundaries <- boundaries[order(starts)]

  # Extract values: from value_start to just before the next boundary's key_start (or end)
  parsed <- list()
  for (i in seq_along(boundaries)) {
    b <- boundaries[[i]]
    val_start <- b$value_start
    if (i < length(boundaries)) {
      # Value ends just before the whitespace preceding the next key
      val_end <- boundaries[[i + 1L]]$key_start - 1L
      # Trim trailing whitespace
      while (val_end >= val_start && grepl("\\s", chars[val_end])) val_end <- val_end - 1L
    } else {
      val_end <- n
    }
    if (val_start <= val_end) {
      parsed[[b$name]] <- paste0(chars[val_start:val_end], collapse = "")
    } else {
      parsed[[b$name]] <- ""
    }
  }

  parsed
}

#' Parse and Execute a /command
#'
#' Parses user input starting with `/`, validates arguments, and executes
#' the corresponding tool function. Returns a list with `ok` (logical) and
#' `result` (character string — either the tool output or a help/error message).
#'
#' @param input Character string of user input (e.g. "/mcs n=5000 tasks=[...]").
#' @return A list with `ok` and `result`.
#' @keywords internal
execute_command <- function(input) {
  input <- trimws(input)
  registry <- pra_command_registry()

  # Split command name from the rest (everything after first whitespace)
  first_space <- regexpr("\\s", input)
  if (first_space > 0) {
    cmd_token <- substring(input, 1, first_space - 1)
    arg_string <- trimws(substring(input, first_space + 1))
  } else {
    cmd_token <- input
    arg_string <- ""
  }
  cmd_name <- sub("^/", "", tolower(cmd_token))

  # /help or /help <command>

  if (cmd_name == "help") {
    if (nchar(arg_string) > 0) {
      help_target <- sub("^/", "", tolower(trimws(arg_string)))
      if (help_target %in% names(registry)) {
        return(list(ok = TRUE, result = format_command_help(help_target, registry[[help_target]])))
      }
      return(list(ok = FALSE, result = paste0(
        "Unknown command: **", help_target, "**\n\n", format_help_overview()
      )))
    }
    return(list(ok = TRUE, result = format_help_overview()))
  }

  # Unknown command
  if (!cmd_name %in% names(registry)) {
    return(list(ok = FALSE, result = paste0(
      "Unknown command: **/", cmd_name, "**\n\n",
      "Did you mean one of these?\n",
      paste0("- /", names(registry), collapse = "\n"),
      "\n\nType `/help` for a list of all commands."
    )))
  }

  cmd <- registry[[cmd_name]]

  # No args — if command has required args, show help; otherwise execute with defaults
  has_required <- any(vapply(cmd$args, function(a) a$required, logical(1)))
  if (nchar(arg_string) == 0 && has_required) {
    return(list(ok = TRUE, result = format_command_help(cmd_name, cmd)))
  }

  # Parse key=value pairs with bracket-aware splitting.
  # Handles JSON values containing spaces, e.g. tasks=[{"type": "normal", "mean": 10}]
  parsed <- parse_command_args(arg_string, vapply(cmd$args, function(a) a$name, character(1)))

  # Check required arguments
  missing_args <- character(0)
  for (a in cmd$args) {
    if (a$required && is.null(parsed[[a$name]])) {
      missing_args <- c(missing_args, a$name)
    }
  }
  if (length(missing_args) > 0) {
    return(list(ok = FALSE, result = paste0(
      "**Missing required argument(s):** ", paste(missing_args, collapse = ", "), "\n\n",
      format_command_help(cmd_name, cmd)
    )))
  }

  # Execute
  tryCatch(
    {
      result <- cmd$fn(parsed)
      # Extract text from ContentToolResult if needed
      is_ctr <- any(grepl("ContentToolResult", class(result), fixed = TRUE))
      result_text <- if (is_ctr) {
        result@value
      } else if (is.character(result)) {
        result
      } else {
        as.character(result)
      }
      list(
        ok = TRUE, result = result_text,
        rich_result = if (is_ctr) result else NULL
      )
    },
    error = function(e) {
      list(ok = FALSE, result = paste0(
        "**Error running /", cmd_name, ":** ", e$message, "\n\n",
        "Check your inputs and try again. Type `/help ", cmd_name, "` for usage."
      ))
    }
  )
}
