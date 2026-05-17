# Default embedding model used across RAG functions
.pra_default_embed_model <- "nomic-embed-text"

#' Create a PRA Risk Analysis Chat Agent
#'
#' Creates an ellmer chat object configured as a project risk analysis expert,
#' with all PRA functions registered as tools and optional RAG context retrieval
#' from the bundled knowledge base.
#'
#' By default, uses a local Ollama model for fully offline, private operation.
#' Alternatively, supply a pre-configured ellmer chat object (e.g.,
#' `ellmer::chat_openai()`) via the `chat` parameter for cloud-hosted models.
#'
#' @param chat An optional pre-configured ellmer chat object. If provided,
#'   `model` is ignored and tools are registered on this object instead.
#' @param model Character. Ollama model name (default `"llama3.2"`). Must support
#'   tool calling. Other options: `"qwen2.5"`, `"llama3.1:70b"`.
#' @param rag Logical. Whether to use RAG context from the PRA knowledge base
#'   (default `TRUE`). Requires Ollama embedding model to be available.
#' @param embed_model Character. Ollama embedding model for RAG (default
#'   `"nomic-embed-text"`). Only used when `rag = TRUE`.
#'
#' @return A configured ellmer chat object with PRA tools registered. Use
#'   `chat$chat("your question")` to interact.
#'
#' @examples
#' \dontrun{
#' # Default: local Ollama model
#' chat <- pra_chat()
#' chat$chat("Run a Monte Carlo simulation for a 3-task project with
#'   Task A: normal(10, 2), Task B: triangular(5, 10, 15), Task C: uniform(8, 12)")
#'
#' # Use a cloud model for better accuracy
#' chat <- pra_chat(chat = ellmer::chat_openai(model = "gpt-4o"))
#'
#' # Follow-up questions use conversation context
#' chat$chat("What is the contingency reserve at 95% confidence?")
#' }
#'
#' @export
pra_chat <- function(chat = NULL, model = "llama3.2", rag = TRUE,
                     embed_model = .pra_default_embed_model) {
  if (!requireNamespace("ellmer", quietly = TRUE)) {
    stop("Package 'ellmer' is required. Install with: install.packages('ellmer')")
  }

  # Build system prompt
  system_prompt <- pra_system_prompt()

  # Create or configure chat object

  if (is.null(chat)) {
    chat <- ellmer::chat_ollama(
      model = model,
      system_prompt = system_prompt,
      api_args = list(
        options = list(num_ctx = 16384L)
      )
    )
  } else {
    # User supplied a pre-configured chat; set the system prompt
    chat$set_system_prompt(system_prompt)
  }

  # Register all PRA tools
  tools <- pra_tools()
  for (tool in tools) {
    chat$register_tool(tool)
  }

  # Optionally set up RAG
  if (rag) {
    tryCatch(
      {
        if (!requireNamespace("ragnar", quietly = TRUE)) {
          message("Package 'ragnar' not installed. Chat will proceed without RAG context.")
        } else {
          store <- build_knowledge_base(embed_model = embed_model)
          .pra_agent_env$rag_store <- store
          message("RAG knowledge base loaded. Context retrieval enabled.")
        }
      },
      error = function(e) {
        warning(
          "Could not build RAG knowledge base: ", e$message,
          "\nChat will proceed without RAG context."
        )
      }
    )
  }

  chat
}

#' Generate the PRA System Prompt
#'
#' Creates the system prompt that defines the agent's persona, methodology
#' guidance, and tool selection heuristics. Kept concise to work well with
#' smaller local models.
#'
#' @return A character string containing the system prompt.
#' @keywords internal
pra_system_prompt <- function() {
  paste0(
    "You are a Project Risk Analysis expert. ",
    "You answer questions about risk analysis methods, interpret results, ",
    "and run computations when the user provides specific numerical data.\n\n",
    "## When to use tools vs. RAG context\n",
    "- If the user provides SPECIFIC NUMERICAL DATA for computation ",
    "(distributions, costs, schedules, probabilities), call the appropriate tool.\n",
    "- If the user asks a CONCEPTUAL or EXPLANATORY question ",
    "(\"what is EVM?\", \"how does Monte Carlo work?\", \"explain CPI\"), ",
    "answer using the RAG context provided. Do NOT call tools for conceptual questions.\n",
    "- If RAG context is present, use it to inform your answer and cite the source(s) ",
    "at the end (e.g., 'Sources: mcs_methods.md, evm_standards.md').\n",
    "- Never fabricate numerical results. If you need to compute something, call a tool.\n",
    "- After tool results, interpret them briefly. Be concise.\n\n",
    "## IMPORTANT: Do not use /commands\n",
    "Slash commands like /mcs, /evm, /risk are for USERS to type directly. ",
    "You must NEVER output /commands in your responses. ",
    "When you need to compute, call the registered tool functions instead.\n\n",
    "## Tool selection (only when user provides numerical data)\n",
    "- Distributions (normal/triangular/uniform) -> mcs_tool -> contingency_tool + sensitivity_tool\n",
    "- Means + variances only -> smm_tool\n",
    "- BAC + schedule + actuals -> evm_analysis_tool\n",
    "- Root cause probabilities -> risk_prob_tool or risk_post_prob_tool\n",
    "- Cost risk simulation -> cost_pdf_tool or cost_post_pdf_tool\n",
    "- Historical x/y data -> fit_and_predict_sigmoidal_tool\n",
    "- Dependency matrix -> parent_dsm_tool or grandparent_dsm_tool\n"
  )
}
