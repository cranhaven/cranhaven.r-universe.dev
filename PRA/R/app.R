#' Launch the PRA Risk Analysis Agent App
#'
#' Starts a Shiny app that provides an interactive chat interface to the PRA
#' risk analysis agent. The agent can select and execute PRA tools (Monte Carlo
#' simulation, EVM, Bayesian inference, etc.) in response to natural language
#' questions. Uses shinychat for a polished streaming chat experience with
#' inline tool result display.
#'
#' Requires Ollama to be running locally with the specified model downloaded.
#'
#' @param model Character. Ollama model name (default `"llama3.2"`).
#' @param rag Logical. Whether to enable RAG context retrieval (default `TRUE`).
#' @param embed_model Character. Ollama embedding model for RAG (default
#'   `"nomic-embed-text"`).
#' @param port Integer. Port for the Shiny app (default `NULL` lets Shiny choose).
#' @param launch.browser Logical. Whether to open a browser (default `TRUE`).
#' @return None. This function is called to launch the shiny app.
#'
#' @examples
#' \dontrun{
#' # Ensure Ollama is running, then:
#' pra_app()
#'
#' # With a specific model:
#' pra_app(model = "qwen2.5")
#' }
#'
#' @export
pra_app <- function(model = "llama3.2", rag = TRUE, embed_model = "nomic-embed-text",
                    port = NULL, launch.browser = TRUE) {
  check_package("shiny")
  check_package("bslib")
  check_package("ellmer")
  check_package("shinychat")

  # Check Ollama connectivity with a lightweight HTTP ping
  ollama_ok <- tryCatch(
    {
      con <- url("http://localhost:11434/api/tags", open = "rb")
      on.exit(close(con))
      TRUE
    },
    error = function(e) FALSE
  )

  if (!ollama_ok) {
    stop(
      "Cannot connect to Ollama. Please ensure:\n",
      "1. Ollama is installed (https://ollama.com)\n",
      "2. Ollama is running (run 'ollama serve' in a terminal)\n",
      "3. The model '", model, "' is downloaded (run 'ollama pull ", model, "')"
    )
  }

  app <- pra_shiny_app(model = model, rag = rag, embed_model = embed_model)
  shiny::runApp(app, port = port, launch.browser = launch.browser)
}

#' Fetch Available Ollama Models
#'
#' Queries the local Ollama API for available models. Falls back to a default
#' list if the API is unreachable.
#'
#' @return A character vector of model names.
#' @keywords internal
get_ollama_models <- function() {
  # 1. Try the Ollama HTTP API
  models <- tryCatch({
    con <- url("http://localhost:11434/api/tags", open = "rb")
    on.exit(close(con))
    raw <- readLines(con, warn = FALSE)
    parsed <- jsonlite::fromJSON(paste(raw, collapse = ""))
    m <- sort(parsed$models$name)
    if (length(m) > 0) m else NULL
  }, error = function(e) NULL)

  if (!is.null(models)) return(models)

  # 2. Fall back to the ollama CLI (more reliable on some systems)
  models <- tryCatch({
    out <- system2("ollama", "list", stdout = TRUE, stderr = FALSE)
    if (length(out) > 1) {
      # Skip the header row; first whitespace-delimited token is the model name
      lines <- out[-1]
      lines <- lines[nchar(trimws(lines)) > 0]
      m <- trimws(sub("\\s+.*", "", lines))
      m <- sort(m[nchar(m) > 0])
      if (length(m) > 0) m else NULL
    } else NULL
  }, error = function(e) NULL)

  if (!is.null(models)) return(models)

  # 3. Hard-coded fallback
  c("llama3.2", "llama3.1", "qwen2.5")
}

#' Create the PRA Shiny App Object
#'
#' @param model Ollama model name.
#' @param rag Whether to enable RAG.
#' @param embed_model Ollama embedding model.
#' @return A Shiny app object.
#' @keywords internal
pra_shiny_app <- function(model = "llama3.2", rag = TRUE, embed_model = "nomic-embed-text") {
  # Fetch models at startup
  available_models <- get_ollama_models()
  if (!model %in% available_models) {
    available_models <- c(model, available_models)
  }

  # Example prompts users can click — use /commands for deterministic execution
  example_prompts <- list(
    list(
      icon = "dice",
      label = "Monte Carlo simulation",
      prompt = '/mcs tasks=[{"type":"normal","mean":10,"sd":2},{"type":"triangular","a":5,"b":10,"c":15},{"type":"uniform","min":8,"max":12}]'
    ),
    list(
      icon = "chart-line",
      label = "Earned Value Management",
      prompt = "/evm bac=500000 schedule=[0.2,0.4,0.6,0.8,1.0] period=3 complete=0.35 costs=[90000,195000,310000]"
    ),
    list(
      icon = "diagram-project",
      label = "Bayesian risk probability",
      prompt = "/risk causes=[0.3,0.2] given=[0.8,0.6] not_given=[0.2,0.4]"
    ),
    list(
      icon = "magnifying-glass-chart",
      label = "Sensitivity analysis",
      prompt = '/sensitivity tasks=[{"type":"normal","mean":10,"sd":2},{"type":"triangular","a":5,"b":10,"c":15},{"type":"uniform","min":8,"max":12}]'
    )
  )

  # Build clickable prompt chip HTML with data attributes (onclick gets stripped by Shiny)
  prompt_chips_html <- paste(vapply(seq_along(example_prompts), function(i) {
    ex <- example_prompts[[i]]
    # Encode prompt as base64 to avoid quote escaping issues
    prompt_b64 <- base64enc::base64encode(charToRaw(ex$prompt))
    paste0(
      '<button class="example-chip" data-prompt="', prompt_b64, '">',
      '<i class="fa-solid fa-', ex$icon, '"></i> ', ex$label, "</button>"
    )
  }, character(1)), collapse = "\n")

  # JS to attach click handlers via event delegation (survives Shiny sanitizer)
  chip_click_js <- shiny::tags$script(shiny::HTML("
    document.addEventListener('click', function(e) {
      var chip = e.target.closest('.example-chip');
      if (chip && chip.dataset.prompt) {
        var prompt = atob(chip.dataset.prompt);
        Shiny.setInputValue('example_click', {prompt: prompt, ts: Date.now()}, {priority: 'event'});
      }
    });
  "))

  ui <- bslib::page_fillable(
    title = "PRA - Risk Analysis Agent",
    theme = bslib::bs_theme(
      version = 5, bootswatch = "flatly",
      primary = "#2c3e50",
      success = "#18bc9c"
    ),
    shiny::tags$head(
      shiny::tags$link(
        rel = "stylesheet",
        href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.5.0/css/all.min.css"
      ),
      chip_click_js,
      shiny::tags$style(shiny::HTML("
        body { background: #f5f6fa; }
        .data-preview { max-height: 180px; overflow-y: auto; font-size: 0.82em; }

        .sidebar-header {
          text-align: center;
          padding-bottom: 10px; border-bottom: 1px solid #dee2e6; margin-bottom: 10px;
        }
        .sidebar-section { margin-bottom: 12px; }
        .sidebar-section-label {
          font-size: 0.7em; font-weight: 600; text-transform: uppercase;
          letter-spacing: 0.05em; color: #95a5a6; margin-bottom: 4px;
        }
        .sidebar .form-group { margin-bottom: 6px; }
        .status-badge {
          display: inline-block; font-size: 0.75em; padding: 2px 8px;
          border-radius: 10px; font-weight: 600;
        }
        .status-connected { background: #d5f5e3; color: #1e8449; }
        .status-error { background: #fadbd8; color: #c0392b; }

        .example-chip {
          display: flex; align-items: center; gap: 8px;
          width: 100%; padding: 10px 14px;
          background: white; border: 1px solid #dee2e6; border-radius: 10px;
          font-size: 0.88em; color: #2c3e50; text-align: left;
          cursor: pointer; transition: all 0.15s ease;
        }
        .example-chip:hover {
          border-color: #18bc9c; background: #f0faf7; color: #18bc9c;
        }
        .example-chip i { font-size: 1em; width: 18px; text-align: center; color: #95a5a6; }
        .example-chip:hover i { color: #18bc9c; }
        .example-chips { display: flex; flex-direction: column; gap: 8px; margin-top: 8px; }
      "))
    ),
    bslib::layout_sidebar(
      fillable = TRUE,
      sidebar = bslib::sidebar(
        id = "sidebar",
        open = "desktop",
        width = 280,
        padding = c(12, 14),

        # Header — compact logo + status
        shiny::div(
          class = "sidebar-header",
          shiny::tags$img(
            src = "https://raw.githubusercontent.com/paulgovan/PRA/main/inst/logo.png",
            height = "70px",
            style = "display: block; margin: 0 auto;",
            onerror = "this.style.display='none'"
          ),
          shiny::uiOutput("status_badge")
        ),

        # Model
        shiny::div(
          class = "sidebar-section",
          shiny::div(
            class = "sidebar-section-label",
            style = "display: flex; justify-content: space-between; align-items: center;",
            "Model",
            shiny::actionLink("refresh_models", shiny::icon("arrows-rotate"),
              style = "font-size: 0.85em; color: #95a5a6;"
            )
          ),
          shiny::selectizeInput(
            "model", NULL,
            choices = available_models,
            selected = model,
            width = "100%",
            options = list(create = TRUE, placeholder = "Select or type model...")
          )
        ),

        # Options
        shiny::div(
          class = "sidebar-section",
          shiny::div(class = "sidebar-section-label", "Options"),
          shiny::checkboxInput("rag_enabled", "RAG Context", value = rag)
        ),

        # Actions
        shiny::div(
          class = "sidebar-section",
          shiny::div(class = "sidebar-section-label", "Actions"),
          shiny::div(
            style = "display: flex; gap: 6px;",
            shiny::actionButton("new_chat_btn", "New Chat",
              class = "btn-outline-secondary btn-sm flex-fill",
              icon = shiny::icon("rotate-right")
            ),
            shiny::downloadButton("download_chat", "Export",
              class = "btn-outline-secondary btn-sm flex-fill",
              icon = shiny::icon("download")
            )
          )
        ),

        # Data upload
        shiny::div(
          class = "sidebar-section",
          shiny::div(class = "sidebar-section-label", "Data"),
          shiny::fileInput("data_file", NULL,
            accept = c(".csv", ".tsv", ".txt"),
            buttonLabel = "Browse",
            placeholder = "Upload CSV..."
          ),
          shiny::uiOutput("data_preview_ui")
        ),

        # Footer
        shiny::div(
          style = "margin-top: auto; padding-top: 8px; border-top: 1px solid #dee2e6;",
          shiny::p(
            class = "text-muted",
            style = "font-size: 0.7em; margin: 0;",
            "Powered by ellmer + shinychat + Ollama"
          )
        )
      ),

      # Main content: chat
      shinychat::chat_ui("chat",
        placeholder = "Ask a risk analysis question...",
        fill = TRUE
      )
    )
  )

  server <- function(input, output, session) {
    # Reactive values
    chat_obj <- shiny::reactiveVal(NULL)
    is_connected <- shiny::reactiveVal(FALSE)

    # Status badge
    output$status_badge <- shiny::renderUI({
      if (is_connected()) {
        shiny::span(class = "status-badge status-connected", "\u2713 Connected")
      } else {
        shiny::span(class = "status-badge status-error", "\u2717 Disconnected")
      }
    })

    # Welcome message with clickable example chips in a single message
    send_welcome <- function(cur_model, cur_rag) {
      welcome_html <- paste0(
        "<p><strong>Welcome to PRA!</strong> Connected to <strong>", cur_model, "</strong>",
        if (cur_rag) " with RAG context" else "", ".</p>",
        "<p>Use <strong>/commands</strong> for instant results or ask questions in natural language. ",
        "Type <code>/help</code> for all commands.</p>",
        '<div class="example-chips">', prompt_chips_html, "</div>"
      )
      shinychat::chat_append("chat", shiny::HTML(welcome_html), role = "assistant")
    }

    # Initialize chat on startup and when model/RAG changes
    init_chat <- function() {
      cur_model <- input$model
      cur_rag <- input$rag_enabled
      shiny::req(cur_model)

      tryCatch(
        {
          chat <- pra_chat(
            model = cur_model,
            rag = cur_rag,
            embed_model = embed_model
          )
          chat_obj(chat)
          is_connected(TRUE)
          send_welcome(cur_model, cur_rag)
        },
        error = function(e) {
          is_connected(FALSE)
          shinychat::chat_append("chat",
            paste0(
              "**Connection error:** ", e$message,
              "\n\nMake sure Ollama is running (`ollama serve`) and the model is pulled (`ollama pull ",
              cur_model, "`)."
            ),
            role = "assistant"
          )
        }
      )
    }

    # Fire on startup
    shiny::observe({
      init_chat()
    }) |>
      shiny::bindEvent(TRUE, once = TRUE)

    # Reinitialize when model or RAG setting changes
    shiny::observeEvent(list(input$model, input$rag_enabled),
      {
        shinychat::chat_clear("chat")
        is_connected(FALSE)
        chat_obj(NULL)
        .pra_agent_env$user_data_context <- NULL
        .pra_agent_env$user_data <- NULL
        init_chat()
      },
      ignoreInit = TRUE
    )

    # Refresh model list
    shiny::observeEvent(input$refresh_models, {
      models <- get_ollama_models()
      cur <- input$model
      if (!is.null(cur) && nchar(cur) > 0 && !cur %in% models) {
        models <- c(cur, models)
      }
      shiny::updateSelectizeInput(session, "model", choices = models,
                                  selected = cur, server = TRUE)
    })

    # New chat button
    shiny::observeEvent(input$new_chat_btn, {
      shinychat::chat_clear("chat")
      chat_obj(NULL)
      is_connected(FALSE)
      .pra_agent_env$user_data_context <- NULL
      .pra_agent_env$user_data <- NULL
      init_chat()
    })

    # Handle example prompt clicks
    shiny::observeEvent(input$example_click, {
      prompt <- input$example_click$prompt
      shiny::req(nchar(trimws(prompt)) > 0)

      # Show the prompt as a user message
      shinychat::chat_append("chat", prompt, role = "user")

      # Route input
      routing <- route_input(prompt)
      if (routing$mode == "command") {
        result <- execute_command(prompt)
        if (!is.null(result$rich_result) && any(grepl("ContentToolResult", class(result$rich_result), fixed = TRUE))) {
          display <- result$rich_result@extra$display
          if (!is.null(display) && !is.null(display$html)) {
            shinychat::chat_append("chat", shiny::HTML(display$html), role = "assistant")
            return()
          }
        }
        shinychat::chat_append("chat", result$result, role = "assistant")
        return()
      }

      shiny::req(chat_obj())

      if (routing$mode == "rag") {
        handle_rag_query(prompt)
        return()
      }

      # Tool mode: LLM with tools, no RAG context
      query <- build_query(prompt, rag_enabled = FALSE)
      stream <- chat_obj()$stream_async(query, stream = "content")
      shinychat::chat_append("chat", stream)
    })

    # Handle /commands — deterministic tool execution without LLM
    handle_slash_command <- function(user_msg) {
      result <- execute_command(user_msg)
      # If we got a rich ContentToolResult, display the HTML version
      if (!is.null(result$rich_result) && any(grepl("ContentToolResult", class(result$rich_result), fixed = TRUE))) {
        display <- result$rich_result@extra$display
        if (!is.null(display) && !is.null(display$html)) {
          shinychat::chat_append("chat", shiny::HTML(display$html), role = "assistant")
          return()
        }
      }
      # Otherwise display the text/markdown result
      shinychat::chat_append("chat", result$result, role = "assistant")
    }

    # Handle RAG queries — conceptual questions answered from knowledge base
    # Uses a separate chat WITHOUT tools to prevent the LLM from calling
    # analysis functions on questions like "what is earned value?"
    handle_rag_query <- function(user_msg) {
      rag_enabled <- shiny::isolate(input$rag_enabled)

      # Retrieve RAG context
      context_chunks <- character(0)
      sources <- character(0)
      if (rag_enabled && !is.null(.pra_agent_env$rag_store)) {
        tryCatch({
          context_chunks <- retrieve_context(.pra_agent_env$rag_store, user_msg, top_k = 3)
          if (length(context_chunks) > 0) {
            sources <- unique(gsub(
              ".*\\[Source: (.+?)\\]$", "\\1",
              context_chunks[grepl("\\[Source:", context_chunks)]
            ))
          }
        }, error = function(e) NULL)
      }

      if (length(context_chunks) > 0) {
        # Build query with RAG context for a tool-free chat
        context_text <- paste(context_chunks, collapse = "\n\n---\n\n")
        source_label <- paste(unique(sources), collapse = ", ")
        query <- paste0(
          "Use the following reference material to answer the question. ",
          "Be concise and cite sources at the end.\n\n",
          "Reference material:\n\n", context_text,
          "\n\n---\n\nQuestion: ", user_msg
        )

        # Create a lightweight chat WITHOUT tools for RAG answers
        rag_chat <- tryCatch({
          ellmer::chat_ollama(
            model = shiny::isolate(input$model),
            system_prompt = paste0(
              "You are a Project Risk Analysis expert. ",
              "Answer the user's question using ONLY the reference material provided. ",
              "Do NOT attempt to run any computations or call any tools. ",
              "Be concise. At the end, cite sources like: Sources: filename.md"
            ),
            api_args = list(options = list(num_ctx = 16384L))
          )
        }, error = function(e) NULL)

        if (!is.null(rag_chat)) {
          stream <- rag_chat$stream_async(query, stream = "content")
          shinychat::chat_append("chat", stream)
        } else {
          # Fallback: display raw RAG context
          shinychat::chat_append("chat", paste0(
            paste(context_chunks, collapse = "\n\n---\n\n"),
            "\n\n*Sources: ", paste(unique(sources), collapse = ", "), "*"
          ), role = "assistant")
        }
      } else {
        # No RAG context available — fall back to LLM with tools
        query <- build_query(user_msg, rag_enabled = FALSE)
        stream <- chat_obj()$stream_async(query, stream = "content")
        shinychat::chat_append("chat", stream)
      }
    }

    # Handle user messages — three-mode routing via route_input()
    shiny::observeEvent(input$chat_user_input, {
      user_msg <- input$chat_user_input
      shiny::req(nchar(trimws(user_msg)) > 0)

      routing <- route_input(user_msg)
      message("[PRA routing] mode=", routing$mode, " | ", routing$reason)

      if (routing$mode == "command") {
        handle_slash_command(user_msg)
        return()
      }

      shiny::req(chat_obj())

      if (routing$mode == "rag") {
        # RAG mode: answer conceptual questions using a dedicated chat without
        # tools, so the LLM cannot accidentally call analysis functions.
        handle_rag_query(user_msg)
        return()
      }

      # Tool mode: LLM has full tool access for numerical computation
      query <- build_query(user_msg, rag_enabled = FALSE)

      # Stream response — shinychat handles progressive display + tool cards
      stream <- chat_obj()$stream_async(query, stream = "content")
      shinychat::chat_append("chat", stream)
    })

    # Build enriched query with optional RAG context and uploaded data
    build_query <- function(user_msg, rag_enabled) {
      query <- user_msg

      # Include uploaded data context
      if (!is.null(.pra_agent_env$user_data_context)) {
        query <- paste0(
          "Available data:\n", .pra_agent_env$user_data_context,
          "\n\n---\n\n", query
        )
      }

      # Optionally prepend RAG context with source attribution
      if (rag_enabled && !is.null(.pra_agent_env$rag_store)) {
        tryCatch(
          {
            context_chunks <- retrieve_context(.pra_agent_env$rag_store, user_msg, top_k = 3)
            if (length(context_chunks) > 0) {
              # Extract source filenames for citation reminder
              sources <- unique(gsub(
                ".*\\[Source: (.+?)\\]$", "\\1",
                context_chunks[grepl("\\[Source:", context_chunks)]
              ))
              context_text <- paste(context_chunks, collapse = "\n\n---\n\n")
              source_note <- if (length(sources) > 0) {
                paste0(
                  "\n\nIMPORTANT: Use this knowledge base context to answer the question. ",
                  "Cite these sources: ", paste(sources, collapse = ", "), ". ",
                  "Only call a tool if the user provides specific numerical data to compute."
                )
              } else {
                ""
              }
              query <- paste0(
                "Reference material from PRA knowledge base:\n\n",
                context_text, source_note, "\n\n---\n\nUser question: ", query
              )
            }
          },
          error = function(e) NULL
        )
      }

      query
    }

    # Handle file upload
    shiny::observeEvent(input$data_file, {
      shiny::req(input$data_file)
      tryCatch(
        {
          df <- utils::read.csv(input$data_file$datapath)

          col_info <- paste(names(df), collapse = ", ")
          preview_lines <- paste(utils::capture.output(
            print(utils::head(df, 5), row.names = FALSE)
          ), collapse = "\n")
          data_ctx <- paste0(
            "User uploaded file: ", input$data_file$name, "\n",
            "Columns: ", col_info, "\n",
            "Rows: ", nrow(df), "\n\n",
            "Preview:\n", preview_lines
          )
          .pra_agent_env$user_data_context <- data_ctx
          .pra_agent_env$user_data <- df

          shinychat::chat_append("chat", paste0(
            "Uploaded **", input$data_file$name, "** (",
            nrow(df), " rows, ", ncol(df), " columns: ", col_info, ")."
          ), role = "assistant")
        },
        error = function(e) {
          shinychat::chat_append("chat",
            paste0("**File error:** ", e$message),
            role = "assistant"
          )
        }
      )
    })

    # Data preview (XSS-safe)
    output$data_preview_ui <- shiny::renderUI({
      shiny::req(input$data_file)
      df <- .pra_agent_env$user_data
      if (is.null(df)) {
        return(NULL)
      }
      preview <- utils::head(df, 5)
      esc <- function(x) htmltools::htmlEscape(as.character(x))
      header <- paste0("<th>", vapply(names(preview), esc, character(1)), "</th>", collapse = "")
      rows <- apply(preview, 1, function(r) {
        paste0("<tr>", paste0("<td>", vapply(r, esc, character(1)), "</td>", collapse = ""), "</tr>")
      })
      html_table <- paste0(
        "<div class='data-preview'><table class='table table-sm table-striped'>",
        "<thead><tr>", header, "</tr></thead><tbody>",
        paste(rows, collapse = ""), "</tbody></table></div>"
      )
      shiny::HTML(html_table)
    })

    # Chat export as markdown
    output$download_chat <- shiny::downloadHandler(
      filename = function() {
        paste0("pra-chat-", format(Sys.time(), "%Y%m%d-%H%M%S"), ".md")
      },
      content = function(file) {
        chat <- chat_obj()
        header <- paste0(
          "# PRA Chat Log\n\n",
          "**Date:** ", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n\n---\n\n"
        )
        if (is.null(chat)) {
          writeLines(paste0(header, "*No conversation to export.*"), file)
        } else {
          turns <- tryCatch(chat$get_turns(), error = function(e) list())
          lines <- vapply(turns, function(turn) {
            role <- tryCatch(turn@role, error = function(e) "unknown")
            text <- tryCatch(turn@text, error = function(e) "")
            if (role == "user") {
              paste0("**User:** ", text)
            } else if (role == "assistant") {
              paste0("**Assistant:**\n\n", text)
            } else {
              text
            }
          }, character(1))
          writeLines(paste0(header, paste(lines, collapse = "\n\n---\n\n")), file)
        }
      }
    )
  }

  shiny::shinyApp(ui = ui, server = server)
}
