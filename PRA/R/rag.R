#' Build the PRA Knowledge Base for RAG Retrieval
#'
#' Reads the curated risk analysis knowledge files bundled with the PRA package,
#' chunks them, generates embeddings via Ollama, and stores them in a DuckDB-backed
#' ragnar knowledge base for retrieval-augmented generation.
#'
#' The knowledge base is built once and cached to disk. Subsequent calls with the
#' same `store_path` load the existing store.
#'
#' @param store_path Path to store the DuckDB knowledge base. Defaults to a
#'   cache directory under [tools::R_user_dir()].
#' @param embed_model Ollama embedding model name (default `"nomic-embed-text"`).
#' @param overwrite Logical. If `TRUE`, rebuild the knowledge base even if a
#'   cached version exists. Default `FALSE`.
#'
#' @return A ragnar store object that can be passed to [retrieve_context()].
#'
#' @examples
#' \dontrun{
#' store <- build_knowledge_base()
#' context <- retrieve_context(store, "How do I run a Monte Carlo simulation?")
#' }
#'
#' @export
build_knowledge_base <- function(store_path = NULL,
                                 embed_model = "nomic-embed-text",
                                 overwrite = FALSE) {
  if (!requireNamespace("ragnar", quietly = TRUE)) {
    stop("Package 'ragnar' is required. Install with: install.packages('ragnar')")
  }
  if (!requireNamespace("ellmer", quietly = TRUE)) {
    stop("Package 'ellmer' is required for embeddings. Install with: install.packages('ellmer')")
  }


  # Default store path in user cache directory
  if (is.null(store_path)) {
    cache_dir <- tools::R_user_dir("PRA", "cache")
    if (!dir.exists(cache_dir)) dir.create(cache_dir, recursive = TRUE)
    store_path <- file.path(cache_dir, "pra_knowledge.duckdb")
  }

  # Return cached store if it exists, is valid, and overwrite is FALSE
  if (file.exists(store_path) && !overwrite) {
    message("Loading cached knowledge base from: ", store_path)
    store <- tryCatch({
      s <- ragnar::ragnar_store_connect(store_path)
      # Validate the store works with current duckdb version
      ragnar::ragnar_retrieve(s, "test", top_k = 1L)
      s
    }, error = function(e) {
      message("Cached knowledge base is incompatible (", conditionMessage(e),
              "). Rebuilding...")
      if (file.exists(store_path)) unlink(store_path)
      NULL
    })
    if (!is.null(store)) return(store)
  }

  # Read knowledge files
  knowledge_dir <- system.file("knowledge", package = "PRA")
  if (knowledge_dir == "") {
    stop("Knowledge directory not found. Is the PRA package installed correctly?")
  }

  md_files <- list.files(knowledge_dir, pattern = "\\.md$", full.names = TRUE)
  if (length(md_files) == 0) {
    stop("No knowledge files found in: ", knowledge_dir)
  }

  message("Building PRA knowledge base from ", length(md_files), " files...")

  # Create store with Ollama embeddings and a source column for attribution
  embed_fn <- ragnar::embed_ollama(model = embed_model)
  store <- ragnar::ragnar_store_create(
    store_path,
    embed = embed_fn,
    overwrite = overwrite,
    extra_cols = data.frame(source = character(0))
  )

  # Read, chunk, and insert each file individually (ragnar v2 requires

  # MarkdownDocumentChunks objects, not plain data frames)
  total_chunks <- 0L
  for (f in md_files) {
    text <- paste(readLines(f, warn = FALSE), collapse = "\n")
    chunks <- ragnar::markdown_chunk(text)
    chunks$source <- basename(f)
    ragnar::ragnar_store_insert(store, chunks)
    total_chunks <- total_chunks + nrow(chunks)
  }

  message("Inserted ", total_chunks, " chunks from ", length(md_files), " knowledge files.")

  # Build retrieval index (BM25 + VSS)
  ragnar::ragnar_store_build_index(store)

  message("Knowledge base built and saved to: ", store_path)
  store
}

#' Add Custom Documents to the PRA Knowledge Base
#'
#' Ingests additional markdown or text documents into an existing PRA knowledge
#' base. Use this to extend the agent's knowledge with your own project
#' documentation, standards, templates, or lessons learned.
#'
#' @param store A ragnar store object from [build_knowledge_base()].
#' @param path Character. Path to a file or directory. If a directory, all
#'   `.md` and `.txt` files are ingested recursively.
#' @param embed_model Character. Ollama embedding model (default
#'   `"nomic-embed-text"`).
#'
#' @return The store object (invisibly), updated with the new documents.
#'
#' @examples
#' \dontrun{
#' store <- build_knowledge_base()
#'
#' # Add a single file
#' add_documents(store, "path/to/my_risk_register.md")
#'
#' # Add all .md and .txt files in a directory
#' add_documents(store, "path/to/project_docs/")
#' }
#'
#' @export
add_documents <- function(store, path, embed_model = "nomic-embed-text") {
  if (!requireNamespace("ragnar", quietly = TRUE)) {
    stop("Package 'ragnar' is required. Install with: install.packages('ragnar')")
  }

  # Collect files
  if (dir.exists(path)) {
    files <- list.files(path,
      pattern = "\\.(md|txt)$", full.names = TRUE,
      recursive = TRUE
    )
    if (length(files) == 0) {
      stop("No .md or .txt files found in: ", path)
    }
  } else if (file.exists(path)) {
    files <- path
  } else {
    stop("Path does not exist: ", path)
  }

  message("Adding ", length(files), " document(s) to knowledge base...")

  total_chunks <- 0L
  for (f in files) {
    text <- paste(readLines(f, warn = FALSE), collapse = "\n")
    chunks <- ragnar::markdown_chunk(text)
    chunks$source <- basename(f)
    ragnar::ragnar_store_insert(store, chunks)
    total_chunks <- total_chunks + nrow(chunks)
  }

  ragnar::ragnar_store_build_index(store)

  message("Added ", total_chunks, " chunks from ", length(files), " file(s).")
  invisible(store)
}

#' Retrieve Relevant Context for a Query
#'
#' Searches the PRA knowledge base using combined vector similarity search (VSS)
#' and BM25 full-text search to find the most relevant chunks for a user query.
#'
#' @param store A ragnar store object from [build_knowledge_base()].
#' @param query Character string. The user's question or query.
#' @param top_k Integer. Number of chunks to retrieve (default 5).
#'
#' @return A character vector of relevant text chunks with source attribution,
#'   suitable for injecting into an LLM prompt as additional context.
#'
#' @examples
#' \dontrun{
#' store <- build_knowledge_base()
#' chunks <- retrieve_context(store, "What is earned value management?")
#' cat(chunks, sep = "\n---\n")
#' }
#'
#' @export
retrieve_context <- function(store, query, top_k = 3) {
  if (!requireNamespace("ragnar", quietly = TRUE)) {
    stop("Package 'ragnar' is required. Install with: install.packages('ragnar')")
  }

  results <- ragnar::ragnar_retrieve(store, query, top_k = top_k)

  if (nrow(results) == 0) {
    return(character(0))
  }


  # Append source attribution to each chunk so LLM can cite references
  if ("source" %in% names(results)) {
    # ragnar v2 may return list columns; unlist for safety
    sources <- unlist(results$source)
  } else {
    sources <- rep("PRA knowledge base", nrow(results))
  }
  paste0(results$text, "\n[Source: ", sources, "]")
}
