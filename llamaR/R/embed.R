#' Embedding provider for ragnar / standalone use
#'
#' Computes embeddings using a local GGUF model. When called without \code{x},
#' returns a function suitable for passing to \code{ragnar_store_create(embed = ...)}.
#'
#' @param x Character vector of texts to embed, a data.frame with a \code{text}
#'   column, or missing/\code{NULL} for partial application.
#' @param model Either a path to a \code{.gguf} file (character) or a model handle
#'   already loaded via \code{\link{llama_load_model}}.
#' @param n_gpu_layers Number of layers to offload to GPU (0 = CPU only, -1 = all).
#'   Ignored when \code{model} is an already-loaded handle.
#' @param n_ctx Context window size for the embedding context. Defaults to 512,
#'   typical for embedding models. Ignored when \code{model} is an already-loaded handle.
#' @param n_threads Number of CPU threads. Ignored when \code{model} is an
#'   already-loaded handle.
#' @param embedding Logical; if \code{TRUE}, use pooled batch decode (efficient for
#'   true embedding models like nomic-embed, bge). If \code{FALSE} (default),
#'   use sequential last-token decode (works with any model).
#' @param normalize Logical; if \code{TRUE} (default), L2-normalize each embedding vector.
#'
#' @return
#' \itemize{
#'   \item If \code{x} is missing or \code{NULL}: a function \code{function(x)} that
#'     returns a list of numeric vectors (one per input string), suitable for ragnar.
#'   \item If \code{x} is a character vector: a numeric matrix with \code{nrow = length(x)}
#'     and \code{ncol = n_embd}.
#'   \item If \code{x} is a data.frame: the same data.frame with an added \code{embedding}
#'     column (list of numeric vectors).
#' }
#'
#' @export
#' @examples
#' \dontrun{
#' # --- Partial application for ragnar ---
#' store <- ragnar_store_create(
#'   "my_store",
#'   embed = embed_llamar(model = "embedding-model.gguf", n_gpu_layers = -1)
#' )
#'
#' # --- Direct use with path ---
#' mat <- embed_llamar(c("hello", "world"), model = "embedding-model.gguf")
#'
#' # --- Direct use with pre-loaded model ---
#' mdl <- llama_load_model("embedding-model.gguf", n_gpu_layers = -1)
#' mat <- embed_llamar(c("hello", "world"), model = mdl)
#' }
embed_llamar <- function(x,
                         model,
                         n_gpu_layers = 0L,
                         n_ctx = 512L,
                         n_threads = parallel::detectCores(),
                         embedding = FALSE,
                         normalize = TRUE) {

    # -- partial application: return a closure for ragnar ----------------------
    if (missing(x) || is.null(x)) {
        # lazy state: model/ctx created on first call
        env <- new.env(parent = emptyenv())
        env$ctx <- NULL

        force(model)
        force(n_gpu_layers)
        force(n_ctx)
        force(n_threads)
        force(embedding)
        force(normalize)

        embed_fn <- function(x) {
            if (is.null(env$ctx)) {
                if (is.character(model)) {
                    env$model_ptr <- llama_load_model(model,
                                                      n_gpu_layers = n_gpu_layers)
                } else {
                    env$model_ptr <- model
                }
                env$ctx <- llama_new_context(env$model_ptr,
                                             n_ctx = n_ctx,
                                             n_threads = n_threads,
                                             embedding = embedding)
            }
            mat <- .embed_texts(env$ctx, x, normalize)
            asplit(mat, 1)
        }
        return(embed_fn)
    }

    # -- direct call -----------------------------------------------------------
    if (is.data.frame(x)) {
        if (!"text" %in% names(x))
            stop("embed_llamar: data.frame must have a 'text' column")
        texts <- x$text
    } else {
        stopifnot(is.character(x))
        texts <- x
    }

    # resolve model
    if (is.character(model)) {
        own_model <- TRUE
        model_ptr <- llama_load_model(model, n_gpu_layers = n_gpu_layers)
        ctx <- llama_new_context(model_ptr, n_ctx = n_ctx, n_threads = n_threads,
                                 embedding = embedding)
        on.exit({
            llama_free_context(ctx)
            llama_free_model(model_ptr)
        })
    } else {
        own_model <- FALSE
        model_ptr <- model
        ctx <- llama_new_context(model_ptr, n_ctx = n_ctx, n_threads = n_threads,
                                 embedding = embedding)
        on.exit(llama_free_context(ctx))
    }

    mat <- .embed_texts(ctx, texts, normalize)

    if (is.data.frame(x)) {
        x$embedding <- asplit(mat, 1)
        return(x)
    }
    mat
}


# Internal: embed a character vector, return matrix
.embed_texts <- function(ctx, texts, normalize) {
    mat <- llama_embed_batch(ctx, texts)
    if (normalize) {
        norms <- sqrt(rowSums(mat^2))
        norms[norms == 0] <- 1
        mat <- mat / norms
    }
    rownames(mat) <- NULL
    mat
}
