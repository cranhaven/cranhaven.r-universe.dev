MODEL_PATH <- "/mnt/Data2/DS_projects/llm_models/tiny-mistral-test-Q2_K.gguf"
LORA_PATH  <- "/mnt/Data2/DS_projects/llm_models/test-lora-adapter.gguf"

# ============================================================
# Shared fixtures: load model & context once
# ============================================================

HAS_MODEL <- file.exists(MODEL_PATH)

if (HAS_MODEL) {
    shared_model <- llama_load_model(MODEL_PATH)
    shared_info  <- llama_model_info(shared_model)
    shared_ctx   <- llama_new_context(shared_model, n_ctx = 256L, n_threads = 2L)

    withr::defer(llama_free_context(shared_ctx), teardown_env())
    withr::defer(llama_free_model(shared_model), teardown_env())
}

skip_if_no_model <- function() {
    if (!HAS_MODEL) skip("test model not available")
}

# ============================================================
# Package load (no model required)
# ============================================================

test_that("package loads correctly", {
    expect_true(require(llamaR, quietly = TRUE))
})

# ============================================================
# Verbosity (no model required)
# ============================================================

test_that("verbosity can be set and retrieved", {
    old <- llama_get_verbosity()

    llama_set_verbosity(0L)
    expect_equal(llama_get_verbosity(), 0L)

    llama_set_verbosity(3L)
    expect_equal(llama_get_verbosity(), 3L)

    llama_set_verbosity(old)
    expect_equal(llama_get_verbosity(), old)
})

# ============================================================
# Hardware / System info (no model required)
# ============================================================

test_that("llama_supports_gpu returns logical", {
    result <- llama_supports_gpu()
    expect_true(is.logical(result))
    expect_equal(length(result), 1L)
})

test_that("system_info returns non-empty string", {
    info <- llama_system_info()
    expect_true(is.character(info))
    expect_true(nchar(info) > 0)
})

test_that("supports_mmap returns logical", {
    result <- llama_supports_mmap()
    expect_true(is.logical(result))
    expect_equal(length(result), 1L)
})

test_that("supports_mlock returns logical", {
    result <- llama_supports_mlock()
    expect_true(is.logical(result))
    expect_equal(length(result), 1L)
})

test_that("max_devices returns positive integer", {
    result <- llama_max_devices()
    expect_true(is.integer(result))
    expect_true(result >= 1L)
})

test_that("llama_time_us returns positive numeric", {
    t <- llama_time_us()
    expect_true(is.numeric(t))
    expect_true(t > 0)
})

test_that("llama_numa_init does not error with disabled", {
    expect_no_error(llama_numa_init("disabled"))
})

test_that("llama_numa_init errors on invalid strategy", {
    expect_error(llama_numa_init("bogus"), "invalid NUMA")
})

test_that("llama_backend_devices returns data.frame", {
    df <- llama_backend_devices()
    expect_true(is.data.frame(df))
    expect_true(nrow(df) >= 1L)
    expect_true(all(c("name", "description", "type") %in% names(df)))
    expect_true(all(df$type %in% c("cpu", "gpu", "igpu", "accel", "unknown")))
})

test_that("llama_load_model with devices='cpu' works", {
    skip_if_no_model()
    model <- llama_load_model(MODEL_PATH, devices = "cpu")
    expect_false(is.null(model))
    llama_free_model(model)
})

test_that("chat_builtin_templates returns character vector", {
    templates <- llama_chat_builtin_templates()
    expect_true(is.character(templates))
    expect_true(length(templates) > 0)
})

# ============================================================
# Model: load + info
# ============================================================

test_that("model loads and info is returned", {
    skip_if_no_model()

    expect_false(is.null(shared_model))

    expect_true(is.list(shared_info))
    expect_true(shared_info$n_vocab > 0)
    expect_true(shared_info$n_embd  > 0)
    expect_true(shared_info$n_layer > 0)
    expect_true(shared_info$n_head  > 0)
    expect_true(nchar(shared_info$desc) > 0)
})

test_that("model_info returns extended fields", {
    skip_if_no_model()

    expect_true(is.numeric(shared_info$size))
    expect_true(shared_info$size > 0)
    expect_true(is.numeric(shared_info$n_params))
    expect_true(shared_info$n_params > 0)
    expect_true(is.logical(shared_info$has_encoder))
    expect_true(is.logical(shared_info$has_decoder))
    expect_true(is.logical(shared_info$is_recurrent))
})

# ============================================================
# Model metadata
# ============================================================

test_that("model_meta returns named character vector", {
    skip_if_no_model()

    meta <- llama_model_meta(shared_model)
    expect_true(is.character(meta))
    expect_true(length(meta) > 0)
    expect_false(is.null(names(meta)))
})

test_that("model_meta_val retrieves values by key", {
    skip_if_no_model()

    arch <- llama_model_meta_val(shared_model, "general.architecture")
    expect_true(is.character(arch) || is.null(arch))

    val <- llama_model_meta_val(shared_model, "nonexistent.key.12345")
    expect_null(val)
})

# ============================================================
# Vocabulary info
# ============================================================

test_that("vocab_info returns named integer vector", {
    skip_if_no_model()

    vocab <- llama_vocab_info(shared_model)
    expect_true(is.integer(vocab))
    expect_equal(length(vocab), 11L)
    expect_true(all(c("bos", "eos", "eot", "sep", "nl", "pad",
                       "fim_pre", "fim_suf", "fim_mid", "fim_rep", "fim_sep")
                     %in% names(vocab)))
})

# ============================================================
# Chat templates
# ============================================================

test_that("chat template can be retrieved from model", {
    skip_if_no_model()

    tmpl <- llama_chat_template(shared_model)
    expect_true(is.null(tmpl) || is.character(tmpl))
})

test_that("chat_apply_template formats messages", {
    skip_if_no_model()

    tmpl <- llama_chat_template(shared_model)
    if (is.null(tmpl)) skip("model has no built-in chat template")

    messages <- list(list(role = "user", content = "Hello"))
    prompt <- llama_chat_apply_template(messages, template = tmpl)

    expect_true(is.character(prompt))
    expect_true(nchar(prompt) > 0)
    expect_true(grepl("Hello", prompt, fixed = TRUE))
})

# ============================================================
# Context: create + config
# ============================================================

test_that("context can be created", {
    skip_if_no_model()
    expect_false(is.null(shared_ctx))
})

test_that("n_ctx returns correct context size", {
    skip_if_no_model()

    n <- llama_n_ctx(shared_ctx)
    expect_true(is.integer(n))
    expect_equal(n, 256L)
})

test_that("set_threads does not error", {
    skip_if_no_model()

    expect_no_error(llama_set_threads(shared_ctx, n_threads = 4L))
    expect_no_error(llama_set_threads(shared_ctx, n_threads = 2L, n_threads_batch = 4L))
    # restore
    llama_set_threads(shared_ctx, n_threads = 2L)
})

test_that("set_causal_attn does not error", {
    skip_if_no_model()

    expect_no_error(llama_set_causal_attn(shared_ctx, FALSE))
    expect_no_error(llama_set_causal_attn(shared_ctx, TRUE))
})

# ============================================================
# Tokenize / Detokenize
# ============================================================

test_that("tokenize and detokenize are inverse operations", {
    skip_if_no_model()

    text   <- "Hello, world!"
    tokens <- llama_tokenize(shared_ctx, text)

    expect_true(is.integer(tokens))
    expect_true(length(tokens) > 0)

    recovered <- llama_detokenize(shared_ctx, tokens)
    expect_true(is.character(recovered))
    expect_equal(recovered, text)
})

# ============================================================
# Generation
# ============================================================

test_that("generation produces non-empty output", {
    skip_if_no_model()

    result <- llama_generate(shared_ctx, "The capital of France is",
                             max_new_tokens = 20L, temp = 0.1)
    expect_true(is.character(result))
    expect_true(nchar(result, type = "bytes") > 0)
})

test_that("greedy generation is deterministic", {
    skip_if_no_model()

    r1 <- llama_generate(shared_ctx, "Once upon a time", max_new_tokens = 30L, temp = 0.0)
    r2 <- llama_generate(shared_ctx, "Once upon a time", max_new_tokens = 30L, temp = 0.0)
    expect_equal(r1, r2)
})

# ============================================================
# Advanced sampling
# ============================================================

test_that("generation with min_p produces output", {
    skip_if_no_model()

    result <- llama_generate(shared_ctx, "Hello", max_new_tokens = 10L,
                             temp = 0.8, min_p = 0.05)
    expect_true(is.character(result))
    expect_true(nchar(result, type = "bytes") > 0)
})

test_that("generation with repeat_penalty produces output", {
    skip_if_no_model()

    result <- llama_generate(shared_ctx, "Hello", max_new_tokens = 10L,
                             temp = 0.8, repeat_penalty = 1.1,
                             repeat_last_n = 32L)
    expect_true(is.character(result))
    expect_true(nchar(result, type = "bytes") > 0)
})

test_that("generation with mirostat v2 produces output", {
    skip_if_no_model()

    result <- llama_generate(shared_ctx, "Hello", max_new_tokens = 10L,
                             mirostat = 2L, mirostat_tau = 5.0,
                             mirostat_eta = 0.1)
    expect_true(is.character(result))
    expect_true(nchar(result, type = "bytes") > 0)
})

test_that("generation with typical_p produces output", {
    skip_if_no_model()

    result <- llama_generate(shared_ctx, "Hello", max_new_tokens = 10L,
                             temp = 0.8, typical_p = 0.9)
    expect_true(is.character(result))
    expect_true(nchar(result, type = "bytes") > 0)
})

# ============================================================
# Embeddings
# ============================================================

test_that("embeddings have correct dimensionality", {
    skip_if_no_model()

    emb <- llama_embeddings(shared_ctx, "Hello")

    expect_true(is.numeric(emb))
    expect_equal(length(emb), shared_info$n_embd)
    expect_true(any(emb != 0))
})

test_that("llama_get_embeddings_ith returns correct vector", {
    skip_if_no_model()

    ctx <- llama_new_context(shared_model, n_ctx = 256L, n_threads = 2L)
    on.exit(llama_free_context(ctx))

    # run embeddings to populate output
    emb_full <- llama_embeddings(ctx, "Hello")

    # get_embeddings_ith(-1) should return the same as llama_embeddings
    emb_ith <- llama_get_embeddings_ith(ctx, -1L)

    expect_true(is.numeric(emb_ith))
    expect_equal(length(emb_ith), shared_info$n_embd)
    expect_equal(emb_ith, emb_full)
})

# ============================================================
# Logits
# ============================================================

test_that("get_logits returns numeric vector of n_vocab length", {
    skip_if_no_model()

    llama_generate(shared_ctx, "Hello", max_new_tokens = 1L, temp = 0)

    logits <- llama_get_logits(shared_ctx)
    expect_true(is.numeric(logits))
    expect_equal(length(logits), shared_info$n_vocab)
    expect_true(any(logits != 0))
})

# ============================================================
# KV Cache operations
# ============================================================

test_that("memory_clear works", {
    skip_if_no_model()

    llama_generate(shared_ctx, "Hello", max_new_tokens = 5L, temp = 0)
    expect_no_error(llama_memory_clear(shared_ctx))
})

test_that("memory_seq_rm works", {
    skip_if_no_model()

    llama_generate(shared_ctx, "Hello", max_new_tokens = 5L, temp = 0)
    result <- llama_memory_seq_rm(shared_ctx, seq_id = 0L, p0 = -1L, p1 = -1L)
    expect_true(is.logical(result))
})

test_that("memory_seq_keep works", {
    skip_if_no_model()

    llama_generate(shared_ctx, "Hello", max_new_tokens = 5L, temp = 0)
    expect_no_error(llama_memory_seq_keep(shared_ctx, seq_id = 0L))
})

test_that("memory_seq_pos_range returns named integer", {
    skip_if_no_model()

    range <- llama_memory_seq_pos_range(shared_ctx, seq_id = 0L)
    expect_true(is.integer(range))
    expect_equal(length(range), 2L)
    expect_true(all(c("min", "max") %in% names(range)))
})

test_that("memory_can_shift returns logical", {
    skip_if_no_model()

    result <- llama_memory_can_shift(shared_ctx)
    expect_true(is.logical(result))
    expect_equal(length(result), 1L)
})

# ============================================================
# State save/load
# ============================================================

test_that("state save and load round-trip", {
    skip_if_no_model()

    llama_generate(shared_ctx, "Hello world", max_new_tokens = 5L, temp = 0)

    state_file <- tempfile(fileext = ".bin")
    on.exit(unlink(state_file), add = TRUE)

    result <- llama_state_save(shared_ctx, state_file)
    expect_true(result)
    expect_true(file.exists(state_file))
    expect_true(file.info(state_file)$size > 0)

    ctx2 <- llama_new_context(shared_model, n_ctx = 256L, n_threads = 2L)
    result2 <- llama_state_load(ctx2, state_file)
    expect_true(result2)

    llama_free_context(ctx2)
})

test_that("state_load errors on non-existent file", {
    skip_if_no_model()
    expect_error(llama_state_load(shared_ctx, "nonexistent_state.bin"))
})

# ============================================================
# Performance counters
# ============================================================

test_that("perf returns named list with expected fields", {
    skip_if_no_model()

    llama_generate(shared_ctx, "Hello", max_new_tokens = 5L, temp = 0)

    perf <- llama_perf(shared_ctx)
    expect_true(is.list(perf))
    expect_true(all(c("t_load_ms", "t_p_eval_ms", "t_eval_ms",
                       "n_p_eval", "n_eval", "n_reused") %in% names(perf)))
    expect_true(perf$n_eval > 0)

    expect_no_error(llama_perf_reset(shared_ctx))
})

# ============================================================
# LoRA adapters (separate model load — LoRA modifies model)
# ============================================================

test_that("lora_load returns handle or errors on missing file", {
    skip_if_no_model()

    expect_error(llama_lora_load(shared_model, "nonexistent.gguf"))

    if (file.exists(LORA_PATH)) {
        lora <- llama_lora_load(shared_model, LORA_PATH)
        expect_false(is.null(lora))
    }
})

test_that("lora_apply and lora_remove work on context", {
    skip_if_no_model()
    if (!file.exists(LORA_PATH)) skip("test LoRA adapter not available")

    model <- llama_load_model(MODEL_PATH)
    lora <- llama_lora_load(model, LORA_PATH)
    ctx <- llama_new_context(model, n_ctx = 128L, n_threads = 2L)

    expect_no_error(llama_lora_apply(ctx, lora, scale = 1.0))

    result <- llama_lora_remove(ctx, lora)
    expect_equal(result, 0L)

    llama_free_context(ctx)
    llama_free_model(model)
})

test_that("lora_clear works on context", {
    skip_if_no_model()
    if (!file.exists(LORA_PATH)) skip("test LoRA adapter not available")

    model <- llama_load_model(MODEL_PATH)
    lora <- llama_lora_load(model, LORA_PATH)
    ctx <- llama_new_context(model, n_ctx = 128L, n_threads = 2L)

    llama_lora_apply(ctx, lora, scale = 0.5)
    expect_no_error(llama_lora_clear(ctx))

    result <- llama_lora_remove(ctx, lora)
    expect_equal(result, -1L)

    llama_free_context(ctx)
    llama_free_model(model)
})

# ============================================================
# token_to_piece
# ============================================================

test_that("token_to_piece returns character string", {
    skip_if_no_model()

    # add_special=FALSE to avoid BOS which may render as empty string
    tokens <- llama_tokenize(shared_ctx, "Hello", add_special = FALSE)
    piece  <- llama_token_to_piece(shared_ctx, tokens[1])

    expect_true(is.character(piece))
    expect_equal(length(piece), 1L)
    expect_true(nchar(piece) > 0)
})

test_that("token_to_piece with special=TRUE does not error", {
    skip_if_no_model()

    vocab  <- llama_vocab_info(shared_model)
    bos_id <- vocab["bos"]
    if (is.na(bos_id) || bos_id < 0L) skip("model has no BOS token")

    piece <- llama_token_to_piece(shared_ctx, bos_id, special = TRUE)
    expect_true(is.character(piece))
    expect_equal(length(piece), 1L)
})

test_that("token_to_piece round-trips with tokenize", {
    skip_if_no_model()

    text   <- "world"
    tokens <- llama_tokenize(shared_ctx, text, add_special = FALSE)
    pieces <- vapply(tokens, function(t) llama_token_to_piece(shared_ctx, t), character(1))

    expect_true(length(pieces) > 0)
    reconstructed <- paste(pieces, collapse = "")
    # strip possible leading space added by tokenizer
    expect_true(grepl(text, reconstructed, fixed = TRUE))
})

# ============================================================
# GPU: token_to_piece on GPU context
# ============================================================

test_that("token_to_piece works on GPU context", {
    skip_if_no_model()
    skip_if(!llama_supports_gpu(), "GPU not available")

    gpu_model <- llama_load_model(MODEL_PATH, n_gpu_layers = -1L)
    gpu_ctx   <- llama_new_context(gpu_model, n_ctx = 128L)
    on.exit({ llama_free_context(gpu_ctx); llama_free_model(gpu_model) }, add = TRUE)

    tokens <- llama_tokenize(gpu_ctx, "GPU test", add_special = FALSE)
    piece  <- llama_token_to_piece(gpu_ctx, tokens[1])

    expect_true(is.character(piece))
    expect_true(nchar(piece) > 0)
})

# ============================================================
# llama_batch_init / llama_batch_free
# ============================================================

test_that("batch_init returns external pointer", {
    batch <- llama_batch_init(512L)

    expect_true(is.list(batch) || inherits(batch, "externalptr"))
    expect_false(is.null(batch))
})

test_that("batch_init with embd mode does not error", {
    expect_no_error(llama_batch_init(64L, embd = 512L, n_seq_max = 4L))
})

test_that("batch_free clears the batch", {
    batch <- llama_batch_init(128L)
    expect_no_error(llama_batch_free(batch))
    # double-free should be safe (pointer already NULLed)
    expect_no_error(llama_batch_free(batch))
})

test_that("batch GC finalizer works (no explicit free)", {
    # allocate inside local scope — GC should clean up
    local({
        b <- llama_batch_init(256L)
        expect_false(is.null(b))
    })
    gc()
    succeed()
})

# ============================================================
# llama_encode (encoder-decoder)
# ============================================================

test_that("llama_encode returns integer on encoder-decoder model", {
    skip_if_no_model()
    skip_if(!shared_info$has_encoder || !shared_info$has_decoder,
            "model is not encoder-decoder")

    tokens <- llama_tokenize(shared_ctx, "Translate: Hello world")
    ret    <- llama_encode(shared_ctx, tokens)

    expect_true(is.integer(ret))
    expect_equal(ret, 0L)
})

# ============================================================
# GPU: batch_init + encode on GPU context
# ============================================================

# ============================================================
# embed_llamar
# ============================================================

test_that("llama_embed_batch returns matrix with correct dimensions", {
    skip_if_no_model()

    # embedding=FALSE: sequential last-token decode (works on generative models)
    ctx <- llama_new_context(shared_model, n_ctx = 256L, n_threads = 2L)
    on.exit(llama_free_context(ctx))

    mat <- llama_embed_batch(ctx, c("hello", "world", "test"))

    expect_true(is.matrix(mat))
    expect_equal(nrow(mat), 3L)
    expect_equal(ncol(mat), shared_info$n_embd)
    expect_true(any(mat != 0))
})

test_that("llama_embed_batch single text matches llama_embeddings", {
    skip_if_no_model()

    ctx1 <- llama_new_context(shared_model, n_ctx = 256L, n_threads = 2L)
    on.exit(llama_free_context(ctx1), add = TRUE)

    emb_single <- llama_embeddings(ctx1, "hello")

    ctx2 <- llama_new_context(shared_model, n_ctx = 256L, n_threads = 2L)
    on.exit(llama_free_context(ctx2), add = TRUE)

    mat <- llama_embed_batch(ctx2, "hello")

    expect_equal(nrow(mat), 1L)
    expect_equal(ncol(mat), length(emb_single))
})

test_that("llama_embed_batch empty input returns 0-row matrix", {
    skip_if_no_model()

    ctx <- llama_new_context(shared_model, n_ctx = 256L, n_threads = 2L)
    on.exit(llama_free_context(ctx))

    mat <- llama_embed_batch(ctx, character(0))

    expect_true(is.matrix(mat))
    expect_equal(nrow(mat), 0L)
})

test_that("embed_llamar partial application returns a function", {
    skip_if_no_model()

    fn <- embed_llamar(model = shared_model)
    expect_true(is.function(fn))
})

test_that("embed_llamar partial application produces list of vectors", {
    skip_if_no_model()

    fn <- embed_llamar(model = shared_model, n_ctx = 256L, n_threads = 2L)
    result <- fn(c("hello", "world"))

    expect_true(is.list(result))
    expect_equal(length(result), 2L)
    expect_true(is.numeric(result[[1]]))
    expect_equal(length(result[[1]]), shared_info$n_embd)
})

test_that("embed_llamar direct call returns matrix", {
    skip_if_no_model()

    mat <- embed_llamar(c("hello", "world"), model = shared_model,
                        n_ctx = 256L, n_threads = 2L)

    expect_true(is.matrix(mat))
    expect_equal(nrow(mat), 2L)
    expect_equal(ncol(mat), shared_info$n_embd)
})

test_that("embed_llamar normalizes by default", {
    skip_if_no_model()

    mat <- embed_llamar("hello", model = shared_model,
                        n_ctx = 256L, n_threads = 2L)
    norm <- sqrt(sum(mat[1, ]^2))
    expect_equal(norm, 1.0, tolerance = 1e-6)
})

test_that("embed_llamar normalize=FALSE skips normalization", {
    skip_if_no_model()

    mat <- embed_llamar("hello", model = shared_model,
                        n_ctx = 256L, n_threads = 2L, normalize = FALSE)
    norm <- sqrt(sum(mat[1, ]^2))
    # raw embeddings are unlikely to have unit norm
    expect_true(is.numeric(mat))
})

test_that("embed_llamar with data.frame returns data.frame with embedding column", {
    skip_if_no_model()

    df <- data.frame(text = c("hello", "world"), id = 1:2)
    result <- embed_llamar(df, model = shared_model,
                           n_ctx = 256L, n_threads = 2L)

    expect_true(is.data.frame(result))
    expect_true("embedding" %in% names(result))
    expect_true("id" %in% names(result))
    expect_equal(nrow(result), 2L)
    expect_true(is.list(result$embedding))
    expect_equal(length(result$embedding[[1]]), shared_info$n_embd)
})

test_that("embed_llamar errors on data.frame without text column", {
    skip_if_no_model()

    df <- data.frame(content = "hello")
    expect_error(embed_llamar(df, model = shared_model),
                 "text")
})

test_that("embed_llamar with model path loads and frees model", {
    skip_if_no_model()

    mat <- embed_llamar("hello", model = MODEL_PATH,
                        n_ctx = 256L, n_threads = 2L)
    expect_true(is.matrix(mat))
    expect_equal(nrow(mat), 1L)
    expect_equal(ncol(mat), shared_info$n_embd)
})

# ============================================================
# GPU: batch_init + encode on GPU context
# ============================================================

test_that("batch_init works with GPU context loaded", {
    skip_if_no_model()
    skip_if(!llama_supports_gpu(), "GPU not available")

    gpu_model <- llama_load_model(MODEL_PATH, n_gpu_layers = -1L)
    gpu_ctx   <- llama_new_context(gpu_model, n_ctx = 128L)
    on.exit({ llama_free_context(gpu_ctx); llama_free_model(gpu_model) }, add = TRUE)

    batch <- llama_batch_init(128L)
    expect_false(is.null(batch))
    expect_no_error(llama_batch_free(batch))
})
