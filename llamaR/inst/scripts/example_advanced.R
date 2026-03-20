#!/usr/bin/env Rscript
# llamaR — Advanced Features Example
#
# Demonstrates new API functions: model introspection, vocabulary info,
# KV cache management, sampling options, performance monitoring,
# state save/load, and grammar-constrained generation.

library(llamaR)

# ============================================================
# 0. System capabilities
# ============================================================

cat("=== System Info ===\n")
cat(llama_system_info(), "\n\n")

cat("GPU offload: ", llama_supports_gpu(), "\n")
cat("mmap:        ", llama_supports_mmap(), "\n")
cat("mlock:       ", llama_supports_mlock(), "\n")
cat("Max devices: ", llama_max_devices(), "\n\n")

# ============================================================
# 1. Load model & deep introspection
# ============================================================

MODEL_PATH <- "/mnt/Data2/DS_projects/llm_models/meta-llama-3.2-1B-Instruct-ft-sarcasm.Q4_K_M.gguf"
if (!file.exists(MODEL_PATH)) stop("Model not found: ", MODEL_PATH)

cat("=== Loading Model ===\n")
cat("Path:", MODEL_PATH, "\n")
llama_set_verbosity(1L)  # errors only

gpu_layers <- if (llama_supports_gpu()) -1L else 0L
model <- llama_load_model(MODEL_PATH, n_gpu_layers = gpu_layers)

# Extended model info
info <- llama_model_info(model)
cat("\n--- Model Info ---\n")
cat("Description:  ", info$desc, "\n")
cat("Parameters:   ", format(info$n_params, big.mark = ","), "\n")
cat("Size on disk: ", sprintf("%.1f MB", info$size / 1e6), "\n")
cat("Embedding dim:", info$n_embd, "\n")
cat("Layers:       ", info$n_layer, "\n")
cat("Heads:        ", info$n_head, "\n")
cat("Context train:", info$n_ctx_train, "\n")
cat("Vocab size:   ", info$n_vocab, "\n")
cat("Has encoder:  ", info$has_encoder, "\n")
cat("Has decoder:  ", info$has_decoder, "\n")
cat("Is recurrent: ", info$is_recurrent, "\n\n")

# GGUF metadata
cat("--- GGUF Metadata (first 10) ---\n")
meta <- llama_model_meta(model)
n_show <- min(10, length(meta))
for (i in seq_len(n_show)) {
    cat(sprintf("  %-40s = %s\n", names(meta)[i],
                substr(meta[i], 1, 60)))
}
if (length(meta) > n_show) {
    cat(sprintf("  ... and %d more keys\n", length(meta) - n_show))
}

# Read specific metadata
arch <- llama_model_meta_val(model, "general.architecture")
cat("\nArchitecture:", if (is.null(arch)) "(unknown)" else arch, "\n")

# Vocabulary special tokens
vocab <- llama_vocab_info(model)
cat("\n--- Special Tokens ---\n")
for (nm in names(vocab)) {
    tok <- vocab[nm]
    label <- if (tok == -1L) "(undefined)" else as.character(tok)
    cat(sprintf("  %-10s = %s\n", nm, label))
}

# Built-in chat templates
templates <- llama_chat_builtin_templates()
cat("\n--- Built-in Chat Templates ---\n")
cat(paste(" ", templates, collapse = "\n"), "\n\n")

# ============================================================
# 2. Context creation & configuration
# ============================================================

cat("=== Context Setup ===\n")
ctx <- llama_new_context(model, n_ctx = 2048L, n_threads = 4L)
cat("Context size: ", llama_n_ctx(ctx), "tokens\n")

# Change thread count dynamically
llama_set_threads(ctx, n_threads = 8L, n_threads_batch = 8L)
cat("Threads set to 8 (gen) / 8 (batch)\n\n")

# ============================================================
# 3. Basic generation + performance monitoring
# ============================================================

cat("=== Generation with Performance Tracking ===\n")

t0 <- proc.time()
result <- llama_generate(ctx, "The three laws of robotics are:",
                         max_new_tokens = 100L, temp = 0)
elapsed <- (proc.time() - t0)["elapsed"]

perf <- llama_perf(ctx)
cat("Generated text:\n", result, "\n\n")
cat(sprintf("Wall time:  %.2f s\n", elapsed))
cat(sprintf("Prompt:     %d tokens (%.1f ms)\n", perf$n_p_eval, perf$t_p_eval_ms))
cat(sprintf("Generation: %d tokens (%.1f ms)\n", perf$n_eval, perf$t_eval_ms))
if (perf$t_eval_ms > 0) {
    cat(sprintf("Speed:      %.1f tok/s\n", perf$n_eval / (perf$t_eval_ms / 1000)))
}
cat("\n")

# ============================================================
# 4. Advanced sampling strategies
# ============================================================

cat("=== Sampling Strategies ===\n\n")

# 4a. Standard with min_p + typical_p
cat("--- min_p + typical_p ---\n")
r1 <- llama_generate(ctx, "Write a haiku about programming:\n",
                     max_new_tokens = 50L, temp = 0.9,
                     min_p = 0.05, typical_p = 0.95,
                     seed = 123L)
cat(r1, "\n\n")

# 4b. With repetition penalty
cat("--- Repetition penalty ---\n")
r2 <- llama_generate(ctx, "List five colors:",
                     max_new_tokens = 60L, temp = 0.7,
                     repeat_penalty = 1.2, repeat_last_n = 64L,
                     frequency_penalty = 0.1, presence_penalty = 0.1)
cat(r2, "\n\n")

# 4c. Mirostat v2 (adaptive perplexity)
cat("--- Mirostat v2 ---\n")
r3 <- llama_generate(ctx, "Explain gravity to a five-year-old:",
                     max_new_tokens = 80L,
                     mirostat = 2L, mirostat_tau = 5.0,
                     mirostat_eta = 0.1, seed = 42L)
cat(r3, "\n\n")

# ============================================================
# 5. Combined sampling: all parameters together
# ============================================================

cat("=== Combined Sampling ===\n")
r4 <- llama_generate(ctx, "Give me a recipe for pancakes:\n",
                     max_new_tokens = 80L, temp = 0.7,
                     top_k = 40L, top_p = 0.9, min_p = 0.05,
                     typical_p = 0.95,
                     repeat_penalty = 1.15, repeat_last_n = 64L,
                     frequency_penalty = 0.05, presence_penalty = 0.05,
                     seed = 42L)
cat(r4, "\n\n")

# ============================================================
# 6. KV cache management
# ============================================================

cat("=== KV Cache Management ===\n")

# Generate something to fill the cache
llama_generate(ctx, "The quick brown fox", max_new_tokens = 20L, temp = 0)

# Check position range
range <- llama_memory_seq_pos_range(ctx, seq_id = 0L)
cat(sprintf("Sequence 0 positions: %d to %d\n", range["min"], range["max"]))
cat("Cache supports shifting:", llama_memory_can_shift(ctx), "\n")

# Clear specific sequence
llama_memory_seq_rm(ctx, seq_id = 0L, p0 = -1L, p1 = -1L)
cat("Cleared sequence 0\n")

# Full clear
llama_memory_clear(ctx)
cat("Full cache cleared\n\n")

# ============================================================
# 7. State save & restore
# ============================================================

cat("=== State Save & Restore ===\n")

# Generate with a specific prompt and save state
r_before <- llama_generate(ctx, "Once upon a time in a land far away,",
                           max_new_tokens = 30L, temp = 0)
cat("Before save:", substr(r_before, 1, 80), "...\n")

state_file <- tempfile("llama_state_", fileext = ".bin")
llama_state_save(ctx, state_file)
cat(sprintf("State saved to %s (%.1f KB)\n",
            basename(state_file), file.info(state_file)$size / 1024))

# Create fresh context, load state, continue generating
ctx2 <- llama_new_context(model, n_ctx = 2048L, n_threads = 8L)
llama_state_load(ctx2, state_file)
cat("State loaded into new context\n")

# Clean up
unlink(state_file)
llama_free_context(ctx2)

# ============================================================
# 8. Logits access
# ============================================================

cat("\n=== Logits Access ===\n")
llama_generate(ctx, "The capital of France is", max_new_tokens = 1L, temp = 0)

logits <- llama_get_logits(ctx)
cat(sprintf("Logits vector: %d values\n", length(logits)))
cat(sprintf("Range: [%.2f, %.2f]\n", min(logits), max(logits)))

# Find top-5 tokens by logit value
top5 <- order(logits, decreasing = TRUE)[1:5]
cat("Top 5 token IDs:", paste(top5 - 1L, collapse = ", "), "\n")

# Detokenize top predictions
for (i in seq_along(top5)) {
    tid <- top5[i] - 1L  # logits are 0-indexed, R vectors are 1-indexed
    tok_text <- llama_detokenize(ctx, as.integer(tid))
    cat(sprintf("  #%d: token %d = '%s' (logit = %.2f)\n",
                i, tid, tok_text, logits[top5[i]]))
}

# ============================================================
# Cleanup
# ============================================================

cat("\n=== Cleanup ===\n")
llama_free_context(ctx)
llama_free_model(model)
cat("Done!\n")
