#!/usr/bin/env Rscript
# llamaR CPU vs GPU Performance Benchmark

library(llamaR)

# Configuration
#MODEL_PATH <- "/mnt/Data2/DS_projects/llm_models/meta-llama-3.2-1B-Instruct-ft-sarcasm.Q4_K_M.gguf"
MODEL_PATH <- "/mnt/Data2/DS_projects/llm_models/Qwen3-14B-Q8_0.gguf"
PROMPT <- "Explain quantum computing in simple terms:"
MAX_TOKENS <- 50L
N_RUNS <- 3
N_THREADS <- 8L

cat("=== llamaR Performance Benchmark ===\n\n")
cat(sprintf("Model: %s\n", basename(MODEL_PATH)))
cat(sprintf("Prompt: %s\n", PROMPT))
cat(sprintf("Max tokens: %d\n", MAX_TOKENS))
cat(sprintf("Runs: %d\n\n", N_RUNS))

benchmark <- function(ctx, label) {
  times <- numeric(N_RUNS)
  tokens <- numeric(N_RUNS)

  for (i in seq_len(N_RUNS)) {
    start <- Sys.time()
    result <- llama_generate(ctx, PROMPT, max_new_tokens = MAX_TOKENS, temp = 0)
    end <- Sys.time()

    times[i] <- as.numeric(difftime(end, start, units = "secs"))
    tokens[i] <- length(llama_tokenize(ctx, result, add_special = FALSE))
  }

  avg_time <- mean(times)
  avg_tokens <- mean(tokens)
  speed <- avg_tokens / avg_time

  cat(sprintf("%s:\n", label))
  cat(sprintf("  Time: %.2f sec (avg of %d runs)\n", avg_time, N_RUNS))
  cat(sprintf("  Tokens: %.0f\n", avg_tokens))
  cat(sprintf("  Speed: %.1f tokens/sec\n\n", speed))

  return(speed)
}

# CPU Benchmark
cat("--- CPU Benchmark ---\n")
model_cpu <- llama_load_model(MODEL_PATH, n_gpu_layers = 0L)
ctx_cpu <- llama_new_context(model_cpu, n_ctx = 512L, n_threads = N_THREADS)

cat("Warming up...\n")
invisible(llama_generate(ctx_cpu, "Hi", max_new_tokens = 5L, temp = 0))

cpu_speed <- benchmark(ctx_cpu, sprintf("CPU (%d threads)", N_THREADS))

llama_free_context(ctx_cpu)
llama_free_model(model_cpu)

# GPU Benchmark
if (llama_supports_gpu()) {
  cat("--- GPU Benchmark ---\n")
  model_gpu <- llama_load_model(MODEL_PATH, n_gpu_layers = -1L)
  ctx_gpu <- llama_new_context(model_gpu, n_ctx = 512L, n_threads = N_THREADS)

  cat("Warming up...\n")
  invisible(llama_generate(ctx_gpu, "Hi", max_new_tokens = 5L, temp = 0))

  gpu_speed <- benchmark(ctx_gpu, "GPU (Vulkan)")

  llama_free_context(ctx_gpu)
  llama_free_model(model_gpu)

  cat("=== Results ===\n")
  cat(sprintf("CPU: %.1f tokens/sec\n", cpu_speed))
  cat(sprintf("GPU: %.1f tokens/sec\n", gpu_speed))
  cat(sprintf("Speedup: %.2fx\n", gpu_speed / cpu_speed))
} else {
  cat("GPU not available. Install with Vulkan support:\n")
  cat("  R CMD INSTALL --configure-args=\"--with-vulkan\" .\n")
}
