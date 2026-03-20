
library(llamaR)

MODEL_PATH <- "/mnt/Data2/DS_projects/llm_models/meta-llama-3.2-1B-Instruct-ft-sarcasm.Q4_K_M.gguf"
PROMPT <- "Explain quantum computing in simple terms:"


model_gpu <- llama_load_model(MODEL_PATH, n_gpu_layers = -1L)
ctx_gpu <- llama_new_context(model_gpu, n_ctx = 4096L, n_threads = N_THREADS)


prompt <- "Write a **DETAILED 5500-word article** about quantum computing for beginners. Include history, principles, qubits, superposition, applications, and future. Structure with headings."
result <- llama_generate(ctx_gpu, prompt, max_new_tokens = 5500L)
cat(result)
