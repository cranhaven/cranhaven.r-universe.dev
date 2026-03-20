# llamaR

R interface to [llama.cpp](https://github.com/ggml-org/llama.cpp) for running local inference of large language models (LLMs) directly from R.

The package supports GPU acceleration via Vulkan, and automatically falls back to CPU when no GPU is available.

## Key Features

- Load and unload models in GGUF format (`llama_load_model`, `llama_free_model`)
- Create and free contexts (`llama_new_context`, `llama_free_context`)
- Tokenization, detokenization and text generation (`llama_tokenize`, `llama_detokenize`, `llama_generate`)
- Embedding extraction: single (`llama_embeddings`), batch (`llama_embed_batch`), ragnar-compatible (`embed_llamar`)
- Hugging Face integration: download and cache models (`llama_hf_download`, `llama_load_model_hf`, etc.)
- Encoder-decoder model support (T5, BART) via `llama_encode`
- Explicit backend/device selection and multi-GPU split (`llama_load_model(devices = ...)`)
- NUMA optimization (`llama_numa_init`)

## GPU and CPU Support

The package uses [ggmlR](https://github.com/Zabis13/ggmlR) as the low-level backend.
If ggmlR was built with Vulkan support enabled, llamaR automatically uses the GPU for computation.
On systems without a GPU, all code runs on CPU with no additional configuration required.

### How Vulkan linking works

Vulkan support is compiled entirely within ggmlR — llamaR does not compile any Vulkan code itself.
However, since llamaR links against `libggml.a` (from ggmlR) using `--whole-archive`, the Vulkan
symbols (e.g. `vkCmdCopyBuffer`, `vkGetInstanceProcAddr`) need to be resolved at link time.

The llamaR `configure` script handles this automatically:
- **Linux**: checks `pkg-config --exists vulkan` and adds `-lvulkan` to the linker flags
- **Windows**: checks for the `VULKAN_SDK` environment variable and adds `-lvulkan-1`

If Vulkan is not found on the system, the build proceeds without it — the Vulkan backend
in `libggml.a` will simply remain unused, and inference runs on CPU only.

## Installation

### Dependencies

Requires [ggmlR](https://github.com/Zabis13/ggmlR) >= 0.5.4:

```r
# Install ggmlR first
remotes::install_github("Zabis13/ggmlR")

# Then llamaR
remotes::install_github("Zabis13/llamaR")
```

### System Requirements

- R >= 4.1.0
- C++17 compiler
- GNU make

## Quick Start

```r
library(llamaR)

# Load model
model <- llama_load_model("path/to/model.gguf")

# Create context
ctx <- llama_new_context(model, n_ctx = 2048L, n_threads = 8L)

# Generate text
result <- llama_generate(ctx, "Once upon a time", max_new_tokens = 100L)
cat(result)

# Free resources (optional, GC handles this automatically)
llama_free_context(ctx)
llama_free_model(model)
```

## Downloading Models from Hugging Face

Download GGUF models directly from Hugging Face with automatic caching:

```r
library(llamaR)

# List available GGUF files in a repository
files <- llama_hf_list("TheBloke/Llama-2-7B-GGUF")
print(files)

# Download a specific quantization
path <- llama_hf_download("TheBloke/Llama-2-7B-GGUF", pattern = "*q4_k_m*")

# Or download and load in one step
model <- llama_load_model_hf("TheBloke/Llama-2-7B-GGUF",
                              pattern = "*q4_k_m*",
                              n_gpu_layers = -1L)

# Manage cache
llama_hf_cache_info()
llama_hf_cache_clear()
```

For private repositories, set the `HF_TOKEN` environment variable or pass `token` directly.

## Usage

### Loading Models

```r
# CPU only
model <- llama_load_model("model.gguf")

# With GPU acceleration (all layers)
model <- llama_load_model("model.gguf", n_gpu_layers = -1L)

# Partial GPU offload (first 20 layers)
model <- llama_load_model("model.gguf", n_gpu_layers = 20L)

# Explicit device selection (see llama_backend_devices())
model <- llama_load_model("model.gguf", n_gpu_layers = -1L, devices = "Vulkan0")

# Check GPU availability
if (llama_supports_gpu()) {
  message("GPU available")
}
```

### Model Information

```r
info <- llama_model_info(model)
cat("Model:", info$desc, "\n")
cat("Layers:", info$n_layer, "\n")
cat("Context:", info$n_ctx_train, "\n")
cat("Embedding size:", info$n_embd, "\n")
```

### Text Generation

```r
ctx <- llama_new_context(model, n_ctx = 4096L)

# Basic generation
result <- llama_generate(ctx, "The meaning of life is")

# Greedy decoding (deterministic)
result <- llama_generate(ctx, "2 + 2 =", temp = 0)

# Creative output
result <- llama_generate(ctx,
  prompt = "Write a haiku about R:",
  max_new_tokens = 50L,
  temp = 1.0,
  top_p = 0.95,
  top_k = 40L
)
```

### Chat Models

```r
model <- llama_load_model("llama-3.2-instruct.gguf", n_gpu_layers = -1L)
ctx <- llama_new_context(model)

# Get template from model
tmpl <- llama_chat_template(model)

# Build conversation
messages <- list(
  list(role = "system", content = "You are a helpful assistant."),
  list(role = "user", content = "What is R?")
)

# Apply template
prompt <- llama_chat_apply_template(messages, template = tmpl)

# Generate response
response <- llama_generate(ctx, prompt, max_new_tokens = 200L)
cat(response)
```

### Tokenization

```r
# Text -> tokens
tokens <- llama_tokenize(ctx, "Hello, world!")

# Tokens -> text
text <- llama_detokenize(ctx, tokens)
```

### Embeddings

```r
# Single text embedding
emb1 <- llama_embeddings(ctx, "machine learning")
emb2 <- llama_embeddings(ctx, "artificial intelligence")

# Cosine similarity
similarity <- sum(emb1 * emb2) / (sqrt(sum(emb1^2)) * sqrt(sum(emb2^2)))
cat("Similarity:", similarity, "\n")

# Batch embeddings (matrix output)
ctx <- llama_new_context(model, n_ctx = 512L, embedding = TRUE)
mat <- llama_embed_batch(ctx, c("hello world", "foo bar", "test"))
# mat is a 3 x n_embd matrix
```

### ragnar Integration

Use `embed_llamar()` as an embedding provider for [ragnar](https://ragnar.tidyverse.org/):

```r
library(ragnar)

# Create store with local embedding model
store <- ragnar_store_create(
  "my_store",
  embed = embed_llamar(
    model = "nomic-embed-text-v1.5.Q8_0.gguf",
    n_gpu_layers = -1,
    embedding = TRUE
  )
)

# Insert and retrieve documents as usual
ragnar_store_insert(store, documents)
ragnar_retrieve(store, "search query")
```

### Backend and Device Selection

```r
# List available devices
llama_backend_devices()
#>         name           description  type
#> 1 CPU        CPU (threads: 16)      cpu
#> 2 Vulkan0    NVIDIA GeForce RTX 4090 gpu

# Load model on specific device
model <- llama_load_model("model.gguf", n_gpu_layers = -1, devices = "Vulkan0")

# CPU-only (even if GPU is available)
model <- llama_load_model("model.gguf", devices = "cpu")

# Multi-GPU split
model <- llama_load_model("model.gguf", n_gpu_layers = -1,
                          devices = c("Vulkan0", "Vulkan1"))
```

### LoRA Adapters

```r
model <- llama_load_model("base-model.gguf")
ctx <- llama_new_context(model)

# Load and apply adapter
lora <- llama_lora_load(model, "fine-tuned.gguf")
llama_lora_apply(ctx, lora, scale = 1.0)

# Generate with LoRA
result <- llama_generate(ctx, "prompt")

# Remove all LoRA adapters
llama_lora_clear(ctx)
```

### Verbosity Control

```r
# Levels: 0 = silent, 1 = errors only, 2 = normal, 3 = verbose
llama_set_verbosity(0)  # Suppress all output
llama_set_verbosity(3)  # Debug mode
```

## Supported Models

Supports all llama.cpp compatible architectures (100+), including:

- LLaMA 1/2/3
- Mistral / Mixtral
- Qwen / Qwen2
- Gemma / Gemma 2
- Phi-2 / Phi-3
- DeepSeek
- Command-R
- and many more

Models must be in GGUF format. Convert models using llama.cpp tools.

## License

MIT

## Author

Yuri Baramykov

## Links

- [GitHub](https://github.com/Zabis13/llamaR)
- [Bug Reports](https://github.com/Zabis13/llamaR/issues)
- [ggmlR](https://github.com/Zabis13/ggmlR) - tensor operations dependency
- [llama.cpp](https://github.com/ggml-org/llama.cpp) - inference backend
