# SafeMapper <a href="https://zaoqu-liu.github.io/SafeMapper/"><img src="https://img.shields.io/badge/docs-pkgdown-blue.svg" alt="pkgdown" align="right" height="20"/></a> <img src="https://img.shields.io/badge/R-276DC3?logo=r&logoColor=white" align="right" height="20"/>

<!-- badges: start -->
[![R-CMD-check](https://github.com/Zaoqu-Liu/SafeMapper/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/Zaoqu-Liu/SafeMapper/actions/workflows/R-CMD-check.yaml)
[![CRAN status](https://www.r-pkg.org/badges/version/SafeMapper)](https://CRAN.R-project.org/package=SafeMapper)
[![r-universe](https://zaoqu-liu.r-universe.dev/badges/SafeMapper)](https://zaoqu-liu.r-universe.dev/SafeMapper)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
<!-- badges: end -->

**Fault-Tolerant Functional Programming with Automatic Checkpointing**

> *Never lose your computational progress again.*

<p align="center">
  <a href="https://zaoqu-liu.github.io/SafeMapper/articles/quick-start.html"><strong>Quick Start</strong></a> •
  <a href="https://zaoqu-liu.github.io/SafeMapper/reference/index.html"><strong>Reference</strong></a> •
  <a href="https://zaoqu-liu.github.io/SafeMapper/articles/"><strong>Tutorials</strong></a> •
  <a href="https://zaoqu-liu.github.io/SafeMapper/news/index.html"><strong>Changelog</strong></a>
</p>

---

## 🎯 The Problem

Long-running computations in R are vulnerable to interruptions:

```r
# Processing 10,000 API calls...
result <- purrr::map(urls, fetch_data)
# ❌ Crashes at item 9,847 after 3 hours
# ❌ All progress lost
# ❌ Must restart from scratch
```

**Common failure scenarios:**
- R session crashes or runs out of memory
- Network timeouts during API calls
- System restarts or power failures
- Accidental interruption (Ctrl+C)

## ✅ The Solution

SafeMapper provides **drop-in replacements** for [`purrr`](https://purrr.tidyverse.org/) and [`furrr`](https://furrr.futureverse.org/) functions with automatic checkpoint-based recovery:

```r
# Same code, but fault-tolerant
result <- s_map(urls, fetch_data)
# ⚡ Crashes at item 9,847...

# Just re-run the same code:
result <- s_map(urls, fetch_data)
# ✅ "Resuming from checkpoint: 9800/10000 items completed"
# ✅ Continues from where it left off
# ✅ No configuration needed
```

---

## 📦 Installation

```r
# From r-universe (recommended)
install.packages("SafeMapper", repos = "https://zaoqu-liu.r-universe.dev")

# From GitHub
devtools::install_github("Zaoqu-Liu/SafeMapper")
```

## 🚀 Quick Start

```r
library(SafeMapper)

# Replace purrr::map() with s_map() - that's it!
results <- s_map(1:1000, function(x) {
  Sys.sleep(0.1)  # Simulate slow operation
  x^2
})

# If interrupted, just re-run - automatic recovery!
```

📖 **See full tutorial**: [Quick Start Guide](https://zaoqu-liu.github.io/SafeMapper/articles/quick-start.html)

---

## ⭐ Key Features

| Feature | Description | Learn More |
|---------|-------------|------------|
| **Zero Configuration** | Works out of the box - no setup required | [Quick Start](https://zaoqu-liu.github.io/SafeMapper/articles/quick-start.html) |
| **Automatic Recovery** | Detects previous runs and resumes automatically | [Core Concepts](https://zaoqu-liu.github.io/SafeMapper/articles/core-concepts.html) |
| **Drop-in Replacement** | Same API as `purrr` and `furrr` | [Map Functions](https://zaoqu-liu.github.io/SafeMapper/articles/map-functions.html) |
| **Transparent Checkpointing** | Progress saved at configurable intervals | [Session Management](https://zaoqu-liu.github.io/SafeMapper/articles/session-management.html) |
| **Parallel Support** | Full `furrr` compatibility for parallel processing | [Parallel Processing](https://zaoqu-liu.github.io/SafeMapper/articles/parallel-processing.html) |
| **Robust Error Handling** | Built-in retry and error capture | [Error Handling](https://zaoqu-liu.github.io/SafeMapper/articles/error-handling.html) |

---

## 🔧 Function Reference

### Sequential Processing ([purrr](https://purrr.tidyverse.org/) replacements)

| SafeMapper | purrr | Returns | Docs |
|------------|-------|---------|------|
| [`s_map()`](https://zaoqu-liu.github.io/SafeMapper/reference/s_map.html) | `map()` | list | [📖](https://zaoqu-liu.github.io/SafeMapper/reference/s_map.html) |
| `s_map_chr()` | `map_chr()` | character | [📖](https://zaoqu-liu.github.io/SafeMapper/reference/s_map.html) |
| `s_map_dbl()` | `map_dbl()` | numeric | [📖](https://zaoqu-liu.github.io/SafeMapper/reference/s_map.html) |
| `s_map_int()` | `map_int()` | integer | [📖](https://zaoqu-liu.github.io/SafeMapper/reference/s_map.html) |
| `s_map_lgl()` | `map_lgl()` | logical | [📖](https://zaoqu-liu.github.io/SafeMapper/reference/s_map.html) |
| `s_map_dfr()` | `map_dfr()` | data.frame (row-bind) | [📖](https://zaoqu-liu.github.io/SafeMapper/reference/s_map.html) |
| `s_map_dfc()` | `map_dfc()` | data.frame (col-bind) | [📖](https://zaoqu-liu.github.io/SafeMapper/reference/s_map.html) |
| [`s_map2()`](https://zaoqu-liu.github.io/SafeMapper/reference/s_map2.html) | `map2()` | list (two inputs) | [📖](https://zaoqu-liu.github.io/SafeMapper/reference/s_map2.html) |
| [`s_pmap()`](https://zaoqu-liu.github.io/SafeMapper/reference/s_pmap.html) | `pmap()` | list (multiple inputs) | [📖](https://zaoqu-liu.github.io/SafeMapper/reference/s_pmap.html) |
| [`s_imap()`](https://zaoqu-liu.github.io/SafeMapper/reference/s_imap.html) | `imap()` | list (with index) | [📖](https://zaoqu-liu.github.io/SafeMapper/reference/s_imap.html) |
| [`s_walk()`](https://zaoqu-liu.github.io/SafeMapper/reference/s_walk.html) | `walk()` | side effects | [📖](https://zaoqu-liu.github.io/SafeMapper/reference/s_walk.html) |

### Parallel Processing ([furrr](https://furrr.futureverse.org/) replacements)

| SafeMapper | furrr | Docs |
|------------|-------|------|
| [`s_future_map()`](https://zaoqu-liu.github.io/SafeMapper/reference/s_future_map.html) | `future_map()` | [📖](https://zaoqu-liu.github.io/SafeMapper/reference/s_future_map.html) |
| [`s_future_map2()`](https://zaoqu-liu.github.io/SafeMapper/reference/s_future_map2.html) | `future_map2()` | [📖](https://zaoqu-liu.github.io/SafeMapper/reference/s_future_map2.html) |
| `s_future_pmap()` | `future_pmap()` | [📖](https://zaoqu-liu.github.io/SafeMapper/reference/s_pmap.html) |
| `s_future_walk()` | `future_walk()` | [📖](https://zaoqu-liu.github.io/SafeMapper/reference/s_walk.html) |
| `s_future_imap()` | `future_imap()` | [📖](https://zaoqu-liu.github.io/SafeMapper/reference/s_imap.html) |

*All variants (`_chr`, `_dbl`, `_int`, `_lgl`, `_dfr`, `_dfc`) are supported.*

### Error Handling

| SafeMapper | purrr | Description | Docs |
|------------|-------|-------------|------|
| [`s_safely()`](https://zaoqu-liu.github.io/SafeMapper/reference/s_safely.html) | `safely()` | Capture errors | [📖](https://zaoqu-liu.github.io/SafeMapper/reference/s_safely.html) |
| [`s_possibly()`](https://zaoqu-liu.github.io/SafeMapper/reference/s_possibly.html) | `possibly()` | Return default on error | [📖](https://zaoqu-liu.github.io/SafeMapper/reference/s_possibly.html) |
| [`s_quietly()`](https://zaoqu-liu.github.io/SafeMapper/reference/s_quietly.html) | `quietly()` | Capture messages/warnings | [📖](https://zaoqu-liu.github.io/SafeMapper/reference/s_quietly.html) |

📚 **Full API Reference**: [Reference Index](https://zaoqu-liu.github.io/SafeMapper/reference/index.html)

---

## ⚙️ Configuration

```r
# Optional: customize settings
s_configure(

  batch_size = 100,      # Items per checkpoint (default: 100)
  retry_attempts = 3     # Retry failed batches (default: 3)
)

# Clean old checkpoint files
s_clean_sessions(older_than_days = 7)
```

📖 **Learn more**: [Session Management Guide](https://zaoqu-liu.github.io/SafeMapper/articles/session-management.html)

---

## 🔄 How It Works

```
┌─────────────────────────────────────────────────────────────┐
│                    First Execution                          │
├─────────────────────────────────────────────────────────────┤
│  Input Data  ──►  Fingerprint  ──►  Process Batches        │
│     [1:1000]       "abc123..."      [1-100] ✓ checkpoint   │
│                                     [101-200] ✓ checkpoint │
│                                     [201-300] ✗ CRASH!     │
└─────────────────────────────────────────────────────────────┘

┌─────────────────────────────────────────────────────────────┐
│                    Re-execution                             │
├─────────────────────────────────────────────────────────────┤
│  Input Data  ──►  Fingerprint  ──►  Find Checkpoint        │
│     [1:1000]       "abc123..."      "200 items completed"  │
│                                                             │
│                    Resume from 201  ──►  Complete!         │
└─────────────────────────────────────────────────────────────┘
```

1. **Fingerprinting**: Each task is identified by a hash of input data characteristics
2. **Checkpointing**: Results are saved to disk at batch intervals
3. **Recovery**: On re-run, matching fingerprints trigger automatic restoration
4. **Cleanup**: Checkpoints are removed after successful completion

📖 **Deep dive**: [Core Concepts & Architecture](https://zaoqu-liu.github.io/SafeMapper/articles/core-concepts.html)

---

## 💼 Use Cases

| Use Case | Description | Example |
|----------|-------------|---------|
| **API Data Collection** | Web scraping, REST API calls with rate limits | [View Example](https://zaoqu-liu.github.io/SafeMapper/articles/real-world-examples.html#example-1-web-api-data-collection) |
| **File Processing** | ETL pipelines, batch transformations | [View Example](https://zaoqu-liu.github.io/SafeMapper/articles/real-world-examples.html#example-2-batch-file-processing) |
| **Machine Learning** | Cross-validation, hyperparameter tuning | [View Example](https://zaoqu-liu.github.io/SafeMapper/articles/real-world-examples.html#example-3-machine-learning-cross-validation) |
| **Web Scraping** | Extracting data from thousands of pages | [View Example](https://zaoqu-liu.github.io/SafeMapper/articles/real-world-examples.html#example-4-web-scraping-pipeline) |
| **Bioinformatics** | Processing large genomic datasets | [View Example](https://zaoqu-liu.github.io/SafeMapper/articles/real-world-examples.html#example-5-parallel-bioinformatics-pipeline) |
| **Database Migration** | Moving data between systems | [View Example](https://zaoqu-liu.github.io/SafeMapper/articles/real-world-examples.html#example-6-database-migration) |

📖 **All examples**: [Real-World Examples](https://zaoqu-liu.github.io/SafeMapper/articles/real-world-examples.html)

---

## 📚 Documentation & Tutorials

### Getting Started

| Tutorial | Description | Time |
|----------|-------------|------|
| [🚀 Quick Start](https://zaoqu-liu.github.io/SafeMapper/articles/quick-start.html) | Get up and running | 5 min |
| [🧠 Core Concepts](https://zaoqu-liu.github.io/SafeMapper/articles/core-concepts.html) | Understand the architecture | 15 min |

### Function Guides

| Tutorial | Description | Level |
|----------|-------------|-------|
| [🗺️ Map Functions](https://zaoqu-liu.github.io/SafeMapper/articles/map-functions.html) | Complete guide to all s_map variants | Intermediate |
| [⚡ Parallel Processing](https://zaoqu-liu.github.io/SafeMapper/articles/parallel-processing.html) | Speed up with s_future_map | Intermediate |
| [🛡️ Error Handling](https://zaoqu-liu.github.io/SafeMapper/articles/error-handling.html) | s_safely, s_possibly, s_quietly | Intermediate |
| [📋 Session Management](https://zaoqu-liu.github.io/SafeMapper/articles/session-management.html) | Configure and manage checkpoints | Intermediate |

### Advanced Topics

| Tutorial | Description | Level |
|----------|-------------|-------|
| [🎯 Real-World Examples](https://zaoqu-liu.github.io/SafeMapper/articles/real-world-examples.html) | Complete production examples | Advanced |
| [🏆 Best Practices](https://zaoqu-liu.github.io/SafeMapper/articles/best-practices.html) | Production patterns & anti-patterns | Advanced |

### Quick Links

- 📖 **[Full Documentation](https://zaoqu-liu.github.io/SafeMapper/)**
- 📚 **[All Tutorials](https://zaoqu-liu.github.io/SafeMapper/articles/)**
- 🔧 **[Function Reference](https://zaoqu-liu.github.io/SafeMapper/reference/index.html)**
- 📰 **[Changelog](https://zaoqu-liu.github.io/SafeMapper/news/index.html)**

---

## 🤝 Contributing

Contributions are welcome! Please feel free to submit a [Pull Request](https://github.com/Zaoqu-Liu/SafeMapper/pulls) or open an [Issue](https://github.com/Zaoqu-Liu/SafeMapper/issues).

## 👤 Author

**Zaoqu Liu**  
- 📧 Email: liuzaoqu@163.com  
- 🐙 GitHub: [@Zaoqu-Liu](https://github.com/Zaoqu-Liu)
- 🔬 ORCID: [0000-0002-0452-742X](https://orcid.org/0000-0002-0452-742X)

## 📄 License

MIT © 2026 Zaoqu Liu

---

<p align="center">
  <a href="https://zaoqu-liu.github.io/SafeMapper/">Documentation</a> •
  <a href="https://github.com/Zaoqu-Liu/SafeMapper/issues">Report Bug</a> •
  <a href="https://github.com/Zaoqu-Liu/SafeMapper/issues">Request Feature</a>
</p>
