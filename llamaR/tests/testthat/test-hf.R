test_that("llama_hf_cache_dir returns a valid path", {
  d <- llama_hf_cache_dir()
  expect_type(d, "character")
  expect_true(nzchar(d))
  expect_true(dir.exists(d))
})

test_that(".hf_format_size formats correctly", {
  expect_equal(llamaR:::.hf_format_size(0), "0 B")
  expect_equal(llamaR:::.hf_format_size(1024), "1 KB")
  expect_equal(llamaR:::.hf_format_size(NA), "unknown")
  expect_match(llamaR:::.hf_format_size(1.5e9), "GB")
})

test_that(".hf_parse_quant extracts quantization", {
  expect_equal(llamaR:::.hf_parse_quant("model-q4_k_m.gguf"), "Q4_K_M")
  expect_equal(llamaR:::.hf_parse_quant("model-Q5_K_S.gguf"), "Q5_K_S")
  expect_true(is.na(llamaR:::.hf_parse_quant("model.gguf")))
})

test_that(".glob_to_regex converts patterns correctly", {
  rx <- llamaR:::.glob_to_regex("*q4_k_m*")
  expect_true(grepl(rx, "model-q4_k_m.gguf", ignore.case = TRUE))
  expect_false(grepl(rx, "model-q5_k_s.gguf", ignore.case = TRUE))
})

test_that(".validate_gguf rejects non-GGUF files", {
  tmp <- tempfile()
  on.exit(unlink(tmp))
  writeBin(charToRaw("NOT_GGUF"), tmp)
  expect_error(llamaR:::.validate_gguf(tmp), "not a valid GGUF")
})

test_that(".validate_gguf accepts GGUF magic", {
  tmp <- tempfile()
  on.exit(unlink(tmp))
  writeBin(as.raw(c(0x47, 0x47, 0x55, 0x46, 0x00, 0x00, 0x00, 0x00)), tmp)
  expect_true(llamaR:::.validate_gguf(tmp))
})

test_that("llama_hf_download requires exactly one selector", {
  expect_error(
    llama_hf_download("org/repo"),
    "Specify exactly one"
  )
  expect_error(
    llama_hf_download("org/repo", filename = "a.gguf", pattern = "*q4*"),
    "Specify exactly one"
  )
})

test_that("llama_hf_cache_info returns data.frame", {
  info <- llama_hf_cache_info()
  expect_s3_class(info, "data.frame")
  expect_true(all(c("repo_id", "filename", "size", "path") %in% names(info)))
})

test_that("llama_hf_cache_clear handles empty cache", {
  tmp_dir <- tempfile("cache_test")
  dir.create(tmp_dir, recursive = TRUE)
  on.exit(unlink(tmp_dir, recursive = TRUE))
  expect_message(
    llama_hf_cache_clear(confirm = FALSE, cache_dir = tmp_dir),
    "empty"
  )
})
