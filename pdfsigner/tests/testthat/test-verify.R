test_that("an unsigned PDF reports no signatures", {
  expect_length(verify_pdf_signature(sample_pdf()), 0)
})

test_that("a tampered signed document fails verification", {
  out <- tempfile(fileext = ".pdf")
  on.exit(unlink(out), add = TRUE)

  sign_pdf(sample_pdf(), out, keystore(), keystore_pw)
  expect_true(verify_pdf_signature(out)[[1]]$valid)

  # Flip a byte inside the first signed segment to break the digest.
  raw <- readBin(out, "raw", file.size(out))
  raw[200] <- as.raw(bitwXor(as.integer(raw[200]), 0xFFL))
  writeBin(raw, out)

  expect_false(verify_pdf_signature(out)[[1]]$valid)
})
