test_that("an invisible signature round-trips and verifies", {
  out <- tempfile(fileext = ".pdf")
  on.exit(unlink(out), add = TRUE)

  res <- sign_pdf(
    pdf_file = sample_pdf(),
    output_file = out,
    keystore_path = keystore(),
    keystore_password = keystore_pw
  )

  expect_identical(res, out)
  expect_true(file.exists(out))

  sigs <- verify_pdf_signature(out)
  expect_length(sigs, 1)
  expect_true(sigs[[1]]$valid)
  expect_true(sigs[[1]]$covers_whole_document)
  expect_true(nzchar(sigs[[1]]$signer))
})

test_that("a visible signature box verifies", {
  out <- tempfile(fileext = ".pdf")
  on.exit(unlink(out), add = TRUE)

  sign_pdf(
    pdf_file = sample_pdf(),
    output_file = out,
    keystore_path = keystore(),
    keystore_password = keystore_pw,
    signtext = "Digitally signed (test)",
    validate_link = "https://example.org/validate",
    translate = TRUE
  )

  sigs <- verify_pdf_signature(out)
  expect_true(sigs[[1]]$valid)
})

test_that("an embedded font and logo still produce a valid signature", {
  out  <- tempfile(fileext = ".pdf")
  logo <- tempfile(fileext = ".png")
  on.exit(unlink(c(out, logo)), add = TRUE)

  grDevices::png(logo, width = 48, height = 48)
  graphics::par(mar = c(0, 0, 0, 0))
  graphics::plot.new()
  graphics::rect(0, 0, 1, 1, col = "navy", border = NA)
  grDevices::dev.off()

  # A TrueType font may not exist on every test machine; skip if none is found.
  font <- Sys.glob(c(
    "/System/Library/Fonts/Supplemental/Arial.ttf",
    "/usr/share/fonts/truetype/dejavu/DejaVuSans.ttf",
    "/usr/share/fonts/truetype/liberation/LiberationSans-Regular.ttf"
  ))
  font <- font[file.exists(font)][1]

  sign_pdf(
    pdf_file = sample_pdf(),
    output_file = out,
    keystore_path = keystore(),
    keystore_password = keystore_pw,
    signtext = "Signed with a logo",
    image = logo,
    font = if (is.na(font)) NULL else font
  )

  expect_true(verify_pdf_signature(out)[[1]]$valid)
})

test_that("a second signature keeps the first one valid", {
  first  <- tempfile(fileext = ".pdf")
  second <- tempfile(fileext = ".pdf")
  on.exit(unlink(c(first, second)), add = TRUE)

  sign_pdf(sample_pdf(), first, keystore(), keystore_pw)
  sign_pdf(first, second, keystore(), keystore_pw)

  sigs <- verify_pdf_signature(second)
  expect_length(sigs, 2)
  expect_true(all(vapply(sigs, function(s) s$valid, logical(1))))
})
