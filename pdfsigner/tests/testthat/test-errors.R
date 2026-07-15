test_that("sign_pdf validates its inputs", {
  out <- tempfile(fileext = ".pdf")

  expect_error(
    sign_pdf("does-not-exist.pdf", out, keystore(), keystore_pw),
    "does not exist"
  )
  expect_error(
    sign_pdf(sample_pdf(), out, "no-keystore.p12", keystore_pw),
    "keystore does not exist"
  )
  expect_error(
    sign_pdf(sample_pdf(), out, keystore(), ""),
    "password cannot be empty"
  )
  expect_error(
    sign_pdf(sample_pdf(), out, keystore(), keystore_pw,
             signtext = "x", font = "no-font.ttf"),
    "font file does not exist"
  )
  expect_error(
    sign_pdf(sample_pdf(), out, keystore(), keystore_pw,
             signtext = "x", image = "no-image.png"),
    "image file does not exist"
  )
})

test_that("levels above bb require a tsa_url", {
  out <- tempfile(fileext = ".pdf")
  expect_error(
    sign_pdf(sample_pdf(), out, keystore(), keystore_pw, pades_level = "bt"),
    "requires a `tsa_url`"
  )
})

test_that("verify_pdf_signature validates its inputs", {
  expect_error(verify_pdf_signature("nope.pdf"), "was not found")
  expect_error(
    verify_pdf_signature(sample_pdf(), roots = "nope.pem"),
    "roots PEM file was not found"
  )
})
