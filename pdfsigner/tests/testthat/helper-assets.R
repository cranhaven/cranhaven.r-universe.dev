# Shared fixtures: a tiny sample PDF and a self-signed PKCS#12 keystore
# (password "password"), both shipped under inst/extdata.
sample_pdf <- function() system.file("extdata", "sample.pdf", package = "pdfsigner")
keystore   <- function() system.file("extdata", "keystore.p12", package = "pdfsigner")
keystore_pw <- "password"
