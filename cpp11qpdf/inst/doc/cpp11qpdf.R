## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----load---------------------------------------------------------------------
library(cpp11qpdf)
input_pdf <- system.file("examples", "sufganiyot.pdf", package = "cpp11qpdf")

## ----compress-----------------------------------------------------------------
output_pdf <- tempfile(fileext = ".pdf")
pdf_compress(input_pdf, output_pdf, password = "")
file.exists(output_pdf)

## ----rotate-------------------------------------------------------------------
output_pdf <- tempfile(fileext = ".pdf")
pdf_rotate_pages(input_pdf, pages = 1, angle = 90, output = output_pdf,
  password = "")
file.exists(output_pdf)

## ----split--------------------------------------------------------------------
output_prefix <- tempfile()
output_files <- pdf_split(input_pdf, output_prefix, password = "")
output_files

## ----combine------------------------------------------------------------------
# using the output files from the previous example
output_pdf <- tempfile(fileext = ".pdf")
pdf_combine(output_files, output_pdf, password = "")
file.exists(output_pdf)

## ----subset-------------------------------------------------------------------
output_pdf <- tempfile(fileext = ".pdf")
pdf_subset(input_pdf, 1, output_pdf, password = "")
file.exists(output_pdf)

## ----overlay------------------------------------------------------------------
stamp_pdf <- system.file("examples", "header.pdf", package = "cpp11qpdf")
output_pdf <- tempfile(fileext = ".pdf")
pdf_overlay_stamp(input_pdf, stamp_pdf, output_pdf, password = "")
file.exists(output_pdf)

## ----preview, echo = FALSE----------------------------------------------------
knitr::include_graphics("added-header.png")

