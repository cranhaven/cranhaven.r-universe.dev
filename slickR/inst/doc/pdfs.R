## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  eval = pdftools::poppler_config()$can_render,
  comment = "#>"
)

knitr::opts_knit$set(root.dir = tempdir())

## ----setup--------------------------------------------------------------------
library(slickR)

## -----------------------------------------------------------------------------

pdf_file <- system.file('examples/slickR.pdf',package = 'slickR')

imgs <- pdftools::pdf_convert(pdf_file,format = 'png',verbose = FALSE)


## -----------------------------------------------------------------------------
slickR(imgs,height = 500)

## -----------------------------------------------------------------------------
bottom_opts <- settings(arrows = FALSE,slidesToShow = 3,slidesToScroll = 1,centerMode = TRUE, focusOnSelect = TRUE,initialSlide = 0)

slickR(imgs,height = 500) %synch% (slickR(imgs,height = 100) + bottom_opts)


