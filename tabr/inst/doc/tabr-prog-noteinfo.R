## ----setup, include = FALSE---------------------------------------------------
options(crayon.enabled = TRUE)
sgr_wrap <- function(x, options){
  paste0("<pre class=\"r-output\"><code>", fansi::sgr_to_html(x = htmltools::htmlEscape(x)), "</code></pre>")
}
knitr::knit_hooks$set(output = sgr_wrap)
knitr::opts_chunk$set(
  collapse = TRUE, comment = "#>", message = FALSE, warning = FALSE, error = FALSE, tidy = FALSE, out.width = "100%"
)
library(tabr)

## ----informable---------------------------------------------------------------
x <- "2. 4.. 8 t8-. t8- t8^ 16"
informable(x)
informable("5")

## ----as_noteinfo--------------------------------------------------------------
x <- as_noteinfo(x)
x

summary(x)

## ----methods------------------------------------------------------------------
length(x)
rev(x)
x[2:4]
x[2:3] <- c("1(", "2)")
x

## ----funs---------------------------------------------------------------------
a <- notate("t8x", "Start here")
x <- paste(a, "t8x t8-. 16 4.. 16- 16 2^ 2 4. 8( 4)( 4) 8*4 1 1")

info_duration(x)
info_slur_on(x)
info_slur_off(x)
info_slide(x)
info_dotted(x)
info_annotation(x)

