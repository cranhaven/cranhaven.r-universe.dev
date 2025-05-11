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

## ----noteworthy---------------------------------------------------------------
x <- "a, r b,*2 ce_g cd#g"
noteworthy(x)
noteworthy("h")

## ----as_noteworthy------------------------------------------------------------
x <- "a# b_*2 c, d'' e3*2 g_4 c2e_2g2*2"
x <- as_noteworthy(x)
x

summary(x)

## ----noteworthy2--------------------------------------------------------------
x <- as_noteworthy(x, format = "vector", octaves = "tick", accidentals = "flat")
x

summary(x)

## ----noteworthy3--------------------------------------------------------------
x <- "a, r b,*2 ce_g cd#g HELLO_WORLD"
is_note(x)
is_chord(x)

## ----notable------------------------------------------------------------------
p1 <- phrase("b, c d ec'g'~ ec'g'", "4( 4)- 2*3", "5*3 432*2")
p1

x <- as.character(p1)
phrasey(x)
identical(as_phrase(x), p1)

notable(p1) # safe logical check
notify(p1)

p2 <- p(phrase_notes(p1), phrase_info(p1), phrase_strings(p1))
identical(p1, p2)

