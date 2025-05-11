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

## ----music1-------------------------------------------------------------------
notes <- "c d e f g a b ceg~ ceg"
info <- "8*8 1"
x <- as_music(notes, info)
x

## ----music2-------------------------------------------------------------------
summary(x)

## ----music3-------------------------------------------------------------------
music_split(x)

music_notes(x)
music_info(x)
music_key(x)
music_time(x)
music_tempo(x)

## ----music4-------------------------------------------------------------------
x <- "a,4*5 b,4- c4 cgc'e'~4 cgc'e'1 e'4 c' g c ce'1"
musical(x)
x <- as_music(x)
x

## ----music5-------------------------------------------------------------------
x <- "a,4;5*5 b,4- c4 cgc'e'~4 cgc'e'1 e'4;2 c';3 g;4 c;5 ce'1;51"
x <- as_music(x)
x
music_strings(x)

## ----music 5b-----------------------------------------------------------------
summary(x)
music_split(x)

## ----music6-------------------------------------------------------------------
tail(x)
x[8:9]
y <- rep(x[9:10], each = 2)
y
music_strings(y)

## ----music7-------------------------------------------------------------------
a <- notate("t8x", "Start here")
notes <- "a, b, c d e f g# a r ac'e' a c' e' c' r*3 ac'e'~ ac'e'"
info <- paste(a, "t8x t8-. 16 4.. 16- 16 2^ 2 4. 8( 4)( 4) 8*4 1 1")
x <- as_music(notes, info)

n_measures(x)
n_beats(x)
bpm(x)

seconds(x)
steps_per_measure(x)
seconds_per_measure(x)
seconds_per_step(x)
steps_start_time(x)

